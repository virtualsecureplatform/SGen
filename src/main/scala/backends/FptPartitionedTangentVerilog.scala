package backends

import backends.Verilog.*
import ir.rtl.RAMControl
import ir.rtl.hardwaretype.{ComplexHW, HW}
import maths.fields.Complex
import transforms.fft.{CTDFT, TangentCTDFT}

/** Emits a forward tangent FFT with an explicit, registered radix-stage cut. */
object FptPartitionedTangentVerilog:
  private def renameTop(rtl: String, name: String): String =
    rtl.replaceFirst("module main\\(", s"(* keep_hierarchy = \"yes\" *) module $name(")

  private def ports(prefix: String, signal: String, lanes: Int): String =
    Vector.tabulate(lanes)(lane => s".$prefix$lane($signal$lane)").mkString(",")

  def emit(
      n: Int,
      r: Int,
      laneLog: Int,
      hw: ComplexHW[Double],
      scalingFactor: Complex[Double],
      inputRadixStages: Int,
      spillLastStageInput: Boolean = false,
      boundaryRegisters: Int = 2,
      top: String = "main"
  ): String =
    require(boundaryRegisters == 2, "the physical SLL boundary requires two registers")
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"))
    given HW[Complex[Double]] = hw
    val lanes = 1 << laneLog
    val width = hw.size
    val (frontSpl, backSpl) =
      if spillLastStageInput then
        require(
          inputRadixStages == CTDFT.radixStages(n, r, scalingFactor).size - 1,
          "the last-stage input spill requires the final radix-stage boundary"
        )
        TangentCTDFT.partitionedSplsWithLastStageInputSpill(
          n,
          r,
          scalingFactor
        )
      else
        TangentCTDFT.partitionedSpls(
          n,
          r,
          scalingFactor,
          inputRadixStages
        )
    val front = frontSpl.stream(laneLog, RAMControl.Single)
    val back = backSpl.stream(laneLog, RAMControl.Single)
    val frontName = s"${top}Front"
    val boundaryName = s"${top}Boundary"
    val backName = s"${top}Back"
    val frontRtl = renameTop(front.toVerilog, frontName)
    val backRtl = renameTop(back.toVerilog, backName)
    val latency = front.latency + boundaryRegisters + back.latency
    val interval = Math.max(front.minGap, back.minGap) + (1 << (n - laneLog))
    val backTokenDelay = back.nextAt
    require(backTokenDelay >= -1, s"partitioned back end requires an early token by ${-backTokenDelay} cycles")
    val backTokenDeclaration =
      if backTokenDelay > 0 then
        s"(* keep = \"TRUE\", shreg_extract = \"no\" *) reg [${backTokenDelay - 1}:0] back_token_delay;"
      else ""
    val backTokenReset = if backTokenDelay > 0 then " back_token_delay <= 0;" else ""
    val backTokenShift =
      if backTokenDelay > 1 then
        s"back_token_delay <= {back_token_delay[${backTokenDelay - 2}:0],rx_next};"
      else if backTokenDelay == 1 then "back_token_delay <= rx_next;"
      else ""
    val backTokenOutput =
      if backTokenDelay > 0 then s"back_token_delay[${backTokenDelay - 1}]"
      else if backTokenDelay == 0 then "rx_next"
      else "tx_next"

    val boundaryInputs = Vector.tabulate(lanes)(lane => s"input [${width - 1}:0] i$lane")
    val boundaryOutputs = Vector.tabulate(lanes)(lane => s"output [${width - 1}:0] o$lane")
    val txRegs = Vector.tabulate(lanes)(lane =>
      s"(* USER_SLL_REG = \"TRUE\", keep = \"TRUE\", shreg_extract = \"no\" *) reg [${width - 1}:0] tx_$lane;"
    )
    val rxRegs = Vector.tabulate(lanes)(lane =>
      s"(* USER_SLL_REG = \"TRUE\", keep = \"TRUE\", shreg_extract = \"no\" *) reg [${width - 1}:0] rx_$lane;"
    )
    val dataPipeline = Vector.tabulate(lanes)(lane =>
      s"tx_$lane <= i$lane; rx_$lane <= tx_$lane;"
    )
    val outputAssignments = Vector.tabulate(lanes)(lane => s"assign o$lane = rx_$lane;")
    val boundaryRtl =
      s"""(* keep_hierarchy = "yes" *) module $boundaryName(
         |  input clk,input reset,input next,output next_out,
         |${(boundaryInputs ++ boundaryOutputs).map("  " + _).mkString(",\n")}
         |);
         |  (* USER_SLL_REG = "TRUE", keep = "TRUE", shreg_extract = "no" *) reg tx_next,rx_next;
         |  $backTokenDeclaration
         |${(txRegs ++ rxRegs).map("  " + _).mkString("\n")}
         |  always @(posedge clk) begin
         |    if (reset) begin tx_next <= 0; rx_next <= 0;$backTokenReset end
         |    else begin
         |      tx_next <= next; rx_next <= tx_next;
         |      $backTokenShift
         |    end
         |${dataPipeline.map("    " + _).mkString("\n")}
         |  end
         |  assign next_out = $backTokenOutput;
         |${outputAssignments.map("  " + _).mkString("\n")}
         |endmodule
         |""".stripMargin

    val topInputs = Vector.tabulate(lanes)(lane => s"input [${width - 1}:0] i$lane")
    val topOutputs = Vector.tabulate(lanes)(lane => s"output [${width - 1}:0] o$lane")
    val frontWires = Vector.tabulate(lanes)(lane => s"wire [${width - 1}:0] front_o$lane;")
    val boundaryWires = Vector.tabulate(lanes)(lane => s"wire [${width - 1}:0] boundary_o$lane;")
    val wrapper =
      s"""// This design operates on datasets of ${1 << n} elements, streamed over ${1 << (n - laneLog)} cycles of $lanes elements.
         |// It has a latency of $latency cycles: the output will begin $latency cycles after the input has begun.
         |// In total, this design can therefore perform a new transformation every $interval cycles.
         |// Partition timing: front latency=${front.latency} nextAt=${front.nextAt}; back latency=${back.latency} nextAt=${back.nextAt}; last-stage input spill=$spillLastStageInput.
         |(* keep_hierarchy = "yes" *) module $top(input clk,input reset,input next,output next_out,
         |${(topInputs ++ topOutputs).map("  " + _).mkString(",\n")}
         |);
         |  wire front_next,boundary_next;
         |${(frontWires ++ boundaryWires).map("  " + _).mkString("\n")}
         |  $frontName front(.clk(clk),.reset(reset),.next(next),.next_out(front_next),${ports("i", "i", lanes)},${ports("o", "front_o", lanes)});
         |  $boundaryName boundary(.clk(clk),.reset(reset),.next(front_next),.next_out(boundary_next),${ports("i", "front_o", lanes)},${ports("o", "boundary_o", lanes)});
         |  $backName back(.clk(clk),.reset(reset),.next(boundary_next),.next_out(next_out),${ports("i", "boundary_o", lanes)},${ports("o", "o", lanes)});
         |endmodule
         |""".stripMargin
    s"$frontRtl\n$boundaryRtl\n$backRtl\n$wrapper"
