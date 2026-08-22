package backends

import backends.Verilog.*
import ir.rtl.RAMControl
import ir.rtl.hardwaretype.{ComplexHW, HW}
import maths.fields.Complex
import transforms.fft.{CTDFT, FptInverseDft, TangentTwistAfterSwitch}
import transforms.perm.SwitchTranspose

/** Rate-preserving FPT tangent FFT for laneLog >= cycleLog.
  * Splits the K-wide input into K/C independent CxC switch-transpose blocks.
  * Each block has its own correctly strided tangent-twiddle path, and the
  * reverse blocks are packed back into the original K-wide FFT input.
  */
object FptParallelSwitchTangentVerilog:
  private def renameTop(rtl: String, name: String): String = rtl.replaceFirst("module main\\(", s"module $name(")

  def emit(n: Int, r: Int, laneLog: Int, hw: ComplexHW[Double], scalingFactor: Complex[Double], inverse: Boolean = false, top: String = "main"): String =
    val cycleLog = n - laneLog
    require(cycleLog >= 0 && laneLog >= cycleLog && laneLog < n)
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"))
    given HW[Complex[Double]] = hw
    val blockLog = cycleLog
    val blockWidth = 1 << blockLog
    val blocks = 1 << (laneLog - blockLog)
    val inputLanes = 1 << laneLog
    val width = hw.size
    val switchName = s"${top}SquareSwitch"
    val switchRtl = renameTop(SwitchTranspose[Complex[Double]](blockLog).stream(blockLog, RAMControl.Single).toVerilog, switchName)
    val twistRtls = (0 until blocks).map { block =>
      val twist = TangentTwistAfterSwitch(n, blockLog, inverse = inverse, laneStride = blocks, cycleOffset = block * blockWidth, cycleLog = blockLog)
      renameTop(twist.stream(blockLog, RAMControl.Single).toVerilog, s"${top}Twist$block")
    }.mkString("\n")
    val fftName = s"${top}${if inverse then "Inverse" else "Forward"}Core"
    val fftRtl = if inverse then renameTop(FptInverseDft.spl(n, r, scalingFactor).stream(laneLog, RAMControl.Single).toVerilog, fftName)
      else renameTop(CTDFT(n, r, scalingFactor).stream(laneLog, RAMControl.Single).toVerilog, fftName)
    val inputs = Vector.tabulate(inputLanes)(lane => s"input [${width - 1}:0] i$lane")
    val outputs = Vector.tabulate(inputLanes)(lane => s"output [${width - 1}:0] o$lane")
    val wires = (0 until blocks).flatMap { block =>
      Vector(s"wire pre_next_$block,twist_next_$block,post_next_$block;") ++
        Vector.tabulate(blockWidth)(lane => s"wire [${width - 1}:0] pre_${block}_$lane,twist_${block}_$lane,post_${block}_$lane;")
    }
    def connections(prefix: String, values: String => String): Vector[String] =
      Vector.tabulate(blockWidth)(lane => s".$prefix$lane(${values(lane.toString)})")
    val blocksRtl = (0 until blocks).flatMap { block =>
      val preAt = (lane: String) => s"pre_${block}_$lane"
      val twistAt = (lane: String) => s"twist_${block}_$lane"
      val postAt = (lane: String) => s"post_${block}_$lane"
      val blockInputs = if inverse then Vector.tabulate(blockWidth)(lane => s".i$lane(fft_${block}_$lane)") else Vector.tabulate(blockWidth)(lane => s".i$lane(i${block * blockWidth + lane})")
      Vector(
        s"$switchName pre_$block(.clk(clk),.reset(reset),.next(${if inverse then "fft_next" else "input_token"}),.next_out(pre_next_$block),${blockInputs.mkString(",")},${connections("o", preAt).mkString(",")});",
        s"${top}Twist$block twist_$block(.clk(clk),.reset(reset),.next(pre_next_$block),.next_out(twist_next_$block),${connections("i", preAt).mkString(",")},${connections("o", twistAt).mkString(",")});",
        s"$switchName post_$block(.clk(clk),.reset(reset),.next(twist_next_$block),.next_out(post_next_$block),${connections("i", twistAt).mkString(",")},${connections("o", postAt).mkString(",")});"
      )
    }.mkString("\n  ")
    val fftInputs = Vector.tabulate(inputLanes) { lane =>
      val block = lane / blockWidth
      val local = lane % blockWidth
      s".i$lane(post_delay1_$lane)"
    }
    val fftOutputs = Vector.tabulate(inputLanes)(lane => if inverse then s".o$lane(fft_${lane / blockWidth}_${lane % blockWidth})" else s".o$lane(o$lane)")
    val blockNext = (0 until blocks).map(block => s"post_next_$block").mkString("&")
    val fftInstance =
      if inverse then s"$fftName fft(.clk(clk),.reset(reset),.next(next),.next_out(fft_next),${Vector.tabulate(inputLanes)(lane => s".i$lane(i$lane)").mkString(",")},${fftOutputs.mkString(",")});"
      else s"$fftName fft(.clk(clk),.reset(reset),.next($blockNext),.next_out(next_out),${fftInputs.mkString(",")},${fftOutputs.mkString(",")});"
    val inverseWires = if inverse then Vector("wire fft_next;") ++ Vector.tabulate(blocks)(block => Vector.tabulate(blockWidth)(lane => s"wire [${width - 1}:0] fft_${block}_$lane;")).flatten else Vector.empty
    val nextAssignment = if inverse then s"assign next_out=$blockNext;" else ""
    val outputAssignments = if inverse then Vector.tabulate(inputLanes) { lane =>
      val block = lane / blockWidth
      val local = lane % blockWidth
      s"assign o$lane=post_${block}_$local;"
    }.mkString("\n  ") else ""
    // FPT streaming cores require their next marker two cycles before the
    // first input vector. post_next marks the first reassembled vector, so
    // delay the vector path rather than moving the marker late.
    val postDelayDeclarations = if inverse then "" else Vector.tabulate(inputLanes)(lane => s"reg [${width - 1}:0] post_delay0_$lane,post_delay1_$lane;").mkString(" ")
    val postDelayLogic = if inverse then "" else Vector.tabulate(inputLanes) { lane =>
      val block = lane / blockWidth
      val local = lane % blockWidth
      s"post_delay0_$lane<=post_${block}_$local;post_delay1_$lane<=post_delay0_$lane;"
    }.mkString(" ")
    val inputTokenLogic = if inverse then "" else
      "reg [1:0] input_token_pipe;wire input_token=input_token_pipe[1];always @(posedge clk)begin if(reset)input_token_pipe<=0;else input_token_pipe<={input_token_pipe[0],next};end"
    s"""// Generated by SGen rate-preserving parallel switch FPT tangent ${if inverse then "inverse FFT" else "FFT"}.
       |// $blocks parallel ${blockWidth}x${blockWidth} switch/twist blocks retain $inputLanes external lanes.
       |$switchRtl
       |$twistRtls
       |$fftRtl
       |module $top(input clk,input reset,input next,output next_out,
       |${(inputs ++ outputs).map("  " + _).mkString(",\n")}
       |);
       |${(wires ++ inverseWires).map("  " + _).mkString("\n")}
       |  $inputTokenLogic
       |  $postDelayDeclarations
       |  always @(posedge clk) begin if(!reset) begin $postDelayLogic end end
       |  $blocksRtl
       |  $fftInstance
       |  $nextAssignment
       |  $outputAssignments
       |endmodule
       |""".stripMargin
