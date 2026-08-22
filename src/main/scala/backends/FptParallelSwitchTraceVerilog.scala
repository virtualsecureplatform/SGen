package backends

import backends.Verilog.*
import ir.rtl.RAMControl
import ir.rtl.hardwaretype.{ComplexHW, HW}
import maths.fields.Complex
import transforms.perm.SwitchTranspose

/** Tagged-data diagnostic for the parallel rate-preserving switch path.
  * It contains only the pre/post square switch blocks; therefore its external
  * mapping must be identity for every frame and exposes any cross-frame error.
  */
object FptParallelSwitchTraceVerilog:
  private def renameTop(rtl: String, name: String): String = rtl.replaceFirst("module main\\(", s"module $name(")

  def emit(n: Int, laneLog: Int, hw: ComplexHW[Double], top: String = "trace"): String =
    val cycleLog = n - laneLog
    require(cycleLog >= 0 && laneLog >= cycleLog && laneLog < n)
    given HW[Complex[Double]] = hw
    val blockWidth = 1 << cycleLog
    val blocks = 1 << (laneLog - cycleLog)
    val lanes = 1 << laneLog
    val width = hw.size
    val switchName = s"${top}SquareSwitch"
    val switchRtl = renameTop(SwitchTranspose[Complex[Double]](cycleLog).stream(cycleLog, RAMControl.Single).toVerilog, switchName)
    val portsIn = Vector.tabulate(lanes)(lane => s"input [${width - 1}:0] i$lane")
    val portsOut = Vector.tabulate(lanes)(lane => s"output [${width - 1}:0] o$lane")
    val wires = (0 until blocks).flatMap { block =>
      Vector(s"wire pre_next_$block,post_next_$block;") ++
        Vector.tabulate(blockWidth)(lane => s"wire [${width - 1}:0] pre_${block}_$lane,post_${block}_$lane;")
    }
    val instances = (0 until blocks).flatMap { block =>
      val preInputs = Vector.tabulate(blockWidth)(lane => s".i$lane(i${block * blockWidth + lane})").mkString(",")
      val preOutputs = Vector.tabulate(blockWidth)(lane => s".o$lane(pre_${block}_$lane)").mkString(",")
      val postInputs = Vector.tabulate(blockWidth)(lane => s".i$lane(pre_${block}_$lane)").mkString(",")
      val postOutputs = Vector.tabulate(blockWidth)(lane => s".o$lane(post_${block}_$lane)").mkString(",")
      Vector(
        s"$switchName pre_$block(.clk(clk),.reset(reset),.next(input_token),.next_out(pre_next_$block),$preInputs,$preOutputs);",
        s"$switchName post_$block(.clk(clk),.reset(reset),.next(pre_next_$block),.next_out(post_next_$block),$postInputs,$postOutputs);"
      )
    }.mkString("\n  ")
    val outputs = Vector.tabulate(lanes) { lane =>
      val block = lane / blockWidth
      val local = lane % blockWidth
      s"assign o$lane=post_${block}_$local;"
    }.mkString("\n  ")
    val nexts = (0 until blocks).map(block => s"post_next_$block").mkString("&")
    s"""// Generated tagged trace for $blocks parallel ${blockWidth}x${blockWidth} switch blocks.
       |$switchRtl
       |module $top(input clk,input reset,input next,output next_out,
       |${(portsIn ++ portsOut).map("  " + _).mkString(",\n")}
       |);
       |  reg [1:0] input_token_pipe;wire input_token=input_token_pipe[1];
       |${wires.map("  " + _).mkString("\n")}
       |  always @(posedge clk)begin if(reset)input_token_pipe<=0;else input_token_pipe<={input_token_pipe[0],next};end
       |  $instances
       |  assign next_out=$nexts;
       |  $outputs
       |endmodule
       |""".stripMargin
