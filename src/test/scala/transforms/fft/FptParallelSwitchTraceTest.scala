package transforms.fft

import backends.{FptParallelSwitchTraceVerilog, FptSquareTransposeVerilog}
import ir.rtl.hardwaretype.{ComplexHW, FixedPoint}
import maths.fields.Complex
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import scala.sys.process.*

class FptParallelSwitchTraceTest extends AnyFunSuite:
  private def toolAvailable: Boolean = Process(Seq("sh", "-c", "command -v iverilog >/dev/null && command -v vvp >/dev/null")).! == 0

  test("exact square FPT transpose matches the required cycle/lane transpose"):
    assume(toolAvailable)
    val hw = ComplexHW(FixedPoint(8, 12))
    val rtl = FptSquareTransposeVerilog.emit(2, hw.size, "trace")
    val width = hw.size
    val tb = s"""module tb;
       |  reg clk=0,reset=1,valid_in=0;wire valid_out;integer warmup=0,drive_cycle=0,out_frame=0,out_cycle=0,out_remaining=0,expected;
       |  reg [${width - 1}:0] i0,i1,i2,i3;wire [${width - 1}:0] o0,o1,o2,o3;
       |  wire [${width * 4 - 1}:0] packed_in;wire [${width * 4 - 1}:0] packed_out;assign packed_in={i3,i2,i1,i0};
       |  trace dut(.clk(clk),.reset(reset),.next(valid_in),.next_out(valid_out),.i0(i0),.i1(i1),.i2(i2),.i3(i3),.o0(o0),.o1(o1),.o2(o2),.o3(o3));
       |  always #5 clk=~clk;
       |  always @(negedge clk) begin
       |    if(reset)begin valid_in=0;i0=0;i1=0;i2=0;i3=0;end
       |    else if(warmup<10)begin warmup=warmup+1;valid_in=0;end
       |    else if(drive_cycle<16)begin valid_in=(drive_cycle%4)==0;i0=(drive_cycle/4)*1000+(drive_cycle%4)*4;i1=i0+1;i2=i0+2;i3=i0+3;drive_cycle=drive_cycle+1;end else valid_in=0;
       |    if(valid_out||out_remaining>0)begin
       |      expected=out_frame*1000+0*4+out_cycle;if(o0!==expected)$$fatal;
       |      expected=out_frame*1000+1*4+out_cycle;if(o1!==expected)$$fatal;
       |      expected=out_frame*1000+2*4+out_cycle;if(o2!==expected)$$fatal;
       |      expected=out_frame*1000+3*4+out_cycle;if(o3!==expected)$$fatal;
       |      if(valid_out)out_remaining=3;else out_remaining=out_remaining-1;
       |      if(out_cycle==3)begin out_cycle=0;out_frame=out_frame+1;end else out_cycle=out_cycle+1;
       |    end
       |  end
       |  initial begin #12 reset=0;#1200;if(out_frame!=4)$$fatal;$$display(\"PASS square transpose mapping\");$$finish;end
       |endmodule
       |""".stripMargin
    val directory = Files.createTempDirectory("sgen-square-switch-")
    val source = directory.resolve("square.v")
    val executable = directory.resolve("square.out")
    Files.writeString(source, rtl + "\n" + tb)
    assert(Process(Seq("iverilog", "-g2012", "-s", "tb", "-o", executable.toString, source.toString)).! == 0)
    assert(Process(Seq("vvp", executable.toString)).! == 0)

  private def checkTrace(n: Int, laneLog: Int, frames: Int): Unit =
    assume(toolAvailable)
    val hw = ComplexHW(FixedPoint(8, 12))
    val lanes = 1 << laneLog
    val cycles = 1 << (n - laneLog)
    val width = hw.size
    val rtl = FptParallelSwitchTraceVerilog.emit(n, laneLog, hw)
    val inputs = Vector.tabulate(lanes)(lane => s"reg [${width - 1}:0] i$lane;").mkString(" ")
    val outputs = Vector.tabulate(lanes)(lane => s"wire [${width - 1}:0] o$lane;").mkString(" ")
    val connectionsIn = Vector.tabulate(lanes)(lane => s".i$lane(i$lane)").mkString(",")
    val connectionsOut = Vector.tabulate(lanes)(lane => s".o$lane(o$lane)").mkString(",")
    val resetInputs = Vector.tabulate(lanes)(lane => s"i$lane=0;").mkString(" ")
    val drive = Vector.tabulate(lanes)(lane => s"i$lane=((drive_tick-2)/$cycles)*1000+((drive_tick-2)%$cycles)*$lanes+$lane;").mkString(" ")
    val check = Vector.tabulate(lanes) { lane =>
      s"if(o$lane!==(out_frame*1000+out_cycle*$lanes+$lane))begin $$display(\"tag mismatch frame=%0d cycle=%0d lane=$lane got=%0d expected=%0d\",out_frame,out_cycle,o$lane,out_frame*1000+out_cycle*$lanes+$lane);$$fatal;end"
    }.mkString(" ")
    val tb = s"""module tb;
       |  reg clk=0,reset=1,next=0;wire next_out;integer warmup=0,drive_tick=0,out_frame=0,out_cycle=0,out_remaining=0,lane,linear,source_lane,source_cycle;
       |  $inputs $outputs
       |  trace dut(.clk(clk),.reset(reset),.next(next),.next_out(next_out),$connectionsIn,$connectionsOut);
       |  always #5 clk=~clk;
       |  always @(negedge clk) begin
       |    if(reset)begin next=0;$resetInputs end
       |    else if(warmup<20)begin warmup=warmup+1;next=0;end
       |    else if(drive_tick<${frames * cycles + 2})begin next=(drive_tick<${frames * cycles})&&((drive_tick%$cycles)==0);if(drive_tick>=2)begin $drive end drive_tick=drive_tick+1;end
       |    else next=0;
       |    if(next_out||out_remaining>0)begin
       |      $check
       |      if(next_out)out_remaining=$cycles-1;else out_remaining=out_remaining-1;
       |      if(out_cycle==$cycles-1)begin out_cycle=0;out_frame=out_frame+1;end else out_cycle=out_cycle+1;
       |    end
       |  end
       |  initial begin #12 reset=0;#${1000 + frames * cycles * 20};if(out_frame!=$frames)begin $$display(\"missing output frames=%0d\",out_frame);$$fatal;end $$display(\"PASS switch trace\");$$finish;end
       |endmodule
       |""".stripMargin
    val directory = Files.createTempDirectory("sgen-switch-trace-")
    val source = directory.resolve("trace.v")
    val executable = directory.resolve("trace.out")
    Files.writeString(source, rtl + "\n" + tb)
    assert(Process(Seq("iverilog", "-g2012", "-s", "tb", "-o", executable.toString, source.toString)).! == 0)
    assert(Process(Seq("vvp", executable.toString)).! == 0)

  test("parallel switch path preserves consecutive tagged frames at FPT widths"):
    checkTrace(5, 3, 4)
    checkTrace(9, 5, 4)
    checkTrace(9, 6, 4)
