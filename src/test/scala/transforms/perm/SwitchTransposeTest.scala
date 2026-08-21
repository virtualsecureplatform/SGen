package transforms.perm

import backends.Verilog.*
import backends.RectangularSwitchTransposeVerilog
import ir.rtl.hardwaretype.Unsigned
import org.scalatest.funsuite.AnyFunSuiteLike

class SwitchTransposeTest extends AnyFunSuiteLike:
  test("switch transpose has square streaming semantics"):
    val transform = SwitchTranspose[Int](3)
    val input = 0 until 64
    val output = transform.eval(input, 0)
    assert(output(3 * 8 + 5) == input(5 * 8 + 3))

  test("switch transpose emits the recursive unit hierarchy"):
    val module = SwitchTranspose[Int](3).stream(3, ir.rtl.RAMControl.Single)(using Unsigned(16))
    val rtl = module.toVerilog
    assert(module.latency == 7)
    assert(rtl.contains("module SGenSwitchTransposeUnit_3_16"))
    assert(rtl.contains("module SGenSwitchTransposeNetwork_2_16"))
    assert(rtl.contains("SGenSwitchTransposeNetwork_3_16 ext_"))

  test("rectangular switch transpose changes the stream width"):
    val rtl=RectangularSwitchTransposeVerilog.emit(cycleLog=2,laneLog=3,dataWidth=16)
    assert(rtl.contains("Exchanges 4 temporal cycles with 8 spatial lanes"))
    assert(rtl.contains("input [15:0] i7"))
    assert(rtl.contains("output reg [15:0] o3"))
    assert(rtl.contains("output ready"))
    assert(rtl.contains("storage_0") && rtl.contains("storage_1"))

  test("fixed-rate rectangular transpose removes the ready port"):
    val rtl=RectangularSwitchTransposeVerilog.emit(cycleLog=2,laneLog=3,dataWidth=16,fixedRate=true)
    assert(!rtl.contains("output ready"))
    assert(rtl.contains("FRAME_INTERVAL=8,MIN_FRAME_GAP=4"))

  test("rate-preserving rectangular transpose retains the input width"):
    val rtl=RectangularSwitchTransposeVerilog.emit(cycleLog=2,laneLog=3,dataWidth=16,ratePreserving=true)
    assert(rtl.contains("output reg [15:0] o7"))
    assert(rtl.contains("FRAME_INTERVAL=4,MIN_FRAME_GAP=0"))
    assert(rtl.contains("packed/split"))
