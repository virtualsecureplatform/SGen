package transforms.perm

import backends.Verilog.*
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
