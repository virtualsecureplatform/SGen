package ir.rtl.hardwaretype

import ir.rtl.{Component, Const as RTLConst, Input as RTLInput, Mux as RTLMux}
import ir.rtl.signals.{Input, ROM, Sig}
import org.scalatest.funsuite.AnyFunSuite

class PrecomputedRomArithmeticTest extends AnyFunSuite:
  test("small ROM arithmetic preserves quantized bits, wrapping, and two-cycle schedule"):
    val hw = FixedPoint(2, 4)
    val mask = (BigInt(1) << hw.size) - 1
    val random = new scala.util.Random(0x465054)
    for depth <- Seq(2, 4) do
      val addr = Input[Int](0)(using Unsigned(if depth == 2 then 1 else 2))
      val rtlAddr = RTLInput(addr.hw.size, "address")
      val cases = Seq(
        Vector(1.99, -1.99, 0.03, -0.03) -> Vector(1.99, 1.90, -0.03, 0.03),
        Vector(0.09, -0.09, 1.0, -1.0) -> Vector(0.09, -0.09, -1.0, 1.0)
      ) ++ Seq.fill(100)(
        Vector.tabulate(4)(i => (i / 2 + random.nextDouble() * 0.5 + 0.1) * (if i % 2 == 0 then 1 else -1)) ->
          Vector.tabulate(4)(i => (i / 2 + random.nextDouble() * 0.5 + 0.1) * (if i % 2 == 0 then 1 else -1))
      )
      for
        (allLeft, allRight) <- cases
        subtract <- Seq(false, true)
      do
        val left = allLeft.take(depth)
        val right = allRight.take(depth)
        val lhs = ROM(left, addr)(using hw)
        val rhs = ROM(right, addr)(using hw)
        val folded = hw.romArithmetic(lhs, rhs, subtract, enabled = true)
        val baseline = hw.romArithmetic(lhs, rhs, subtract, enabled = false)
        assert(folded.pipeline == 2, s"depth=$depth left=$left right=$right lhs=$lhs rhs=$rhs")
        assert(baseline.pipeline + lhs.pipeline == 2)
        assert(folded.parents == Seq(addr -> 0))
        val component = folded.implement((signal, _) =>
          assert(signal == addr)
          rtlAddr
        )
        def evaluate(c: Component, address: Int): BigInt = c match
          case value if value == rtlAddr => BigInt(address)
          case RTLConst(_, bits) => bits
          case RTLMux(selector, values) => evaluate(values(evaluate(selector, address).toInt), address)
          case other => fail(s"Unexpected folded component $other")
        for i <- left.indices do
          val a = hw.bitsOf(left(i))
          val b = hw.bitsOf(right(i))
          assert(evaluate(component, i) == ((if subtract then a - b else a + b) & mask))

  test("disabled, deep, unequal-address and dynamic expressions remain arithmetic"):
    val hw = FixedPoint(2, 4)
    def rom(depth: Int, port: Int): Sig[Double] =
      ROM(Vector.tabulate(depth)(i => (i + 1).toDouble / 16),
        Input[Int](port)(using Unsigned(Integer.numberOfTrailingZeros(depth))))(using hw)
    for subtract <- Seq(false, true) do
      assert(hw.romArithmetic(rom(4, 0), rom(4, 0), subtract, enabled = false).pipeline == 1)
      assert(hw.romArithmetic(rom(8, 0), rom(8, 0), subtract, enabled = true).pipeline == 1)
      assert(hw.romArithmetic(rom(4, 0), rom(4, 1), subtract, enabled = true).pipeline == 1)
      assert(hw.romArithmetic(Input[Double](0)(using hw), rom(4, 0), subtract, enabled = true).pipeline == 1)
