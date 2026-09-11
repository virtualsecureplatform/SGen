package backends

import ir.rtl.*
import backends.Verilog.*
import org.scalatest.funsuite.AnyFunSuite

class TwiddleIslandTest extends AnyFunSuite:
  test("local column encoding reconstructs every coefficient bit exactly"):
    val random = new scala.util.Random(0x434f4546)
    for depth <- Seq(2, 4); width <- Seq(4, 26, 30); trial <- 0 until 100 do
      val values = Vector.fill(depth)(BigInt(width, random))
      val table = TwiddleIslandVerilog.Table(Some(Input(if depth == 2 then 1 else 2, "phase")), 2, width, values)
      val encoding = TwiddleIslandVerilog.encode(table)
      assert(encoding.columns.size <= (if depth == 2 then 1 else 7))
      for row <- values.indices do
        val rebuilt = encoding.bits.zipWithIndex.foldLeft(BigInt(0)) { case (acc, ((index, invert), bit)) =>
          val stored = index >= 0 && encoding.encoded(row).testBit(index)
          if stored != invert then acc.setBit(bit) else acc
        }
        assert(rebuilt == values(row))

  test("address regions reuse an existing edge and serve at most 32 islands"):
    val mod = new Module:
      lazy val inputs = Seq(Input(2, "phase")) ++ (0 until 65).map(i => Input(30, s"data$i"))
      lazy val outputs =
        val phase = Register(inputs.head)
        inputs.tail.zipWithIndex.map { (data, i) =>
          val coeff = Register(Mux(phase, Seq(i + 1, i + 73, i + 531, i + 1257).map(v => Const(26, v))), 2)
          Output(Register(Times(data, coeff), 3), s"y$i")
        }
    val rtl = mod.toVerilogWithMuxControlBudget(0, twiddleConsumers = 4)
    assert(rtl.linesIterator.count(_.startsWith("// twiddle_region ")) == 3)
    assert(rtl.contains(".d(phase)"))
    assert(rtl.contains(".phase(twiddle_region_2_value)"))

  test("registered tables fold exactly at every width and preserve schedule"):
    val phase = Input(2, "phase")
    val left = Mux(phase, Seq(15, 7, 8, 1).map(v => Const(4, v)))
    val right = Mux(phase, Seq(2, 9, 8, 15).map(v => Const(4, v)))
    val sum = Register(Plus(Seq(Register(left), Register(right))), 2)
    val sub = Register(Minus(Register(left), Register(right)))
    val tables = TwiddleIslandVerilog.analyze(Seq(sum, sub))
    assert(tables(sum).values == Vector(1, 0, 0, 0).map(BigInt(_)))
    assert(tables(sum).cycles == 3)
    assert(tables(sub).values == Vector(13, 14, 0, 2).map(BigInt(_)))
    assert(tables(sub).cycles == 2)
    assert(tables(sum).address.contains(phase))

  test("different phases, unequal delays and dynamic data are not folded"):
    val a = Input(1, "a")
    val b = Input(1, "b")
    def rom(p: Component) = Mux(p, Seq(Const(8, 13), Const(8, 254)))
    val invalid = Seq(Plus(Seq(Register(rom(a)), Register(rom(b)))),
      Minus(Register(rom(a)), Register(rom(a), 2)), Plus(Seq(Register(rom(a)), Input(8, "data"))))
    val tables = TwiddleIslandVerilog.analyze(invalid)
    assert(invalid.forall(c => !tables.contains(c)))

  test("local islands partition consumers, preserve edges, and remain opt-in"):
    val mod = new Module:
      lazy val inputs = Seq(Input(2, "phase")) ++ (0 until 9).map(i => Input(8, s"data$i"))
      lazy val outputs =
        val coeff = Register(Mux(inputs.head, Seq(3, 7, 129, 254).map(v => Const(8, v))), 3)
        inputs.tail.zipWithIndex.map((data, i) => Output(Register(Times(data, coeff), 3), s"y$i"))
    val baseline = mod.toVerilogWithMuxControlBudget(0)
    val local = mod.toVerilogWithMuxControlBudget(0, twiddleConsumers = 4)
    assert(local.linesIterator.count(_.startsWith("// twiddle_island ")) == 3)
    assert(local.contains("phase_1 <= phase_0;"))
    assert(local.contains("case (phase_1)"))
    assert(!baseline.contains("twiddle_island"))
    assert(local == mod.toVerilogWithMuxControlBudget(0, twiddleConsumers = 4))
    assertThrows[IllegalArgumentException](mod.toVerilogWithMuxControlBudget(0, twiddleConsumers = 3))
