package backends

import backends.Verilog.*
import ir.rtl.*
import org.scalatest.funsuite.AnyFunSuite

class MuxControlReplicationTest extends AnyFunSuite:
  def fixture(cycles: Int, widths: Seq[Int], alias: Boolean = false): Module = new Module:
    lazy val inputs = Seq(Input(1, "select")) ++ widths.zipWithIndex.flatMap { (width, i) =>
      Seq(Input(width, s"a$i"), Input(width, s"b$i"))
    }
    lazy val outputs =
      val reg = Register(inputs.head, cycles)
      val selector = if alias then
        val wire = new Wire(1)
        wire.input = reg
        wire
      else reg
      widths.indices.map(i => Output(Mux(selector, inputs.slice(1 + i * 2, 3 + i * 2)), s"y$i")) :+
        Output(reg, "unrelated_consumer")

  for cycles <- Seq(1, 2, 7); alias <- Seq(false, true) do
    test(s"copies only final stage at depth $cycles, alias=$alias"):
      val mod = fixture(cycles, Seq.fill(9)(30), alias)
      val rtl = mod.toVerilogWithMuxControlBudget(240)
      val copies = "// mux_control_copy name=(\\w+) source=(\\w+) cycles=(\\d+) bits=(\\d+) muxes=([^\\n]+)".r
        .findAllMatchIn(rtl).toSeq
      assert(copies.size == 2)
      assert(copies.map(_.group(4).toInt).sorted == Seq(30, 240))
      copies.foreach { c =>
        val source = c.group(2)
        val internal = "s" + (source.drop(1).toInt + 1)
        val predecessor = cycles match
          case 1 => "select"
          case 2 => internal
          case n => s"$internal [${n - 2}]"
        assert(rtl.contains(s"${c.group(1)} <= $predecessor;"))
        assert(rtl.contains(s"assign unrelated_consumer = $source;"))
        assert(rtl.contains(s"(* KEEP = \"TRUE\", DONT_TOUCH = \"TRUE\", SHREG_EXTRACT = \"NO\" *) reg ${c.group(1)};"))
      }
      assert(rtl == mod.toVerilogWithMuxControlBudget(240))
      assert(!mod.toVerilogWithMuxControlBudget(0).contains("mux_control"))

  test("threshold is inclusive and individual muxes are never split"):
    assert(!fixture(1, Seq.fill(8)(30)).toVerilogWithMuxControlBudget(240).contains("mux_control_copy"))
    val rtl = fixture(1, Seq(241, 30, 30)).toVerilogWithMuxControlBudget(240)
    assert(rtl.contains("bits=241 reason=oversized_mux"))
    assert(rtl.contains("bits=60 muxes="))
    assertThrows[IllegalArgumentException](fixture(1, Seq(30)).toVerilogWithMuxControlBudget(-1))
