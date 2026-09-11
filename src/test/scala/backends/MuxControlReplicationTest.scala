package backends

import backends.Verilog.*
import ir.rtl.*
import org.scalatest.funsuite.AnyFunSuite

class MuxControlReplicationTest extends AnyFunSuite:
  for depth <- Seq(1, 2, 7); alias <- Seq(false, true) do
    test(s"registered islands retain capture depth $depth alias=$alias"):
      val mod = new Module:
        lazy val inputs = Seq(Input(1, "select"), Input(30, "a"), Input(30, "b"))
        lazy val outputs =
          val selector = Register(inputs.head, 2)
          (0 until 9).map { i =>
            val mux = Mux(selector, Seq(inputs(1), Plus(Seq(inputs(2), Const(30, i)))))
            val input = if alias then
              val w = new Wire(30)
              w.input = mux
              w
            else mux
            Output(Register(input, depth), s"y$i")
          }
      val rtl = mod.toVerilogWithMuxControlBudget(240, 120)
      val islands = "// mux_island name=(\\w+) source=(\\w+) cycles=(\\d+) bit=(-?\\d+) bits=(\\d+) muxes=(\\S+) captures=(\\S+)".r
        .findAllMatchIn(rtl).toSeq
      assert(islands.map(_.group(5).toInt) == Seq(120, 120, 30))
      assert(islands.flatMap(_.group(7).split(",")).size == 9)
      assert(rtl.contains("captured <= select_local ? b : a;"))
      assert(rtl.contains("// mux_predecessor name=mux_predecessor_0"))
      assert(rtl.contains(".d(select)"))
      assert(rtl.contains(".select_next(mux_predecessor_0_q)"))
      assert(!rtl.contains("reason=unregistered_selector"))
      assert(rtl == mod.toVerilogWithMuxControlBudget(240, 120))
      assert(!mod.toVerilogWithMuxControlBudget(240, 0).contains("mux_island"))
      if depth > 2 then assert(rtl.contains("_island_tail[5]"))

  test("unregistered output consumers remain outside islands"):
    assert(!fixture(1, Seq.fill(9)(30)).toVerilogWithMuxControlBudget(240, 120).contains("// mux_island"))
    assertThrows[IllegalArgumentException](fixture(1, Seq(30)).toVerilogWithMuxControlBudget(240, 60))

  test("islands preserve selected bits and reject a shared combinational consumer"):
    val mod = new Module:
      lazy val inputs = Seq(Input(3, "select"), Input(30, "a"), Input(30, "b"))
      lazy val outputs =
        val selector = Tap(Register(inputs.head, 1), 2 until 3)
        val safe = Mux(selector, inputs.tail)
        val shared = Mux(selector, Seq(inputs(2), inputs(1)))
        Seq(Output(Register(safe), "captured"), Output(Register(shared), "shared_capture"), Output(shared, "raw"))
    val rtl = mod.toVerilogWithMuxControlBudget(240, 120)
    assert(rtl.linesIterator.count(_.startsWith("// mux_island ")) == 1)
    assert(rtl.contains("bit=2 bits=30"))
    assert(rtl.contains(".select_next((select >> 2))"))

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

  for cycles <- Seq(1, 2, 7); bit <- Seq(0, 2) do
    test(s"replicates selected bit $bit at depth $cycles without another stage"):
      val mod = new Module:
        lazy val inputs = Seq(Input(3, "select"), Input(30, "a"), Input(30, "b"))
        lazy val outputs =
          val reg = Register(inputs.head, cycles)
          val alias = new Wire(3)
          alias.input = reg
          val selected = Tap(alias, bit until bit + 1)
          // Distinct inputs avoid graph-level mux deduplication.
          (0 until 9).map(i => Output(Mux(selected, Seq(inputs(1), Plus(Seq(inputs(2), Const(30, i))))), s"y$i")) :+
            Output(reg, "unrelated_consumer")
      val rtl = mod.toVerilogWithMuxControlBudget(240)
      val copies = "// mux_control_copy name=(\\w+) source=(\\w+) cycles=(\\d+) bits=(\\d+) muxes=\\S+ selected_bit=(\\d+)".r.findAllMatchIn(rtl).toSeq
      assert(copies.size == 2)
      assert(copies.map(_.group(4).toInt).sorted == Seq(30, 240))
      copies.foreach { c =>
        val internal = "s" + (c.group(2).drop(1).toInt + 1)
        val predecessor = cycles match
          case 1 => "select"
          case 2 => internal
          case n => s"$internal [${n - 2}]"
        assert(c.group(5).toInt == bit)
        assert(rtl.contains(s"${c.group(1)} <= ($predecessor >> $bit);"))
      }
      assert(!mod.toVerilogWithMuxControlBudget(0).contains("mux_control"))
