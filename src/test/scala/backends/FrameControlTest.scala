package backends

import ir.rtl.*
import org.scalatest.funsuite.AnyFunSuite

class FrameControlTest extends AnyFunSuite:
  test("descriptor period covers the non-power-of-two IFFT bank sequences"):
    val next = Input(1, "next")
    val counters = Seq(2,3,4,6).map { n =>
      FrameCounterValue(Input(BigInt(n-1).bitLength, s"prev$n"),
        Register(next, 9), Register(next, 1), n, 0, true)
    }
    def name(c: Component): String = c match
      case Input(_, n) => n
      case _ => "local"
    val (rtl, values) = FrameControlVerilog.emit(counters, name)
    assert(rtl.contains("period=12 descriptor_bits=4 max_delay=9"))
    assert(rtl.contains("frame_descriptor[0] <= frame_ordinal"))
    assert(rtl.contains("if (reset) frame_ordinal <= 0"))
    assert("if \\(reset\\)".r.findAllIn(rtl).size == 1)
    counters.foreach { c =>
      assert(values(c).contains(s"frame_mod_${c.limit}_0(frame_descriptor[0])"))
      assert(values(c).contains(s"frame_mod_${c.limit}_1(frame_descriptor[8])"))
      for frame <- 0 until 48 do
        assert((frame % 12) % c.limit == frame % c.limit)
    }

  test("immediate counters include the first increment and unsupported tokens fail closed"):
    val next = Input(1, "next")
    val c = FrameCounterValue(Input(2, "previous"), next, next, 3, 2, false)
    val (_, values) = FrameControlVerilog.emit(Seq(c), _ => "next")
    assert(values(c).contains("frame_mod_3_0(frame_ordinal)"))
    val invalid = c.copy(trigger = Or(Seq(next, Input(1, "other"))))
    assertThrows[IllegalArgumentException](FrameControlVerilog.emit(Seq(invalid), _ => "invalid"))
