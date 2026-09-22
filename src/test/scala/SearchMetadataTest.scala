import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.Files
import ir.rtl.hardwaretype.{FixedPoint, HW}
import ir.rtl.signals.{Const, Input, Times}

class SearchMetadataTest extends AnyFunSuite:
  test("wide fixed-point scaling preserves unity and exact powers of two"):
    for fractional <- Seq(16, 32, 52, 60, 66) do
      val hw = FixedPoint(2, fractional)
      assert(hw.bitsOf(1.0) == (BigInt(1) << fractional))
      assert(hw.bitsOf(0.5) == (BigInt(1) << (fractional - 1)))
      assert(hw.bitsOf(-1.0) == (BigInt(3) << fractional))

  test("strict fixed point preserves negative-product truncation and restores policy"):
    given HW[Double] = FixedPoint(8, 8)
    val input = Input[Double](0)
    val normal = Times(input, Const(-0.3))
    assert(!normal.isInstanceOf[Times[?]])
    FixedPoint.strictRounding.withValue(true) {
      assert(Times(input, Const(-0.3)).isInstanceOf[Times[?]])
    }
    assert(!FixedPoint.strictRounding.value)

  test("both FFT families emit named RTL and numerical timing contracts"):
    val dir = Files.createTempDirectory("sgen-contract-")
    try
      for terminal <- Seq("dft", "idft", "dftcompact", "idftcompact") do
        val rtl = dir.resolve(terminal + ".sv")
        Main.main(Array("-nologo", "-metadata", "-strict-fixedpoint", "-n", "3", "-k", "1", "-r", "1",
          "-dualramcontrol", "-hw", "complex", "fixedpoint", "16", "24", "-top", "TestFFT", "-o", rtl.toString, terminal))
        assert(new String(Files.readAllBytes(rtl), "UTF-8").contains("module TestFFT("))
        val meta = new String(Files.readAllBytes(dir.resolve(terminal + ".json")), "UTF-8")
        assert(meta.contains("\"schema\":\"sgen-search-v1\""))
        assert(meta.contains("\"transform_size\":8"))
        assert(meta.contains("\"next_offset\":"))
        assert(meta.contains("\"twiddle_fractional_bits\":38"))
        assert(!FixedPoint.strictRounding.value)
    finally
      val files = Files.list(dir)
      try files.forEach(path => Files.delete(path)) finally files.close()
      Files.delete(dir)
