package transforms.fft

import maths.fields.Complex
import maths.fields.Complex.*
import backends.Verilog.*
import backends.FptSwitchTangentVerilog
import backends.FptParallelSwitchTangentVerilog
import ir.rtl.hardwaretype.{ComplexHW, FixedPoint}
import ir.rtl.RAMControl
import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import scala.sys.process.*

import scala.math.Numeric.Implicits.infixNumericOps

class TangentTwistTest extends AnyFunSuite:
  private def close(actual: Complex[Double], expected: Complex[Double]): Unit =
    val error = actual - expected
    assert(error.norm2 < 1e-10, s"$actual differs from $expected")

  for n <- 1 to 8 do
    test(s"Forward and inverse twists are conjugates at size ${1 << n}"):
      val forward = TangentTwist(n, inverse = false)
      val inverse = TangentTwist(n, inverse = true)
      for i <- 0 until 1 << n do
        close(forward.coef(i) * inverse.coef(i), Complex(1.0))

  for
    n <- 1 to 8
    r <- 1 to n if n % r == 0
  do
    test(s"Tangent FFT round trip (size ${1 << n}, radix ${1 << r})"):
      val forward = TangentCTDFT(n, r, Complex(1.0))
      val inverse = TangentICTDFT(n, r, Complex(0.5))
      val inputs = Seq.tabulate(1 << n)(i =>
        Complex(Math.sin(0.31 * i), Math.cos(0.19 * i))
      )
      val outputs = inverse.eval(forward.eval(inputs, 0), 0)
      outputs.zip(inputs).foreach(close)

  test("radix-8 tangent FFT round trip at 512 points"):
    val forward = TangentCTDFT(9, 3, Complex(1.0))
    val inverse = TangentICTDFT(9, 3, Complex(0.5))
    val inputs = Seq.tabulate(1 << 9)(i => Complex(Math.sin(0.013 * i), Math.cos(0.017 * i)))
    inverse.eval(forward.eval(inputs, 0), 0).zip(inputs).foreach(close)

  test("radix-8 stage-2 spill partition preserves the forward transform"):
    val standard = TangentCTDFT(9, 3, Complex(1.0))
    val (front, back) =
      TangentCTDFT.partitionedSplsWithLastStageInputSpill(
        9,
        3,
        Complex(1.0)
      )
    val inputs = Seq.tabulate(1 << 9)(i =>
      Complex(Math.sin(0.013 * i), Math.cos(0.017 * i))
    )
    back
      .eval(front.eval(inputs, 0), 0)
      .zip(standard.eval(inputs, 0))
      .foreach(close)

  test("normalized inverse radix-8 decomposition matches normalized radix-2 mathematics"):
    val reference = ICTDFT(9, 3, Complex(0.5))
    val normalized = NormalizedICTDFT(9, 3)
    val inputs = Seq.tabulate(1 << 9)(i => Complex(Math.sin(0.009 * i), Math.cos(0.015 * i)))
    reference.eval(inputs, 0).zip(normalized.eval(inputs, 0)).foreach(close)

  test("normalized inverse fixed-point butterfly keeps the carry before scaling"):
    assume(Process(Seq("sh", "-c", "command -v iverilog >/dev/null && command -v vvp >/dev/null")).! == 0)
    given ir.rtl.hardwaretype.HW[Complex[Double]] = ComplexHW(FixedPoint(4, 0))
    val transform = NormalizedInverseDFT2()
    val module = transform.stream(1, RAMControl.Single)
    val rtl = module.toVerilog
    val testbench = module.getTestBench(Seq(Complex(7.0), Complex(7.0)), "wide normalized inverse butterfly")
    val directory = Files.createTempDirectory("sgen-normalized-butterfly-")
    val source = directory.resolve("design.v")
    val executable = directory.resolve("sim.out")
    Files.write(
      source,
      (rtl + "\n" + testbench).getBytes(StandardCharsets.UTF_8)
    )
    assert(Process(Seq("iverilog", "-g2012", "-s", "test", "-o", executable.toString, source.toString)).! == 0)
    assert(Process(Seq("vvp", executable.toString)).! == 0)

  for
    n <- 2 to 8 by 2
    r <- 1 to n if n % r == 0
  do
    test(s"Switch-backed tangent FFT preserves tangent FFT semantics (size ${1 << n}, radix ${1 << r})"):
      val laneLog = n / 2
      val standard = TangentCTDFT(n, r, Complex(1.0))
      val switched = TangentCTDFTWithSwitch(n, r, laneLog, Complex(1.0))
      val inputs = Seq.tabulate(1 << n)(i => Complex(Math.sin(0.17 * i), Math.cos(0.23 * i)))
      standard.eval(inputs, 0).zip(switched.eval(inputs, 0)).foreach(close)

  test("switch-backed tangent FFT emits the recursive switch network"):
    val transform = TangentCTDFTWithSwitch(6, 1, 3, Complex(1.0))
    val module = transform.stream(3, ir.rtl.RAMControl.Single)(using ComplexHW(FixedPoint(8, 12)))
    val rtl = module.toVerilog
    assert(rtl.contains("SGenSwitchTransposeNetwork_3"))

  test("raw switch and tangent timing contracts require explicit data alignment"):
    given ir.rtl.hardwaretype.HW[Complex[Double]] = ComplexHW(FixedPoint(8, 12))
    val pre = transforms.perm.SwitchTranspose[Complex[Double]](2).stream(2, RAMControl.Single)
    val twist = TangentTwistAfterSwitch(5, 2, inverse = false, laneStride = 2, cycleLog = 2).stream(2, RAMControl.Single)
    val fft = TangentCTDFT(5, 1, Complex(1.0)).stream(3, RAMControl.Single)
    assert(pre.nextAt == 0)
    assert(twist.nextAt == -2)
    // TangentCTDFT includes the explicit input StreamingDelay added for the
    // FPT wrapper, so its launch token is one cycle later than the raw twist.
    assert(fft.nextAt == -1)
    assert(pre.minGap == 0 && twist.minGap == 0 && fft.minGap == 0)

  test("rectangular switch-backed tangent wrapper adapts the twist width"):
    val rtl = FptSwitchTangentVerilog.emit(5, 1, 3, ComplexHW(FixedPoint(8, 12)), Complex(1.0), inverse = false)
    assert(rtl.contains("Input width: 8 lanes; twist width: 4 lanes"))
    assert(rtl.contains("module mainRectForward"))
    assert(rtl.contains("module mainRectReverse"))

  test("rate-preserving tangent wrapper replicates square switch/twist blocks"):
    val rtl = FptParallelSwitchTangentVerilog.emit(5, 1, 3, ComplexHW(FixedPoint(8, 12)), Complex(1.0))
    assert(rtl.contains("2 parallel 4x4 switch/twist blocks"))
    assert(rtl.contains("module mainTwist0"))
    assert(rtl.contains("module mainTwist1"))

  test("rate-preserving inverse tangent wrapper places blocks after the inverse FFT"):
    val rtl = FptParallelSwitchTangentVerilog.emit(5, 1, 3, ComplexHW(FixedPoint(8, 12)), Complex(0.5), inverse = true)
    assert(rtl.contains("parallel switch FPT tangent inverse FFT"))
    assert(rtl.contains("module mainInverseCore"))
    assert(rtl.contains(".next(fft_next)"))

  test("parallel block addressing equals the global tangent twist for n=9"):
    def parallelTwist(laneLog: Int, inverse: Boolean, input: Seq[Complex[Double]]): Vector[Complex[Double]] =
      val cycleLog = 9 - laneLog
      val cycles = 1 << cycleLog
      val blocks = 1 << (laneLog - cycleLog)
      val lanes = 1 << laneLog
      val twist = TangentTwist(9, inverse)
      val output = input.toArray
      for block <- 0 until blocks; cycle <- 0 until cycles; lane <- 0 until cycles do
        val index = lane * lanes + block * cycles + cycle
        output(index) = input(index) * twist.coef(index)
      output.toVector
    for laneLog <- Seq(5, 6); inverse <- Seq(false, true) do
      val input = Vector.tabulate(512)(i => Complex(Math.sin(0.07 * i), Math.cos(0.11 * i)))
      TangentTwist(9, inverse).eval(input, 0).zip(parallelTwist(laneLog, inverse, input)).foreach(close)
