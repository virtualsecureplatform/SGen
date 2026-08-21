package transforms.fft

import maths.fields.Complex
import maths.fields.Complex.*
import backends.Verilog.*
import backends.FptSwitchTangentVerilog
import backends.FptParallelSwitchTangentVerilog
import ir.rtl.hardwaretype.{ComplexHW, FixedPoint}
import org.scalatest.funsuite.AnyFunSuite

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
