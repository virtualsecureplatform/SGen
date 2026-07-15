package transforms.fft

import maths.fields.Complex
import maths.fields.Complex.*
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
