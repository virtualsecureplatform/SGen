/*
 *    _____ ______          SGen - A Generator of Streaming Hardware
 *   / ___// ____/__  ____  Department of Computer Science, ETH Zurich, Switzerland
 *   \__ \/ / __/ _ \/ __ \
 *  ___/ / /_/ /  __/ / / / Copyright (C) 2020-2025 François Serre (serref@inf.ethz.ch)
 * /____/\____/\___/_/ /_/  https://github.com/fserre/sgen
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 */

package transforms.fft

import ir.rtl.hardwaretype.{ComplexHW, FixedPoint, HW}
import ir.rtl.{AcyclicStreamingModule, Component, RAMControl}
import ir.rtl.signals.{Operator, ROM, Sig, Timer}
import ir.spl.{Repeatable, SPL}
import maths.fields.Complex
import maths.fields.Complex.*
import transforms.HighLevelTransform

import scala.math.Numeric.Implicits.infixNumericOps

private case class PipelineRegister[T](input: Sig[T])
    extends Operator[T](input)(using input.hw):
  override val pipeline = 1
  override def implement(implicit cp: Sig[?] => Component): Component =
    cp(input)

/** One registered cycle of transform-preserving input alignment. */
private case class StreamingDelay[T](override val n: Int)
    extends SPL[T](n)
    with Repeatable[T]:
  override def eval(inputs: Seq[T], set: Int): Seq[T] = inputs

  override def stream(k: Int, control: RAMControl)(using
      HW[T]
  ): AcyclicStreamingModule[T] =
    new AcyclicStreamingModule(n - k, k):
      override def implement(inputs: Seq[Sig[T]]): Seq[Sig[T]] =
        inputs.map(PipelineRegister(_))

      override def spl: SPL[T] = StreamingDelay(StreamingDelay.this.n)

/** Pointwise tangent-FFT twist for a folded, real polynomial.
  *
  * A transform of 2^n complex points represents a real polynomial with
  * 2^(n+1) coefficients. The forward operator multiplies point i by
  * exp(+j*pi*i/2^(n+1)); the inverse operator uses its conjugate.
  */
case class TangentTwist(override val n: Int, inverse: Boolean)
    extends SPL[Complex[Double]](n)
    with Repeatable[Complex[Double]]:
  private val direction = if inverse then -1.0 else 1.0

  def coef(i: Int): Complex[Double] =
    val angle = direction * Math.PI * (i % (1 << n)) / (1 << (n + 1))
    Complex(Math.cos(angle), Math.sin(angle))

  override def eval(
      inputs: Seq[Complex[Double]],
      set: Int
  ): Seq[Complex[Double]] =
    inputs.zipWithIndex.map((input, i) => input * coef(i))

  override def stream(k: Int, control: RAMControl)(using
      HW[Complex[Double]]
  ): AcyclicStreamingModule[Complex[Double]] =
    new AcyclicStreamingModule(n - k, k):
      override def implement(
          inputs: Seq[Sig[Complex[Double]]]
      ): Seq[Sig[Complex[Double]]] =
        (0 until K).map(p =>
          val twiddles = Vector.tabulate(T)(c => coef(c * K + p))
          val twiddleHW = hw match
            case ComplexHW(FixedPoint(magnitude, fractional)) =>
              ComplexHW(FixedPoint(2, magnitude + fractional - 6))
            case _ => hw
          val twiddle = ROM(twiddles, Timer(T))(using twiddleHW)
          inputs(p) * twiddle
        )

      override def spl: SPL[Complex[Double]] =
        TangentTwist(TangentTwist.this.n, inverse)

/** Forward tangent FFT: twist first, then apply the cyclic DFT. */
case class TangentCTDFT(
    override val n: Int,
    r: Int,
    scalingFactor: Complex[Double]
) extends DFT(n, r):
  override protected val spl: SPL[Complex[Double]] =
    CTDFT(n, r, scalingFactor).spl * TangentTwist(n, inverse = false)

/** Inverse tangent FFT: apply the cyclic inverse DFT, then untwist. */
case class TangentICTDFT(
    override val n: Int,
    r: Int,
    scalingFactor: Complex[Double]
) extends DFT(n, r):
  override protected val spl: SPL[Complex[Double]] =
    TangentTwist(n, inverse = true) * ICTDFT(n, r, scalingFactor).spl
