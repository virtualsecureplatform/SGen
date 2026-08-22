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
import ir.rtl.signals.{Const, Operator, ROM, Sig, Timer}
import ir.rtl.signals.{Cpx, Im, Re}
import ir.spl.{ITensor, Product, Repeatable, SPL}
import maths.fields.Complex
import maths.fields.Complex.*
import transforms.HighLevelTransform
import transforms.perm.SwitchTranspose
import transforms.perm.LinearPerm.{Lmat, Qmat, Rmat}
import transforms.perm.LinearPerm.given

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

/** One inverse radix-2 level with a wide add/subtract followed by exact /2 narrowing. */
case class NormalizedInverseDFT2() extends SPL[Complex[Double]](1) with Repeatable[Complex[Double]]:
  override def eval(inputs: Seq[Complex[Double]], set: Int): Seq[Complex[Double]] =
    inputs.grouped(2).toSeq.flatMap(pair => Seq((pair.head + pair.last) * 0.5, (pair.head - pair.last) * 0.5))

  override def stream(k: Int, control: RAMControl)(using HW[Complex[Double]]): AcyclicStreamingModule[Complex[Double]] =
    require(k == 1, s"normalized inverse radix-2 requires k=1, got $k")
    new AcyclicStreamingModule(0, 1):
      override def implement(inputs: Seq[Sig[Complex[Double]]]): Seq[Sig[Complex[Double]]] =
        val left = inputs.head
        val right = inputs.last
        hw match
          case ComplexHW(fixed: FixedPoint) =>
            Vector(
              Cpx(fixed.normalizedHalf(Re(left), Re(right), subtract = false), fixed.normalizedHalf(Im(left), Im(right), subtract = false)),
              Cpx(fixed.normalizedHalf(Re(left), Re(right), subtract = true), fixed.normalizedHalf(Im(left), Im(right), subtract = true))
            )
          case _ => Vector((left + right) * Const(Complex(0.5)), (left - right) * Const(Complex(0.5)))

      override def spl: SPL[Complex[Double]] = NormalizedInverseDFT2()

/** CT decomposition whose radix-2 leaves retain the inverse normalization boundary. */
case class NormalizedInverseCTDFT(override val n: Int, r: Int) extends DFT(n, r):
  override val spl: SPL[Complex[Double]] =
    if n == 1 then NormalizedInverseDFT2()
    else
      Lmat(r, n) * Product(n / r)(level =>
        ITensor(n - r, NormalizedInverseCTDFT(r, 1).spl) * DiagE(n, r, level) * Qmat(n, r, level)
      ) * Rmat(r, n)

case class NormalizedICTDFT(override val n: Int, r: Int) extends DFT(n, r):
  override val spl: SPL[Complex[Double]] = Swap(n) * NormalizedInverseCTDFT(n, r).spl * Swap(n)

object FptInverseDft:
  def spl(n: Int, r: Int, scalingFactor: Complex[Double]): SPL[Complex[Double]] =
    if r == 3 && scalingFactor == Complex(0.5) then NormalizedICTDFT(n, r).spl
    else ICTDFT(n, r, scalingFactor).spl

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

/** Tangent twist expressed in the coordinates after a square lane/time switch.
  * The value at post-switch (cycle, lane) originates from pre-switch
  * (lane, cycle), so its coefficient address is lane*T + cycle.
  */
case class TangentTwistAfterSwitch(
    override val n: Int,
    laneLog: Int,
    inverse: Boolean,
    laneStride: Int = 1,
    cycleOffset: Int = 0,
    cycleLog: Int = -1
)
    extends SPL[Complex[Double]](n)
    with Repeatable[Complex[Double]]:
  private val effectiveCycleLog = if cycleLog < 0 then laneLog else cycleLog
  require(laneLog >= 0 && effectiveCycleLog >= 0 && laneLog + effectiveCycleLog <= n)
  require(laneStride > 0 && cycleOffset >= 0)
  private val base = TangentTwist(n, inverse)

  override def eval(inputs: Seq[Complex[Double]], set: Int): Seq[Complex[Double]] =
    val lanes = 1 << laneLog
    val cycles = 1 << effectiveCycleLog
    Vector.tabulate(N) { index =>
      val cycle = index / lanes
      val lane = index % lanes
      inputs(index) * base.coef(lane * laneStride * cycles + cycleOffset + cycle)
    }

  override def stream(k: Int, control: RAMControl)(using
      HW[Complex[Double]]
  ): AcyclicStreamingModule[Complex[Double]] =
    require(k == laneLog, s"switch-aware tangent twist requires k=$laneLog, got $k")
    new AcyclicStreamingModule(effectiveCycleLog, k):
      override def implement(inputs: Seq[Sig[Complex[Double]]]): Seq[Sig[Complex[Double]]] =
        (0 until K).map { lane =>
          val twiddles = Vector.tabulate(T)(cycle => base.coef(lane * laneStride * T + cycleOffset + cycle))
          val twiddleHW = hw match
            case ComplexHW(FixedPoint(magnitude, fractional)) => ComplexHW(FixedPoint(2, magnitude + fractional - 6))
            case _ => hw
          inputs(lane) * ROM(twiddles, Timer(T))(using twiddleHW)
        }

      override def spl: SPL[Complex[Double]] =
        TangentTwistAfterSwitch(TangentTwistAfterSwitch.this.n, laneLog, inverse, laneStride, cycleOffset, effectiveCycleLog)

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
    TangentTwist(n, inverse = true) * FptInverseDft.spl(n, r, scalingFactor)

/** Tangent FFT lowered through two square SwitchTransposeUnit networks.
  * The paired transposes preserve the external stream order while allowing the
  * twist ROM to follow the transposed lane/time coordinate system.
  */
case class TangentCTDFTWithSwitch(
    override val n: Int,
    r: Int,
    laneLog: Int,
    scalingFactor: Complex[Double]
) extends DFT(n, r):
  require(n == 2 * laneLog, s"switch-backed tangent FFT requires n=2k; got n=$n, k=$laneLog")
  override protected val spl: SPL[Complex[Double]] =
    CTDFT(n, r, scalingFactor).spl *
      SwitchTranspose[Complex[Double]](laneLog) *
      TangentTwistAfterSwitch(n, laneLog, inverse = false) *
      SwitchTranspose[Complex[Double]](laneLog)

case class TangentICTDFTWithSwitch(
    override val n: Int,
    r: Int,
    laneLog: Int,
    scalingFactor: Complex[Double]
) extends DFT(n, r):
  require(n == 2 * laneLog, s"switch-backed tangent inverse FFT requires n=2k; got n=$n, k=$laneLog")
  override protected val spl: SPL[Complex[Double]] =
    SwitchTranspose[Complex[Double]](laneLog) *
      TangentTwistAfterSwitch(n, laneLog, inverse = true) *
      SwitchTranspose[Complex[Double]](laneLog) *
      FptInverseDft.spl(n, r, scalingFactor)
