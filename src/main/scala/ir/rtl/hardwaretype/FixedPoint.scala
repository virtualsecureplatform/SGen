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
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 *
 */

package ir.rtl.hardwaretype

import Utils.BigIterator
import ir.rtl.Component
import ir.rtl.signals.{Const, Minus, Plus, Sig, Times}

/**
 * Fixed point arithmetic representation
 *
 * @param magnitude Number of bits of the integer part
 * @param fractional Number of bits of the fractional part
 */
case class FixedPoint(magnitude: Int, fractional: Int) extends HW[Double](magnitude + fractional):
  override def plus(lhs: Sig[Double], rhs: Sig[Double]): Sig[Double] = FixPlus(lhs, rhs)

  override def minus(lhs: Sig[Double], rhs: Sig[Double]): Sig[Double] = FixMinus(lhs, rhs)

  override def times(lhs: Sig[Double], rhs: Sig[Double]): Sig[Double] = FixTimes(lhs, rhs)

  override def bitsOf(const: Double): BigInt = {
    require(const.isFinite)
    if const < 0 then
      val opposite = ((BigInt(1) << fractional).toDouble * BigDecimal(-const)).toBigInt
      if opposite == 0 then
        opposite
      else
        val res = (opposite ^ ((BigInt(1) << size) - 1)) + 1
        if res.bitLength != size then
          throw IllegalArgumentException(s"Overflow during the conversion of ${const} to a ${this}")
          BigInt(1) << (size - 1)
        else
          res
    else
      val res = ((BigInt(1) << fractional).toDouble * BigDecimal(const)).toBigInt
      if res.bitLength >= size then
        throw IllegalArgumentException(s"Overflow during the conversion of ${const} to a ${this}")
        (BigInt(1) << (size - 1)) - 1
      else
        res
  }


  override def valueOf(const: BigInt): Double = {
    require(const.bitLength <= size)
    if const.testBit(size - 1) then
      -((const ^ ((BigInt(1) << size) - 1)) + 1).toDouble / Math.pow(2, fractional)
    else
      const.toDouble / Math.pow(2, fractional)
  }

  override def description: String = if fractional == 0 then s"$magnitude-bits signed integer in two's complement format" else s"signed fixed-point number ($magnitude. $fractional bits representation)"

  private case class FixPlus(override val lhs: Sig[Double], override val rhs: Sig[Double]) extends Plus(lhs, rhs):
    override def pipeline = 1

    override def implement(implicit cp: Sig[?] => Component) = ir.rtl.Plus(Seq(cp(this.lhs), cp(this.rhs)))

  private case class FixMinus(override val lhs: Sig[Double], override val rhs: Sig[Double]) extends Minus(lhs, rhs):
    override def pipeline = 1

    override def implement(implicit cp: Sig[?] => Component) = ir.rtl.Minus(cp(this.lhs), cp(this.rhs))

  private case class FixTimes(override val lhs: Sig[Double], override val rhs: Sig[Double]) extends Times(lhs, rhs):
    override def pipeline = this.rhs match
      case Const(value) if value > 0 && this.rhs.hw.bitsOf(value).bitCount == 1 => 0
      case _ => 3

    override def implement(implicit cp: Sig[?] => Component): Component =
      this.rhs match
        case Const(value) if value > 0 && this.rhs.hw.bitsOf(value).bitCount == 1 =>
          val shift = this.rhs.hw.bitsOf(value).lowestSetBit - this.rhs.hw.asInstanceOf[FixedPoint].fractional
          if shift > 0 then
            ir.rtl.Concat(Seq(ir.rtl.Tap(cp(this.lhs), 0 until (this.lhs.hw.size - shift)),ir.rtl.Const(shift,0)))
          else
            val input = cp(this.lhs)
            val rightShift = -shift
            val sign = ir.rtl.Tap(input, (this.lhs.hw.size - 1) until this.lhs.hw.size)
            ir.rtl.Concat(
              Seq.fill(rightShift)(sign) :+
                ir.rtl.Tap(input, rightShift until this.lhs.hw.size)
            )
        case _ =>
          val shift = this.rhs.hw.asInstanceOf[FixedPoint].fractional
          val left = cp(this.lhs)
          val right = cp(this.rhs)
          if left.size > 27 && left.size <= 35 && right.size <= 27 then
            // DSP48E2 has a signed 27x18 multiplier. Split the wider left
            // operand into an 18-bit signed low chunk and a signed high
            // chunk. Treating the low chunk as signed subtracts 2^18 when
            // its sign bit is set, so add that bit to the high chunk. This
            // reconstructs the exact full product with two multipliers for
            // FPT's 30x26 products instead of a generic four-way split.
            val lowWidth = 18
            val low = ir.rtl.Tap(left, 0 until lowWidth)
            val high = ir.rtl.Tap(left, lowWidth until left.size)
            val highSign = ir.rtl.Tap(high, (high.size - 1) until high.size)
            val extendedHigh = ir.rtl.Concat(Seq(highSign, high))
            val lowSign = ir.rtl.Tap(low, (lowWidth - 1) until lowWidth)
            val lowCorrection = ir.rtl.Concat(
              Seq(ir.rtl.Const(high.size, 0), lowSign)
            )
            val adjustedHigh = ir.rtl.Plus(Seq(extendedHigh, lowCorrection))
            val lowProduct = ir.rtl.Times(low, right)
            val highProduct = ir.rtl.Times(adjustedHigh, right)
            val sumWidth = left.size + right.size + 1

            def signExtend(input: Component, width: Int): Component =
              require(width >= input.size)
              if width == input.size then input
              else
                val sign = ir.rtl.Tap(
                  input,
                  (input.size - 1) until input.size
                )
                ir.rtl.Concat(Seq.fill(width - input.size)(sign) :+ input)

            val extendedLowProduct = signExtend(lowProduct, sumWidth)
            val shiftedHighProduct = ir.rtl.Concat(
              Seq(
                signExtend(highProduct, sumWidth - lowWidth),
                ir.rtl.Const(lowWidth, 0)
              )
            )
            val product = ir.rtl.Plus(
              Seq(extendedLowProduct, shiftedHighProduct)
            )
            ir.rtl.Tap(product, shift until (shift + left.size))
          else
            ir.rtl.Tap(
              ir.rtl.Times(left, right),
              shift until (shift + left.size)
            )

  override def MID_VALUE: Double = valueOf(BigInt(1) << ((size - 1)/2))

  override def MAX_VALUE: Double = valueOf((BigInt(1) << (size - 1)) - 1)

  override def values: Iterator[Double] = BigIterator(0, BigInt(1)<<size).map(valueOf)

  override def toString: String = (magnitude, fractional) match
    case (8, 0) => "char"
    case (16, 0) => "short"
    case (32, 0) => "int"
    case (64, 0) => "long"
    case _ => s"FixedPoint($magnitude, $fractional)"
