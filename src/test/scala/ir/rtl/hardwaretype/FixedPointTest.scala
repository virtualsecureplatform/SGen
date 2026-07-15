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

import ir.rtl.{Component, Concat, Const as RTLConst, Input as RTLInput, Plus as RTLPlus, Tap, Times as RTLTimes}
import ir.rtl.signals.{Const, Input, Sig}
import org.scalatest.funsuite.AnyFunSuite

class FixedPointTest extends AnyFunSuite:
  test("bitsOf should work with a set of doubles"):
    val hw = FixedPoint(2, 1)
    assertThrows[IllegalArgumentException](hw.bitsOf(Double.NegativeInfinity))
    assertThrows[IllegalArgumentException](hw.bitsOf(Double.MinValue))
    assertThrows[IllegalArgumentException](hw.bitsOf(-2.5d))
    assert(hw.bitsOf(-2d) == 4)
    assert(hw.bitsOf(-1.5d) == 5)
    assert(hw.bitsOf(-1d) == 6)
    assert(hw.bitsOf(-0.5d) == 7)
    assert(hw.bitsOf(-Double.MinPositiveValue) == 0)
    assert(hw.bitsOf(-0d) == 0)
    assert(hw.bitsOf(0d) == 0)
    assert(hw.bitsOf(Double.MinPositiveValue) == 0)
    assert(hw.bitsOf(0.5d) == 1)
    assert(hw.bitsOf(1d) == 2)
    assert(hw.bitsOf(1.5d) == 3)
    assertThrows[IllegalArgumentException](hw.bitsOf(2d))
    assertThrows[IllegalArgumentException](hw.bitsOf(Double.MaxValue))
    assertThrows[IllegalArgumentException](hw.bitsOf(Double.PositiveInfinity))
    assertThrows[IllegalArgumentException](hw.bitsOf(Double.NaN))
    
  test("valueOf should work with a set of inputs"):
    val hw = FixedPoint(2, 1)
    assert(hw.valueOf(0) == 0d)
    assert(hw.valueOf(1) == 0.5d)
    assert(hw.valueOf(2) == 1d)
    assert(hw.valueOf(3) == 1.5d)
    assert(hw.valueOf(4) == -2d)
    assert(hw.valueOf(5) == -1.5d)
    assert(hw.valueOf(6) == -1d)
    assert(hw.valueOf(7) == -0.5d)
    assertThrows[IllegalArgumentException](hw.valueOf(8))

  test("fractional power-of-two products should arithmetic-shift negatives"):
    given hw: FixedPoint = FixedPoint(27, 3)
    val input = Input[Double](0)
    val inputComponent = RTLInput(hw.size, "input")

    def lower(product: Sig[Double]): Component =
      product.implement((signal, _) =>
        if signal == input then inputComponent
        else throw IllegalArgumentException(s"Unexpected signal $signal")
      )

    def evaluate(component: Component, inputBits: BigInt): BigInt =
      component match
        case current if current == inputComponent => inputBits
        case RTLConst(_, value) => value
        case Tap(parent, range) =>
          (evaluate(parent, inputBits) >> range.start) &
            ((BigInt(1) << range.size) - 1)
        case Concat(parts) =>
          parts.foldLeft(BigInt(0))((result, part) =>
            (result << part.size) | evaluate(part, inputBits)
          )
        case other => throw IllegalArgumentException(s"Unexpected component $other")

    val mask = (BigInt(1) << hw.size) - 1
    def bits(raw: BigInt): BigInt = raw & mask

    val half = lower(input * Const(0.5))
    assert(evaluate(half, bits(-12)) == bits(-6))
    assert(evaluate(half, bits(12)) == bits(6))

    val quarter = lower(input * Const(0.25))
    assert(evaluate(quarter, bits(-10)) == bits(-3))
    assert(evaluate(quarter, bits(10)) == bits(2))

  test("wide fixed-point products should split exactly into two DSP-sized products"):
    val dataHW = FixedPoint(18, 12)
    val twiddleHW = FixedPoint(2, 24)
    val lhs = Input[Double](0)(using dataHW)
    val rhs = Input[Double](1)(using twiddleHW)
    val lhsComponent = RTLInput(dataHW.size, "lhs")
    val rhsComponent = RTLInput(twiddleHW.size, "rhs")
    val product = (lhs * rhs).implement((signal, _) =>
      if signal == lhs then lhsComponent
      else if signal == rhs then rhsComponent
      else throw IllegalArgumentException(s"Unexpected signal $signal")
    )

    val multipliers = collection.mutable.Set[Component]()
    def collect(component: Component): Unit =
      if multipliers.add(component) then
        component.parents.foreach(collect)
    collect(product)
    assert(multipliers.count(_.isInstanceOf[RTLTimes]) == 2)

    def mask(width: Int): BigInt = (BigInt(1) << width) - 1
    def signed(value: BigInt, width: Int): BigInt =
      val truncated = value & mask(width)
      if truncated.testBit(width - 1) then truncated - (BigInt(1) << width)
      else truncated

    def evaluate(
        component: Component,
        lhsBits: BigInt,
        rhsBits: BigInt
    ): BigInt =
      val result = component match
        case current if current == lhsComponent => lhsBits
        case current if current == rhsComponent => rhsBits
        case RTLConst(_, value) => value
        case Tap(parent, range) =>
          evaluate(parent, lhsBits, rhsBits) >> range.start
        case Concat(parts) =>
          parts.foldLeft(BigInt(0))((result, part) =>
            (result << part.size) | evaluate(part, lhsBits, rhsBits)
          )
        case RTLPlus(terms) =>
          terms.map(evaluate(_, lhsBits, rhsBits)).sum
        case RTLTimes(left, right) =>
          signed(evaluate(left, lhsBits, rhsBits), left.size) *
            signed(evaluate(right, lhsBits, rhsBits), right.size)
        case other =>
          throw IllegalArgumentException(s"Unexpected component $other")
      result & mask(component.size)

    val lhsLimit = BigInt(1) << (dataHW.size - 1)
    val rhsLimit = BigInt(1) << (twiddleHW.size - 1)
    val boundaryLhs = Seq(
      -lhsLimit,
      -lhsLimit + 1,
      BigInt(-1),
      BigInt(0),
      BigInt(1),
      lhsLimit - 1
    )
    val boundaryRhs = Seq(
      -rhsLimit,
      -rhsLimit + 1,
      BigInt(-1),
      BigInt(0),
      BigInt(1),
      rhsLimit - 1
    )
    val random = new scala.util.Random(0x465054)
    val randomInputs = Seq.fill(10000)(
      (BigInt(dataHW.size, random) - lhsLimit,
       BigInt(twiddleHW.size, random) - rhsLimit)
    )
    val inputs = for
      left <- boundaryLhs
      right <- boundaryRhs
    yield (left, right)

    for (left, right) <- inputs ++ randomInputs do
      val actual = evaluate(
        product,
        left & mask(dataHW.size),
        right & mask(twiddleHW.size)
      )
      val expected = ((left * right) >> twiddleHW.fractional) &
        mask(dataHW.size)
      assert(actual == expected, s"$left * $right: $actual != $expected")
