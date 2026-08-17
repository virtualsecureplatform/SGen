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

package transforms.perm

import ir.rtl.{RAMControl, StreamingModule}
import ir.rtl.hardwaretype.HW
import transforms.Transform

/** Square transpose between 2^logSize lanes and 2^logSize stream cycles. */
case class SwitchTranspose[T](logSize: Int) extends Transform[T](2 * logSize):
  require(logSize > 0)
  private val permutation = LinearPerm.Lmat(logSize, 2 * logSize)

  override def eval(inputs: Seq[T], set: Int): Seq[T] = LinearPerm.permute(permutation, inputs)

  override def stream(k: Int, control: RAMControl)(using HW[T]): StreamingModule[T] =
    require(k == logSize, s"switchtranspose requires k=$logSize for n=${2 * logSize}, got k=$k")
    ir.rtl.SwitchTranspose[T](logSize)

  override def testParams: PartialFunction[HW[T], (Seq[T], Double)] =
    case hw =>
      def values: Iterator[T] = hw.values ++ values
      (values.take((1 << (2 * logSize)) * 5).toSeq, 0)
