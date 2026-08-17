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

package ir.rtl

import ir.rtl.hardwaretype.HW
import ir.spl.SPL
import transforms.perm.LinearPerm

/** Full-throughput square lane/time transpose built from recursive switch units. */
case class SwitchTranspose[T: HW](logSize: Int) extends StreamingModule[T](logSize, logSize):
  require(logSize > 0)

  override val latency: Int = T - 1
  override val spl: SPL[T] = LinearPerm[T](LinearPerm.Lmat(logSize, 2 * logSize))

  override def implement(rst: Component, token: Int => Component, inputs: Seq[Component]): Seq[Component] =
    require(inputs.size == K)
    val validIn = Or(Vector.tabulate(T)(token))
    val packed = SwitchTransposeNetworkComponent(inputs, validIn, rst, logSize, hw.size)
    Vector.tabulate(K)(lane => Tap(packed, lane * hw.size until (lane + 1) * hw.size))
