/*
 * Copyright (c) 2019-2021 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

package object internal {

  type XIterableOnceBound[+T] = TraversableOnce[T]

  implicit final class XIterableOnce[T](val underlying: TraversableOnce[T]) extends AnyVal {
    def knownSize: Int = -1

    def iterator: Iterator[T] =
      underlying match {
        case x: Iterable[T] => x.iterator
        case x              => x.toIterable.iterator
      }
  }

  def unapplyOption[P, T](f: P => Option[T]): P => Option[T] = f
}
