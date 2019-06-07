/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.input

import java.nio.charset.StandardCharsets

import io.bullet.borer._
import io.bullet.borer.internal.Util.RichIterator

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait FromInputIteratorInput {

  implicit def inputIteratorProvider[T](implicit p: Input.Provider[T]): Input.Provider[Iterator[T]] =
    new Input.Provider[Iterator[T]] {
      type Bytes = p.Bytes
      type In    = FromInputIterator[p.Bytes]
      def byteAccess                = p.byteAccess
      def apply(value: Iterator[T]) = new FromInputIterator[p.Bytes](value.map(p(_)), byteAccess)
    }

  final class FromInputIterator[Bytes](
      private[this] var inputIterator: Iterator[Input[Bytes]],
      byteAccess: ByteAccess[Bytes])
      extends Input.PaddingProvider[Bytes] with Input[Bytes] {

    private[this] var history                                            = List.empty[Input[Bytes]]
    private[this] var previous: Input[Bytes]                             = _
    private[this] var current: Input[Bytes]                              = _
    private[this] var currentStart: Long                                 = _
    private[this] var outerPaddingProvider: Input.PaddingProvider[Bytes] = _
    private[this] var padBytesRecursion                                  = false
    private[this] var padBytesRecursionRest: Bytes                       = _
    private[this] var padBytesRecursionMissing: Long                     = _

    private def currentCursor: Long = if (current ne null) current.cursor else 0L

    def cursor: Long = currentStart + currentCursor

    def moveCursor(offset: Int): this.type = {
      def rollBack(cursor: Long, target: Long): Unit = {
        @tailrec def rec(input: Input[Bytes], index: Long, remainingInputs: Iterator[Input[Bytes]]): Unit = {
          val inputCursor = input.cursor
          val inputStart  = index - inputCursor
          if (inputStart <= target) {
            input.moveCursor((target - index).toInt)
            previous = null
            current = input
            inputIterator = remainingInputs
          } else {
            input.moveCursor(-inputCursor.toInt)
            val head :: tail = history // mismatch => illegal state: rollback too far
            history = tail
            rec(head, inputStart, input +: inputIterator)
          }
        }
        rec(previous, cursor, current +: inputIterator)
      }

      val currentCursor = this.currentCursor
      if (currentCursor + offset < 0) rollBack(currentStart + currentCursor, cursor + offset)
      else current.moveCursor(offset)
      this
    }

    def readByte(): Byte = current.readByte()

    def readBytePadded(pp: Input.PaddingProvider[Bytes]): Byte = {
      outerPaddingProvider = pp
      if (current ne null) current.readBytePadded(this)
      else pp.padByte()
    }

    def padByte(): Byte =
      if (inputIterator.hasNext) {
        fetchNext(0)
        current.readBytePadded(this)
      } else outerPaddingProvider.padByte()

    def readDoubleByteBigEndian(): Char = current.readDoubleByteBigEndian()

    def readDoubleByteBigEndianPadded(pp: Input.PaddingProvider[Bytes]): Char = {
      outerPaddingProvider = pp
      if (current ne null) current.readDoubleByteBigEndianPadded(this)
      else pp.padDoubleByte(0)
    }

    def padDoubleByte(remaining: Int): Char =
      if (inputIterator.hasNext) {
        fetchNext(remaining)
        if (remaining < 1) current.readDoubleByteBigEndianPadded(this)
        else ((previous.readByte() << 8) | current.readBytePadded(this) & 0xFF).toChar
      } else outerPaddingProvider.padDoubleByte(remaining)

    def readQuadByteBigEndian(): Int = current.readQuadByteBigEndian()

    def readQuadByteBigEndianPadded(pp: Input.PaddingProvider[Bytes]): Int = {
      outerPaddingProvider = pp
      if (current ne null) current.readQuadByteBigEndianPadded(this)
      else pp.padQuadByte(0)
    }

    def padQuadByte(remaining: Int): Int =
      if (inputIterator.hasNext) {
        fetchNext(remaining)
        remaining match {
          case 0 => current.readQuadByteBigEndianPadded(this)
          case 1 =>
            (previous.readByte() << 24) |
              (current.readDoubleByteBigEndianPadded(this) << 8) |
              current.readBytePadded(this) & 0xFF
          case 2 => (previous.readDoubleByteBigEndian() << 16) | current.readDoubleByteBigEndianPadded(this)
          case 3 =>
            (previous.readDoubleByteBigEndian() << 16) |
              ((previous.readByte() & 0xFF) << 8) |
              current.readBytePadded(this) & 0xFF
          case _ => throw new IllegalStateException
        }
      } else outerPaddingProvider.padQuadByte(remaining)

    def readOctaByteBigEndian(): Long = current.readOctaByteBigEndian()

    def readOctaByteBigEndianPadded(pp: Input.PaddingProvider[Bytes]): Long = {
      outerPaddingProvider = pp
      if (current ne null) current.readOctaByteBigEndianPadded(this)
      else pp.padOctaByte(0)
    }

    def padOctaByte(remaining: Int): Long =
      if (inputIterator.hasNext) {
        fetchNext(remaining)
        remaining match {
          case 0 => current.readOctaByteBigEndianPadded(this)
          case 1 =>
            (previous.readByte().toLong << 56) |
              ((current.readQuadByteBigEndianPadded(this) & 0XFFFFFFFFL) << 24) |
              ((current.readDoubleByteBigEndianPadded(this) & 0XFFFFL) << 8) |
              current.readBytePadded(this) & 0XFFL
          case 2 =>
            (previous.readDoubleByteBigEndian().toLong << 48) |
              ((current.readQuadByteBigEndianPadded(this) & 0XFFFFFFFFL) << 16) |
              current.readDoubleByteBigEndianPadded(this) & 0XFFFFL
          case 3 =>
            (previous.readDoubleByteBigEndian().toLong << 48) |
              ((previous.readByte() & 0XFFL) << 40) |
              ((current.readQuadByteBigEndianPadded(this) & 0XFFFFFFFFL) << 8) |
              current.readBytePadded(this) & 0XFFL
          case 4 =>
            (previous.readQuadByteBigEndian().toLong << 32) | (current.readQuadByteBigEndianPadded(this) & 0XFFFFFFFFL)
          case 5 =>
            (previous.readQuadByteBigEndian().toLong << 32) |
              ((previous.readByte() & 0XFFL) << 24) |
              ((current.readDoubleByteBigEndianPadded(this) & 0XFFFFL) << 8) |
              current.readBytePadded(this) & 0XFFL
          case 6 =>
            (previous.readQuadByteBigEndian().toLong << 32) |
              ((previous.readDoubleByteBigEndian() & 0XFFFFL) << 16) |
              current.readDoubleByteBigEndianPadded(this) & 0XFFFFL
          case 7 =>
            (previous.readQuadByteBigEndian().toLong << 32) |
              ((previous.readDoubleByteBigEndian() & 0XFFFFL) << 16) |
              ((previous.readByte() & 0XFFL) << 8) |
              current.readBytePadded(this) & 0XFFL
          case _ => throw new IllegalStateException
        }
      } else outerPaddingProvider.padOctaByte(remaining)

    def readBytes(length: Long, pp: Input.PaddingProvider[Bytes]): Bytes = {
      outerPaddingProvider = pp
      if (current ne null) current.readBytes(length, this)
      else pp.padBytes(byteAccess.empty, length)
    }

    def padBytes(rest: Bytes, missing: Long): Bytes =
      if (!padBytesRecursion) {
        padBytesRecursion = true

        @tailrec def rec(result: Bytes, missing: Long): Bytes = {
          padBytesRecursionRest = byteAccess.empty
          padBytesRecursionMissing = 0L
          if (inputIterator.hasNext) {
            fetchNext(0)
            val nextBytes = current.readBytes(missing, this)
            if (padBytesRecursionMissing == 0) byteAccess.concat(result, nextBytes)
            else rec(byteAccess.concat(result, padBytesRecursionRest), padBytesRecursionMissing)
          } else outerPaddingProvider.padBytes(result, missing)
        }

        val result = rec(rest, missing)
        padBytesRecursion = false
        result
      } else {
        padBytesRecursionRest = rest
        padBytesRecursionMissing = missing
        byteAccess.empty
      }

    def precedingBytesAsAsciiString(length: Int): String =
      if (currentCursor < length) {
        moveCursor(-length)
        val bytes = byteAccess.toByteArray(readBytes(length.toLong, this))
        new String(bytes, StandardCharsets.ISO_8859_1)
      } else current.precedingBytesAsAsciiString(length)

    private def fetchNext(remaining: Int): Unit = {
      val currentCursor = this.currentCursor
      val cursor        = currentStart + currentCursor

      history = if (currentCursor < 256) {
        // keep the prefix of the history that is required to maintain a total history length of >= 256 bytes
        @tailrec def rec(rest: List[Input[Bytes]], size: Long, buf: ListBuffer[Input[Bytes]]): List[Input[Bytes]] =
          if (size < 256 && rest.nonEmpty) rec(rest.tail, size + rest.head.cursor, buf += rest.head)
          else buf.toList
        rec(history, previous.cursor, new ListBuffer[Input[Bytes]] += previous)
      } else Nil

      previous = current
      currentStart = cursor + remaining.toLong
      current = inputIterator.next()
    }
  }
}
