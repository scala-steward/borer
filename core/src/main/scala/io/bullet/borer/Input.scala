/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import io.bullet.borer.input.{FromByteArrayInput, FromByteBufferInput, FromFileInput}

/**
  * Mutable abstraction wrapping some source of bytes to serve as parser input.
  */
trait Input[Bytes] {

  /**
    * The index of the next byte to be read.
    */
  def cursor: Long

  /**
    * Moves the cursor by the given offset, which is guaranteed to be in the range [-255, 1].
    * This method will only ever be used to move the cursor back "a little" from the current reading head,
    * so a cache of the last 256 bytes will always suffice.
    */
  def moveCursor(offset: Int): this.type

  /**
    * Returns the next byte, if possible without any range checks.
    * Advances the cursor by 1.
    */
  def readByte(): Byte

  /**
    * Returns the next byte if not out-of-range, otherwise the one returned by the current [[Input.PaddingProvider]].
    */
  def readBytePadded(pp: Input.PaddingProvider[Bytes]): Byte

  /**
    * Returns the next two bytes as an unsigned 16-bit value,
    * with the first becoming the more-significant byte (i.e. big endian/network byte order),
    * if possible without any range checks.
    * Advances the cursor by 2.
    */
  def readDoubleByteBigEndian(): Char

  /**
    * Returns the next two bytes as an unsigned 16-bit value,
    * with the first becoming the more-significant byte (i.e. big endian/network byte order).
    * If the input has less than 2 bytes left the current [[Input.PaddingProvider]] is called to perform the padding
    * and its result returned.
    */
  def readDoubleByteBigEndianPadded(pp: Input.PaddingProvider[Bytes]): Char

  /**
    * Returns the next four bytes as an [[Int]],
    * with the first becoming the most-significant byte (i.e. big endian/network byte order),
    * if possible without any range checks.
    * Advances the cursor by 4.
    */
  def readQuadByteBigEndian(): Int

  /**
    * Returns the next four bytes as an [[Int]],
    * with the first becoming the most-significant byte (i.e. big endian/network byte order).
    * If the input has less than 4 bytes left the current [[Input.PaddingProvider]] is called to perform the padding
    * and its result returned.
    */
  def readQuadByteBigEndianPadded(pp: Input.PaddingProvider[Bytes]): Int

  /**
    * Returns the eight eight bytes as a [[Long]],
    * with the first becoming the most-significant byte (i.e. big endian/network byte order),
    * if possible without any range checks.
    * Advances the cursor by 8.
    */
  def readOctaByteBigEndian(): Long

  /**
    * Returns the next eight bytes as a [[Long]],
    * with the first becoming the most-significant byte (i.e. big endian/network byte order).
    * If the input has less than 8 bytes left the current [[Input.PaddingProvider]] is called to perform the padding
    * and its result returned.
    */
  def readOctaByteBigEndianPadded(pp: Input.PaddingProvider[Bytes]): Long

  /**
    * Returns the next `length` bytes as [[Bytes]] if the input still has this many bytes available.
    * Otherwise the current [[Input.PaddingProvider]] is called to perform the padding and its result returned.
    */
  def readBytes(length: Long, pp: Input.PaddingProvider[Bytes]): Bytes

  /**
    * Returns the given number of bytes _before_ the current cursor position as an ASCII string.
    * Does not move the cursor.
    *
    * The given `length` is guaranteed to be in the range [0, 255].
    */
  def precedingBytesAsAsciiString(length: Int): String
}

object Input extends FromByteArrayInput with FromByteBufferInput with FromFileInput {

  abstract class PaddingProvider[Bytes] {
    def padByte(): Byte
    def padDoubleByte(remaining: Int): Char
    def padQuadByte(remaining: Int): Int
    def padOctaByte(remaining: Int): Long
    def padBytes(rest: Bytes, missing: Long): Bytes
  }

  implicit final class InputOps[Bytes](val underlying: Input[Bytes]) extends AnyVal {
    def position(cursor: Long): Input.Position = Input.Position(underlying, cursor)
  }

  /**
    * Responsible for converting an instance of [[T]] in a respective [[Input]] instance.
    */
  trait Provider[T] {
    type Bytes
    type In <: Input[Bytes]
    def byteAccess: ByteAccess[Bytes]
    def apply(value: T): In
  }

  final case class Position(input: Input[_], index: Long) {
    override def toString = s"input position $index"
  }
}
