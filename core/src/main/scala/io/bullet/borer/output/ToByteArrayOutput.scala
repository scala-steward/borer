/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.output

import io.bullet.borer.{Borer, ByteAccess, Output}
import io.bullet.borer.Output.ToTypeProvider

import scala.annotation.tailrec

trait ToByteArrayOutput {

  implicit object ToByteArrayProvider extends ToTypeProvider[Array[Byte]] {
    type Out = ToByteArray
    def apply(bufferSize: Int) = new ToByteArray(bufferSize)
  }

  /**
    * Default, mutable implementation for serializing to plain byte arrays.
    */
  final class ToByteArray(bufferSize: Int) extends Output {
    private[this] var currentChunkBuffer            = new Array[Byte](bufferSize)
    private[this] var currentChunkBufferCursor: Int = _
    private[this] val rootChunk                     = new Chunk(currentChunkBuffer, next = null)
    private[this] var currentChunk                  = rootChunk
    private[this] var filledChunksSize              = 0L

    type Self   = ToByteArray
    type Result = Array[Byte]

    @inline def size: Long = filledChunksSize + currentChunkBufferCursor.toLong

    def writeByte(byte: Byte): this.type = {
      if (currentChunkBufferCursor == bufferSize) appendChunk()
      val cursor = currentChunkBufferCursor
      currentChunkBuffer(cursor) = byte
      currentChunkBufferCursor = cursor + 1
      this
    }

    def writeBytes(a: Byte, b: Byte): this.type =
      if (currentChunkBufferCursor < bufferSize - 1) {
        val cursor = currentChunkBufferCursor
        currentChunkBuffer(cursor) = a
        currentChunkBuffer(cursor + 1) = b
        currentChunkBufferCursor = cursor + 2
        this
      } else writeByte(a).writeByte(b)

    def writeBytes(a: Byte, b: Byte, c: Byte): this.type =
      if (currentChunkBufferCursor < bufferSize - 2) {
        val cursor = currentChunkBufferCursor
        currentChunkBuffer(cursor) = a
        currentChunkBuffer(cursor + 1) = b
        currentChunkBuffer(cursor + 2) = c
        currentChunkBufferCursor = cursor + 3
        this
      } else writeByte(a).writeByte(b).writeByte(c)

    def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte): this.type =
      if (currentChunkBufferCursor < bufferSize - 3) {
        val cursor = currentChunkBufferCursor
        currentChunkBuffer(cursor) = a
        currentChunkBuffer(cursor + 1) = b
        currentChunkBuffer(cursor + 2) = c
        currentChunkBuffer(cursor + 3) = d
        currentChunkBufferCursor = cursor + 4
        this
      } else writeByte(a).writeByte(b).writeByte(c).writeByte(d)

    def writeBytes[Bytes](bytes: Bytes)(implicit byteAccess: ByteAccess[Bytes]): this.type = {
      @tailrec def rec(rest: Bytes): this.type = {
        val remaining = bufferSize - currentChunkBufferCursor
        val len       = byteAccess.sizeOf(rest)
        val newRest   = byteAccess.copyToByteArray(rest, currentChunkBuffer, currentChunkBufferCursor)
        if (len > remaining) {
          appendChunk()
          rec(newRest)
        } else {
          currentChunkBufferCursor += len.toInt
          if (currentChunkBufferCursor < 0) throw new Borer.Error.Overflow(this, f"Output size exceed 2^31 bytes")
          this
        }
      }
      rec(bytes)
    }

    def result(): Array[Byte] = {
      val longSize = size
      val intSize  = longSize.toInt
      if (intSize != longSize) {
        throw new Borer.Error.Overflow(this, f"Output size of $longSize%,d bytes too large for byte array")
      }
      val array = new Array[Byte](intSize)

      @tailrec def rec(chunk: Chunk[Array[Byte]], cursor: Int): Array[Byte] =
        if (chunk ne null) {
          val len = if (chunk.next eq null) currentChunkBufferCursor else bufferSize
          System.arraycopy(chunk.buffer, 0, array, cursor, len)
          rec(chunk.next, cursor + bufferSize)
        } else array

      rec(rootChunk, 0)
    }

    private def appendChunk(): Unit = {
      currentChunkBuffer = new Array[Byte](bufferSize)
      val newChunk = new Chunk(currentChunkBuffer, null)
      currentChunkBufferCursor = 0
      currentChunk.next = newChunk
      currentChunk = newChunk
      filledChunksSize += bufferSize.toLong
    }

    override def toString = s"Output.ToByteArray index $size"
  }

  final private class Chunk[T <: AnyRef](val buffer: T, var next: Chunk[T])
}
