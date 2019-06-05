/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.cbor

import java.lang.{Double => JDouble, Float => JFloat}

import io.bullet.borer.{Borer, _}
import io.bullet.borer.internal.Util

import scala.annotation.{switch, tailrec}

/**
  * Encapsulates the basic CBOR decoding logic.
  */
final private[borer] class CborParser[In <: Input](val input: In, config: CborParser.Config)
    extends Receiver.Parser[In] {

  private[this] var _valueIndex: Long = _

  def valueIndex: Long = _valueIndex

  /**
    * Reads the next data item from the input and sends it to the given [[Receiver]].
    * The given [[Receiver]] receives exactly one call to one of its methods.
    * The returned `Int` is the [[DataItem]] code for the value the [[Receiver]] received.
    */
  def readNextDataItem(receiver: Receiver): Int = {

    @inline def decodePositiveInteger(uLong: Long): Int = {
      if (Util.isUnsignedInt(uLong)) {
        receiver.onInt(uLong.toInt)
        DataItem.Int
      } else if (Util.isUnsignedLong(uLong)) {
        receiver.onLong(uLong)
        DataItem.Long
      } else {
        receiver.onOverLong(negative = false, uLong)
        DataItem.OverLong
      }
    }

    @inline def decodeNegativeInteger(uLong: Long): Int = {
      if (Util.isUnsignedInt(uLong)) {
        receiver.onInt((~uLong).toInt)
        DataItem.Int
      } else if (Util.isUnsignedLong(uLong)) {
        receiver.onLong(~uLong)
        DataItem.Long
      } else {
        receiver.onOverLong(negative = true, uLong)
        DataItem.OverLong
      }
    }

    @inline def decodeByteString(uLong: Long, indefiniteLength: Boolean): Int =
      if (indefiniteLength) {
        receiver.onBytesStart()
        DataItem.BytesStart
      } else if (Util.isUnsignedLong(uLong)) {
        if (!input.prepareRead(uLong)) failUnexpectedEOI(s"ByteString with length $uLong")
        receiver.onBytes(input.readBytes(uLong))(input.byteAccess)
        DataItem.Bytes
      } else failOverflow("This decoder does not support byte strings with size >= 2^63")

    @inline def decodeTextString(uLong: Long, indefiniteLength: Boolean): Int =
      if (indefiniteLength) {
        receiver.onTextStart()
        DataItem.TextStart
      } else if (Util.isUnsignedLong(uLong)) {
        if (!input.prepareRead(uLong)) failUnexpectedEOI(s"TextString with length $uLong")
        receiver.onText(input.readBytes(uLong))(input.byteAccess)
        DataItem.Text
      } else failOverflow("This decoder does not support text strings with size >= 2^63")

    @inline def decodeArray(uLong: Long, indefiniteLength: Boolean): Int = {
      if (indefiniteLength) {
        receiver.onArrayStart()
        DataItem.ArrayStart
      } else if (Util.isUnsignedLong(uLong)) {
        receiver.onArrayHeader(uLong)
        DataItem.ArrayHeader
      } else failOverflow("This decoder does not support arrays with size >= 2^63")
    }

    @inline def decodeMap(uLong: Long, indefiniteLength: Boolean): Int = {
      if (indefiniteLength) {
        receiver.onMapStart()
        DataItem.MapStart
      } else if (Util.isUnsignedLong(uLong)) {
        receiver.onMapHeader(uLong)
        DataItem.MapHeader
      } else failOverflow("This decoder does not support maps with size >= 2^63")
    }

    def decodeTag(uLong: Long): Int = {
      val tag =
        uLong match {
          case 0     => Tag.DateTimeString
          case 1     => Tag.EpochDateTime
          case 2     => Tag.PositiveBigNum
          case 3     => Tag.NegativeBigNum
          case 4     => Tag.DecimalFraction
          case 5     => Tag.BigFloat
          case 21    => Tag.HintBase64url
          case 22    => Tag.HintBase64
          case 23    => Tag.HintBase16
          case 24    => Tag.EmbeddedCBOR
          case 32    => Tag.TextUri
          case 33    => Tag.TextBase64Url
          case 34    => Tag.TextBase64
          case 35    => Tag.TextRegex
          case 36    => Tag.TextMime
          case 55799 => Tag.MagicHeader
          case x     => Tag.Other(x)
        }
      receiver.onTag(tag)
      DataItem.Tag
    }

    @inline def decodeExtra(info: Int, uLong: Long): Int =
      (info: @switch) match {
        case 20 =>
          receiver.onBoolean(value = false)
          DataItem.Boolean
        case 21 =>
          receiver.onBoolean(value = true)
          DataItem.Boolean
        case 22 =>
          receiver.onNull()
          DataItem.Null
        case 23 =>
          receiver.onUndefined()
          DataItem.Undefined
        case 24 =>
          uLong.toInt match {
            case x if SimpleValue.isLegal(x) => receiver.onSimpleValue(x)
            case x                           => failInvalidInput(s"Simple value must be in the range ${SimpleValue.legalRange}, but was $x")
          }
          DataItem.SimpleValue
        case 25 =>
          receiver.onFloat16(Float16.shortToFloat(uLong.toInt))
          DataItem.Float16
        case 26 =>
          receiver.onFloat(JFloat.intBitsToFloat(uLong.toInt))
          DataItem.Float
        case 27 =>
          receiver.onDouble(JDouble.longBitsToDouble(uLong))
          DataItem.Double
        case 31 =>
          receiver.onBreak()
          DataItem.Break
        case x =>
          if (SimpleValue.isLegal(x)) {
            receiver.onSimpleValue(x)
            DataItem.SimpleValue
          } else failUnsupported(s"CBOR major type 7 code $x is unsupported by this decoder")
      }

    _valueIndex = input.cursor
    if (input.prepareRead(1)) {
      val byte      = input.readByte()
      val majorType = byte << 24 >>> 29
      val info      = byte & 0x1F
      val uLong     = readULong(majorType, info)
      val result = (majorType: @switch) match {
        case 0 => decodePositiveInteger(uLong)
        case 1 => decodeNegativeInteger(uLong)
        case 2 => decodeByteString(uLong, info == 31)
        case 3 => decodeTextString(uLong, info == 31)
        case 4 => decodeArray(uLong, info == 31)
        case 5 => decodeMap(uLong, info == 31)
        case 6 => decodeTag(uLong)
        case 7 => decodeExtra(info, uLong)
      }
      input.releaseBeforeCursor()
      result
    } else {
      receiver.onEndOfInput()
      DataItem.EndOfInput
    }
  }

  def tryReadStringCompare(utf8Bytes: Input.FromByteArray): Int = {

    def compareTextString(majorType: Int, info: Int): Int = {
      val uLong = readULong(majorType, info)

      @tailrec def rec(inputRemaining: Int): Int =
        if (inputRemaining > 0) {
          val inputOcta =
            if (inputRemaining >= 8) input.readOctaByteBigEndian()
            else input.readOctaByteBigEndianPadded(inputRemaining, 0L)
          val testOcta = utf8Bytes.readOctaByteBigEndianPadded00()
          val cmp      = java.lang.Long.compareUnsigned(inputOcta, testOcta)
          if (cmp == 0) rec(inputRemaining - 8) else cmp
        } else {
          input.moveCursor(inputRemaining) // correct "overreading" the end of the textstring
          uLong.toInt - utf8Bytes.byteArray.length
        }

      if (!Util.isUnsignedInt(uLong)) failOverflow("This decoder does not support map key TextStrings w/ size >= 2^31")
      if (!input.prepareRead(uLong)) failUnexpectedEOI(s"Map key TextString with length $uLong")
      rec(uLong.toInt)
    }

    @tailrec def compareTextStream(level: Int): Int =
      if (input.prepareRead(1)) {
        val byte = input.readByte()
        if (byte == -1) { // BREAK
          if (level > 0) compareTextStream(level - 1) else 0
        } else {
          val majorType = byte << 24 >>> 29
          if (majorType == 3) {
            val info = byte & 0x1F
            if (info != 31) {
              val cmp = compareTextString(majorType, info)
              if (cmp == 0) compareTextStream(level) else cmp
            } else compareTextStream(level + 1)
          } else failInvalidInput(s"Expected TextString or BREAK but got major type $majorType")
        }
      } else failUnexpectedEOI("BREAK")

    val mark = input.cursor
    if (input.prepareRead(1)) {
      val byte      = input.readByte()
      val majorType = byte << 24 >>> 29
      if (majorType == 3) {
        val info = byte & 0x1F
        val cmp =
          if (info == 31) compareTextStream(0)
          else compareTextString(majorType, info)
        if (cmp == 0) {
          input.releaseBeforeCursor()
          0
        } else {
          input.resetTo(mark)
          cmp
        }
      } else Int.MinValue
    } else Int.MinValue
  }

  def tryReadBreak() = {
    val break = input.readByteOr00() == -1
    if (!break) input.moveCursor(-1)
    break
  }

  private def readULong(majorType: Int, info: Int): Long =
    (info: @switch) match {
      case 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 |
          23 =>
        info.toLong
      case 24 =>
        if (!input.prepareRead(1)) failUnexpectedEOI("8-bit integer")
        input.readByte() & 0XFFL
      case 25 =>
        if (!input.prepareRead(2)) failUnexpectedEOI("16-bit integer")
        input.readDoubleByteBigEndian() & 0XFFFFL
      case 26 =>
        if (!input.prepareRead(4)) failUnexpectedEOI("32-bit integer")
        input.readQuadByteBigEndian() & 0XFFFFFFFFL
      case 27 =>
        if (!input.prepareRead(8)) failUnexpectedEOI("64-bit integer")
        input.readOctaByteBigEndian()
      case 31 if 2 <= majorType && majorType <= 5 || majorType == 7 =>
        0L // handled specially
      case 28 | 29 | 30 => failInvalidInput(s"Additional info `$info` is invalid (major type `$majorType`)")
    }

  private def failUnexpectedEOI(expected: String) = throw new Borer.Error.UnexpectedEndOfInput(lastPos, expected)
  private def failInvalidInput(msg: String)       = throw new Borer.Error.InvalidInputData(lastPos, msg)
  private def failOverflow(msg: String)           = throw new Borer.Error.Overflow(lastPos, msg)
  private def failUnsupported(msg: String)        = throw new Borer.Error.Unsupported(lastPos, msg)

  private def lastPos = input.position(_valueIndex)
}

object CborParser {

  trait Config {
    def maxByteStringLength: Int
    def maxTextStringLength: Int
  }

  private[this] val _creator: Receiver.ParserCreator[Input, CborParser.Config] =
    (input, config) => new CborParser[Input](input, config)

  def creator[In <: Input, Conf <: CborParser.Config]: Receiver.ParserCreator[In, Conf] =
    _creator.asInstanceOf[Receiver.ParserCreator[In, Conf]]
}
