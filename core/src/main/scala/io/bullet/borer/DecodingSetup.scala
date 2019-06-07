/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer

import java.lang.{StringBuilder => JStringBuilder}

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object DecodingSetup {

  sealed trait Api[In <: Input[_], Config <: Borer.DecodingConfig] {

    /**
      * Indicates that this decoding run is not expected to consume the complete [[Input]].
      */
    def withPrefixOnly: this.type

    /**
      * Configures the [[Config]] for this decoding run.
      */
    def withConfig(config: Config): this.type

    /**
      * Enables logging of this decoding run to the console.
      * Each data item that is consumed from the underlying CBOR stream is pretty printed to the console
      * on its own line.
      */
    def withPrintLogging(maxShownByteArrayPrefixLen: Int = 20, maxShownStringPrefixLen: Int = 50): this.type

    /**
      * Enables logging of this decoding run to the given [[JStringBuilder]].
      * Each data item that is consumed from the underlying CBOR stream is formatted and appended as its own line.
      */
    def withStringLogging(
        stringBuilder: JStringBuilder,
        maxShownByteArrayPrefixLen: Int = 20,
        maxShownStringPrefixLen: Int = 50,
        lineSeparator: String = System.lineSeparator()): this.type

    /**
      * Allows for injecting custom logic into the decoding process.
      * Used, for example, for on-the-side [[Logging]].
      */
    def withWrapper(receiverWrapper: Receiver.Wrapper[Config]): this.type

    /**
      * Decodes an instance of [[T]] from the configured [[Input]] using the configured options.
      */
    def to[T: Decoder]: Sealed[In, T]
  }

  sealed trait Sealed[In <: Input[_], T] {

    def value: T

    def valueTry: Try[T]

    def valueEither: Either[Borer.Error[Input.Position], T]

    def valueAndInput: (T, In)

    def valueAndInputTry: Try[(T, In)]

    def valueAndInputEither: Either[Borer.Error[Input.Position], (T, In)]
  }

  final private[borer] class Impl[Bytes, In <: Input[Bytes], Config <: Borer.DecodingConfig](
      input: In,
      byteAccess: ByteAccess[Bytes],
      defaultConfig: Config,
      defaultWrapper: Receiver.Wrapper[Config],
      parserCreator: Receiver.ParserCreator[Bytes, Config],
      target: Target)
      extends Borer.AbstractSetup[Config](defaultConfig, defaultWrapper) with Api[In, Config] with Sealed[In, AnyRef] {

    private[this] var prefixOnly: Boolean      = _
    private[this] var decoder: Decoder[AnyRef] = _

    def withPrefixOnly: this.type = {
      this.prefixOnly = true
      this
    }

    def to[T](implicit decoder: Decoder[T]): Sealed[In, T] = {
      this.decoder = decoder.asInstanceOf[Decoder[AnyRef]]
      this.asInstanceOf[Sealed[In, T]]
    }

    def value: AnyRef = {
      val reader = newReader()
      try {
        decodeFrom(reader)
      } catch {
        case e: Borer.Error[_] => throw e.withPosOf(reader)
        case NonFatal(e)       => throw new Borer.Error.General(reader.position, e)
      }
    }

    def valueTry: Try[AnyRef] = {
      val reader = newReader()
      try {
        Success(decodeFrom(reader))
      } catch {
        case e: Borer.Error[_] => Failure(e.withPosOf(reader))
        case NonFatal(e)       => Failure(new Borer.Error.General(reader.position, e))
      }
    }

    def valueEither: Either[Borer.Error[Input.Position], AnyRef] = {
      val reader = newReader()
      try {
        Right(decodeFrom(reader))
      } catch {
        case e: Borer.Error[_] => Left(e.withPosOf(reader))
        case NonFatal(e)       => Left(new Borer.Error.General(reader.position, e))
      }
    }

    def valueAndInput: (AnyRef, In) = {
      val reader = newReader()
      try {
        decodeFrom(reader) -> input
      } catch {
        case e: Borer.Error[_] => throw e.withPosOf(reader)
        case NonFatal(e)       => throw new Borer.Error.General(reader.position, e)
      }
    }

    def valueAndInputTry: Try[(AnyRef, In)] = {
      val reader = newReader()
      try {
        Success(decodeFrom(reader) -> input)
      } catch {
        case e: Borer.Error[_] => Failure(e.withPosOf(reader))
        case NonFatal(e)       => Failure(new Borer.Error.General(reader.position, e))
      }
    }

    def valueAndInputEither: Either[Borer.Error[Input.Position], (AnyRef, In)] = {
      val reader = newReader()
      try {
        Right(decodeFrom(reader) -> input)
      } catch {
        case e: Borer.Error[_] => Left(e.withPosOf(reader))
        case NonFatal(e)       => Left(new Borer.Error.General(reader.position, e))
      }
    }

    private def newReader(): Reader =
      new InputReader(parserCreator(input, byteAccess, config), receiverWrapper, config, target)

    private def decodeFrom(reader: Reader): AnyRef = {
      val value = decoder.read(reader)
      if (!prefixOnly) reader.readEndOfInput()
      value
    }
  }
}
