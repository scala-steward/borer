/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation.internal

/**
  * This annotation can be attached to the implicit `gen` method of a type class companion,
  * which is implemented by the `Magnolia.gen` macro.
  * It causes magnolia to dump the macro-generated code to the console during compilation.
  *
  * @param typeNamePart If non-empty restricts the output generation to types
  *                     whose full name contains the given [[String]]
  */
final private[derivation] class debug(typeNamePart: String = "") extends scala.annotation.StaticAnnotation
