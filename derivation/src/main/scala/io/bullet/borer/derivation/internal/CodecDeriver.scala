/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.derivation.internal

import io.bullet.borer.derivation.key

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox
import scala.util.control.NonFatal

abstract private[derivation] class CodecDeriver[C <: blackbox.Context](ctx: C) extends Deriver[C](ctx) {
  import c.universe._
  import MacroSupport._

  def deriveForCaseObject(tpe: Type, module: ModuleSymbol) =
    error(
      s"Cannot derive Encoder for case object `$tpe`. " +
        "You'll have to implement a custom encoding strategy or turn the case object into a nullary case class.")

  def adtSubtypeWritingCases(tpe: Type, subTypes: List[SubType]) = {
    val typeIds = getTypeIds(tpe, subTypes)
    val cases   = new ListBuffer[Tree]
    subTypes.zip(typeIds).foreach {
      case (subType, typeId) =>
        val writeTypeId = TermName(s"write${typeId.productPrefix}")
        cases += cq"x: ${subType.tpe} => w.$writeTypeId(${literal(typeId.value)}).write(x)"
    }
    val errorMsg = s"""s"The given value `$$x` is not a sub type of `$tpe`""""
    cases += cq"""x => throw new IllegalArgumentException($errorMsg)"""
    cases.toList
  }

  def r(methodNamePrefix: String, key: Key, methodNameSuffix: String = "") = {
    val method = TermName(s"$methodNamePrefix${key.productPrefix}$methodNameSuffix")
    q"r.$method(${literal(key.value)})"
  }

  def getTypeIds(tpe: Type, subTypes: List[SubType]): Array[Key] = {
    val annos: Array[Key] = subTypes.map(_.key())(collection.breakOut)

    @tailrec def rec(i: Int, j: Int): Array[Key] =
      if (i < annos.length) {
        if (j < annos.length) {
          if (i != j && annos(i) == annos(j)) {
            c.abort(
              tpe.typeSymbol.pos,
              s"@key collision: sub types `${subTypes(i).tpe}` and `${subTypes(j).tpe}` " +
                s"of ADT `$tpe` share the same type id `${annos(i).value}`")
          } else rec(i, j + 1)
        } else rec(i + 1, 0)
      } else annos
    rec(0, 0)
  }

  def isBasicType(tpe: Type): Boolean =
    tpe =:= typeOf[String] ||
      tpe =:= definitions.IntTpe ||
      tpe =:= definitions.LongTpe ||
      tpe =:= definitions.BooleanTpe ||
      tpe =:= definitions.DoubleTpe ||
      tpe =:= definitions.FloatTpe ||
      tpe =:= definitions.CharTpe ||
      tpe =:= definitions.ByteTpe ||
      tpe =:= definitions.ShortTpe

  def eval[T](tree: Tree): T = c.eval[T](c.Expr[T](c.untypecheck(tree.duplicate)))

  def literal(value: Any) = Literal(Constant(value))

  def toType(typeTree: Tree): Type = c.typecheck(typeTree, c.TYPEmode).tpe

  implicit class RichCaseParam(underlying: CaseParam) {
    def isBasicType: Boolean         = CodecDeriver.this.isBasicType(underlying.paramType.tpe)
    def basicTypeNameOrEmpty: String = if (isBasicType) underlying.paramType.tpe.toString else ""
  }

  implicit class RichWithAnnotations(underlying: WithAnnotations) {

    def key(): Key = {
      val keyAnnoTrees = underlying.annotations.filter(_.tpe =:= typeOf[key])
      val keyAnnos = keyAnnoTrees.map { anno =>
        try {
          eval[key](anno).value match {
            case x: String => Key.String(x)
            case x: Int    => Key.Long(x.toLong)
            case x: Long   => Key.Long(x)
            case _         => c.abort(anno.pos, s"The '@key' annotation only supports String or Int/Long arguments.")
          }
        } catch {
          case NonFatal(_) => c.abort(anno.pos, s"Cannot evaluate a parameter of the '@key' annotation")
        }
      }
      keyAnnos.lengthCompare(1) match {
        case -1 => Key.String(underlying.name.decodedName.toString)
        case 0  => keyAnnos.head
        case 1  => c.abort(keyAnnoTrees.tail.head.pos, s"Duplicate '@key' annotation")
      }
    }
  }
}
