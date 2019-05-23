/* Magnolia, version 0.10.0. Copyright 2018 Jon Pretty, Propensive Ltd.
 *
 * The primary distribution site is: http://co.ntextu.al/
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 */
package io.bullet.borer.magnolia

import scala.annotation.compileTimeOnly
import scala.collection.mutable
import scala.reflect.macros._

/** the object which defines the Magnolia macro */
object Magnolia {
  import CompileTimeState._

  /** derives a generic typeclass instance for the type `T`
    *
    *  This is a macro definition method which should be bound to a method defined inside a Magnolia
    *  generic derivation object, that is, one which defines the methods `combine`, `dispatch` and
    *  the type constructor, `Typeclass[_]`. This will typically look like,
    *  <pre>
    *  object Derivation {
    *    // other definitions
    *    implicit def gen[T]: Typeclass[T] = Magnolia.gen[T]
    *  }
    *  </pre>
    *  which would support automatic derivation of typeclass instances by calling
    *  `Derivation.gen[T]` or with `implicitly[Typeclass[T]]`, if the implicit method is imported
    *  into the current scope.
    *
    *  If the `gen` is not `implicit`, semi-auto derivation is used instead, whereby implicits will
    *  not be generated outside of this ADT.
    *
    *  The definition expects a type constructor called `Typeclass`, taking one *-kinded type
    *  parameter to be defined on the same object as a means of determining how the typeclass should
    *  be genericized. While this may be obvious for typeclasses like `Show[T]` which take only a
    *  single type parameter, Magnolia can also derive typeclass instances for types such as
    *  `Decoder[Format, Type]` which would typically fix the `Format` parameter while varying the
    *  `Type` parameter.
    *
    *  While there is no "interface" for a derivation, in the object-oriented sense, the Magnolia
    *  macro expects to be able to call certain methods on the object within which it is bound to a
    *  method.
    *
    *  Specifically, for deriving case classes (product types), the macro will attempt to call the
    *  `combine` method with an instance of [[CaseClass]], like so,
    *  <pre>
    *    &lt;derivation&gt;.combine(&lt;caseClass&gt;): Typeclass[T]
    *  </pre>
    *  That is to say, the macro expects there to exist a method called `combine` on the derivation
    *  object, which may be called with the code above, and for it to return a type which conforms
    *  to the type `Typeclass[T]`. The implementation of `combine` will therefore typically look
    *  like this,
    *  <pre>
    *    def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = ...
    *  </pre>
    *  however, there is the flexibility to provide additional type parameters or additional
    *  implicit parameters to the definition, provided these do not affect its ability to be invoked
    *  as described above.
    *
    *  Likewise, for deriving sealed traits (coproduct or sum types), the macro will attempt to call
    *  the `dispatch` method with an instance of [[SealedTrait]], like so,
    *  <pre>
    *    &lt;derivation&gt;.dispatch(&lt;sealedTrait&gt;): Typeclass[T]
    *  </pre>
    *  so a definition such as,
    *  <pre>
    *    def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ...
    *  </pre>
    *  will suffice, however the qualifications regarding additional type parameters and implicit
    *  parameters apply equally to `dispatch` as to `combine`.
    *  */
  def gen[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = Stack.withContext(c) { stack =>
    import c.universe._
    import c.internal._

    val prefixType   = c.prefix.tree.tpe
    val prefixObject = prefixType.typeSymbol
    val prefixName   = prefixObject.name.decodedName

    def error(msg: String) = c.abort(c.enclosingPosition, msg)

    def checkMethod(termName: String, category: String, expected: String): Unit = {
      val term = TermName(termName)
      val combineClass = c.prefix.tree.tpe.baseClasses
        .find(cls => cls.asType.toType.decl(term) != NoSymbol)
        .getOrElse(error(
          s"magnolia: the method `$termName` must be defined on the derivation $prefixObject to derive typeclasses for $category"))

      val firstParamBlock = combineClass.asType.toType.decl(term).asTerm.asMethod.paramLists.head
      if (firstParamBlock.lengthCompare(1) != 0)
        error(s"magnolia: the method `$termName` should take a single parameter of type $expected")
    }

    def knownSubclasses(sym: ClassSymbol): List[Symbol] = {
      val children                       = sym.knownDirectSubclasses.toList
      val (abstractTypes, concreteTypes) = children.partition(_.isAbstract)
      abstractTypes.map(_.asClass).flatMap(knownSubclasses(_)) ::: concreteTypes
    }

    val primitiveTypes = Set(
      definitions.DoubleTpe,
      definitions.FloatTpe,
      definitions.ShortTpe,
      definitions.ByteTpe,
      definitions.IntTpe,
      definitions.LongTpe,
      definitions.CharTpe,
      definitions.BooleanTpe,
      definitions.UnitTpe)

    val javaAnnotationType                      = typeOf[java.lang.annotation.Annotation]
    def isJavaAnnotation(tree: c.Tree): Boolean = tree.tpe <:< javaAnnotationType

    val debug = c.macroApplication.symbol.annotations
      .find(_.tree.tpe <:< typeOf[debug])
      .flatMap(_.tree.children.tail.collectFirst { case Literal(Constant(s: String)) => s })

    val genericType: Type = weakTypeOf[T]

    val enclosingVals = Iterator
      .iterate(enclosingOwner)(_.owner)
      .takeWhile(encl => encl != null && encl != NoSymbol)
      .collect { case x if x.isTerm => x.asTerm }
      .filter(encl => encl.isVal || encl.isLazy)
      .toSet[Symbol]

    val typeConstructor = {
      val typeDefs = prefixType.baseClasses.flatMap { cls =>
        cls.asType.toType.decls.collectFirst {
          case x if x.isType && x.name.toString == "Typeclass" => x.asType.toType.asSeenFrom(prefixType, cls)
        }
      }
      if (typeDefs.nonEmpty) typeDefs.head.typeConstructor
      else error(s"magnolia: the derivation $prefixObject does not define a `Typeclass` type constructor")
    }

    checkMethod("combine", "case classes", "CaseClass[Typeclass, _]")

    // fullauto means we should directly infer everything, including external
    // members of the ADT, that isn't inferred by the compiler.
    //
    // semiauto means that we should directly derive only the sealed ADT but not
    // external members (i.e. things that are not a subtype of T).
    val fullauto                   = c.macroApplication.symbol.isImplicit
    val tSealed                    = genericType.typeSymbol.isClass && genericType.typeSymbol.asClass.isSealed
    def semiauto(s: Type): Boolean = tSealed && s <:< genericType

    val magnoliaPkg = c.mirror.staticPackage("io.bullet.borer.magnolia")
    val scalaPkg    = c.mirror.staticPackage("scala")

    def directInferImplicit(genericType: Type, typeConstructor: Type): Option[Tree] = {
      def deferredVal(name: TermName, tpe: Type, rhs: Tree): Tree = {
        val shouldBeLazy = rhs.exists {
          case q"$magnolia.Deferred.apply[$_]($_)" => magnolia.symbol == magnoliaPkg
          case tree                                => enclosingVals.contains(tree.symbol)
        }

        if (!fullauto || shouldBeLazy) q"lazy val $name: $tpe = $rhs"
        else q"val $name = $rhs"
      }

      def typeclassTree(tpe: Type): Tree = {
        val searchType = appliedType(typeConstructor, tpe)
        val deferredRef = for (methodName <- stack find searchType) yield {
          val methodAsString = methodName.decodedName.toString
          q"$magnoliaPkg.Deferred.apply[$searchType]($methodAsString)"
        }

        deferredRef.getOrElse {
          val path  = ChainedImplicit(s"$prefixName.Typeclass", tpe.toString)
          val frame = stack.Frame(path, searchType, termNames.EMPTY)
          stack.recurse(frame, searchType) {
            Option(c.inferImplicitValue(searchType))
              .filterNot(_.isEmpty)
              .orElse(
                if (fullauto || semiauto(tpe))
                  directInferImplicit(tpe, typeConstructor)
                else None
              )
              .getOrElse {
                val missingType   = stack.top.fold(searchType)(_.searchType)
                val typeClassName = s"${missingType.typeSymbol.name.decodedName}.Typeclass"
                val genericType   = missingType.typeArgs.head
                val trace         = stack.trace.mkString("    in ", "\n    in ", "\n")
                error(s"magnolia: could not find $typeClassName for type $genericType\n$trace")
              }
          }
        }
      }

      val genericTypeName = genericType.typeSymbol.name.decodedName.toString.toLowerCase
      val assignedName    = TermName(c.freshName(s"${genericTypeName}Typeclass")).encodedName.toTermName
      val typeSymbol      = genericType.typeSymbol
      val classType       = if (typeSymbol.isClass) Some(typeSymbol.asClass) else None
      val isCaseClass     = classType.exists(_.isCaseClass)
      val isCaseObject    = classType.exists(_.isModuleClass)
      val isSealedTrait   = classType.exists(_.isSealed)
      val isValueClass    = genericType <:< definitions.AnyValTpe && !primitiveTypes.exists(_ =:= genericType)

      val hasPrivateConstructor =
        genericType.decls
          .collectFirst { case m: MethodSymbol if m.isConstructor => m.isPrivate }
          .getOrElse(false)

      val classAnnotationTrees = typeSymbol.annotations.map(_.tree).filterNot(isJavaAnnotation)

      val resultType = appliedType(typeConstructor, genericType)

      val typeName = TermName(c.freshName("typeName"))

      def typeNameRec(t: Type): Tree = {
        val ts           = t.typeSymbol
        val typeArgNames = t.typeArgs.map(typeNameRec(_))
        q"$magnoliaPkg.TypeName(${ts.owner.fullName}, ${ts.name.decodedName.toString}, $typeArgNames)"
      }
      val typeNameDef = q"val $typeName = ${typeNameRec(genericType)}"

      val result = if (isCaseObject) {
        val impl = q"""
          $typeNameDef
          ${c.prefix}.combine(new $magnoliaPkg.CaseClass[$typeConstructor, $genericType](
            $typeName,
            true,
            false,
            new $scalaPkg.Array(0),
            $scalaPkg.Array(..$classAnnotationTrees)
          ) {
            override def construct[Return](makeParam: $magnoliaPkg.Param[$typeConstructor, $genericType] => Return): $genericType =
              ${genericType.typeSymbol.asClass.module}

            def rawConstruct(fieldValues: $scalaPkg.Array[$scalaPkg.Any]): $genericType =
              ${genericType.typeSymbol.asClass.module}
          })
        """
        Some(impl)
      } else if ((isCaseClass || isValueClass) && !hasPrivateConstructor) {

        val companionRef = GlobalUtil.patchedCompanionRef(c)(genericType.dealias)

        val headParamList = {
          val primaryConstructor = classType map (_.primaryConstructor)
          val optList: Option[List[c.universe.Symbol]] =
            primaryConstructor flatMap (_.asMethod.typeSignature.paramLists.headOption)
          optList.map(_.map(_.asTerm))
        }

        val caseClassParameters = genericType.decls.collect {
          case m: MethodSymbol if m.isCaseAccessor || (isValueClass && m.isParamAccessor) =>
            m.asMethod
        }

        case class CaseParam(sym: MethodSymbol, repeated: Boolean, typeclass: Tree, paramType: Type, ref: TermName)

        val repeatedParamClass = definitions.RepeatedParamClass
        val scalaSeqType       = typeOf[Seq[_]].typeConstructor

        val caseParams = caseClassParameters
          .foldLeft[List[CaseParam]](Nil) { (acc, param) =>
            val paramName            = param.name.decodedName.toString
            val paramTypeSubstituted = param.typeSignatureIn(genericType).resultType

            val (repeated, paramType) = paramTypeSubstituted match {
              case TypeRef(_, `repeatedParamClass`, typeArgs) => true  -> appliedType(scalaSeqType, typeArgs)
              case tpe                                        => false -> tpe
            }

            acc.find(_.paramType =:= paramType) match {
              case Some(backRef) => CaseParam(param, repeated, q"()", paramType, backRef.ref) :: acc

              case None =>
                val path            = ProductType(paramName, genericType.toString)
                val frame           = stack.Frame(path, resultType, assignedName)
                val searchType      = appliedType(typeConstructor, paramType)
                val derivedImplicit = stack.recurse(frame, searchType)(typeclassTree(paramType))

                val ref      = TermName(c.freshName("paramTypeclass"))
                val assigned = deferredVal(ref, searchType, derivedImplicit)
                CaseParam(param, repeated, assigned, paramType, ref) :: acc
            }
          }
          .reverse

        val paramsVal: TermName = TermName(c.freshName("parameters"))

        val preAssignments = caseParams.map(_.typeclass)

        val defaults = headParamList map { plist =>
          // note: This causes the namer/typer to generate the synthetic default methods by forcing
          // the typeSignature of the "default" factory method to be visited.
          // It feels like it shouldn't be needed, but we'll get errors otherwise (as discovered after 6 hours debugging)

          val companionSym         = companionRef.symbol.asModule.info
          val primaryFactoryMethod = companionSym.decl(TermName("apply")).alternatives.lastOption
          primaryFactoryMethod.foreach(_.asMethod.typeSignature)

          val indexedConstructorParams = plist.zipWithIndex
          indexedConstructorParams.map {
            case (p, idx) =>
              if (p.isParamWithDefault) {
                val method = TermName("apply$default$" + (idx + 1))
                q"$scalaPkg.Some($companionRef.$method)"
              } else q"$scalaPkg.None"
          }
        } getOrElse List(q"$scalaPkg.None")

        val annotations: List[List[Tree]] = headParamList.toList.flatten map { param =>
          param.annotations map { _.tree } filterNot isJavaAnnotation
        }

        val assignments = caseParams.zip(defaults).zip(annotations).zipWithIndex.map {
          case (((CaseParam(param, repeated, _, paramType, ref), defaultVal), annList), idx) =>
            val call = if (isValueClass) q"$magnoliaPkg.Magnolia.valueParam" else q"$magnoliaPkg.Magnolia.param"
            q"""$paramsVal($idx) = $call[$typeConstructor, $genericType, $paramType](
            ${param.name.decodedName.toString},
            ${if (!isValueClass) q"$idx" else q"(g: $genericType) => g.${param.name}: $paramType"},
            $repeated,
            $magnoliaPkg.CallByNeed($ref),
            $magnoliaPkg.CallByNeed($defaultVal),
            $scalaPkg.Array(..$annList)
          )"""
        }

        val genericParams = caseParams.zipWithIndex.map {
          case (typeclass, idx) =>
            val arg = q"makeParam($paramsVal($idx)).asInstanceOf[${typeclass.paramType}]"
            if (typeclass.repeated) q"$arg: _*" else arg
        }

        val rawGenericParams = caseParams.zipWithIndex.map {
          case (typeclass, idx) =>
            val arg = q"fieldValues($idx).asInstanceOf[${typeclass.paramType}]"
            if (typeclass.repeated) q"$arg: _*" else arg
        }

        Some(q"""{
            ..$preAssignments
            val $paramsVal: $scalaPkg.Array[$magnoliaPkg.Param[$typeConstructor, $genericType]] =
              new $scalaPkg.Array(${assignments.length})
            ..$assignments

            $typeNameDef

            ${c.prefix}.combine(new $magnoliaPkg.CaseClass[$typeConstructor, $genericType](
              $typeName,
              false,
              $isValueClass,
              $paramsVal,
              $scalaPkg.Array(..$classAnnotationTrees)
            ) {
              override def construct[Return](makeParam: $magnoliaPkg.Param[$typeConstructor, $genericType] => Return): $genericType =
                new $genericType(..$genericParams)

              def rawConstruct(fieldValues: $scalaPkg.Array[$scalaPkg.Any]): $genericType =
                new $genericType(..$rawGenericParams)
            })
          }""")
      } else if (isSealedTrait) {
        checkMethod("dispatch", "sealed traits", "SealedTrait[Typeclass, _]")
        val genericSubtypes = knownSubclasses(classType.get)
        val subtypes = genericSubtypes.map { sub =>
          val subType     = sub.asType.toType // FIXME: Broken for path dependent types
          val typeParams  = sub.asType.typeParams
          val typeArgs    = thisType(sub).baseType(genericType.typeSymbol).typeArgs
          val mapping     = (typeArgs.map(_.typeSymbol), genericType.typeArgs).zipped.toMap
          val newTypeArgs = typeParams.map(mapping.withDefault(_.asType.toType))
          val applied     = appliedType(subType.typeConstructor, newTypeArgs)
          existentialAbstraction(typeParams, applied)
        }

        if (subtypes.isEmpty) {
          c.info(c.enclosingPosition, s"magnolia: could not find any direct subtypes of $typeSymbol", force = true)

          error("")
        }

        val subtypesVal: TermName = TermName(c.freshName("subtypes"))

        val typeclasses = for (subType <- subtypes) yield {
          val path  = CoproductType(genericType.toString)
          val frame = stack.Frame(path, resultType, assignedName)
          subType -> stack.recurse(frame, appliedType(typeConstructor, subType))(typeclassTree(subType))
        }

        val assignments = typeclasses.zipWithIndex.map {
          case ((typ, typeclass), idx) =>
            q"""$subtypesVal($idx) = $magnoliaPkg.Magnolia.subtype[$typeConstructor, $genericType, $typ](
            ${typeNameRec(typ)},
            $idx,
            $scalaPkg.Array(..${typ.typeSymbol.annotations.map(_.tree).filterNot(isJavaAnnotation)}),
            $magnoliaPkg.CallByNeed($typeclass),
            (t: $genericType) => t.isInstanceOf[$typ],
            (t: $genericType) => t.asInstanceOf[$typ]
          )"""
        }

        Some(q"""{
            val $subtypesVal: $scalaPkg.Array[$magnoliaPkg.Subtype[$typeConstructor, $genericType]] =
              new $scalaPkg.Array(${assignments.size})

            ..$assignments

            $typeNameDef

            ${c.prefix}.dispatch(new $magnoliaPkg.SealedTrait(
              $typeName,
              $subtypesVal: $scalaPkg.Array[$magnoliaPkg.Subtype[$typeConstructor, $genericType]],
              $scalaPkg.Array(..$classAnnotationTrees)
            )): $resultType
          }""")
      } else if (!typeSymbol.isParameter) {
        c.prefix.tree.tpe.baseClasses.collectFirst {
          case x if x.asType.toType.decl(TermName("fallback")) != NoSymbol =>
            q"""${c.prefix}.fallback[$genericType]"""
        }
      } else None

      for (term <- result) yield q"""{
        ${deferredVal(assignedName, resultType, term)}
        $assignedName
      }"""
    }

    val searchType        = appliedType(typeConstructor, genericType)
    val directlyReentrant = stack.top.exists(_.searchType =:= searchType)
    if (directlyReentrant) throw DirectlyReentrantException()

    val result = stack
      .find(searchType)
      .map(enclosingRef => q"$magnoliaPkg.Deferred[$searchType](${enclosingRef.toString})")
      .orElse(directInferImplicit(genericType, typeConstructor))

    for (tree <- result) if (debug.isDefined && genericType.toString.contains(debug.get)) {
      c.echo(c.enclosingPosition, s"Magnolia macro expansion for $genericType")
      c.echo(NoPosition, s"... = ${showCode(tree)}\n\n")
    }

    val dereferencedResult =
      if (stack.isEmpty) {
        val expandDeferred = new Transformer {
          override def transform(tree: Tree) = tree match {
            case q"$mag.Deferred.apply[$_](${Literal(Constant(method: String))})" if mag.symbol == magnoliaPkg =>
              q"${TermName(method)}"
            case _ => super.transform(tree)
          }
        }

        for (tree <- result) yield c.untypecheck(expandDeferred.transform(tree))
      } else result

    dereferencedResult.getOrElse(error(s"magnolia: could not infer $prefixName.Typeclass for type $genericType"))
  }

  /** constructs a new [[Subtype]] instance
    *
    *  This method is intended to be called only from code generated by the Magnolia macro, and
    *  should not be called directly from users' code. */
  def subtype[Tc[_], T, S <: T](
      name: TypeName,
      idx: Int,
      anns: Array[Any],
      tc: CallByNeed[Tc[S]],
      isType: T => Boolean,
      asType: T => S): Subtype[Tc, T] =
    new Subtype[Tc, T](name, idx, anns) with PartialFunction[T, S] {
      type SType = S
      def typeclass: Tc[SType]            = tc.value
      def cast: PartialFunction[T, SType] = this
      def isDefinedAt(t: T)               = isType(t)
      def apply(t: T): SType              = asType(t)
      override def toString: String       = s"Subtype(${typeName.full})"
    }

  /** constructs a new [[Param]] instance
    *
    *  This method is intended to be called only from code generated by the Magnolia macro, and
    *  should not be called directly from users' code. */
  def param[Tc[_], T, P](
      name: String,
      idx: Int,
      isRepeated: Boolean,
      typeclassParam: CallByNeed[Tc[P]],
      defaultVal: CallByNeed[Option[P]],
      annotationsArray: Array[Any]): Param[Tc, T] =
    new Param[Tc, T](name, idx, isRepeated, annotationsArray, defaultVal, typeclassParam) {
      type PType = P
      def dereference(t: T): PType = t.asInstanceOf[Product].productElement(idx).asInstanceOf[PType]
    }

  def valueParam[Tc[_], T, P](
      name: String,
      deref: T => P,
      isRepeated: Boolean,
      typeclassParam: CallByNeed[Tc[P]],
      defaultVal: CallByNeed[Option[P]],
      annotationsArray: Array[Any]): Param[Tc, T] =
    new Param[Tc, T](name, 0, isRepeated, annotationsArray, defaultVal, typeclassParam) {
      type PType = P
      def dereference(t: T): PType = deref(t)
    }
}

final private[magnolia] case class DirectlyReentrantException() extends Exception("attempt to recurse directly")

@compileTimeOnly("magnolia.Deferred is used for derivation of recursive typeclasses")
object Deferred { def apply[T](method: String): T = ??? }

private[magnolia] object CompileTimeState {

  sealed abstract class TypePath(path: String) { override def toString = path }
  final case class CoproductType(typeName: String) extends TypePath(s"coproduct type $typeName")

  final case class ProductType(paramName: String, typeName: String)
      extends TypePath(s"parameter '$paramName' of product type $typeName")

  final case class ChainedImplicit(typeClassName: String, typeName: String)
      extends TypePath(s"chained implicit $typeClassName for type $typeName")

  final class Stack[C <: whitebox.Context with Singleton] {
    private var frames = List.empty[Frame]
    private val cache  = mutable.Map.empty[C#Type, C#Tree]

    def isEmpty: Boolean         = frames.isEmpty
    def nonEmpty: Boolean        = frames.nonEmpty
    def top: Option[Frame]       = frames.headOption
    def pop(): Unit              = frames = frames drop 1
    def push(frame: Frame): Unit = frames ::= frame

    def clear(): Unit = {
      frames = Nil
      cache.clear()
    }

    def find(searchType: C#Type): Option[C#TermName] = frames.collectFirst {
      case Frame(_, tpe, term) if tpe =:= searchType => term
    }

    def recurse[T <: C#Tree](frame: Frame, searchType: C#Type)(fn: => C#Tree): C#Tree = {
      push(frame)
      val result = cache.getOrElseUpdate(searchType, fn)
      pop()
      result
    }

    def trace: List[TypePath] =
      (frames.drop(1), frames).zipped.collect {
        case (Frame(path, tp1, _), Frame(_, tp2, _)) if !(tp1 =:= tp2) => path
      }.toList

    override def toString: String =
      frames.mkString("magnolia stack:\n", "\n", "\n")

    case class Frame(path: TypePath, searchType: C#Type, term: C#TermName)
  }

  object Stack {
    // Cheating to satisfy Singleton bound (which improves type inference).
    private val dummyContext: whitebox.Context = null
    private val global                         = new Stack[dummyContext.type]
    private val workSet                        = mutable.Set.empty[whitebox.Context#Symbol]

    def withContext(c: whitebox.Context)(fn: Stack[c.type] => c.Tree): c.Tree = {
      workSet += c.macroApplication.symbol
      val depth = c.enclosingMacros.count(m => workSet(m.macroApplication.symbol))
      try fn(global.asInstanceOf[Stack[c.type]])
      finally if (depth <= 1) {
        global.clear()
        workSet.clear()
      }
    }
  }
}

object CallByNeed { def apply[A](a: => A): CallByNeed[A] = new CallByNeed(() => a) }
final class CallByNeed[+A](private[this] var eval: () => A) extends Serializable {
  lazy val value: A = {
    val result = eval()
    eval = null
    result
  }
}
