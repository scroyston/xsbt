package xsbti.api

import java.lang.IllegalArgumentException
import scala.collection.mutable.WrappedArray

object DefinitionOrderings {
  import Ordering._

  implicit def Array[T](implicit ord: Ordering[T]): Ordering[Array[T]] =
    new Ordering[Array[T]] {
      def compare(x: Array[T], y: Array[T]): Int = {
        val lenComp = Ordering.Int.compare(x.length, y.length)
        if (lenComp != 0) return lenComp
        var i = 0
        while (i < x.length) {
          val res = ord.compare(x(i), y(i))
          if (res != 0) return res
          i += 1
        }
        0
      }
    }

  implicit def WrappedArray[T](implicit ord: Ordering[T]): Ordering[WrappedArray[T]] =
    new Ordering[WrappedArray[T]] {
      def compare(x: WrappedArray[T], y: WrappedArray[T]): Int = {
        val lenComp = Ordering.Int.compare(x.length, y.length)
        if (lenComp != 0) return lenComp
        var i = 0
        while (i < x.length) {
          val res = ord.compare(x(i), y(i))
          if (res != 0) return res
          i += 1
        }
        0
      }
    }

    trait QualifierOrdering extends Ordering[Qualifier] {
      def classOrder(x: Qualifier): (Int, String) = x match {
        case Unqualified => (1, "")
        case ThisQualifier => (2, "")
        case id: IdQualifier => (3, id.value)
      }

      def compare(x: Qualifier, y: Qualifier): Int = {
        if (x eq y) return 0
        val x1 = classOrder(x)
        val y1 = classOrder(y)
        Ordering[(Int, String)].compare(x1, y1)
      }
    }
    implicit object Qualifier extends QualifierOrdering

    trait AccessOrdering extends Ordering[Access] {
      def classOrder(x: Access): (Int, Qualifier) = x match {
        case Public => (1, Unqualified)
        case a: Protected => (2, a.qualifier)
        case a: Private => (3, a.qualifier)
      }

      def compare(x: Access, y: Access): Int = {
        if (x eq y) return 0
        val x1 = classOrder(x)
        val y1 = classOrder(y)
        Ordering[(Int, Qualifier)].compare(x1, y1)
      }
    }
    implicit object Access extends AccessOrdering

  implicit val typeOrdering = new Ordering[Type] {
    def compare(x: Type, y: Type): Int = 0
  }

  implicit val singletonOrdering: Ordering[Singleton] = Ordering.by[Singleton, Path](x => x.path)

  trait SimpleTypeOrdering extends Ordering[SimpleType] {
    def classOrder(x: SimpleType): Int = x match {
      case EmptyType => 1
      case x: Singleton => 2
      case id: Projection => 3
      case p: Parameterized => 4
      case ref: ParameterRef => 5
    }

    def compare(x: SimpleType, y: SimpleType): Int = {
      if (x eq y) return 0
      if (Ordering.Int.compare(classOrder(x), classOrder(y)) == 0) 0
      else {
        (x, y) match {
          case (s1: Singleton, s2: Singleton) => singletonOrdering.compare(s1, s2)
          case (p1: Projection, p2: Projection) => projectionOrdering.compare(p1, p2)
          case (s1: Parameterized, s2: Parameterized) => parameterizedOrdering.compare(s1, s2)
          case (s1: ParameterRef, s2: ParameterRef) => paramRefOrdering.compare(s1, s2)
          case _ => throw new IllegalArgumentException()
        }
      }
    }
  }
  implicit object SimpleType extends SimpleTypeOrdering

  implicit val varianceOrdering = Ordering.by[Variance, Int](x => x.ordinal())

  implicit val annotationArgOrdering = Ordering.by((arg: AnnotationArgument) => (arg.name, arg.value))
  implicit val annotationOrdering = {
    Ordering.by[Annotation, (WrappedArray[AnnotationArgument], SimpleType)](ann => (ann.arguments, ann.base))
  }

  implicit val projectionOrdering = Ordering.by[Projection, (String, SimpleType)](proj => (proj.id, proj.prefix))

  implicit val parameterizedOrdering = Ordering.by[Parameterized, (SimpleType, WrappedArray[Type])](prm => (prm.baseType, prm.typeArguments))
  implicit val paramRefOrdering = new Ordering[ParameterRef] { def compare(x: ParameterRef, y: ParameterRef): Int = 0 }
  implicit val polymorphicOrdering = Ordering.by[Polymorphic, (Type, WrappedArray[TypeParameter])](poly => (poly.baseType, poly.parameters))
  implicit val existentialOrdering = Ordering.by[Existential, (Type, WrappedArray[TypeParameter])](exst => (exst.baseType, exst.clause))
  implicit val structureOrdering = {
    Ordering.by[Structure, (WrappedArray[Type], WrappedArray[Definition], WrappedArray[Definition])](str => (str.parents, str.declared, str.inherited))
  }
  implicit val constantOrdering = Ordering.by[Constant, (String, Type)](cnst => (cnst.value, cnst.baseType))

  implicit val modifierOrdering = Ordering.by[Modifiers, Int](x => x.raw)

  implicit val varOrdering = Ordering.by[Var, (String, Access, Modifiers, Type, WrappedArray[Annotation])](x => {
    (x.name, x.access, x.modifiers, x.tpe, x.annotations)
  })

  implicit val valOrdering = Ordering.by[Val, (String, Access, Modifiers, Type, WrappedArray[Annotation])](x => {
    (x.name, x.access, x.modifiers, x.tpe, x.annotations)
  })

  implicit val typeAliasOrdering = {
    Ordering.by[TypeAlias, (String, Access, Modifiers, Type, WrappedArray[Annotation], WrappedArray[TypeParameter])](x => {
      (x.name, x.access, x.modifiers, x.tpe, x.annotations, x.typeParameters)
    })
  }

  implicit val typeDecl = {
    Ordering.by[TypeDeclaration, (String, Access, Modifiers, Type, Type, WrappedArray[Annotation], WrappedArray[TypeParameter])](x => {
      (x.name, x.access, x.modifiers, x.lowerBound, x.upperBound, x.annotations, x.typeParameters)
    })
  }

    trait DefinitionOrdering extends Ordering[Definition] {
      def compare(x: Definition, y: Definition) =
        if (x < y) -1
        else if (x == y) 0
        else 1
    }
    implicit object Definition extends DefinitionOrdering

}