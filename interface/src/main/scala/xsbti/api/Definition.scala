package xsbti.api

import java.util.concurrent.ConcurrentHashMap
import collection.mutable.WrappedArray
import java.io.Serializable

trait SingletonMap[T] {
  val map = new ConcurrentHashMap[T, T]()
  def find[A <: T](obj: A): A =  {
    val other = map.putIfAbsent(obj, obj)
    if (other == null) obj else other.asInstanceOf[A]
  }
}

object Definition extends SingletonMap[Definition]

abstract class Definition extends Serializable {
  val name: String
  val access: Access
  val modifiers: Modifiers
  val annotations: Array[Annotation]
  //private def readResolve(): AnyRef = Definition.find(this)
}

object Access extends SingletonMap[Access]

sealed trait Access

abstract class Qualified extends Access {
  val qualifier: Qualifier
}

sealed trait Qualifier

case object Unqualified extends Qualifier
case object ThisQualifier extends Qualifier

object IdQualifier extends SingletonMap[IdQualifier]
case class IdQualifier(value: String) extends Qualifier {
  private def readResolve(): AnyRef = IdQualifier.find(this)
}

case object Public extends Access with Serializable

case class Private(qualifier: Qualifier) extends Qualified with Serializable {
  private def readResolve(): AnyRef = Access.find(this)
}

case class Protected(qualifier: Qualifier) extends Qualified {
  private def readResolve(): AnyRef = Access.find(this)
}

object Annotation extends SingletonMap[Annotation]

case class Annotation(base: SimpleType, arguments: Array[AnnotationArgument]) extends Serializable {
  private def readResolve(): AnyRef = Annotation.find(this)
}

object AnnotationArgument extends SingletonMap[AnnotationArgument]

case class AnnotationArgument(name: String, value: String) extends Serializable {
  private def readResolve(): AnyRef = AnnotationArgument.find(this)
}

object FieldLike extends SingletonMap[FieldLike]

abstract class FieldLike extends Definition with Serializable {
  val tpe: Type
}

case class Val(tpe: Type, name: String, access: Access, modifiers: Modifiers, annotations: Array[Annotation]) extends FieldLike

case class Var(tpe: Type, name: String, access: Access, modifiers: Modifiers, annotations: Array[Annotation]) extends FieldLike

abstract class ParameterizedDefinition extends Definition {
  val typeParameters: Array[TypeParameter]
}

abstract class TypeMember extends ParameterizedDefinition

case class TypeAlias(tpe: Type, typeParameters: Array[TypeParameter], name: String, access: Access, modifiers: Modifiers, annotations: Array[Annotation]) extends TypeMember

case class TypeDeclaration(lowerBound: Type, upperBound: Type, typeParameters: Array[TypeParameter], name: String, access: Access, modifiers: Modifiers, annotations: Array[Annotation]) extends TypeMember

case class Def(valueParameters: Array[ParameterList], returnType: Type, typeParameters: Array[TypeParameter], name: String, access: Access, modifiers: Modifiers, annotations: Array[Annotation]) extends ParameterizedDefinition

case class ClassLike(definitionType: DefinitionType, selfType: Nothing, structure: Nothing, typeParameters: Array[TypeParameter], name: String, access: Access, modifiers: Modifiers, annotations: Array[Annotation]) extends ParameterizedDefinition

class TypeParameter(id: Int, annotations: Array[Annotation], typeParameters: Array[TypeParameter], variance: Variance, lowerBound: Type, upperBound: Type) extends java.io.Serializable

sealed trait Type

class Polymorphic(baseType: Type, parameters: Array[TypeParameter]) extends Type

class Existential(baseType: Type, clause: Array[TypeParameter]) extends Type

class Structure(parents: Lazy[Array[Type]], declared: Lazy[Array[Definition]], inherited: Lazy[Array[Definition]]) extends Type

class Constant(baseType: Type, value: String) extends Type

class Annotated(baseType: SimpleType, annotations: Array[Annotation]) extends Type

abstract class SimpleType extends Type

class Singleton(path: Path) extends SimpleType

class EmptyType extends SimpleType

class Projection(prefix: SimpleType, id: String) extends SimpleType

class Parameterized(baseType: SimpleType, typeArguments: Array[Type]) extends SimpleType

class ParameterRef(id: Int) extends SimpleType

class Path(components: Array[PathComponent]) extends Serializable

abstract class PathComponent extends Serializable

class Id(id: String) extends PathComponent

class This extends PathComponent

class Super(qualifier: Path) extends PathComponent

class MethodParameter(name: String, tpe: Type, hasDefault: Boolean, modifier: ParameterModifier) extends Serializable

class Package(name: String) extends Serializable

class ParameterList(parameters: Array[MethodParameter], isImplicit: Boolean) extends Serializable

class Source(packages: Array[Package], definitions: Array[Definition]) extends java.io.Serializable
