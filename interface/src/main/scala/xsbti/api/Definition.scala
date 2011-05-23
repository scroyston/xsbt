package xsbti.api

import java.util.concurrent.ConcurrentHashMap
import java.io.Serializable

import GraphCompare.ProductNode
import DefinitionOrderings._
import scala.collection.mutable.WrappedArray
import scala.util.Sorting._

trait TypedId {
  val id: Int
}

trait SingletonMap[T] {
  val map = new ConcurrentHashMap[T, T]()
  def find[A <: T](obj: A): A =  {
    val other = map.putIfAbsent(obj, obj)
    if (other == null) obj else other.asInstanceOf[A]
  }
}

abstract class Definition extends ProductNode with Serializable {
  val name: String
  val access: Access
  val modifiers: Modifiers
  val annotations: WrappedArray[Annotation]
}

object Access extends SingletonMap[Access]

sealed abstract class Access

abstract class Qualified extends Access {
  val qualifier: Qualifier
}

sealed trait Qualifier

case object Unqualified extends Qualifier
case object ThisQualifier extends Qualifier

object IdQualifier extends SingletonMap[IdQualifier] {
  def unique(value: String): IdQualifier = find(new IdQualifier(value.intern))
}

case class IdQualifier private(value: String) extends Qualifier {
  private def readResolve(): AnyRef = IdQualifier.find(this)
}

case object Public extends Access with Serializable

object Private extends SingletonMap[Private] {
  def unique(qualifier: Qualifier) = find(new Private(qualifier))
}

case class Private private(qualifier: Qualifier) extends Qualified with Serializable {
  private def readResolve(): AnyRef = Access.find(this)
}

object Protected extends SingletonMap[Protected] {
  def unique(qualifier: Qualifier) = find(new Protected(qualifier))
}
case class Protected private(qualifier: Qualifier) extends Qualified {
  private def readResolve(): AnyRef = Access.find(this)
}

object Annotation extends SingletonMap[Annotation] {
  def unique(base: SimpleType, arguments: Array[AnnotationArgument]) = {
    stableSort(arguments)
    find(new Annotation(base, arguments))
  }
}

case class Annotation private(base: SimpleType, arguments: WrappedArray[AnnotationArgument]) extends ProductNode with Serializable {
  private def readResolve(): AnyRef = Annotation.find(this)
}

object AnnotationArgument extends SingletonMap[AnnotationArgument] {
  def unique(name: String, value: String) = find(new AnnotationArgument(name.intern, value.intern))
}

case class AnnotationArgument private(name: String, value: String) extends Serializable {
  private def readResolve(): AnyRef = AnnotationArgument.find(this)
}

abstract class FieldLike extends Definition with Serializable {
  val tpe: Type
}

object Val extends SingletonMap[Val] {
  def unique(tpe: Type, name: String, access: Access, modifiers: Modifiers, annotations: Array[Annotation]) = {
    stableSort(annotations)
    find(new Val(tpe, name.intern, access, modifiers, annotations))
  }
}

case class Val private(tpe: Type, name: String, access: Access, modifiers: Modifiers, annotations: WrappedArray[Annotation])
        extends FieldLike {
  private def readResolve(): AnyRef = Val.find(this)
}

object Var extends SingletonMap[Var] {
  def unique(tpe: Type, name: String, access: Access, modifiers: Modifiers, annotations: Array[Annotation]) = {
    stableSort(annotations)
    find(new Var(tpe, name.intern, access, modifiers, annotations))
  }
}

case class Var private(tpe: Type, name: String, access: Access, modifiers: Modifiers, annotations: WrappedArray[Annotation])
        extends FieldLike {
  private def readResolve(): AnyRef = Var.find(this)
}


abstract class ParameterizedDefinition extends Definition {
  val typeParameters: WrappedArray[TypeParameter]
}

abstract class TypeMember extends ParameterizedDefinition

object TypeAlias extends SingletonMap[TypeAlias] {
  def unique(tpe: Type, typeParameters: Array[TypeParameter], name: String, access: Access,
                     modifiers: Modifiers, annotations: Array[Annotation]) = {
    //stableSort(typeParameters)
    stableSort(annotations)
    find(new TypeAlias(tpe, typeParameters, name.intern, access, modifiers, annotations))
  }
}

case class TypeAlias private(tpe: Type, typeParameters: WrappedArray[TypeParameter], name: String, access: Access,
                             modifiers: Modifiers, annotations: WrappedArray[Annotation]) extends TypeMember {
  private def readResolve(): AnyRef = TypeAlias.find(this)
}

object TypeDeclaration extends SingletonMap[TypeDeclaration] {
  def unique(lowerBound: Type, upperBound: Type, typeParameters: Array[TypeParameter], name: String, access: Access,
                     modifiers: Modifiers, annotations: Array[Annotation]) = {
    //stableSort(typeParameters)
    stableSort(annotations)
    find(new TypeDeclaration(lowerBound, upperBound, typeParameters, name.intern, access, modifiers, annotations))
  }
}

case class TypeDeclaration private(lowerBound: Type, upperBound: Type, typeParameters: WrappedArray[TypeParameter],
                                   name: String, access: Access, modifiers: Modifiers,
                                   annotations: WrappedArray[Annotation]) extends TypeMember {
  private def readResolve(): AnyRef = TypeDeclaration.find(this)
}

object Def extends SingletonMap[Def] {
  def unique(valueParameters: Array[ParameterList], returnType: Type, typeParameters: Array[TypeParameter], name: String,
             access: Access, modifiers: Modifiers, annotations: Array[Annotation]) = {
    //stableSort(valueParameters)
    //stableSort(typeParameters)
    stableSort(annotations)
    find(new Def(valueParameters, returnType, typeParameters, name.intern, access, modifiers, annotations))
  }
}

case class Def private(valueParameters: WrappedArray[ParameterList], returnType: Type,
                       typeParameters: WrappedArray[TypeParameter], name: String, access: Access, modifiers: Modifiers,
                       annotations: WrappedArray[Annotation]) extends ParameterizedDefinition {
  private def readResolve(): AnyRef = Def.find(this)
}

object ClassLike extends SingletonMap[ClassLike] {
  def unique(definitionType: DefinitionType, selfType: Type, structure: Structure,
             typeParameters: Array[TypeParameter], name: String, access: Access, modifiers: Modifiers,
             annotations: Array[Annotation]) = {
    //stableSort(typeParameters)
    stableSort(annotations)
    find(new ClassLike(definitionType, selfType, structure, typeParameters, name.intern, access, modifiers, annotations))
  }
}

case class ClassLike private(definitionType: DefinitionType, selfType: Type, structure: Structure,
                     typeParameters: WrappedArray[TypeParameter], name: String, access: Access, modifiers: Modifiers,
                     annotations: WrappedArray[Annotation]) extends ParameterizedDefinition {
  private def readResolve(): AnyRef = ClassLike.find(this)
}


object TypeParameter extends SingletonMap[TypeParameter] {
  def unique(id: Int, annotations: Array[Annotation], typeParameters: Array[TypeParameter],
             variance: Variance, lowerBound: Type, upperBound: Type) = {
    stableSort(annotations)
    //stableSort(typeParameters)
    find(new TypeParameter(id, annotations, typeParameters, variance, lowerBound, upperBound))
  }
}

case class TypeParameter private(id: Int, annotations: WrappedArray[Annotation],
                                 typeParameters: WrappedArray[TypeParameter], variance: Variance,
                                 lowerBound: Type, upperBound: Type)
        extends ProductNode with TypedId with Ordered[TypeParameter] with Serializable {
  private def readResolve(): AnyRef = TypeParameter.find(this)

  def compare(that: TypeParameter): Int = {
    Ordering[(WrappedArray[Annotation], WrappedArray[TypeParameter], Variance, Type, Type)].compare(
      (annotations, typeParameters, variance, lowerBound, upperBound),
            (that.annotations, that.typeParameters, that.variance, that.lowerBound, that.upperBound))
  }

  override def equiv(node: GraphCompare.Node): Boolean = {
    node match {
      case that: TypeParameter => (variance == that.variance)
      case _ => false
    }
  }
}

sealed trait Type

object Polymorphic extends SingletonMap[Polymorphic] {
  def unique(baseType: Type, parameters: Array[TypeParameter]) = {
    find(new Polymorphic(baseType, parameters))
  }
}

case class Polymorphic private(baseType: Type, parameters: WrappedArray[TypeParameter]) extends ProductNode with Type {
  private def readResolve(): AnyRef = Polymorphic.find(this)
}

object Existential extends SingletonMap[Existential] {
  def unique(baseType: Type, clause: Array[TypeParameter]) = {
    find(new Existential(baseType, clause))
  }
}

case class Existential private(baseType: Type, clause: WrappedArray[TypeParameter]) extends ProductNode with Type {
  private def readResolve(): AnyRef = Existential.find(this)
}

object Structure extends SingletonMap[Structure] {
  def unique(parents: Array[Type], declared: Array[Definition], inherited: Array[Definition]) = {
    find(new Structure(parents, declared, inherited))
  }
}

case class Structure private(parents: WrappedArray[Type], declared: WrappedArray[Definition], inherited: WrappedArray[Definition])
        extends ProductNode with Type {
  private def readResolve(): AnyRef = Structure.find(this)
}

object Constant extends SingletonMap[Constant] {
  def unique(baseType: Type, value: String) = find(new Constant(baseType, value))
}

case class Constant private(baseType: Type, value: String) extends ProductNode with Type {
  private def readResolve(): AnyRef = Constant.find(this)
}

object Annotated extends SingletonMap[Annotated] {
  def unique(baseType: SimpleType, annotations: Array[Annotation]) = {
    find (new Annotated(baseType, annotations))
  }
}

case class Annotated(baseType: SimpleType, annotations: WrappedArray[Annotation]) extends ProductNode with Type

trait SimpleType extends Type

object Singleton extends SingletonMap[Singleton] {
  def unique(path: Path) = find(new Singleton(path))
}

case class Singleton private(path: Path) extends SimpleType

trait EmptyType extends SimpleType
case object EmptyType extends EmptyType

object Projection extends SingletonMap[Projection] {
  def unique(prefix: SimpleType, id: String) = find(new Projection(prefix, id.intern))
}

case class Projection private(prefix: SimpleType, id: String) extends ProductNode with SimpleType

object Parameterized extends SingletonMap[Parameterized] {
  def unique(baseType: SimpleType, typeArguments: Array[Type]) = find(new Parameterized(baseType, typeArguments))
}

case class Parameterized private(baseType: SimpleType, typeArguments: WrappedArray[Type]) extends ProductNode with SimpleType

object ParameterRef extends SingletonMap[ParameterRef] {
  def unique(id: Int) = find(new ParameterRef(id))
}
case class ParameterRef private(id: Int) extends ProductNode with SimpleType with TypedId


object Path extends SingletonMap[Path] {
  def unique(components: Array[PathComponent]) = find(new Path(components))
}

case class Path private(components: WrappedArray[PathComponent]) extends Ordered[Path] with Serializable {
  def compare(that: Path): Int = components compare that.components
}

sealed abstract class PathComponent extends Ordered[PathComponent] with Serializable

object Id extends SingletonMap[Id] {
  def unique(id: String) = find(new Id(id.intern))
}
case class Id private(id: String) extends PathComponent {
  def compare(that: PathComponent): Int = that match {
    case x: Id => Ordering.String.compare(id, x.id)
    case This => -1
    case _ : Super => -1
  }
}

sealed abstract class This extends PathComponent
case object This extends This {
  def compare(that: PathComponent): Int = that match {
    case This => 0
    case _ : Id => 1
    case _ : Super => -1
  }
}

object Super extends SingletonMap[Super] {
  def unique(qualifier: Path) = find(new Super(qualifier))
}
case class Super private(qualifier: Path) extends PathComponent {
  def compare(that: PathComponent): Int = that match {
    case This => 1
    case _ : Id => 1
    case x : Super => qualifier compare x.qualifier
  }
}


object MethodParameter extends SingletonMap[MethodParameter] {
  def unique(name: String, tpe: Type, hasDefault: Boolean, modifier: ParameterModifier) = {
    find(new MethodParameter(name.intern, tpe, hasDefault, modifier))
  }
}
case class MethodParameter private(name: String, tpe: Type, hasDefault: Boolean, modifier: ParameterModifier) extends ProductNode with Serializable

object Package extends SingletonMap[Package] {
  def unique(name: String) = find(new Package(name.intern))
}
case class Package private(name: String) extends Serializable

object ParameterList extends SingletonMap[ParameterList] {
  def unique(parameters: Array[MethodParameter], isImplicit: Boolean) = find(new ParameterList(parameters, isImplicit))
}
case class ParameterList private(parameters: WrappedArray[MethodParameter], isImplicit: Boolean) extends ProductNode with Serializable

object Source extends SingletonMap[Source] {
  def unique(packages: Array[Package], definitions: Array[Definition]) = {
    stableSort(definitions)
    find(new Source(packages, definitions))
  }
}
case class Source(packages: WrappedArray[Package], definitions: WrappedArray[Definition]) extends Serializable
