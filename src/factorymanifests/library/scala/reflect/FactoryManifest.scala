/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.reflect

import scala.collection.mutable.{ ArrayBuilder, WrappedArray }
import FactoryClassManifest.Factory

/** A `FactoryManifest[T]` is an opaque descriptor for type T.  Its supported use
 *  is to give access to the erasure of the type as a `Class` instance, as
 *  is necessary for the creation of native `Arrays` if the class is not
 *  known at compile time.
 *
 *  As opposed to `Manifest[T]`, `FactoryManifest[T]` uses  utilizes instance 
 *  of a `Factory[T]` for Array creation. That instance is supplied by compiler 
 *  at `factorymanifests` compiler phase.
 *
 *  Example usages:
{{{
  def arr[T] = new Array[T](0)                          // does not compile
  def arr[T](implicit m: Manifest[T]) = new Array[T](0) // compiles
  def arr[T: Manifest] = new Array[T](0)                // shorthand for the preceding
  
  // Methods manifest, classManifest, and optManifest are in [[scala.Predef]].
  def isApproxSubType[T: Manifest, U: Manifest] = manifest[T] <:< manifest[U]
  isApproxSubType[List[String], List[AnyRef]] // true
  isApproxSubType[List[String], List[Int]]    // false

  def methods[T: ClassManifest] = classManifest[T].erasure.getMethods
  def retType[T: ClassManifest](name: String) =
    methods[T] find (_.getName == name) map (_.getGenericReturnType)

  retType[Map[_, _]]("values")  // Some(scala.collection.Iterable<B>)
}}}
 *
 */
@annotation.implicitNotFound(msg = "No Manifest available for ${T}.")
trait FactoryManifest[T] extends Manifest[T] with FactoryClassManifest[T] {

  override def arrayManifest: FactoryManifest[Array[T]] = 
    FactoryManifest.classType[Array[T]](arrayClass[T](factory), factory.forArrayOf)

}

/** The object `FactoryManifest` defines factory methods for manifests.
 *  It is intended for use by the compiler and should not be used
 *  in client code.
 */
object FactoryManifest {
  private def ObjectClass = classOf[java.lang.Object]
  
  trait AnyValManifest[T] extends scala.reflect.AnyValManifest[T] with FactoryManifest[T]

  val Byte: AnyValManifest[Byte] = new AnyValManifest[scala.Byte] {
    def erasure = java.lang.Byte.TYPE
    val factory: Factory[scala.Byte] = sys.error("stub. to be implemented by `factorymanifests` compiler plug-in.")
    override def toString = "Byte"
    override def newArray(len: Int): Array[Byte] = new Array[Byte](len)
    override def newWrappedArray(len: Int): WrappedArray[Byte] = new WrappedArray.ofByte(new Array[Byte](len))
    override def newArrayBuilder(): ArrayBuilder[Byte] = new ArrayBuilder.ofByte()
    private def readResolve(): Any = Manifest.Byte
  }

  val Short: AnyValManifest[Short] = new AnyValManifest[scala.Short] {
    def erasure = java.lang.Short.TYPE
    val factory: Factory[scala.Short] = sys.error("stub. to be implemented by `factorymanifests` compiler plug-in.")
    override def toString = "Short"
    override def newArray(len: Int): Array[Short] = new Array[Short](len)
    override def newWrappedArray(len: Int): WrappedArray[Short] = new WrappedArray.ofShort(new Array[Short](len))
    override def newArrayBuilder(): ArrayBuilder[Short] = new ArrayBuilder.ofShort()
    private def readResolve(): Any = Manifest.Short
  }

  val Char: AnyValManifest[Char] = new AnyValManifest[scala.Char] {
    def erasure = java.lang.Character.TYPE
    val factory: Factory[scala.Char] = sys.error("stub. to be implemented by `factorymanifests` compiler plug-in.")
    override def toString = "Char"
    override def newArray(len: Int): Array[Char] = new Array[Char](len)
    override def newWrappedArray(len: Int): WrappedArray[Char] = new WrappedArray.ofChar(new Array[Char](len))
    override def newArrayBuilder(): ArrayBuilder[Char] = new ArrayBuilder.ofChar()
    private def readResolve(): Any = Manifest.Char
  }

  val Int: AnyValManifest[Int] = new AnyValManifest[scala.Int] {
    def erasure = java.lang.Integer.TYPE
    val factory: Factory[scala.Int] = sys.error("stub. to be implemented by `factorymanifests` compiler plug-in.")
    override def toString = "Int"
    override def newArray(len: Int): Array[Int] = new Array[Int](len)
    override def newWrappedArray(len: Int): WrappedArray[Int] = new WrappedArray.ofInt(new Array[Int](len))
    override def newArrayBuilder(): ArrayBuilder[Int] = new ArrayBuilder.ofInt()
    private def readResolve(): Any = Manifest.Int
  }

  val Long: AnyValManifest[Long] = new AnyValManifest[scala.Long] {
    def erasure = java.lang.Long.TYPE
    val factory: Factory[scala.Long] = sys.error("stub. to be implemented by `factorymanifests` compiler plug-in.")
    override def toString = "Long"
    override def newArray(len: Int): Array[Long] = new Array[Long](len)
    override def newWrappedArray(len: Int): WrappedArray[Long] = new WrappedArray.ofLong(new Array[Long](len))
    override def newArrayBuilder(): ArrayBuilder[Long] = new ArrayBuilder.ofLong()
    private def readResolve(): Any = Manifest.Long
  }

  val Float: AnyValManifest[Float] = new AnyValManifest[scala.Float] {
    def erasure = java.lang.Float.TYPE
    val factory: Factory[scala.Float] = sys.error("stub. to be implemented by `factorymanifests` compiler plug-in.")
    override def toString = "Float"
    override def newArray(len: Int): Array[Float] = new Array[Float](len)
    override def newWrappedArray(len: Int): WrappedArray[Float] = new WrappedArray.ofFloat(new Array[Float](len))
    override def newArrayBuilder(): ArrayBuilder[Float] = new ArrayBuilder.ofFloat()
    private def readResolve(): Any = Manifest.Float
  }

  val Double: AnyValManifest[Double] = new AnyValManifest[scala.Double] {
    def erasure = java.lang.Double.TYPE
    val factory: Factory[scala.Double] = sys.error("stub. to be implemented by `factorymanifests` compiler plug-in.")
    override def toString = "Double"
    override def newArray(len: Int): Array[Double] = new Array[Double](len)
    override def newWrappedArray(len: Int): WrappedArray[Double] = new WrappedArray.ofDouble(new Array[Double](len))
    override def newArrayBuilder(): ArrayBuilder[Double] = new ArrayBuilder.ofDouble()
    private def readResolve(): Any = Manifest.Double
  }

  val Boolean: AnyValManifest[Boolean] = new AnyValManifest[scala.Boolean] {
    def erasure = java.lang.Boolean.TYPE
    val factory: Factory[scala.Boolean] = sys.error("stub. to be implemented by `factorymanifests` compiler plug-in.")
    override def toString = "Boolean"
    override def newArray(len: Int): Array[Boolean] = new Array[Boolean](len)
    override def newWrappedArray(len: Int): WrappedArray[Boolean] = new WrappedArray.ofBoolean(new Array[Boolean](len))
    override def newArrayBuilder(): ArrayBuilder[Boolean] = new ArrayBuilder.ofBoolean()
    private def readResolve(): Any = Manifest.Boolean
  }

  val Unit: AnyValManifest[Unit] = new AnyValManifest[scala.Unit] {
    def erasure = java.lang.Void.TYPE
    val factory: Factory[scala.Unit] = sys.error("stub. to be implemented by `factorymanifests` compiler plug-in.")
    override def toString = "Unit"
    override def newArray(len: Int): Array[Unit] = new Array[Unit](len)
    override def newWrappedArray(len: Int): WrappedArray[Unit] = new WrappedArray.ofUnit(new Array[Unit](len))
    override def newArrayBuilder(): ArrayBuilder[Unit] = new ArrayBuilder.ofUnit()
    private def readResolve(): Any = Manifest.Unit
  }

  private val AnyFactory: Factory[Any] = sys.error("stub. to be implemented by `factorymanifests` compiler plug-in.")
  val Any: FactoryManifest[Any] = new ClassTypeManifest[scala.Any](None, ObjectClass, AnyFactory, Nil) {
    override def toString = "Any"
    override def <:<(that: ClassManifest[_]): Boolean = (that eq this)
    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    override def hashCode = System.identityHashCode(this)
    private def readResolve(): Any = Manifest.Any
  }

  private val ObjectFactory: Factory[Object] = sys.error("stub. to be implemented by `factorymanifests` compiler plug-in.")
  val Object: FactoryManifest[Object] = new ClassTypeManifest[java.lang.Object](None, ObjectClass, ObjectFactory, Nil) {
    override def toString = "Object"
    override def <:<(that: ClassManifest[_]): Boolean = (that eq this) || (that eq Any)
    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    override def hashCode = System.identityHashCode(this)
    private def readResolve(): Any = Manifest.Object
  }
  
  private val AnyValFactory: Factory[AnyVal] = sys.error("stub. to be implemented by `factorymanifests` compiler plug-in.")
  val AnyVal: FactoryManifest[AnyVal] = new ClassTypeManifest[scala.AnyVal](None, ObjectClass, AnyValFactory, Nil) {
    override def toString = "AnyVal"
    override def <:<(that: ClassManifest[_]): Boolean = (that eq this) || (that eq Any)
    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    override def hashCode = System.identityHashCode(this)
    private def readResolve(): Any = Manifest.AnyVal
  }

  private val NullFactory: Factory[Null] = sys.error("stub. to be implemented by `factorymanifests` compiler plug-in.")
  val Null: FactoryManifest[Null] = new ClassTypeManifest[scala.Null](None, ObjectClass, NullFactory, Nil) {
    override def toString = "Null"
    override def <:<(that: ClassManifest[_]): Boolean =
      (that ne null) && (that ne Nothing) && !(that <:< AnyVal)
    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    override def hashCode = System.identityHashCode(this)
    private def readResolve(): Any = Manifest.Null
  }

  private val NothingFactory: Factory[Nothing] = sys.error("stub. to be implemented by `factorymanifests` compiler plug-in.")
  val Nothing: FactoryManifest[Nothing] = new ClassTypeManifest[scala.Nothing](None, ObjectClass, NothingFactory, Nil) {
    override def toString = "Nothing"
    override def <:<(that: ClassManifest[_]): Boolean = (that ne null)
    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    override def hashCode = System.identityHashCode(this)
    private def readResolve(): Any = Manifest.Nothing
  }
  
  private class SingletonTypeManifest[T <: AnyRef](value: AnyRef, val factory: Factory[T]) extends FactoryManifest[T] {
    lazy val erasure = value.getClass
    override lazy val toString = value.toString + ".type"
  }

  /** Manifest for the singleton type `value.type`. */
  def singleType[T <: AnyRef](value: AnyRef, factory: Factory[T]): FactoryManifest[T] =
    new SingletonTypeManifest[T](value, factory)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
    * a top-level or static class.
    * @note This no-prefix, no-arguments case is separate because we
    *       it's called from ScalaRunTime.boxArray itself. If we
    *       pass varargs as arrays into this, we get an infinitely recursive call
    *       to boxArray. (Besides, having a separate case is more efficient)
    */
  def classType[T](clazz: Predef.Class[_], factory: Factory[T]): FactoryManifest[T] =
    new ClassTypeManifest[T](None, clazz, factory, Nil)

  /** Manifest for the class type `clazz`, where `clazz` is
    * a top-level or static class and args are its type arguments. */
  def classType[T](clazz: Predef.Class[T], factory: Factory[T], arg1: Manifest[_], args: Manifest[_]*): FactoryManifest[T] =
    new ClassTypeManifest[T](None, clazz, factory, arg1 :: args.toList)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
    * a class with non-package prefix type `prefix` and type arguments `args`.
    */
  def classType[T](prefix: Manifest[_], clazz: Predef.Class[_], factory: Factory[T], args: Manifest[_]*): FactoryManifest[T] =
    new ClassTypeManifest[T](Some(prefix), clazz, factory, args.toList)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
    * a top-level or static class. */
  private class ClassTypeManifest[T](prefix: Option[Manifest[_]], 
                                     val erasure: Predef.Class[_],
                                     val factory: Factory[T],
                                     override val typeArguments: List[Manifest[_]]) extends FactoryManifest[T] {
    override def toString = 
      (if (prefix.isEmpty) "" else prefix.get.toString+"#") +
      (if (erasure.isArray) "Array" else erasure.getName) +
      argString
   }

  def arrayType[T](arg: Manifest[_]): Manifest[Array[T]] = 
    arg.asInstanceOf[Manifest[T]].arrayManifest

  /** Manifest for the abstract type `prefix # name'. `upperBound` is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection. */
  def abstractType[T](prefix: Manifest[_], name: String, clazz: Predef.Class[_], factoryy: Factory[T], args: Manifest[_]*): FactoryManifest[T] =
    new FactoryManifest[T] {
      def erasure = clazz
      def factory = factoryy
      override val typeArguments = args.toList
      override def toString = prefix.toString+"#"+name+argString
    }

  /** Manifest for the unknown type `_ >: L <: U` in an existential.
    */
  def wildcardType[T](lowerBound: Manifest[_], upperBound: Manifest[_]): FactoryManifest[T] =
    new FactoryManifest[T] {
      def erasure = upperBound.erasure
      def factory = {
        assert(upperBound.isInstanceOf[FactoryManifest[_]], "Whack! You run into bug in factory manifests implementation.")
        upperBound.asInstanceOf[FactoryManifest[T]].factory
      }
      override def toString = 
        "_" +
        (if (lowerBound eq Nothing) "" else " >: "+lowerBound) + 
        (if (upperBound eq Nothing) "" else " <: "+upperBound)
    }

  /** Manifest for the intersection type `parents_0 with ... with parents_n'. */
  def intersectionType[T](parents: Manifest[_]*): FactoryManifest[T] =
    new FactoryManifest[T] {
      def erasure = parents.head.erasure
      def factory = {
        assert(parents.forall(_.isInstanceOf[FactoryManifest[_]]), "Whack! You run into bug in factory manifests implementation.")
        parents.head.asInstanceOf[FactoryManifest[T]].factory
      }
      override def toString = parents.mkString(" with ")
    }
}
