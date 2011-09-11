/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.reflect

import scala.collection.mutable.{ WrappedArray, ArrayBuilder }
import java.lang.{ Class => JClass }
import FactoryClassManifest.Factory

/** A `FactoryClassManifest[T]` is an opaque descriptor for type `T`.
 *  It is used by the compiler to preserve information necessary
 *  for instantiating `Arrays` in those cases where the element type
 *  is unknown at compile time.
 *
 *  As opposed to `ClassManifest[T]`, `FactoryClassManifest[T]` uses
 *  utilizes instance of a `Factory[T]` for Array creation. That
 *  instance is supplied by compiler at `factorymanifests` compiler
 *  phase.
 */
trait FactoryClassManifest[T] extends ClassManifest[T] {
  /** A class representing the type `U` to which `T` would be erased. Note
    * that there is no subtyping relationship between `T` and `U`. */
  
  def factory: Factory[T]
  
  protected def arrayClass[T](f: Factory[T]): JClass[Array[T]] =
    f.newInstance(0).getClass().asInstanceOf[JClass[Array[T]]]

  override def arrayManifest: FactoryClassManifest[Array[T]] =
    FactoryClassManifest.classType[Array[T]](arrayClass(factory), factory.forArrayOf)

  override def newArray(len: Int): Array[T] = factory.newInstance(len)

  override def newArray2(len: Int): Array[Array[T]] = factory.forArrayOf.newInstance(len)

  override def newArray3(len: Int): Array[Array[Array[T]]] = factory.forArrayOf.forArrayOf.newInstance(len)

  override def newArray4(len: Int): Array[Array[Array[Array[T]]]] = factory.forArrayOf.forArrayOf.forArrayOf.newInstance(len)

  override def newArray5(len: Int): Array[Array[Array[Array[Array[T]]]]] = factory.forArrayOf.forArrayOf.forArrayOf.forArrayOf.newInstance(len)

}

/** The object `FactoryClassManifest` defines factory methods for manifests.
 *  It is intended for use by the compiler and should not be used in client code.
 */
object FactoryClassManifest {
  
  val Byte    = FactoryManifest.Byte
  val Short   = FactoryManifest.Short
  val Char    = FactoryManifest.Char
  val Int     = FactoryManifest.Int
  val Long    = FactoryManifest.Long
  val Float   = FactoryManifest.Float
  val Double  = FactoryManifest.Double
  val Boolean = FactoryManifest.Boolean
  val Unit    = FactoryManifest.Unit
  val Any     = FactoryManifest.Any
  val Object  = FactoryManifest.Object
  val AnyVal  = FactoryManifest.AnyVal
  val Nothing = FactoryManifest.Nothing
  val Null    = FactoryManifest.Null
  
  trait Factory[T] {
    def newInstance(len: Int): Array[T]
    def forArrayOf: Factory[Array[T]]
  }

  /** FactoryClassManifest for the class type `clazz`, where `clazz` is
    * a top-level or static class.
    * @note This no-prefix, no-arguments case is separate because we
    *       it's called from ScalaRunTime.boxArray itself. If we
    *       pass varargs as arrays into this, we get an infinitely recursive call
    *       to boxArray. (Besides, having a separate case is more efficient)
    */
  def classType[T <: AnyRef](clazz: JClass[_], factory: Factory[T]): FactoryClassManifest[T] =
    new ClassTypeFactoryManifest[T](None, clazz, factory, Nil)

  /** FactoryClassManifest for the class type `clazz[args]`, where `clazz` is
    * a top-level or static class and `args` are its type arguments */
  def classType[T <: AnyRef](clazz: JClass[_], factory: Factory[T], arg1: OptManifest[_], args: OptManifest[_]*): FactoryClassManifest[T] =
    new ClassTypeFactoryManifest[T](None, clazz, factory, arg1 :: args.toList)

  /** FactoryClassManifest for the class type `clazz[args]`, where `clazz` is
    * a class with non-package prefix type `prefix` and type arguments `args`.
    */
  def classType[T <: AnyRef](prefix: OptManifest[_], clazz: JClass[_], factory: Factory[T], args: OptManifest[_]*): FactoryClassManifest[T] =
    new ClassTypeFactoryManifest[T](Some(prefix), clazz, factory, args.toList)

  def fromClass[T](clazz: JClass[T], factory: Factory[T]): FactoryClassManifest[T] = clazz match {
    case java.lang.Byte.TYPE      => Byte.asInstanceOf[FactoryClassManifest[T]]
    case java.lang.Short.TYPE     => Short.asInstanceOf[FactoryClassManifest[T]]
    case java.lang.Character.TYPE => Char.asInstanceOf[FactoryClassManifest[T]]
    case java.lang.Integer.TYPE   => Int.asInstanceOf[FactoryClassManifest[T]]
    case java.lang.Long.TYPE      => Long.asInstanceOf[FactoryClassManifest[T]]
    case java.lang.Float.TYPE     => Float.asInstanceOf[FactoryClassManifest[T]]
    case java.lang.Double.TYPE    => Double.asInstanceOf[FactoryClassManifest[T]]
    case java.lang.Boolean.TYPE   => Boolean.asInstanceOf[FactoryClassManifest[T]]
    case java.lang.Void.TYPE      => Unit.asInstanceOf[FactoryClassManifest[T]]
    case _                        => classType[T with AnyRef](clazz, factory.asInstanceOf[Factory[T with AnyRef]]).asInstanceOf[FactoryClassManifest[T]]
  }

  def singleType[T <: AnyRef](value: AnyRef, factory: Factory[T]): FactoryManifest[T] = FactoryManifest.singleType(value, factory)

  def arrayType[T](arg: OptManifest[_]): FactoryClassManifest[Array[T]] = arg match {
    case NoManifest => Object.asInstanceOf[FactoryClassManifest[Array[T]]]
    case m: FactoryClassManifest[_] => m.asInstanceOf[FactoryClassManifest[T]].arrayManifest
  }

  /** FactoryClassManifest for the abstract type `prefix # name`. `upperBound` is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection. */
  def abstractType[T](prefix: OptManifest[_], name: String, clazz: JClass[_], args: OptManifest[_]*): ClassManifest[T] =
    new ClassManifest[T] {
      def erasure = clazz
      override val typeArguments = args.toList
      override def toString = prefix.toString+"#"+name+argString
    }

  /** FactoryClassManifest for the abstract type `prefix # name`. `upperBound` is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection.
    * todo: remove after next boostrap
    */
  def abstractType[T](prefix: OptManifest[_], name: String, upperbound: ClassManifest[_], args: OptManifest[_]*): ClassManifest[T] =
    new ClassManifest[T] {
      def erasure = upperbound.erasure
      override val typeArguments = args.toList
      override def toString = prefix.toString+"#"+name+argString
    }

}

/** FactoryClassManifest for the class type `clazz[args]`, where `clazz` is
  * a top-level or static class. */
private class ClassTypeFactoryManifest[T <: AnyRef](
  prefix: Option[OptManifest[_]], 
  val erasure: JClass[_],
  val factory: Factory[T],
  override val typeArguments: List[OptManifest[_]]) extends FactoryClassManifest[T]
{
  override def toString = 
    (if (prefix.isEmpty) "" else prefix.get.toString+"#") + 
    (if (erasure.isArray) "Array" else erasure.getName) +
    argString
}
