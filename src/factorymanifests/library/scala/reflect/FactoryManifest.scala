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
import FactoryManifest.Factory

/** A `FactoryManifest[T]` is an opaque descriptor for type `T`.
 *  It is used by the compiler to preserve information necessary
 *  for instantiating `Arrays` in those cases where the element type
 *  is unknown at compile time.
 *
 *  As opposed to `ClassManifest[T]`, `FactoryManifest[T]` uses
 *  utilizes instance of a `Factory[T]` for Array creation. That
 *  instance is supplied by compiler at `factorymanifests` compiler
 *  phase.
 */
trait FactoryManifest[T] extends ClassManifest[T] {
  /** A class representing the type `U` to which `T` would be erased. Note
    * that there is no subtyping relationship between `T` and `U`. */
  
  def factory: Factory[T]
  
  private def arrayClass[T](f: Factory[T]): JClass[Array[T]] =
    factory.newInstance(0).getClass().asInstanceOf[JClass[Array[T]]]

  override def arrayManifest: FactoryManifest[Array[T]] =
    FactoryManifest.classType[Array[T]](arrayClass(factory), factory.forArrayOf)

  override def newArray(len: Int): Array[T] = factory.newInstance(len)

  override def newArray2(len: Int): Array[Array[T]] = factory.forArrayOf.newInstance(len)

  override def newArray3(len: Int): Array[Array[Array[T]]] = factory.forArrayOf.forArrayOf.newInstance(len)

  override def newArray4(len: Int): Array[Array[Array[Array[T]]]] = factory.forArrayOf.forArrayOf.forArrayOf.newInstance(len)

  override def newArray5(len: Int): Array[Array[Array[Array[Array[T]]]]] = factory.forArrayOf.forArrayOf.forArrayOf.forArrayOf.newInstance(len)

}

/** The object `ClassManifest` defines factory methods for manifests.
 *  It is intended for use by the compiler and should not be used in client code.
 */
object FactoryManifest {
  
  trait Factory[T] {
    def newInstance(len: Int): Array[T]
    def forArrayOf: Factory[Array[T]]
  }

  /** FactoryManifest for the class type `clazz`, where `clazz` is
    * a top-level or static class.
    * @note This no-prefix, no-arguments case is separate because we
    *       it's called from ScalaRunTime.boxArray itself. If we
    *       pass varargs as arrays into this, we get an infinitely recursive call
    *       to boxArray. (Besides, having a separate case is more efficient)
    */
  def classType[T <: AnyRef](clazz: JClass[_], factory: Factory[T]): FactoryManifest[T] =
    new ClassTypeFactoryManifest[T](None, clazz, factory, Nil)

  /** FactoryManifest for the class type `clazz[args]`, where `clazz` is
    * a top-level or static class and `args` are its type arguments */
  def classType[T <: AnyRef](clazz: JClass[_], factory: Factory[T], arg1: OptManifest[_], args: OptManifest[_]*): FactoryManifest[T] =
    new ClassTypeFactoryManifest[T](None, clazz, factory, arg1 :: args.toList)

  /** FactoryManifest for the class type `clazz[args]`, where `clazz` is
    * a class with non-package prefix type `prefix` and type arguments `args`.
    */
  def classType[T <: AnyRef](prefix: OptManifest[_], clazz: JClass[_], factory: Factory[T], args: OptManifest[_]*): FactoryManifest[T] =
    new ClassTypeFactoryManifest[T](Some(prefix), clazz, factory, args.toList)

}

/** Manifest for the class type `clazz[args]`, where `clazz` is
  * a top-level or static class. */
private class ClassTypeFactoryManifest[T <: AnyRef](
  prefix: Option[OptManifest[_]], 
  val erasure: JClass[_],
  val factory: Factory[T],
  override val typeArguments: List[OptManifest[_]]) extends FactoryManifest[T]
{
  override def toString = 
    (if (prefix.isEmpty) "" else prefix.get.toString+"#") + 
    (if (erasure.isArray) "Array" else erasure.getName) +
    argString
}
