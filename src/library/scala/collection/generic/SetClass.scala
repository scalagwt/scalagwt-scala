/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// $Id: Traversable.scala 15188 2008-05-24 15:01:02Z stepancheg $
package scala.collection.generic

trait SetClass[A, +CC[X] <: Set[X]] extends TraversableClass[A, CC] { 
  def empty: CC[A] = companion.empty[A]
}

