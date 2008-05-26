/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Either.scala 14320 2008-03-07 10:53:38Z washburn $

package scala

/**
 * The left side of the disjoin union, as opposed to the <code>Right</code> side.
 */
final case class Left[+A, +B](a: A) extends Either[A, B]