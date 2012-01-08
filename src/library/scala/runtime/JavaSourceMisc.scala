/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime

/**
 * Miscellaneous utilities for Java source code.
 *
 * NOTE: Although these compile to correct Java bytecode, they
 * do not compile to correct Java source code.
 *
 * TODO(spoon): Check with scala-devel on how to make this
 * object be ignored for .NET.
 *
 * @author Lex Spoon
 */
object JavaSourceMisc {
  /**
   * Throw an exception.  This is used, when generating Java
   * source code, as a replacement for the Java <code>throw</code>
   * expression.  By using this method instead of the Java
   * <code>throw</code>, the caller does not need to annotate
   * their method as throwing any exceptions.
   */
  def hiddenThrow(exc: Throwable) = throw exc
}
