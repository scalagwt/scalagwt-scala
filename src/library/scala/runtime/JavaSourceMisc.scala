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
 * TODO(spoon): annotate this as only to be translated when
 * building for Java bytecode.  When generating Java source,
 * this should not be included.
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
