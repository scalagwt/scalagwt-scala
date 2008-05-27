/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.nsc.backend.jvm
import scala.tools.nsc.symtab.SymbolTable

/**
 * Standard symbols that are used only by the Java backends, along with
 * convenience methods for generating trees using them.
 */
trait JavaDefinitions {
  val global: SymbolTable
  import global._
  
  object javaDefinitions {
    import definitions.{getMember, getModule}
    lazy val JavaSourceMiscModule: Symbol = getModule("scala.runtime.JavaSourceMisc")
    lazy val JavaSourceMisc_hiddenThrow = getMember(JavaSourceMiscModule, "hiddenThrow")
    lazy val RuntimeExceptionClass = definitions.getClass("java.lang.RuntimeException")
  }
}