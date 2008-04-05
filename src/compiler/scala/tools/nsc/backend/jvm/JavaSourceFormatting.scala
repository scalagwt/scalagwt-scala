/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$
package scala.tools.nsc.backend.jvm
import nsc.symtab.SymbolTable
import nsc.backend.icode.TypeKinds

/**
 * Utilities for formatting Scala constructs in Java syntax.
 * 
 *  @author  Lex Spoon
 */
trait JavaSourceFormatting {
  val global: SymbolTable { def abort(msg: String): Nothing }
  import global._
  protected val typeKinds: TypeKinds {
    val global: JavaSourceFormatting.this.global.type
  }
  import typeKinds._
  
  protected def javaName(sym: Symbol, fullyQualify: Boolean): String = {
    def suffix = if (sym.isModuleClass && !sym.isTrait) "$" else ""

    // TODO(spoon): why the special cases?  double check that they are needed
    if (sym == definitions.AllClass)
      return "scala.runtime.Nothing$"
    else if (sym == definitions.AllRefClass)
      return "scala.runtime.Null$"

    val name = if (fullyQualify) sym.fullNameString('.') else sym.simpleName
    name + suffix
  }
  
  protected def javaShortName(sym: Symbol): String =
    javaName(sym, false)
  
  protected def javaName(sym: Symbol): String =
    javaName(sym, true)
  
  protected def javaName(tpe: Type): String = {
    def tpstr(typ: TypeKind): String =
      typ match {
        case UNIT => "void" // TODO(spoon): depends on context?  a Scala variable can be of type unit!
        case BOOL            => "boolean"
        case BYTE            => "byte"
        case SHORT           => "short"
        case CHAR            => "char"
        case INT             => "int"
        case LONG            => "long"
        case FLOAT           => "float"
        case DOUBLE          => "double"
        case REFERENCE(cls)  => javaName(cls)
        case ARRAY(elem)     => tpstr(elem) + "[]"
      }
    return tpstr(toTypeKind(tpe))
  }
}
