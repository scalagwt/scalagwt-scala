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
import nsc.typechecker.Typers

/**
 * Utilities for normalizing code for the purpose of generating Java source code.
 * 
 * @author Lex Spoon
 */
trait JavaSourceNormalization {
  self: JavaSourceAnalysis {
    val global: SymbolTable
  } =>
  import global._
  protected def typed(tree: Tree): Tree
  
  // TODO(spoon): don't hide exceptions that the method is declared as throwing
  def hideExceptions(body: Tree): Tree = {
    val exceptions = exceptionsThrown(body)
    if (exceptions.isEmpty) {
      body
    } else {
      val catches = for (exc <- exceptions) yield {
        val excType = exc.tpe
        val exSym = NoSymbol.newValue(body.pos, "ex")
        exSym setInfo excType
        
        CaseDef(Bind(exSym, Typed(Ident(nme.WILDCARD), TypeTree(excType))),
                EmptyTree,
                Throw(Ident(exSym)))
      }
      typed(Try(body, catches, EmptyTree))
    }
  }
}
