/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.nsc.backend.jvm
import nsc.ast.TreeGen
import nsc.symtab.SymbolTable
import nsc.typechecker.Typers
import nsc.util.Position

/**
 * Utilities for normalizing code for the purpose of generating Java source code.
 * 
 * @author Lex Spoon
 */
trait JavaSourceNormalization
extends JavaDefinitions
with JavaSourceAnalysis
{
  import global._
  import definitions._
  import javaDefinitions._
  
  object treeGen extends TreeGen {
    val global: JavaSourceNormalization.this.global.type = JavaSourceNormalization.this.global
  }
  import treeGen._
  
  // TODO(spoon): don't hide exceptions that the method is declared as throwing
  // TODO(spoon): don't rewrite throws of run-time exceptions
  // TODO(spoon): make this part of RemoveJavaNonExpressions
  def hideExceptions(body: Tree): Tree = {
    val exceptions = exceptionsThrown(body)
    if (exceptions.isEmpty) {
      body
    } else {
      val catches = for (exc <- exceptions) yield {
        val excType = exc.tpe
        val exSym = NoSymbol.newValue(body.pos, "ex")
        exSym setInfo excType
        
        val hiddenThrow = 
          Apply(mkAttributedSelect(mkAttributedRef(JavaSourceMiscModule), JavaSourceMisc_hiddenThrow),
                List(mkAttributedRef(exSym))) setSymbol JavaSourceMisc_hiddenThrow
        val notReachedThrow =
          Throw(New(TypeTree(RuntimeExceptionClass.tpe), List(List(Literal("not reached"))))) setType AllClass.tpe
        
        CaseDef(Bind(exSym, Typed(Ident(nme.WILDCARD), TypeTree(excType))),
                EmptyTree,
                Block(
                  List(hiddenThrow, notReachedThrow),
                  Literal(())))
      }
      Try(body, catches, EmptyTree) setType body.tpe
    }
  }
  
  /**
   * Add an explicit block around a tree if tree isn't one already.
   * As one exception, if the argument is the empty tree, then the
   * empty tree is returned.
   */
  def explicitBlock(tree: Tree): Tree =
    tree match {
    case tree@EmptyTree => tree
    case block: Block => block
    case tree => Block(Nil, tree)
    }
    
  /**
   * Add an explicit block around a tree, and make the last statement
   * be a return statement, unless its type is Unit or Nothing.
   */
  def explicitBlockWithReturn(tree: Tree): Tree =
    if (typeReturnable(tree.tpe) : @unchecked)
      explicitBlock(tree) match {
      case tree@EmptyTree => tree
      case tree@Block(stats, Return(exp)) => tree
      case tree@Block(stats, exp) => Block(stats, Return(exp))
      }
    else
      explicitBlock(tree)
}
