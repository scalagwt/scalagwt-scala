/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.nsc
package backend.jribble
import ast.TreeGen
import symtab.SymbolTable
import typechecker.Typers
import util.Position

import scala.collection.mutable.ListBuffer

/**
 * Miscellaneous methods that create and modify trees.
 * 
 * @author Lex Spoon, Grzegorz Kossakowski
 */
trait JribbleNormalization
extends JavaDefinitions
with JribbleAnalysis
{
  import global._
  import definitions._
  import javaDefinitions._
  
  object treeGen extends TreeGen {
    val global: JribbleNormalization.this.global.type = JribbleNormalization.this.global
  }
  import treeGen._

  def box(boxFunction: Symbol, expr: Tree): Tree = {
    val boxedType = boxFunction.tpe.paramTypes(0).typeSymbol
    mkApply(mkAttributedRef(javaBoxMethod(boxedType)), List(expr))
  }
  
  def unbox(boxFunction: Symbol, expr: Tree): Tree = {
    val boxedType = boxFunction.tpe.resultType.typeSymbol
    mkApply(mkAttributedRef(javaUnboxMethod(boxedType)), List(expr))
  }
  
  /** Defines trees corresponding to accessing static fields of java.lang.Float class */
  object Float {
    def mkNaN         = mkAttributedRef(javaDefinitions.Float.NaN)
    def mkPosInfinity = mkAttributedRef(javaDefinitions.Float.PosInfinity)
    def mkNegInfinity = mkAttributedRef(javaDefinitions.Float.NegInfinity)
  }
  
  /** Defines trees corresponding to accessing static fields of java.lang.Double class */
  object Double {
    def mkNaN         = mkAttributedRef(javaDefinitions.Double.NaN)
    def mkPosInfinity = mkAttributedRef(javaDefinitions.Double.PosInfinity)
    def mkNegInfinity = mkAttributedRef(javaDefinitions.Double.NegInfinity)
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
    case tree => mkBlock(Nil, tree)
    }
    
  /**
   * Add an explicit block around a tree, and make the last statement
   * be a return statement, unless its type is Unit or Nothing.
   */
  def explicitBlockWithReturn(tree: Tree): Tree =
    if (typeReturnable(tree.tpe))
      (explicitBlock(tree) : @unchecked) match {
        case tree@EmptyTree => tree
        case tree@Block(_, _:Return) => tree
        case tree@Block(_, exp) if isNothing(exp) => tree
        case tree@Block(stats, exp) => mkBlock(stats, mkReturn(exp))
      }
    else
      explicitBlock(tree)
  
  def mkApply(fun: Tree, args: List[Tree]) =
    Apply(fun, args) setType fun.symbol.tpe.resultType setSymbol fun.symbol
  def mkBlock(stats: List[Tree], exp: Tree) =
    Block(stats, exp) setType (
     // set type to Nothing if any stat has type nothing.
     if (stats exists (t => isNothing(t))) definitions.NothingClass.tpe else exp.tpe)
  def mkReturn(exp: Tree) = Return(exp) setType NothingClass.tpe

  def mkDefDef(sym: MethodSymbol, mods: Modifiers, params: List[ValDef], rhs: Tree) =
    DefDef(sym, mods, List(params), rhs)
  
  val unitLiteral = Literal(Constant()) setType UnitClass.tpe
}
