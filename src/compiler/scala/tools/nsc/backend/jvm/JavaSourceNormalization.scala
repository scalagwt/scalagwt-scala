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
 * Miscellaneous methods that create and modify trees.
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
  // TODO(spoon): don't use setType explicitly; use mkFoo methods
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
          Throw(New(TypeTree(RuntimeExceptionClass.tpe), List(List(Literal("not reached") /* TODO(spoon): setType string*/)))) setType AllClass.tpe
        
        CaseDef(Bind(exSym, Typed(Ident(nme.WILDCARD), TypeTree(excType))),
                EmptyTree,
                Block(
                  List(hiddenThrow, notReachedThrow),
                  unitLiteral) setType AllClass.tpe)
      }
      Try(body, catches, EmptyTree) setType body.tpe
    }
  }

  def box(boxFunction: Symbol, expr: Tree): Tree = {
    val boxedType = boxFunction.tpe.paramTypes(0).typeSymbol
    mkApply(mkAttributedRef(javaBoxMethod(boxedType)), List(expr))
  }
  
  def unbox(boxFunction: Symbol, expr: Tree): Tree = {
    val boxedType = boxFunction.tpe.resultType.typeSymbol
    mkApply(mkAttributedRef(javaUnboxMethod(boxedType)), List(expr))
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
        case tree@Block(stats, exp) => mkBlock(stats, mkReturn(exp))
      }
    else
      explicitBlock(tree)
  
  def mkApply(fun: Tree, args: List[Tree]) =
    Apply(fun, args) setType fun.symbol.tpe.resultType setSymbol fun.symbol
  def mkBlock(stats: List[Tree], exp: Tree) = Block(stats, exp) setType exp.tpe
  def mkReturn(exp: Tree) = Return(exp) setType AllClass.tpe
  
  val unitLiteral = Literal(Constant()) setType UnitClass.tpe
}
