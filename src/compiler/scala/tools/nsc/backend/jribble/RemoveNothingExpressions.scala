/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.tools.nsc
package backend.jribble

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.symtab.SymbolTable
import scala.tools.nsc.util.Position

/**
 * Move expressions of type Nothing up to top-level statements.
 */
trait RemoveNothingExpressions
extends Transform
with JribbleAnalysis
{
  val global: Global
  import global._
  import definitions._

  override val phaseName = "nothingexps"
  
  override protected def newTransformer(unit: CompilationUnit): Transformer =
    new Trans(unit)
  
  /**
   * Rewrite a tree to be a sequence of statements followed by a single expression.
   */
  private class Trans(cunit: CompilationUnit) extends Transformer {
    override def transform(inputTree: Tree): Tree = {
      val tree = super.transform(inputTree)
      tree match {
        case ValDef(mods, name, tpt, rhs) =>
          if (tree.symbol.isLocal && isNothing(rhs)) {
            // return just the rhs; the val will not be needed
            // because all statements after this one in
            // the enclosing block will be discarded
            rhs
          } else {
            tree
          }
          
        case Block(stats, expr) =>
          val nothingIndex = stats indexWhere isNothing
          if (nothingIndex >= 0) {
            (Block(stats take (nothingIndex + 1), expr)
              copyAttrs tree)
          } else {
            tree
          }

        case Assign(lhs, rhs) =>
          if (isNothing(lhs)) {
            lhs
          } else if (isNothing(rhs)) {
            (Block(List(lhs), rhs)
              copyAttrs tree setType definitions.NothingClass.tpe)
          } else {
            tree
          }

        case If(cond, thenp, elsep) =>
          if (isNothing(cond)) {
            cond
          } else {
            // NormalizeForJribble takes care of the case where
            // a conditional expression has a type-Nothing branch
            tree
          }

        case Match(selector, cases) =>
          if (isNothing(selector))  selector else tree
  
        case Return(expr) =>
          if (isNothing(expr)) expr else tree  

        case Throw(expr) =>
          if (isNothing(expr)) expr else tree

        case Apply(Select(rcvr, _), args) =>
          if (isNothing(rcvr)) {
            rcvr
          } else {
            val indexOfNothing = args indexWhere isNothing
            if (indexOfNothing < 0) {
              tree
            } else {
              Block(List(rcvr) ::: args take indexOfNothing, args(indexOfNothing))
            }
          }

        case Select(qualifier, _) =>
          if (isNothing(qualifier)) qualifier else tree

        case tree => tree
      }
    }
  }
}
