package scala.tools.nsc.backend.jvm
import scala.collection.mutable.ListBuffer

/**
 * Several expressions in Scala can only be statements in Java.  This
 * trait rewrites method bodies with such expressions whenever they
 * appear in a non-statement position.
 */
trait RemoveNonJavaExpressions {
  val global: nsc.symtab.SymbolTable
  import global._

  /**
   * Rewrite a tree to be a sequence of statements followed by a single expression.
   */
  private class Trans extends Transformer {
    /** A list of statements that need to be inserted at the
     *  nearest enclosing block. */
    val newStats = new ListBuffer[Tree]
    
    /**
     * Add an explicit block around a tree if tree isn't one already.
     */
    private def explicitBlock(tree: Tree): Block =
      tree match {
        case block: Block => block
        case tree => Block(Nil, tree)
      }
    
  
    /**
     * This is tricky.  If any of the trees add new statements
     * to be inserted in <code>newStats</code>, then trees earlier
     * in the list need to be extracted to their own local
     * variable in order to preserve order of execution.
     */
    override def transformTrees(trees: List[Tree]): List[Tree] = {
      super.transformTrees(trees) // TODO(spoon)
    }
 
    /**
     * Same as above but some of the trees are in statement position.
     */
    override def transformTrees(trees: List[Tree], isStat: List[Boolean]): List[Tree] = {
      super.transformTrees(trees) // TODO(spoon)
    }
    
    /**
     * Transform a tree that is known to be used in statement position.
     */
    def transformStatement(tree: Tree): Tree =
      tree match {
        case If(cond, exp1, exp2) =>
          val exp1B = explicitBlock(exp1)
          val exp2B = explicitBlock(exp2)
          val List(condT, exp1T, exp2T) = 
            transformTrees(List(cond, exp1B, exp2B), List(false, true, true))
          copy.If(tree, condT, exp1T, exp2T)
        case Block(stats, exp) =>
          val statsT = List.mapConserve(stats)(transformStatement)
          val expT = transformStatement(exp)
          copy.Block(tree, statsT, expT)
        case tree =>
          transform(tree)
      }

    override def transform(tree: Tree): Tree =
      tree match {
	    case ValDef(mods, name, tpt, rhs) =>
          super.transform(copy.ValDef(tree, mods, name, tpt, explicitBlock(rhs)))
	    case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          super.transform(copy.DefDef(tree, mods, name, tparams, vparamss, tpt, explicitBlock(rhs)))
	    case LabelDef(name, params, rhs) =>
          super.transform(copy.LabelDef(tree, name, params, explicitBlock(rhs)))
	    case Block(stats, expr) =>
           assert(newStats.isEmpty)
           var statsLeft = stats
           while (!statsLeft.isEmpty) {
             newStats += transformStatement(statsLeft.head)
             statsLeft = statsLeft.tail
           }
           val exprT = transform(expr)
           val newStatsList = newStats.toList
           newStats.clear()
           copy.Block(tree, newStatsList, exprT)
	    case If(cond, exp1, exp2) =>
          val List(condT, exp1T, exp2T) = transformTrees(List(cond, exp1, exp2))
          mkCall(ifExpression, condT, exp1T, exp2T)
	    case Try(block, catches, finalizer) =>
          super.transform(copy.Try(tree, explicitBlock(block), catches, explicitBlock(finalizer)))
	    case Apply(fun, args) =>
          val funT :: argsT = transformTrees(fun :: args)
          copy.Apply(tree, funT, argsT)
        case tree => super.transform(tree)
      }
  }
}
