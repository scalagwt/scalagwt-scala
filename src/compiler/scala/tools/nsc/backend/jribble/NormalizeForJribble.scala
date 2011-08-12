/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RemoveNonJavaExpressions.scala 15515 2008-07-09 15:30:59Z spoon $

package scala.tools.nsc
package backend.jribble
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.symtab.SymbolTable

/**
 * Several expressions in Scala can only be statements in Jribble.  This
 * transform rewrites method bodies with such expressions whenever they
 * appear in an expression context.  For example, arguments to
 * method calls are in an expression context, but the statements of
 * a block are not in an expression context.
 * 
 * In addition, this transform does the following normalizations:
 * <ul>
 * <li> All methods that don't return Unit get an explicit return
 * <li> Expressions in statement position that Jribble disallows as
 *      statements are discarded.  Examples are literals,
 *      field selections, and <code>this</code>.  TODO(spoon): check that field selects are safe to drop; the instance can have a side effect!
 * <li> All thrown, checked exceptions are hidden from Java using
 *      <code>JavaSourceMisc.hiddenThrow()</code>.
 * <li> Many expressions are made to be a block if they aren't already:
 *      the body of a LabelDef, the three subexpressions of
 *      a try-catch-finally expression, the two branches of any
 *      if expression appearing as a statement in a block, the
 *      rhs of a DefDef.
 * <li> The expression of any block is simply a unit constant.
 *      Thus, after this phase, blocks are only used for
 *      side effects.
 * <li> Replace box and unbox operations by calls into the Scala
 *      runtime.
 * </ul>
 * 
 * TODO(spoon): in constructor calls within a constructor, the extracted
 * code needs to be moved to a method in some top-level object, because the
 * Jribble compiler will not allow any statements to precede the constructor call.
 */
trait NormalizeForJribble
extends Transform
with JavaDefinitions
with JribbleAnalysis
with JribbleNormalization
{
  val global: Global
  import global._
  import global.definitions._
  import global.gen.mkAttributedIdent
  import javaDefinitions._

  protected lazy val scalaPrimitives: global.scalaPrimitives.type = global.scalaPrimitives

  override val phaseName = "normjribble"
  
  override protected def newTransformer(unit: CompilationUnit): Transformer =
    new Trans(unit)

  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

  class Phase(prev: scala.tools.nsc.Phase) extends super.Phase(prev) {
    override def run: Unit = {
      scalaPrimitives.init
      super.run
    }
  }
  
  /**
   * The main transformer of this phase.
   */
  private class Trans(cunit: CompilationUnit) extends Transformer {
    /** A list of statements that need to be inserted at the
     *  nearest enclosing block. */
    var newStats = new ListBuffer[Tree]
    
    /**
     * The nearest enclosing method.
     */
    var currentMethodSym: Symbol = NoSymbol
    
    /**
     * The symbols for LabelDef's that enclose the current scope.
     */
    val labelDefs = mutable.Map.empty[Symbol, List[Symbol]]

    def recordLabelDefDuring[A](label: Symbol, paramNames: List[Symbol])(f: =>A) = {
      labelDefs += label -> paramNames
      val res = f
      labelDefs -= label
      res
    }

    def allocLocal(tpe: Type, pos: scala.tools.nsc.util.Position): Symbol = {
      assert (tpe != UnitClass.tpe) // don't create a unit variable
      assert (tpe != null)
      val newLocal = currentMethodSym.newValue(pos, cunit.fresh.newName())
      newLocal.setInfo(tpe)
      newLocal
    }
    
    /**
     * Transform a tree into a list of statements followed by a new tree.
     * In all of the resulting trees, the only place a non-Jribble expression appears
     * is in a statement context.  If <code>isStat</code> is true,
     * the final tree is considered to be in a statement context.
     */
    def removeNonJribbleExpressions(tree: Tree, isStat: Boolean): (List[Tree], Tree) =
      newStatsAndValue { if (isStat) transformStatement(tree) else transform(tree) }

    /**
     * Transform a list of Scala expressions into a list of Jribble statements 
     * followed by a list of Jribble expressions.  The list of Jribble expressions
     * will be the same length as the original list of trees, and evaluating 
     * one of them will give the same value as the original Scala expression.
     */
    def removeNonJribbleExpressions(trees: List[Tree]): (List[Tree], List[Tree]) =
      newStatsAndValue { transformTrees(trees) }
      
    /**
     * Run an expresison, and return a tuple of the new statements generated by running
     * the expression along with the result of the expression.
     */
    def newStatsAndValue[T](exp: => T): (List[Tree], T) = {
      val savedNewStats = newStats
      newStats = new ListBuffer
      val result = exp
      val treeNewStats = newStats.toList
      newStats = savedNewStats
      (treeNewStats, result)
    }
    
    /**
     * Transform a list of trees, assuming none is in statement
     * position.  Preserve order of evaluation.
     */
    override def transformTrees(trees: List[Tree]): List[Tree] = {
      val transformedWithStats: List[(List[Tree], Tree)] = 
        trees.map(removeNonJribbleExpressions(_, false))
      
      val lastWithStats = transformedWithStats.lastIndexWhere {
        case (stats, exp) => !stats.isEmpty
      }
      if (lastWithStats < 0) {
        // none of the trees needed special treatment
        transformedWithStats.map{case (stats, exp) => exp}
      } else {
        // To preserve order of evaluation, all expressions up through
        // lastWithStats must be put in local variables
        val (toExtract, toLeaveAlone) = transformedWithStats.splitAt(lastWithStats)
        val resultExps = new ListBuffer[Tree]
        for ((stats, exp) <- toExtract) {
          newStats ++= stats
          if (exp.tpe.isInstanceOf[MethodType]) {
            // Don't try to save a method to a val!
            // TODO(spoon): also skip the val for stable, side-effect-free expressions like literals
            resultExps += exp
          } else {
            if (isUnit(exp.tpe)) {
              newStats += exp
              resultExps += unitLiteral
            } else {
              val newLocal = allocLocal(exp.tpe, exp.pos)
              val newValDef = ValDef(newLocal, exp)
              newStats += newValDef
              resultExps += (mkAttributedIdent(newLocal) setType exp.tpe)
            }
          }
        }
        for ((stats, exp) <- toLeaveAlone) {
          newStats ++= stats // the first one in toLeaveAlone does not need
                             // a val, but it can have statements
          resultExps += exp
        }
        resultExps.toList
      }
    }
    
    /**
     * Transform a tree that is known to be used in statement position.
     */
    def transformStatement(tree: Tree): Tree = tree match {
      case If(cond, exp1, exp2) =>
        val condT = transform(cond)
        val exp1T = transformStatement(explicitBlock(exp1))
        val exp2T = transformStatement(explicitBlock(exp2))
        treeCopy.If(tree, condT, exp1T, exp2T)
      case tree@Match(selector, cases) =>
        val casesT = cases map {
          case x@CaseDef(pat, guard, body) =>
            treeCopy.CaseDef(x, pat, guard, transformStatement(explicitBlock(body)))
        }
        treeCopy.Match(tree, selector, casesT)
      case Block(stats, exp) =>
        val newBlockStats = new ListBuffer[Tree]
        for (stat <- stats) {
          val (statNewStats, statT) = removeNonJribbleExpressions(stat, true)
          newBlockStats ++= statNewStats
          if (canBeStatement(statT)) {
            // The only non-statement expressions also have no side
            // effects, so they can safely be discarded
            newBlockStats += statT
          }
        }
        val (expNewStats, expT) = removeNonJribbleExpressions(exp, false)
        newBlockStats ++= expNewStats
        if (canBeStatement(expT))
          newBlockStats += expT
            
        // TODO(spoon): reuse the original tree when possible
        mkBlock(newBlockStats.toList, unitLiteral)
      case tree@ValDef(mods, name, tpt, rhs) if isLocalValDef(tree) && mods.isPrivate =>
        import scala.reflect.generic.Flags._
        val newTree = treeCopy.ValDef(tree, mods &~ PRIVATE, name, tpt, rhs)
        newTree.symbol.flags = newTree.symbol.flags & ~ PRIVATE
        transform(newTree)
      case ValDef(mods, name, tpt, rhs) =>
        treeCopy.ValDef(tree, mods, name, tpt, transform(rhs))
      case Try(block, catches, finalizer) =>
        val blockT = transformStatement(explicitBlock(block))
        val catchesT = catches map (transform(_).asInstanceOf[CaseDef])
        val finalizerT = transformStatement(explicitBlock(finalizer))
        treeCopy.Try(tree, blockT, catchesT, finalizerT)
      case tree =>
        transform(tree)
    }

    /**
     * Transform a tree to a new tree that does not use any non-Jribble expressions
     * in an expression context.  As a side effect, add statements to <code>newStats</code>
     * whose execution should precede that of the returned tree.  The <code>tree</code>
     * passed as an argument is assumed to be in an expression context.
     * 
     * TODO(spoon): alphabetize the cases
     */
    override def transform(tree: Tree): Tree = tree match {
      case tree@CaseDef(pat, guard, body) =>
        CaseDef(pat, guard, transformStatement(explicitBlock(body)))
      case tree@DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        val savedMethodSym = currentMethodSym
        currentMethodSym = tree.symbol
        // TODO(spoon): handle constructors
        val res = treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt,
                                transformStatement(explicitBlockWithReturn(rhs)))
        currentMethodSym = savedMethodSym
        res
      case tree@LabelDef(name, params, rhs) =>
        val paramLocals = params.map(x => allocLocal(x.tpe, x.pos))
        newStats ++= paramLocals map (ValDef)
        recordLabelDefDuring(tree.symbol, paramLocals) {
          if (isUnitOrNothing(rhs)) {
            treeCopy.LabelDef(tree, name, params, transformStatement(explicitBlock(rhs)))
          } else {
            val resultLocal = allocLocal(rhs.tpe, tree.pos)
            newStats += ValDef(resultLocal)
            val newRhs = transformStatement {
              val assign = Assign(mkAttributedIdent(resultLocal), rhs) setType rhs.tpe
              explicitBlock(assign)
            }
            newStats += treeCopy.LabelDef(tree, name, params, newRhs)
            mkAttributedIdent(resultLocal) setPos tree.pos
          }
        }

      case Block(stats, expr) =>
        for (stat <- stats) {
          newStats += transformStatement(stat)
        }
        transform(expr)
      //TODO(grek): Handle translation of if expression into jribble's conditional expression
      case tree@If(cond, exp1, exp2) =>
        val condT = transform(cond)
        if (isUnit(tree.tpe)) {
          newStats += transformStatement(If(condT, exp1, exp2) setType tree.tpe setPos tree.pos)
          unitLiteral
        } else {
          val resV = allocLocal(tree.tpe, tree.pos)
          newStats += ValDef(resV)
          def statForBranch(branch: Tree) =
            if (isNothing(branch)) branch else {
              Assign(mkAttributedIdent(resV), branch) setType branch.tpe  // TODO(spoon): should be type unit?
            }
          newStats += transformStatement(
              If(condT, statForBranch(exp1), statForBranch(exp2)) setType tree.tpe setPos tree.pos)
          mkAttributedIdent(resV)
        }
      case tree@Match(selector, cases) =>
        val (selectorStats, selectorExpr) = removeNonJribbleExpressions(selector, false)
        newStats ++= selectorStats
        val selVal = allocLocal(selectorExpr.tpe, tree.pos)
        newStats += ValDef(selVal, selectorExpr)
        val selValIdent = mkAttributedIdent(selVal)
        if (isUnit(tree.tpe)) {
          newStats += transformStatement(treeCopy.Match(tree, selValIdent, cases))
          unitLiteral
        } else {
          val resV = allocLocal(tree.tpe, tree.pos)
          newStats += ValDef(resV)
          def statForBody(body: Tree) =
            if (isNothing(body)) body else {
              Assign(mkAttributedIdent(resV), body) setType body.tpe  // TODO(grek): should be type unit?
            }
          val casesT = cases map { case x: CaseDef => x.copy(body = statForBody(x.body)) setType x.tpe setPos x.pos }
          newStats += transformStatement(Match(selValIdent, casesT) setType tree.tpe setPos tree.pos)
          mkAttributedIdent(resV)
        }

      case tree@Try(block, catches, finalizer) =>
        if (isUnit(tree.tpe)) {
          newStats += transformStatement(tree)
          unitLiteral
        } else {
          val resV = allocLocal(tree.tpe, tree.pos)
          newStats += ValDef(resV)

          val newBlock =
            if (isUnitOrNothing(block)) block
            else Assign(mkAttributedIdent(resV), block) setType definitions.UnitClass.tpe

          val newCatches =
            for (CaseDef(pat, guard, body) <- catches)
            yield
              if (isUnitOrNothing(body)) CaseDef(pat, guard, body)
              else CaseDef(pat, guard, Assign(mkAttributedIdent(resV), body) setType definitions.UnitClass.tpe)
          newStats += transformStatement(Try(newBlock, newCatches, finalizer) setType tree.tpe)
          mkAttributedIdent(resV)
        }
      case Apply(fun, List(expr)) if (definitions.isBox(fun.symbol)) =>
        transform(box(fun.symbol, expr))
      case Apply(fun, List(expr)) if (definitions.isUnbox(fun.symbol)) =>
        transform(unbox(fun.symbol, expr))
      //rewrite T <: AnyVal in isInstanceOf[T] checks into boxed ones
      case Apply(TypeApply(fun, List(tpe)), Nil)
      //unit is handled by earlier phases
      if isNonUnitValueClass(tpe.symbol) && fun.symbol == definitions.Object_isInstanceOf =>
        val boxedTpe = boxedClass(tpe.symbol).tpe
        Apply(TypeApply(fun, List(TypeTree(boxedTpe))), Nil)
      case tree@Apply(fun: Ident, args) =>
        labelDefs.get(fun.symbol) match {
          case Some(paramLocals) =>
            (paramLocals zip args) foreach {
              case (local, arg) =>
                newStats += transformStatement(Assign(mkAttributedIdent(local), arg) setType arg.tpe)
            }
            // the type of a continue should be Nothing
            newStats += mkApply(fun, args) setType definitions.NothingClass.tpe
            unitLiteral
          case None =>
            cunit.error(tree.pos, "Jump to LabelDef that is not enclosing jump instruction are not supported in jribble.")
            tree
        }
      case tree@Apply(fun @ Select(receiver, name), args) if isEqOnAnyRef(fun) => {
        assert(args.size == 1)
        assert(args.head.tpe <:< definitions.AnyRefClass.tpe)
        atPos(tree.pos) {
          mkApply(treeGen.mkAttributedRef(global.platform.externalEquals), receiver :: args)
        }
      }
      //replace NaN, PosInfinity, NegInfinity float/double values with references to static fields defining those
      //we do that because jribble doesn't supporting those values as literals
      case tree@Literal(Constant(c: Float)) if c.isNaN =>
        Float.mkNaN
      case tree@Literal(Constant(c: Float)) if c.isPosInfinity =>
        Float.mkPosInfinity
      case tree@Literal(Constant(c: Float)) if c.isNegInfinity =>
        Float.mkNegInfinity
      case tree@Literal(Constant(c: Double)) if c.isNaN =>
        Double.mkNaN
      case tree@Literal(Constant(c: Double)) if c.isPosInfinity =>
        Double.mkPosInfinity
      case tree@Literal(Constant(c: Double)) if c.isNegInfinity =>
        Double.mkNegInfinity
      //jribble doesn't support synchronized construct so we just get rid of it
      case Apply(fun @ Select(receiver, name), args) if isSynchronized(fun) =>
        assert(args.tail.isEmpty)
        transform(args.head)
      //type coercion in jribble is achieved through casting like in Java
      case tree@Apply(fun @ Select(receiver, name), args) if isCoercion(fun.symbol) =>
        assert(args.isEmpty)
        treeGen.mkCast(transform(receiver), tree.tpe)
      case Apply(fun, args) =>
        val funT :: argsT = transformTrees(fun :: args)
        treeCopy.Apply(tree, funT, argsT)
      case tree => super.transform(tree)
    }
  }
}
