/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.nsc
package backend.jribble
import scala.collection.mutable
import scala.tools.nsc.transform.{Transform, TypingTransformers}

/**
 * Forward jumps to LabelDefs cannot be expressed in jribble so rewrite methods containing them into stubs.
 *
 * This transformation has two phases (executed for each Template):
 * 1. It scans using FindLabelForwardJumpsTraverser the tree to find list of all symbols corresponding to
 *    LabelDefs that we found a forward jump to. This phase doesn't collect symbols for LabelDefs that
 *    have jump instructions only in scope of LabelDefs. Those can be translated to while loop with label and
 *    jumps can be translated to continue <label>; instructions.
 * 2. Set of symbols from 1. is mapped to Set of symbols correspodning to methods enclosing LabelDefs.
 * 3. For each method from 2. we rewrite it's body to call sys.error(...) method (effectively: throw an exception).
 *
 * TODO(grek): This is really a temporary work-around until we find proper solution to deal with forward jumps.
 * It turns out that forward jumps are not used that often in real Scala programs so we can get non-trivial, Scala
 * programs running without dealing with (complex) issue of forward jumps.
 */
trait RemoveForwardJumps extends Transform with TypingTransformers with JribbleNormalization {
  val global: Global
  import global._

  override val phaseName = "fjumpsjribble"

  override protected def newTransformer(unit: CompilationUnit): Transformer =
    new Trans(unit)

  /**
   * The main transformer of this phase.
   */
  private class Trans(cunit: CompilationUnit) extends TypingTransformer(cunit) {

    var methodsToBeRewritten = Set.empty[Symbol]

    var currentTemplateSym: Symbol = NoSymbol

    override def transformTemplate(tree: Template): Template = {
      currentTemplateSym = tree.symbol
      val findForwardJumps = new FindLabelForwardJumpsTraverser
      findForwardJumps.traverse(tree)
      val forwardJumps = findForwardJumps.forwardJumps
      def enclMethod(s: Symbol): Symbol = if (s.isMethod & !s.isLabel) s else enclMethod(s.owner)
      methodsToBeRewritten = forwardJumps.map(enclMethod)
      methodsToBeRewritten foreach { s =>
        cunit.warning(s.pos, "Forward jump detected, we'll be rewritten to sys.error(...).")
      }
      super.transformTemplate(tree)
    }

    override def transform(tree: Tree): Tree = tree match {
      case tree@DefDef(mods, name, tparams, vparamss, tpt, _) if methodsToBeRewritten contains tree.symbol =>
        val errorCall = localTyper typed gen.mkSysErrorCall("whack! you stepped on broken handling of jribble backend.")
        treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, errorCall)
      case x => super.transform(x)
    }

    class FindLabelForwardJumpsTraverser extends Traverser {
      /**
       * The symbols for LabelDef's that enclose the current scope.
       */
      val labelDefs = mutable.Set.empty[Symbol]
      private val _forwardJumps = mutable.Set.empty[Symbol]
      override def traverse(tree: Tree) {
        tree match {
          case x: LabelDef =>
            labelDefs += x.symbol
            super.traverse(x)
            labelDefs -= x.symbol
          case x@Apply(ident: Ident, _) =>
            if (!(labelDefs contains ident.symbol)) _forwardJumps += ident.symbol
            super.traverse(x)
          case x => super.traverse(x)
        }
      }
      def forwardJumps: Set[Symbol] = _forwardJumps.toSet
    }

  }
}
