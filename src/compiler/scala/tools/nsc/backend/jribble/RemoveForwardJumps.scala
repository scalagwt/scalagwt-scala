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
import scala.tools.nsc.transform.Transform

/**
 * Forward jumps to LabelDefs cannot be expressed in jribble so they need to be transformed into method calls.
 *
 * This transformation has two phases (executed for each Template):
 * 1. It scans using FindLabelForwardJumpsTraverser the tree to find list of all symbols corresponding to
 *    LabelDefs that we found a forward jump to. This phase doesn't collect symbols for LabelDefs that
 *    have jump instructions only in scope of LabelDefs. Those can be translated to while loop with label and
 *    jumps can be translated to continue <label>; instructions.
 * 2. For all LabelDefs that have forward jumps we lift them to top level, final, private methods and we put
 *    a call to that method in place of LabelDef. All jumps to that label are being translated to method calls too.
 *
 * The reason why we don't translate all jumps to method calls is that they are less efficient that while loops
 * and continue instructions. This is especially important for tailcall optimizations that would be broken
 * (essentially inverted) if we would translate all jumps into method calls.
 *
 * Our hope is that forward jumps are only emitted by pattern matching logic which won't be hurt by this transformation
 * because we don't expect deep recursive calls.
 */
trait RemoveForwardJumps extends Transform with JribbleNormalization {
  val global: Global
  import global._

  override val phaseName = "fjumpsjribble"

  override protected def newTransformer(unit: CompilationUnit): Transformer =
    new Trans(unit)

  /**
   * The main transformer of this phase.
   */
  private class Trans(cunit: CompilationUnit) extends Transformer {

    var forwardJumps = Set.empty[Symbol]

    val methodsForLabels = mutable.Map.empty[Symbol, DefDef]

    var currentTemplateSym: Symbol = NoSymbol

    override def transformTemplate(tree: Template): Template = {
      currentTemplateSym = tree.symbol
      val savedForwardJumps = forwardJumps
      val traverser = new FindLabelForwardJumpsTraverser
      traverser.traverse(tree)
      forwardJumps = traverser.forwardJumps
      val newTree@Template(parents, self, body) = super.transformTemplate(tree)
      val newTemplate = treeCopy.Template(newTree, parents, self, body ++ methodsForLabels.values)
      methodsForLabels.clear()
      newTemplate
    }

    override def transform(tree: Tree): Tree = tree match {
      case tree@LabelDef(name, params, rhs) if (forwardJumps contains tree.symbol) =>
        import scala.reflect.generic.Flags._
        val valParams = params.map(x => ValDef(x.symbol))
        val methodSymbol = currentTemplateSym.owner.newMethod(cunit.fresh.newName(tree.pos, name + "%"))
        methodSymbol.setFlag(PRIVATE | FINAL | SYNTHETIC)
        methodSymbol.setInfo(tree.symbol.tpe)
        methodSymbol.initialize
        val newRhs = transform(rhs)
        //TODO we should supply privateWithin
        val methodTree = mkDefDef(methodSymbol, Modifiers(methodSymbol.flags), valParams, newRhs)
        val methodSelect = treeGen.mkAttributedRef(currentTemplateSym.owner.tpe, methodTree.symbol)
        methodsForLabels += tree.symbol -> methodTree
        mkApply(methodSelect, params)
      case tree@Apply(fun: Ident, args) if (forwardJumps contains fun.symbol) =>
        methodsForLabels.get(fun.symbol) match {
          case Some(methodTree) =>
            val methodSelect = treeGen.mkAttributedRef(currentTemplateSym.owner.tpe, methodTree.symbol)
            mkApply(methodSelect, args)  
          case None =>
            Predef.error("methodsForLabel is broken")
        }
      case tree => super.transform(tree)
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
