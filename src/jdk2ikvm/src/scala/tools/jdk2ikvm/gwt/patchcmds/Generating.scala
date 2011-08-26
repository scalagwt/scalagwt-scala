/* jdk2ikvm -- Converts JDK-based Scala source files to use the IKVM library instead.
 * Copyright 2011 Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaNET/
 */

package scala.tools
package jdk2ikvm
package gwt.patchcmds

import scala.tools.nsc.{ast, plugins, symtab, util, Global}
import plugins.Plugin
import scala.collection._
import nsc.util.RangePosition

trait Generating extends Patching { this : Plugin =>

  import global._
  import definitions._

  private val msgPrefix = "["+JDK2IKVMPlugin.PluginName +"] "
  def pluginError(pos: Position, msg: String)   = reporter.error(pos, msgPrefix + msg)
  def warning(pos: Position, msg: String) = reporter.warning(pos, msgPrefix + msg) 
  def info(pos: Position, msg: String)    = reporter.info(pos, msgPrefix + msg, false)

  /* ------------------ utility methods invoked by more than one patch-collector ------------------ */

  private[Generating] class CallsiteUtils(patchtree: PatchTree) {

    /** in case tree is a ClassDef explicitly listing csym in its extends clause, replace that reference to point to newBase */
    def rebaseFromTo(tree: Tree, csym: Symbol, newBase: String) = tree match {
      case cd: ClassDef if (!cd.symbol.isSynthetic) =>
        val Template(parents, self, body) = cd.impl
        val ps = parents filter (p => (p.symbol eq csym))
        for(p <- ps) p match {
          case tt : TypeTree if (tt.original == null) => () // synthetic
          case _ =>
            if(p.pos.isInstanceOf[RangePosition]) {
              val pos = p.pos.asInstanceOf[RangePosition]
              patchtree.replace(pos.start, pos.end - 1, newBase)
            } else {
              warning(p.pos, "couldn't rebase from " + asString(p) + " to " + newBase)
            }
        }
      case _ => ()
    }

    /** whether the idef ClassDef or ModuleDef explicitly lists csym in its extends clause*/
    def explicitlyExtends(idef: ImplDef, csym: Symbol): Boolean = {
      val parentOpt = idef.impl.parents find (p => (p.symbol eq csym))
      parentOpt.isDefined && parentOpt.get.pos.isRange
    }

    /** splices in the body of the idef ClassDef or ModuleDef (adds the whole body if none available) the method definition given by methodDef */
    def addToBody(idef: ImplDef, methodDef: String) {
      // TODO in case more than one addToBody adds a body to the same ClassDef, problems will emerge. A buffer should be kept in PatchTree.  
      idef.impl.body match {
        case List() => spliceAfter(idef, " { " + methodDef + " } ")
        case stats  => spliceAfter(stats.last, methodDef)  
      }
    }

    def addToExtendsClause(idef: ImplDef, extraSuper: String) {
      idef.impl.parents match {
        case List()  => patchtree.splice(idef.impl.pos.start, " extends " + extraSuper + " ")
        case parents => spliceAfter(parents.last, " with " + extraSuper + " ")
      }
    }

    /** inserts the definition given by ikvmDef right after the existing jdkTree (that represents a JDK-based definition). */
    def spliceAfter(jdkTree: Tree, ikvmDefs: String*) {
      jdkTree.pos match {
        case ranPos : RangePosition =>
          // TODO are modifiers also included in the outermost range? (say, when 'inserting before', does before mean before the very first modifier?)
          val col    = scala.math.max(0, jdkTree.pos.focusStart.column - 1) 
          val indent = List.fill(col)(' ').mkString
          val str    = ikvmDefs.toList.map(d => indent + d).mkString("\n")
          patchtree.splice(ranPos.end, "\n" + str) // splice AFTER
        case startPos => () // synthetic  
      }
    }

    def renameOverride(d: ValOrDefDef, txtFrom: String, txtTo: String) {
      if(!d.pos.isInstanceOf[RangePosition]) {
        /* must be a synthetic DefDef */  
        return
      }
      val start = d.pos.point
      val toRepl = patchtree.asString(start, start + txtFrom.length - 1)
      assert(toRepl == txtFrom)
      patchtree.replace(start, start + txtFrom.length - 1, txtTo)
    }

    def rename(d: ValOrDefDef, newName: String) {
      val start = d.pos.point
      patchtree.replace(start, start + d.name.length - 1, newName)
    }

    protected def hasModifier(md: MemberDef, mflag: Long) = md.mods.positions.contains(mflag)

    private def delModifier(md: MemberDef, mflag: Long) {
      md.mods.positions.get(mflag) match {
        case Some(ovrd) => ovrd match {
            case rp : RangePosition => patchtree.replace(rp.start, rp.end + 1, "")
            case _ => warning(md.pos, "couldn't delete '"+scala.reflect.generic.ModifierFlags.flagToString(mflag)+"' modifier: " + asString(md))
          }
        case _ => ()
      }
    }

    def delOverrideModif(dd: DefDef) {
      import scala.reflect.generic.ModifierFlags
      delModifier(dd, ModifierFlags.OVERRIDE)
    }
    
    def delFinalModif(vd: ValDef) {
      import scala.reflect.generic.ModifierFlags
      delModifier(vd, ModifierFlags.FINAL)
    }

    /*  Example:
     *        new CharSequence { . . . }
     *  -->
     *        new java.lang.CharSequence.__Interface { . . . }
     **/
    protected def newNew(tree: Tree, csym: Symbol, newBase: String) {
      tree match {
        case ntree @ New(tpt) if (tpt.tpe.typeSymbol eq csym) =>
          if(tpt.pos.isRange) {
            /* for example, in
            *    new java.lang.ThreadLocal[Integer]
            *  we want to replace just the type name with `newBase' and not the type-arg part. */
            val tptPos = tpt.pos.asInstanceOf[RangePosition]
            val replPos = tpt match {
              case tt: TypeTree if (tt.original != null) =>
                tt.original match {
                  case att @ AppliedTypeTree(atttype, attargs) => atttype.pos
                  case _ => tptPos
                }
              case _ => tptPos
            }
            patchtree.replace(tptPos.start, replPos.end - 1, newBase)
          }
          /* trees without range positions reaching here are usually synthetics for case classes
            (e.g., in the productElemet method). No need to rewrite those.  */
        case _ => ()
      }
    }

  }

  /* ------------ individual patch-collectors ------------ */

  /* ------------------------ The main patcher ------------------------ */

  class RephrasingTraverser(patchtree: PatchTree) extends Traverser {

    override def traverse(tree: Tree): Unit = {
      
      //TODO: call collectPatches on each collector
      
      super.traverse(tree) // "longest patches first" that's why super.traverse after collectPatches(tree).
    }

  }

  /* -----------------------------  Utilities -----------------------------   */

  /** Collect tree nodes having a range position.  */
  class CollectRangedNodes extends Traverser {
    val buf = new collection.mutable.ListBuffer[Tree]
    override def traverse(tree: Tree) = tree match {
      case node =>
        if(node.pos.isRange) { buf += node }
        super.traverse(tree)
    }
    def apply(tree: Tree) = {
      traverse(tree)
      buf.toList
    }
  }

}
