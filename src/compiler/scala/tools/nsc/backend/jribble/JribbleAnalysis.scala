/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$
package scala.tools.nsc.backend.jribble

import scala.tools.nsc._
import scala.tools.nsc.symtab.SymbolTable
import scala.tools.nsc.symtab.Flags._

import scala.collection.mutable


/**
 * Analyses that are used by {@link GenJribble}.
 */
trait JribbleAnalysis {
  val global: Global
  import global._
  
  /**
   * Return whether the argument, which must be a valid Scala expression, can
   * be used as a Jribble statement.
   */
  def canBeStatement(exp: Tree): Boolean =
    exp match {
      case EmptyTree => false
      case _:Super => false
      case _:This => false
      case _:Select => false
      case _:Ident => false
      case _:Literal => false
      case _ => true
    }
  
  def isConstructor(defDef: DefDef) = defDef.name == nme.CONSTRUCTOR
    
  def isNothing(tpe: Type): Boolean =
    (tpe != null) && (tpe =:= definitions.NothingClass.tpe)
  
  def isNothing(tree: Tree): Boolean = isNothing(tree.tpe)
  
  def isUnit(tpe: Type): Boolean =
    (tpe != null) && (tpe =:= definitions.UnitClass.tpe)
  
  def isUnit(tree: Tree): Boolean = isUnit(tree.tpe)
  
  def isUnitOrNothing(tree: Tree) = isUnit(tree) || isNothing(tree)

  def typeReturnable(tpe: Type) = !isUnit(tpe) && !isNothing(tpe)

  def isUnitLiteral(tree: Tree): Boolean =
    tree match {
      case Literal(Constant(())) => true
      case _ => false
    }

  def isEqOnAnyRef(fun: Select) = {
    import scalaPrimitives._
    val Select(receiver, _) = fun
    if (isPrimitive(fun.symbol)) {
      (getPrimitive(fun.symbol) == EQ) && (receiver.tpe <:< definitions.AnyRefClass.tpe)
    } else false
  }

  def isSynchronized(fun: Select) = {
    import scalaPrimitives._
    isPrimitive(fun.symbol) && (getPrimitive(fun.symbol) == SYNCHRONIZED)
  }

  def isLocalValDef(tree: ValDef) = !tree.symbol.owner.isClass

  def isCoercion(sym: Symbol): Boolean = {
    import scalaPrimitives._
    isPrimitive(sym) && scalaPrimitives.isCoercion(getPrimitive(sym))
  }

  
  def isInterface(sym: Symbol): Boolean = sym.hasFlag(INTERFACE)
 
  //copied from GenJVM
  def isStaticModule(sym: Symbol): Boolean = {
    import scala.reflect.generic.Flags
    sym.isModuleClass && !sym.isImplClass && !sym.hasFlag(Flags.LIFTED)
  }

  //copied from GenJVM
  def isTopLevelModule(sym: Symbol): Boolean =
    atPhase (currentRun.picklerPhase.next) {
      sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
    }
}
