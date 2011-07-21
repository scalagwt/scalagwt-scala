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
import symtab.SymbolTable
import scala.collection.mutable

/**
 * Analyses that are used by {@link GenJribble}.
 */
trait JribbleAnalysis {
  val global: Global
  import global._
  
  private lazy val remoteClass = definitions.getClass("scala.remote")
  private lazy val remoteExceptionClass = definitions.getClass("java.rmi.RemoteException")
  
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
  
  /**
   * Return whether the given expression can ever complete and fall through
   * to an expression that follows it in a block.
   */
  def canFallThrough(exp: Tree) = !isNothing(exp.tpe)

  /**
   * Analyze an expression and return the classes of the exceptions
   * it can throw.
   * 
   * TODO(spoon): this is currently very sloppy.  It needs to deal with
   * catch expressions, and it needs to combine expressions that overlap,
   * and it needs to deal with declared Jribble exceptions.
   */
  def exceptionsThrown(exp: Tree): List[Symbol] = {
    val exceptions = mutable.Set.empty[Symbol]
    for (e <- exp)
      e match {
        case Throw(exc) => 	
          val sym = exc.tpe.typeSymbol
          if (sym != NoSymbol && sym.isClass)
            exceptions += sym
          
        case Apply(fun, args) =>
          if (fun.symbol.hasAnnotation(remoteClass))
            exceptions += remoteExceptionClass
          
        case _ =>
      }
    
    return exceptions.toList
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

  def isReturnable(tree: Tree): Boolean = {
    def treeTypeReturnable(tree: Tree) = tree match {
      case _:Try => false
      case _:Block => false
      case _:Return => false
      case _ => true
    }
    return treeTypeReturnable(tree) && typeReturnable(tree.tpe)
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
}
