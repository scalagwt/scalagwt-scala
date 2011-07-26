/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.nsc.backend.jribble
import scala.tools.nsc.symtab.SymbolTable

/**
 * Standard symbols that are used only by the Java backends, along with
 * convenience methods for generating trees using them.
 */
trait JavaDefinitions {
  val global: SymbolTable
  import global._
  
  object javaDefinitions {
    import definitions.{getMember, getModule}
    lazy val BoxesRuntimeModule = getModule("scala.runtime.BoxesRunTime")
    
    /**
     * A map from symbols of primitive types to the static method
     * used to box them.
     */
    lazy val javaBoxMethod: Map[Symbol, Symbol] = {
      val boxNames =
        (Map.empty[Symbol, String]
           + (definitions.BooleanClass -> "boxToBoolean")
           + (definitions.ByteClass -> "boxToByte")
           + (definitions.CharClass -> "boxToCharacter")
           + (definitions.DoubleClass -> "boxToDouble")
           + (definitions.FloatClass -> "boxToFloat")
           + (definitions.IntClass -> "boxToInteger")
           + (definitions.LongClass -> "boxToLong")
           + (definitions.ShortClass -> "boxToShort"))
      (Map.empty[Symbol, Symbol] /: boxNames.keys) ((map,prim) =>
        map + (prim -> getMember(BoxesRuntimeModule, boxNames(prim))))
    }
    
    /**
     * A map from symbols of primitive types to the static method
     * used to unbox them.
     */
    lazy val javaUnboxMethod: Map[Symbol, Symbol] = {
      val unboxNames =
        (Map.empty[Symbol, String]
           + (definitions.BooleanClass -> "unboxToBoolean")
           + (definitions.ByteClass -> "unboxToByte")
           + (definitions.CharClass -> "unboxToChar")
           + (definitions.DoubleClass -> "unboxToDouble")
           + (definitions.FloatClass -> "unboxToFloat")
           + (definitions.IntClass -> "unboxToInt")
           + (definitions.LongClass -> "unboxToLong")
           + (definitions.ShortClass -> "unboxToShort"))
      (Map.empty[Symbol, Symbol] /: unboxNames.keys) ((map,prim) =>
        map + (prim -> getMember(BoxesRuntimeModule, unboxNames(prim))))
    }
    
    /** Helper symbols for referring static fields of java.lang.Float class */
    object Float {
      private val sym = definitions.BoxedFloatClass.companionSymbol 
      val NaN         = getMember(sym, "NaN")
      val PosInfinity = getMember(sym, "POSITIVE_INFINITY")
      val NegInfinity = getMember(sym, "NEGATIVE_INFINITY")
    }
    
    /** Helper symbols for referring static fields of java.lang.Double class */
    object Double {
      private val sym = definitions.BoxedDoubleClass.companionSymbol 
      val NaN         = getMember(sym, "NaN")
      val PosInfinity = getMember(sym, "POSITIVE_INFINITY")
      val NegInfinity = getMember(sym, "NEGATIVE_INFINITY")
    }
  }
}
