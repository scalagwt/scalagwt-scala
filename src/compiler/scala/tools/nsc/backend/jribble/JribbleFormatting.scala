/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$
package scala.tools.nsc
package backend
package jribble
import symtab.SymbolTable
import backend.icode.TypeKinds

/**
 * Utilities for formatting Scala constructs in Jribble syntax.
 *
 *  @author  Lex Spoon
 */
trait JribbleFormatting {
  val global: SymbolTable { def abort(msg: String): Nothing }
  import global._
  protected val typeKinds: TypeKinds {
    val global: JribbleFormatting.this.global.type
  }
  import typeKinds._
  protected val scalaPrimitives: ScalaPrimitives {
    val global: JribbleFormatting.this.global.type
  }

  private def nameSuffix(sym: Symbol) = {
    import symtab.Flags._
    if (sym.isModuleClass && !sym.isTrait && !sym.hasFlag(JAVA)) "$" else ""
  }

  protected def jribbleName(sym: Symbol, fullyQualify: Boolean): String = {
    //copy of AbsSymbol.fullName adapted to jribble syntax for fully qualified names which is
    // com/foo/Bar
    def fullName = {
      val separator = '/'
      if (sym.isRoot || sym.isRootPackage || sym == scala.reflect.NoSymbol) sym.toString
      else if (sym.owner.isEmptyPackageClass) sym.encodedName
      else sym.owner.enclClass.fullName(separator) + separator + sym.encodedName
    }

    // TODO(spoon): why the special cases?  double check that they are needed
    if (sym == definitions.NothingClass)
      return "scala.runtime.Nothing$"
    else if (sym == definitions.NullClass)
      return "scala.runtime.Null$"
    else if (isJribblePrimitive(sym.tpe))
      return jribbleName(sym.tpe)

    if (fullyQualify) "L" + fullName + nameSuffix(sym) + ";" else sym.simpleName.toString
  }

  protected def jribbleShortName(sym: Symbol): String =
    jribbleName(sym, false)

  protected def jribbleName(sym: Symbol): String =
    jribbleName(sym, true)

  protected def jribbleName(tpe: Type): String = {
    def tpstr(typ: TypeKind): String =
      typ match {
        case UNIT => "V" // TODO(spoon): depends on context?  a Scala variable can be of type unit!
        case BOOL            => "Z"
        case BYTE            => "B"
        case SHORT           => "S"
        case CHAR            => "C"
        case INT             => "I"
        case LONG            => "L"
        case FLOAT           => "F"
        case DOUBLE          => "D"
        case REFERENCE(cls)  => jribbleName(cls)
        case ARRAY(elem)     => tpstr(elem) + "["
      }
    return tpstr(toTypeKind(tpe))
  }

  protected def jribbleMethodSignature(s: Symbol): String = {
    val paramsTypes = s.tpe.paramTypes.map(_.typeSymbol).map(jribbleName)
    val on = jribbleName(s.owner)
    val name = s.name.encode.toString
    val returnType = jribbleName(s.tpe.resultType.typeSymbol)
    "(" + on + "::" + name + (paramsTypes).mkString("(", "", ")") + returnType + ")"
  }

  protected def jribbleSuperConstructorSignature(s: Symbol): String = {
    val paramsTypes = s.tpe.paramTypes.map(_.typeSymbol).map(jribbleName)
    val on = jribbleName(s.owner)
    val name = "super"
    val returnType = "V"
    "(" + on + "::" + name + (paramsTypes).mkString("(", "", ")") + returnType + ")"
  }

  protected def jribbleConstructorSignature(s: Symbol): String = {
    val paramsTypes = s.tpe.paramTypes.map(_.typeSymbol).map(jribbleName)
    val on = jribbleName(s.owner)
    val name = jribbleShortName(s.owner)
    val returnType = "V"
    "(" + on + "::" + name + nameSuffix(s.owner) + (paramsTypes).mkString("(", "", ")") + returnType + ")"
  }

  private def isJribblePrimitive(tpe: Type): Boolean = typeKinds.primitiveTypeMap.values.map(_.toType).exists(_ =:= tpe)

  protected def jribblePrimName(prim: Int): String = {
    import scalaPrimitives._

    (prim : @unchecked) match {
      // Arithmetic unary operations
	  case POS => "+"                            // +x
	  case NEG => "-"                           // -x
	  case NOT => "~"                           // ~x

	  // Arithmetic binary operations
	  case ADD => "+"                          // x + y
	  case SUB => "-"                           // x - y
	  case MUL => "*"                           // x * y
	  case DIV => "/"                           // x / y
	  case MOD => "%"                           // x % y

	  // Bitwise operations
	  case OR  => "|"                           // x | y
	  case XOR => "^"                           // x ^ y
	  case AND => "&"                           // x & y

	  // Shift operations
	  case LSL => "<<"                           // x << y
	  case LSR => ">>"                           // x >>> y
	  case ASR => ">>>"                           // x >> y

	  // Comparison operations
	  case ID => "=="                            // x eq y
	  case NI => "!="                            // x ne y
	  case EQ => "=="                            // x == y
	  case NE => "!="                            // x != y
	  case LT => "<"                            // x < y
	  case LE => "<="                            // x <= y
	  case GE => ">="                            // x > y
	  case GT => ">"                            // x >= y

	  // Boolean unary operations
	  case ZNOT => "!"                          // !x

	  // Boolean binary operations
	  case ZOR => "||"                           // x || y
	  case ZAND => "&&"                          // x && y

	  // Array operations
	  case LENGTH => ""                        // x.length
	  case APPLY  => ""                        // x(y)
	  case UPDATE => ""                        // x(y) => ""z

	  // String operations
	  case CONCAT => "+"                       // String.valueOf(x)+String.valueOf(y)
    }
  }
}
