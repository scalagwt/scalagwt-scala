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

  private val keywords = Set("public", "final", "abstract", "class", "interface",
                              "extends", "implements", "static", "super", "this",
                              "new", "false", "true", "if", "else", "instanceof",
                              "cast", "private", "try", "catch", "finally", "while",
                              "continue", "break", "switch", "default", "return",
                              "protected", "throw")

  /** Return the suffix of a class name */
  //copied from GenJVM
  protected def moduleSuffix(sym: Symbol) = {
    import scala.reflect.generic.Flags
    if (sym.hasFlag(Flags.MODULE) && !sym.isMethod &&
       !sym.isImplClass && !sym.hasFlag(Flags.JAVA)) "$"
    else "";
  }

  protected def escapeKeyword(s: String): String = if (keywords contains s) "`"+s+"`" else s

  protected def jribbleName(sym: Symbol, fullyQualify: Boolean, suffix: Symbol => String): String = {
    //copy of AbsSymbol.fullName adapted to jribble syntax for fully qualified names which is
    // com/foo/Bar
    def fullName(sym: Symbol): String = {
      val separator = '/'
      if (sym.isRoot || sym.isRootPackage || sym == scala.reflect.NoSymbol) escapeKeyword(sym.toString)
      else if (sym.owner.isEffectiveRoot) escapeKeyword(sym.encodedName)
      else fullName(sym.owner.enclClass) + separator + escapeKeyword(sym.encodedName)
    }

    // TODO(spoon): why the special cases?  double check that they are needed
    if (sym == definitions.NothingClass)
      return "Lscala/runtime/Nothing$;"
    else if (sym == definitions.NullClass)
      return "Lscala/runtime/Null$;"
    else if (isJribblePrimitive(sym.tpe))
      return jribbleName(sym.tpe)

    if (fullyQualify) "L" + fullName(sym) + suffix(sym) + ";" else escapeKeyword(sym.simpleName.toString)
  }

  protected def jribbleShortName(sym: Symbol): String =
    jribbleName(sym, false, moduleSuffix)

  protected def jribbleName(sym: Symbol): String =
    jribbleName(sym, true, moduleSuffix)

  protected def jribbleName(tpe: Type): String = {
    def tpstr(typ: TypeKind): String =
      typ match {
        case UNIT => "V;" // TODO(spoon): depends on context?  a Scala variable can be of type unit!
        case BOOL            => "Z;"
        case BYTE            => "B;"
        case SHORT           => "S;"
        case CHAR            => "C;"
        case INT             => "I;"
        case LONG            => "J;"
        case FLOAT           => "F;"
        case DOUBLE          => "D;"
        case REFERENCE(cls)  => jribbleName(cls)
        case ARRAY(elem)     => tpstr(elem) + "["
      }
    return tpstr(toTypeKind(tpe))
  }

  protected def jribbleModuleMirrorName(sym: Symbol): String =
    jribbleName(sym, true, _ => "")

  protected def jribbleMethodSignature(s: Symbol): String = {
    val paramsTypes = s.tpe.paramTypes.map(jribbleName)
    val on = jribbleName(s.owner)
    val name = escapeKeyword(s.name.encode.toString)
    val returnType = jribbleName(s.tpe.resultType)
    "(" + on + "::" + name + (paramsTypes).mkString("(", "", ")") + returnType + ")"
  }

  protected def jribbleSuperConstructorSignature(s: Symbol): String = {
    val paramsTypes = s.tpe.paramTypes.map(jribbleName)
    val on = jribbleName(s.owner)
    val name = "super"
    val returnType = "V;"
    "(" + on + "::" + name + (paramsTypes).mkString("(", "", ")") + returnType + ")"
  }

  protected def jribbleConstructorSignature(s: Symbol): String = {
    val paramsTypes = s.tpe.paramTypes.map(jribbleName)
    val on = jribbleName(s.owner)
    val name = "this"
    val returnType = "V;"
    "(" + on + "::" + name + (paramsTypes).mkString("(", "", ")") + returnType + ")"
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
	  case LENGTH => ".<length>"               // x.length
	  case APPLY  => ""                        // x(y)
	  case UPDATE => ""                        // x(y) => ""z

	  // String operations
	  case CONCAT => "+"                       // String.valueOf(x)+String.valueOf(y)
    }
  }
}
