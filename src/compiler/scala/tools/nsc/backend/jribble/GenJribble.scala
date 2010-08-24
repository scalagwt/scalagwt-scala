/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 */

// $Id$

package scala.tools.nsc
package backend.jribble

import scala.collection.{mutable=>mut}
import java.io.{File, FileOutputStream, PrintWriter, IOException}

import symtab.Flags._

import scala.collection.mutable.ListBuffer


/** Generates code in the form of Jribble source.
 *
 *  @author  Nikolay Mihaylov, Lex Spoon
 */
abstract class GenJribble
extends SubComponent
with JribbleAnalysis
with JavaDefinitions
with JribbleFormatting
with JribbleNormalization
{
  val global: Global // TODO(spoon): file a bug report about this.  The declarations
                     // in JribbleFormatting and JribbleAnalysis seem to
                     // be widening this inherited declaration.
  import global._
  import global.scalaPrimitives._
  protected lazy val typeKinds: global.icodes.type = global.icodes
  protected lazy val scalaPrimitives: global.scalaPrimitives.type = global.scalaPrimitives

  val phaseName = "genjribble"

  /** Create a new phase */
  override def newPhase(p: Phase) = new JribblePhase(p)

  /** Jribble code generation phase
   */
  final class JribblePhase(prev: Phase) extends StdPhase(prev) {

    override def run: Unit = {
      scalaPrimitives.init
      super.run
    }

    def apply(unit: CompilationUnit): Unit =
      gen(unit.body, unit)

    var pkgName: String = null

    private def getJribblePrinter(clazz: Symbol, unit: CompilationUnit): JribblePrinter = {
      val file = {
        val suffix = if (clazz.isModuleClass) "$.jribble" else ".jribble"
        getFile(clazz, suffix)
      }
      val out = new PrintWriter(new FileOutputStream(file))
      new JribblePrinter(out, unit)
    }

    private def gen(tree: Tree, unit: CompilationUnit): Unit = tree match {
      case EmptyTree => ()
      case PackageDef(packaged, stats) =>
        val oldPkg = pkgName
        pkgName =
          if (packaged == nme.EMPTY_PACKAGE_NAME)
            null
          else
            tree.symbol.fullName
        stats foreach { gen(_, unit) }
        pkgName = oldPkg
      case tree@ClassDef(mods, name, tparams, impl) => {
        val clazz = tree.symbol
        try {
          {
            // print the main class
            val printer = getJribblePrinter(clazz, unit)
            printer.print(tree)
            printer.close()
          }
          println()
          if (!clazz.isNestedClass && clazz.isModuleClass) {
            // print the mirror class
            // TODO(spoon): only dump a mirror if the same-named class does not already exist
            val printer = getJribblePrinter(clazz.companionSymbol, unit)
            dumpMirrorClass(printer)(clazz)
            printer.close()
          }
          currentRun.symData -= clazz
          currentRun.symData -= clazz.companionSymbol
        } catch {
          case ex: IOException =>
            if (settings.debug.value) ex.printStackTrace()
          error("could not write class " + clazz)
        }
      }
    }

    // TODO(spoon): change this to make a tree and then print the tree with the
    // Jribble Printer.  using raw print's gives bad output and risks giving
    // incorrect output.
    def dumpMirrorClass(printer: JribblePrinter)(clazz: Symbol): Unit = {
      import printer.{print, println, indent, undent}

      print("public final class "); print(jribbleName(clazz.companionSymbol))
      print(" {"); indent; println
      for (m <- clazz.tpe.nonPrivateMembers // TODO(spoon) -- non-private, or public?
           if m.owner != definitions.ObjectClass && !m.hasFlag(PROTECTED) &&
           m.isMethod && !m.hasFlag(CASE) && !m.isConstructor && !m.isStaticMember)
      {
        print("public final static "); print(m.tpe.resultType); print(" ")
        print(m.name); print("(");
        val paramTypes = m.tpe.paramTypes
        for (i <- 0 until paramTypes.length) {
          if (i > 0) print(", ")
          print(paramTypes(i)); print(" x_" + i)
        }
        print(") { ")
        if (!isUnit(m.tpe.resultType))
          print("return ")
        print(jribbleName(clazz)); print("."); print(nme.MODULE_INSTANCE_FIELD)
        print("."); print(jribbleMethodSignature(m)); print("(")
        for (i <- 0 until paramTypes.length) {
          if (i > 0) print(", ");
          print("x_" + i)
        }
        print("); }")
        println
      }
      undent; println; print("}"); println
    }

  }

  private final class JribblePrinter(out: PrintWriter, unit: CompilationUnit) extends TreePrinter(out) {
    /**
     * Symbols in scope that are for a while loop.  Apply's to
     * them should be printed as continue's.
     */
    val labelSyms = mut.Set.empty[Symbol]

    override def printRaw(tree: Tree): Unit = printRaw(tree, false)

    override def print(name: Name) = super.print(name.encode)

    def printStats(stats: List[Tree]) =
    	printSeq(stats) {s => print(s); if (needsSemi(s)) print(";")} {println}


    override def symName(tree: Tree, name: Name): String =
      if (tree.symbol != null && tree.symbol != NoSymbol) {
        ((if (tree.symbol.isMixinConstructor) "/*"+tree.symbol.owner.name+"*/" else "") +
         tree.symbol.simpleName.encode.toString)
      } else name.encode.toString;

    def logIfException[T](tree: Tree)(process: =>T): T =
      try {
        process
      } catch {
      case ex:Error =>
        Console.println("Exception while traversing: " + tree)
        throw ex
      }

    // TODO(spoon): read all cases carefully.
    // TODO(spoon): sort the cases in alphabetical order
    // TODO(spoon): remove the "ret" flag
    def printRaw(tree: Tree, ret: Boolean): Unit =
      logIfException(tree) { tree match {
      case EmptyTree =>

      case ClassDef(mods, name, _, Template(superclass :: ifaces, _, body)) =>
        //printAttributes(tree)
        //printFlags(mods.flags)
        printFlags(tree.symbol)
        print((if (mods hasFlag TRAIT) "interface " else "class ") + jribbleName(tree.symbol))
        print(" extends ")
        print(superclass.tpe)
        if (!ifaces.isEmpty) {
          print(" implements ")
          var first = true
          for (iface <- ifaces) {
            if (!(iface eq ifaces.head))
                print(", ")
            print(iface.tpe)
          }
        }
        print(" {"); indent;
        if (tree.symbol.isModuleClass) {
          println
          val constructorSymbol = definitions.getMember(tree.symbol, nme.CONSTRUCTOR)
          print("public static " + jribbleName(tree.symbol) + " " +
                    nme.MODULE_INSTANCE_FIELD + " = new " + jribbleConstructorSignature(constructorSymbol) + "();")
        }
        for(member <- body) {
          println; println;
          print(member)
        }
        undent; println; print("}"); println

      case ValDef(mods, name, tp, rhs) =>
        //printAttributes(tree)
        //printFlags(mods.flags)
        printFlags(tree.symbol)
        print(tp.tpe)
        print(" ")
        print(tree.symbol.simpleName)
        if (!rhs.isEmpty) { print(" = "); print(rhs) }
        print(";")

      case tree@DefDef(mods, name, tparams, vparamss, tp, rhs) =>
        val resultType = tree.symbol.tpe.resultType

        // TODO(spoon): decide about these two prints; put them in or delete the comments
        //printAttributes(tree)
        //printFlags(mods.flags)
        printFlags(tree.symbol)
        if (isConstructor(tree)) {
          print(jribbleShortName(tree.symbol.owner))
          if (tree.symbol.owner.isModuleClass) print("$")
        } else {
          print(tp.tpe)
          print(" ")
          print(tree.symbol.simpleName)
        }
        vparamss foreach printValueParams
        if (rhs.isEmpty) {
          print(";")
        }
        else {
          print(" ")
          printInBraces(rhs, true)
        }

      case Block(stats, expr) =>
        assert (expr equalsStructure Literal(()))
        print("{"); indent; println
        printStats(stats)
        undent; println; print("}")

      case tree@LabelDef(_, List(), body@Block(bodyStats, bodyExpr)) =>
        assert (bodyExpr equalsStructure Literal(()))
        labelSyms += tree.symbol
        print(tree.symbol.name); print(": while(true) {"); indent; println;
        printStats(bodyStats)
        if (canFallThrough(body)) {
          println; print("break;")
        }
        undent; println; print("}")
        labelSyms -= tree.symbol

      case tree:Apply if labelSyms.contains(tree.symbol) =>
        print("continue "); print(tree.symbol.name)

      case Apply(t @ Select(New(tpt), nme.CONSTRUCTOR), args) if (tpt.tpe.typeSymbol == definitions.ArrayClass) =>
        tpt.tpe match {
          case TypeRef(_, _, List(elemType)) =>
            print("new "); print(elemType)
            print("["); print(args.head); print("]")
        }

      case Apply(fun @ Select(receiver, name), args) if isPrimitive(fun.symbol) => {
        val prim = getPrimitive(fun.symbol)
        prim match {
          case POS | NEG | NOT | ZNOT =>
            print(jribblePrimName(prim)); print("("); print(receiver); print(")")
          case ADD | SUB | MUL | DIV | MOD | OR | XOR | AND | ID |
               LSL | LSR | ASR |EQ | NE | LT | LE | GT | GE | ZOR | ZAND |
               CONCAT =>
            // TODO(spoon): this does not seem to parenthesize for precedence handling
            print(receiver); print(" "); print(jribblePrimName(prim)); print(" "); print(args.head)
          case APPLY => print(receiver); print("["); print(args.head); print("]")
          case UPDATE =>
            print(receiver); print("["); print(args.head); print("] = ")
            print(args.tail.head); print("")
          case SYNCHRONIZED => print("synchronized ("); print(receiver); print(") {")
            indent; println; print(args.head); undent; println; print("}")
          case prim => print("Unhandled primitive ("+prim+") for "+tree)
        }
      }

      case Apply(TypeApply(fun@Select(rcvr, _), List(tpe)), Nil)
      if fun.symbol == definitions.Object_asInstanceOf =>
        //the syntax for casting in jribble is like this: expr.<cast>(type)
        print(rcvr)
        print(".<cast>(")
        print(tpe)
        print(")")

      case Apply(TypeApply(fun@Select(rcvr, _), List(tpe)), Nil)
      if fun.symbol == definitions.Object_isInstanceOf =>
        //the syntax for isInstanceOf check in jribble is like this: expr.<instanceOf>(type)
        print(rcvr)
        print(".<instanceof>(")
        print(tpe)
        print(")")

      case tree@Apply(_, args) if tree.symbol != NoSymbol && tree.symbol.isStaticMember =>
        print(jribbleName(tree.symbol.owner))
        print(".")
        print(jribbleMethodSignature(tree.symbol))
        printParams(args)

      case Apply(fun @ Select(_: New, nme.CONSTRUCTOR), args) if tree.symbol.isConstructor =>
        print("new ");
        print(jribbleConstructorSignature(fun.symbol))
        printParams(args)

      case tree@Apply(Select(_: Super, nme.CONSTRUCTOR), args) if tree.symbol.isConstructor =>
        print(jribbleSuperConstructorSignature(tree.symbol))
        printParams(args)

      case tree@Apply(Select(receiver, _), args) if !tree.symbol.isConstructor =>
        print(receiver)
        print(".")
        print(jribbleMethodSignature(tree.symbol))
        printParams(args)

      case tree@Select(qualifier, selector) if tree.symbol.isModule =>
        printLoadModule(tree.symbol) // TODO(spoon): handle other loadModule cases from GenIcodes

      case This(_) => print("this")

      case If(cond, exp1: Block, exp2) =>
        // If statement
        super.printRaw(tree)

      case If(cond, exp1, exp2) =>
        // If expression
        print("(")
        print(cond)
        print(") ? (")
        print(exp1)
        print(") : (")
        print(exp2)
        print(")")

      case Try(block, catches, finalizer) =>
        print("try ");
        printInBraces(block, ret)
        for (caseClause <- catches)
          caseClause match {
            case CaseDef(exBinding @ Bind(exName, _),
                         _,
                         catchBody) =>
              print(" catch("); print(exBinding.symbol.tpe); print(" ");
              print(exName); print(") ");
              printInBraces(catchBody, ret)

            // TODO(spoon): handle any other patterns that are possible here
          }
        if (finalizer != EmptyTree) {
          print(" finally ")
          indent; print(finalizer); undent; println
          print("}")
        }

      case Throw(expr) =>
        print("throw "); print(expr)

      case tree@TypeTree() =>
        print(tree.tpe)

      case tree: Literal => super.printRaw(tree)
      case tree: Return => super.printRaw(tree)
      case tree: Ident => super.printRaw(tree)
      case tree: Assign => super.printRaw(tree)
      case tree: Select => super.printRaw(tree)

      case x => unit.error(x.pos, "Unhandled tree type " + x.getClass)
      } }

    // TODO(spoon): drop the explicit "ret" and "return" from this file;
    // earlier normalization should make returns explicit
    def printInBraces(exp: Tree, ret: Boolean) {
      exp match {
        case _:Block => printRaw(exp, ret);
        case _ =>
          assert(false, exp.toString)
          // TODO(spoon): try to eliminate this case through earlier normalization
          print("{")
          indent; println
          if (ret && isReturnable(exp)) {
            print("return ")
            print(exp)
          } else {
            printRaw(exp, ret)
          }
          if (needsSemi(exp))
            print(";")
          undent; println
          print("}");
      }
    }

    /** load a top-level module */
    def printLoadModule(sym: Symbol) {
      print(jribbleName(sym)); print("$."); print(nme.MODULE_INSTANCE_FIELD)
    }

    def printParams(xs: List[Tree]): Unit = {
      print("(")
      for (x <- xs) {
        if (x != xs.head)
          print(", ")
        print(x)
      }
      print(")")
    }

    override def printParam(tree: Tree): Unit = tree match {
      case ValDef(mods, name, tp, rhs) =>
        //printAttributes(tree)
        print(tp.tpe); print(" "); print(symName(tree, name))
    }

    def printFlags(sym: Symbol): Unit = {
      val flags = sym.flags
      val fs = new ListBuffer[String]
      def printFlag(f: Long, s: String) =
        if ((f & flags) != 0)
          fs += s
      if ((flags & (PRIVATE | PROTECTED)) == 0 && sym.owner.isClass)
        fs += "public"
      else {
        printFlag(PRIVATE, "private")
        printFlag(PROTECTED, "protected")
      }
      printFlag(STATIC, "static")
      printFlag(FINAL, "final")
      if ((flags & (DEFERRED | ABSTRACT)) != 0)
        fs += "abstract"
      val flagstr = fs.mkString("", " ", "")
      if (flagstr.length != 0) { print(flagstr); print(" ")  }
    }

    def print(tpe: Type) {
      print(jribbleName(tpe))
    }

    def needsSemi(exp: Tree): Boolean = exp match {
      case _:ValDef => false
      case _:DefDef=> false
      case _:LabelDef => false
      case _:Template => false
      case _:Block => false
      case _:ArrayValue => true
      case _:Assign=> true
      case _:If => false
      case _:Match => false
      case _:Return => true
      case _:Try => false
      case _:Throw => true
      case _:New => true
      case _:TypeApply => true
      case _:Apply => true
      case _:Super => true
      case _:This => true
      case _:Select => true
      case _:Ident => true
      case _:Literal => true
    }
  }
}
