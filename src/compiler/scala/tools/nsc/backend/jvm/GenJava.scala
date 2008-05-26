/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 */

// $Id$

package scala.tools.nsc.backend.jvm

import java.io.{File, FileOutputStream, PrintWriter, IOException}

import symtab.Flags._

import scala.collection.mutable.ListBuffer


/** Generates code in the form of Java source
 *
 *  @author  Nikolay Mihaylov, Lex Spoon
 */
abstract class GenJava 
extends SubComponent 
with JavaSourceAnalysis 
with JavaSourceFormatting 
with JavaSourceNormalization
{
  val global: Global // TODO(spoon): file a bug report about this.  The declarations
                     // in JavaSourceFormatting and JavaSourceAnalysis seem to
                     // be widening this inherited declaration.
  import global._
  import global.scalaPrimitives._
  protected val typeKinds: global.icodes.type = global.icodes
  protected val scalaPrimitives: global.scalaPrimitives.type = global.scalaPrimitives
  protected def typed(tree: Tree): Tree = global.typer.typed(tree)
  
  val phaseName = "genjavasrc"

  /** Create a new phase */
  override def newPhase(p: Phase) = new JavaPhase(p)

  /** JVM code generation phase
   */
  final class JavaPhase(prev: Phase) extends StdPhase(prev) {

    override def run: Unit = {
      scalaPrimitives.init
      super.run
    }

    def apply(unit: CompilationUnit): Unit =
      gen(unit.body)

    var pkgName: String = null

    private def getJavaPrinter(clazz: Symbol): JavaPrinter = {
      val file = {
        val suffix = if (clazz.isModuleClass) "$.java" else ".java"
        getFile(clazz, suffix)
      }
      val out = new PrintWriter(new FileOutputStream(file))
      new JavaPrinter(out)
    }
      
    private def gen(tree: Tree): Unit = tree match {
      case EmptyTree => ()
      case PackageDef(packaged, stats) =>
        val oldPkg = pkgName
        pkgName =
          if (packaged == nme.EMPTY_PACKAGE_NAME)
            null
          else
            tree.symbol.fullNameString
        stats foreach gen
        pkgName = oldPkg
      case tree@ClassDef(mods, name, tparams, impl) => {
        val clazz = tree.symbol
        try {
          {
            // print the main class
            val printer = getJavaPrinter(clazz)
            if (pkgName != null) {
              printer.print("package ")
              printer.print(pkgName)
              printer.print(";")
              printer.println
            }
            printer.print(tree)
            printer.close()
          }
          println()
          if (!clazz.isNestedClass && clazz.isModuleClass) {
            // print the mirror class
            // TODO(spoon): only dump a mirror if the same-named class does not already exist
            val printer = getJavaPrinter(clazz.linkedSym)
            dumpMirrorClass(printer)(clazz)
            printer.close()
          }
          currentRun.symData -= clazz
          currentRun.symData -= clazz.linkedSym
        } catch {
          case ex: IOException =>
            if (settings.debug.value) ex.printStackTrace()
          error("could not write class " + clazz)
        }
      }
    }

    def isStaticSymbol(s: Symbol): Boolean =
      s.hasFlag(STATIC) || s.hasFlag(STATICMEMBER) || s.owner.isImplClass 

    def dumpMirrorClass(printer: JavaPrinter)(clazz: Symbol): Unit = {
      import printer.{print, println, indent, undent}
      
      if (pkgName != null) {
        print("package "); print(pkgName); print(";"); println
      }
      print("public final class "); print(javaShortName(clazz.linkedSym)) 
      print("{"); indent; println
      for (val m <- clazz.tpe.nonPrivateMembers; // TODO(spoon) -- non-private, or public?
           m.owner != definitions.ObjectClass && !m.hasFlag(PROTECTED) &&
           m.isMethod && !m.hasFlag(CASE) && !m.isConstructor && !isStaticSymbol(m) )
      {
        print("public final static "); print(m.tpe.resultType); print(" ") 
        print(m.name); print("(");
        val paramTypes = m.tpe.paramTypes
        for (val i <- 0 until paramTypes.length) {
          if (i > 0) print(", ") 
          print(paramTypes(i)); print(" x_" + i)
        }
        print(") { ")
        if (!isUnit(m.tpe.resultType))
          print("return ") 
        print(javaName(clazz)); print("."); print(nme.MODULE_INSTANCE_FIELD)
        print("."); print(m.name); print("(") 
        for (val i <- 0 until paramTypes.length) {
          if (i > 0) print(", ");
          print("x_" + i)
        }
        print("); }") 
        println
      }
      undent; println; print("}"); println
    }

  }

  private final class JavaPrinter(out: PrintWriter) extends treePrinters.TreePrinter(out) {

    override def printRaw(tree: Tree): Unit = printRaw(tree, false)
    
    override def print(name: Name) = super.print(name.encode)

    def printStats(stats: List[Tree]) =
    	printSeq(stats) {s => print(s); if (needsSemi(s)) print(";")} {println}

    
    override def symName(tree: Tree, name: Name): String =
      if (tree.symbol != null && tree.symbol != NoSymbol) {
        ((if (tree.symbol.isMixinConstructor) "/*"+tree.symbol.owner.name+"*/" else "") +
         tree.symbol.simpleName.encode.toString)
      } else name.encode.toString;

    // TODO(spoon): give this a comment.  It has a weird name. I don't understand print vs. printRaw.
    // TODO(spoon): read all cases carefully.
    // TODO(spoon): sort the cases in the order they are listed in Trees
    def printRaw(tree: Tree, ret: Boolean): Unit = tree match {
      case EmptyTree =>  
        
      case ClassDef(mods, name, _, Template(superclass :: ifaces, _, body)) =>
        //printAttributes(tree)
        //printFlags(mods.flags)
        printFlags(tree.symbol)
        print((if (mods hasFlag TRAIT) "interface " else "class ") + javaShortName(tree.symbol))
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
          print("public static " + javaShortName(tree.symbol) + " " + 
                    nme.MODULE_INSTANCE_FIELD + " = new " + javaShortName(tree.symbol) + "();")
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

      case DefDef(mods, name, tparams, vparamss, tp, rhs0) =>
        val isCons = name == nme.CONSTRUCTOR
        val resultType = tree.symbol.tpe.resultType
        val rhs = if (isCons) rhs0 else hideExceptions(rhs0)  // TODO(spoon) handle constructors
        
        //printAttributes(tree)
        //printFlags(mods.flags)
        printFlags(tree.symbol)
        if (isCons) print(javaShortName(tree.symbol.owner))
        else {
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
        print("{");indent; println
        printStats(stats)
        println
        if (isUnitLiteral(expr)) {
          if (ret)
            print("return;")
        } else {
          if (ret && isReturnable(expr))
            print("return ");
          print(expr)
          if (needsSemi(expr)) print(";")
        }
        undent; println; print("}")

      case lab@LabelDef(_, List(), If(cond,
                                      Block(body, app@Apply(_, List())),
                                      Literal(Constant(()))))
      => // TODO(spoon) put the real test back in
//      if (lname1.startsWith("while$") && lname1 == lname2) =>
                 // if (lab.symbol.name.toString.startsWith("while$") && lab.symbol==app.symbol) =>
        print("while ("); print(cond); print(") {") 
        indent; println
        printStats(body);
        undent; println; print("}") 

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
            print(javaPrimName(prim)); print("("); print(receiver); print(")") 
          case ADD | SUB | MUL | DIV | MOD | OR | XOR | AND | ID |
               LSL | LSR | ASR |EQ | NE | LT | LE | GT | GE | ZOR | ZAND |
               CONCAT =>
            // TODO(spoon): this does not seem to parenthesize for precedence handling
            print(receiver); print(" "); print(javaPrimName(prim)); print(" "); print(args.head)
          case APPLY => print(receiver); print("["); print(args.head); print("]")
          case UPDATE =>
            print(receiver); print("["); print(args.head); print("] = ") 
            print(args.tail.head); print("")
          case SYNCHRONIZED => print("synchronized ("); print(receiver); print(") {") 
            indent; println; print(args.head); undent; println; print("}") 
          case prim => print("Unhandled primitive ("+prim+") for "+tree)
        }
      }
        
      case tree@Apply(fun, args) if genicode.isStaticSymbol(tree.symbol) =>
        print(javaName(tree.symbol.owner))
        print(".")
        print(tree.symbol.name)
        print("(")
        for	(arg <- args) {
          if (arg != args.head)
            print(", ")
          print(arg)
        }
        print(")")
        
      case tree@Select(qualifier, selector) if tree.symbol.isModule =>
        printLoadModule(tree.symbol) // TODO(spoon): handle other loadModule cases from GenIcodes
        
      case This(_) => print("this")

      case Super(_, _) | Select(Super(_, _), nme.CONSTRUCTOR) => print("super")
      
      
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
        // TODO(spoon): this impl assumes the throw is at the statement level
        print("scala.runtime.JavaSourceMisc.hiddenThrow(")
        print(expr)
        print("); ")
        print("throw new RuntimeException(\"not reached\")")

      case _ => super.printRaw(tree)
    }

    def printInBraces(exp: Tree, ret: Boolean) {
      exp match {
        case _:Block => printRaw(exp, ret);
        case _ => 
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
      print(javaName(sym)); print("$."); print(nme.MODULE_INSTANCE_FIELD)
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
    
    def print(tpe: Type): Unit = {
      print(javaName(tpe))
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


// TODO(spoon): add in missing cases
//         case ValDef(mods, name, tpt, rhs) => ;
//         case DefDef(mods, name, tparams, vparamss, tpt, rhs) => ;
//         case LabelDef(name, params, rhs) => ;
//         case Template(parents, body) => ;
//         case Block(stats, expr) => ;
//         case ArrayValue(elemtpt, trees) => ; //                              (introduced by uncurry)
//         case Assign(lhs, rhs) => ;
//         case If(cond, thenp, elsep) => ;
//         case Match(selector, cases) => ;
//         case Return(expr) => ;
//         case Try(block, catches, finalizer) => ;
//         case Throw(expr) => ;
//         case New(tpt) => ;
//         //case TypeApply(fun, args) => ;
//         case Apply(fun, args) => ;
//         case Super(qual, mix) => ;
//         case This(qual) => ;
//         case Select(qualifier, selector) => ;
//         case Ident(name) => ;
//         case Literal(value) => ;
//         case TypeTree() => ;