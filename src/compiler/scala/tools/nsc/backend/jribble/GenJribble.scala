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

    private def getJribblePrinter(clazz: Symbol, unit: CompilationUnit, suffix: String): JribblePrinter = {
      val file = {
        val fileSuffix = suffix + ".jribble"
        getFile(clazz, fileSuffix)
      }
      import java.io.OutputStreamWriter
      //Scala assumes Unicode (BMP) as an input, we should produce Unicode as an output too
      val out = new PrintWriter(new OutputStreamWriter(new FileOutputStream(file), "UTF-8"))
      new JribblePrinter(clazz, out, unit)
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
            val printer = getJribblePrinter(clazz, unit, moduleSuffix(clazz))
            printer.print(tree)
            printer.close()
          }
          if (isStaticModule(clazz) && isTopLevelModule(clazz) && clazz.companionClass == NoSymbol) {
            // print the mirror class
            // TODO(spoon): only dump a mirror if the same-named class does not already exist
            val printer = getJribblePrinter(clazz, unit, "")
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
      
      print("public final class "); print(jribbleModuleMirrorName(clazz))
      print(" {"); indent; println
      addForwarders(printer)(clazz)
      undent; println; print("}"); println
    }

  }

  /**
   * Adds static forwarders for methods defined in modules (objects).
   *
   * Copied from GenJVM.
   */
  def addForwarders(printer: JribblePrinter)(module: Symbol): Unit = {
    def conflictsIn(cls: Symbol, name: Name) =
        cls.info.members exists (_.name == name)

    /** List of parents shared by both class and module, so we don't add forwarders
     *  for methods defined there - bug #1804 */
    lazy val commonParents = {
      val cps = module.info.baseClasses
      val mps = module.companionClass.info.baseClasses
      cps.filter(mps contains)
    }
    /* The setter doesn't show up in members so we inspect the name
    * ... and clearly it helps to know how the name is encoded, see ticket #3004.
    * This logic is grossly inadequate! Name mangling needs a devotee.
    */
    def conflictsInCommonParent(name: Name) =
      commonParents exists { cp =>
        (name startsWith (cp.name + "$")) || (name containsName ("$" + cp.name + "$"))
      }

    /** Should method `m' get a forwarder in the mirror class? */
    def shouldForward(m: Symbol): Boolean =
      atPhase(currentRun.picklerPhase) (
        m.owner != definitions.ObjectClass
          && m.isMethod
          && !m.hasFlag(CASE | PRIVATE | PROTECTED | DEFERRED | SPECIALIZED)
          && !m.isConstructor
          && !m.isStaticMember
          && !(m.owner == definitions.AnyClass)
          && !module.isSubClass(module.companionClass)
          && !conflictsIn(definitions.ObjectClass, m.name)
          && !conflictsInCommonParent(m.name)
          && !conflictsIn(module.companionClass, m.name)
      )

    assert(module.isModuleClass)
    if (settings.debug.value)
      log("Dumping mirror class for object: " + module);

    for (m <- module.info.nonPrivateMembers; if shouldForward(m)) {
      log("Adding static forwarder '%s' to '%s'".format(m, module))
      addForwarder(printer)(module, m)
    }
  }

  //TODO(grek): Using raw output can lead to bad output. It would be probably
  //a better idea to generate a Tree and use JribblePrinter to print it.
  //Another idea would be generating jribble ast once we switch to it from
  //printing text representation of jribble.
  def addForwarder(printer: JribblePrinter)(module: Symbol, m: Symbol): Unit = {
    import printer.{print, println}
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
    print(jribbleName(module)); print("."); print(nme.MODULE_INSTANCE_FIELD)
    print("."); print(jribbleMethodSignature(m)); print("(")
    for (i <- 0 until paramTypes.length) {
      if (i > 0) print(", ");
      print("x_" + i)
    }
    print("); }")
    println
  }

  /**
   * clazz stores symbol of a class this printer is printing
   */
  private final class JribblePrinter(clazz: Symbol, out: PrintWriter, unit: CompilationUnit) extends TreePrinter(out) {
    /**
     * Symbols in scope that are for a while loop.  Apply's to
     * them should be printed as continue's.
     */
    val labelSyms = mut.Set.empty[Symbol]

    override def printRaw(tree: Tree): Unit = printRaw(tree, false)
    
    override def print(name: Name) = print(escapeKeyword(name.encode.toString))

    def printStats(stats: List[Tree]) =
    	printSeq(stats) {s => print(s); if (needsSemi(s)) print(";")} {println}

    
    override def symName(tree: Tree, name: Name): String = escapeKeyword {
      if (tree.symbol != null && tree.symbol != NoSymbol) {
        ((if (tree.symbol.isMixinConstructor) "/*"+tree.symbol.owner.name+"*/" else "") +
         tree.symbol.simpleName.encode.toString)
      } else name.encode.toString;
    }
    
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
        
      case ClassDef(mods, name, _, Template(parents, _, body)) =>
        //printAttributes(tree)
        //printFlags(mods.flags)
        val (superclass, ifaces) = parents match {
          case Nil => (None, Nil)
          case x :: xs => (Some(x), xs)
        }
        printFlags(tree.symbol)
        print((if (isInterface(tree.symbol)) "interface " else "class ") + jribbleName(tree.symbol))

        def printSep(types: List[Type], sep: String = ", "): Unit = types match {
          case Nil => ()
          case x :: Nil => print(x)
          case x :: xs => print(x); print(sep); printSep(xs, sep)
        }
        if (isInterface(tree.symbol)) {
          val interfaceParents = parents.map(_.tpe).filterNot(_ == definitions.ObjectClass.tpe)
          if (!interfaceParents.isEmpty) {
            print(" extends ")
            printSep(interfaceParents)
          }
        } else {
          superclass foreach { x =>
            print(" extends ")
            print(x.tpe)
          }
          if (!ifaces.isEmpty) {
            print(" implements ")
            printSep(ifaces.map(_.tpe))
          }
        }
        print(" {"); indent;
        //TODO(grek): Check if this works correctly
        if (isStaticModule(tree.symbol)) {
          println
          val constructorSymbol = definitions.getMember(tree.symbol, nme.CONSTRUCTOR)
          print("public static " + jribbleName(tree.symbol) + " " +
                    nme.MODULE_INSTANCE_FIELD + " = new " + jribbleConstructorSignature(constructorSymbol) + "();")
        } else {
          //logic that adds static forwarders. Copied from GenJVM (see genClass method)
          val lmoc = tree.symbol.companionModule
          // it must be a top level class (name contains no $s)
          def isCandidateForForwarders(sym: Symbol): Boolean =
            atPhase (currentRun.picklerPhase.next) {
              !(sym.name.toString contains '$') && (sym hasFlag MODULE) && !sym.isImplClass && !sym.isNestedClass
            }

          // add static forwarders if there are no name conflicts; see bugs #363 and #1735
          if (lmoc != NoSymbol && !tree.symbol.hasFlag(INTERFACE)) {
            if (isCandidateForForwarders(lmoc) && !settings.noForwarders.value) {
              println
              log("Adding forwarders to existing class '%s' found in module '%s'".format(tree.symbol, lmoc))
              addForwarders(this)(lmoc.moduleClass)
            }
          }
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
          print("this")
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

      case tree@LabelDef(_, _, body@Block(bodyStats, bodyExpr)) =>
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
        
      case Apply(t @ Select(New(tpt), nme.CONSTRUCTOR), args) if (tpt.tpe.typeSymbol == definitions.ArrayClass) => {
        def extractDims(tpe: Type): (Int, Type) = if (tpe.typeSymbol == definitions.ArrayClass) {
          tpe match {
            case TypeRef(_, _, List(elemType)) => {
              val (dims, tpe) = extractDims(elemType)
              (dims+1, tpe)
            }
          }
        } else (0, tpe)
        val (dims, elemType) = extractDims(tpt.tpe)
        assert(args.size <= dims)
        print("new "); print(elemType);
        (args.map(Some(_)) ::: List.fill(dims-args.size)(None)) foreach {
          case Some(arg) => print("["); print(arg); print("]")
          case None => print("[]")
        }
      }

      case Apply(fun @ Select(receiver, name), args) if isPrimitive(fun.symbol) => {
        val prim = getPrimitive(fun.symbol) 
        prim match {
          case POS | NEG | NOT | ZNOT =>
            print(jribblePrimName(prim)); print("("); print(receiver); print(")") 
          case ADD | SUB | MUL | DIV | MOD | OR | XOR | AND | ID | NI |
               LSL | LSR | ASR |EQ | NE | LT | LE | GT | GE | ZOR | ZAND |
               CONCAT =>
            // TODO(spoon): this does not seem to parenthesize for precedence handling
            //TODO(grek): printing parenthesis in every case not matter if needed or not
            print("(");
            print(receiver); print(" "); print(jribblePrimName(prim)); print(" "); print(args.head);
            print(")");
          case APPLY => print(receiver); print("["); print(args.head); print("]")
          case UPDATE =>
            print(receiver); print("["); print(args.head); print("] = ") 
            print(args.tail.head); print("")
          case LENGTH => print(receiver); print(jribblePrimName(prim));
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
        //TODO(grek): printing parenthesis in every case not matter if needed or not
        print("(")
        print("new ");
        print(jribbleConstructorSignature(fun.symbol))
        printParams(args)
        print(")")

      case tree@Apply(Select(_: Super, nme.CONSTRUCTOR), args) if tree.symbol.isConstructor =>
        print(jribbleSuperConstructorSignature(tree.symbol))
        printParams(args)

      //trigerred by call to auxiliary constructor
      case tree@Apply(Select(receiver, nme.CONSTRUCTOR), args) if tree.symbol.isConstructor =>
        print(jribbleConstructorSignature(tree.symbol))
        printParams(args)

      case tree@Apply(Select(receiver, _), args) if !tree.symbol.isConstructor =>
        print(receiver)
        print(".")
        print(jribbleMethodSignature(tree.symbol))
        printParams(args)
        
      case tree@Select(qualifier, selector) if tree.symbol.isModule =>
        printLoadModule(tree.symbol)

      //copied from GenICode, this might refer to ModuleClass (including java class if static field
      //is being accessed) so we have to check if symbol is ModuleClass and if it's not equal to
      //enclosing class. If so we just treat it as a qualifier for static reference so we just
      //print symbol's name
      case tree@This(_) if tree.symbol.isModuleClass && tree.symbol != clazz =>
        printLoadModule(tree.symbol)

      case This(_) => print("this")

      case Super(_, _) => print("super")
      
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
        //TODO(grek): Make sure that such name won't cause any troubles (clashing, etc)
        val WILDCARD_VAR_NAME = "$WILDCARD"
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
            case CaseDef(Typed(Ident(nme.WILDCARD), tpt), _, catchBody) =>
              print(" catch("); print(tpt.tpe); print(" ");
              print(WILDCARD_VAR_NAME); print(") ");
              printInBraces(catchBody, ret)
            case CaseDef(Ident(nme.WILDCARD), _, catchBody) =>
              print(" catch("); print(definitions.ThrowableClass.tpe); print(" ");
              print(WILDCARD_VAR_NAME); print(") ");
              printInBraces(catchBody, ret)

            //TODO(grek): figure out if we can translated cases in above way without messing up
            //logic behind catching exceptions
          }
        if (finalizer != EmptyTree) {
          print(" finally ")
          printInBraces(finalizer, ret);
        }

      case tree@Match(selector, cases) => {
        assert(cases forall { case CaseDef(_, guard, _) => guard == EmptyTree })
        val ident: Ident = selector match {
          //we can safely drop Typed wrapper here because we rely on coercion (for numerical types) in jribble
          case Typed(expr: Ident, tpt) if (tpt.tpe =:= definitions.IntClass.tpe) => expr
          case expr: Ident if (expr.tpe =:= definitions.IntClass.tpe) => expr
          case x => Predef.error("Unrecognized selector in Match node " + x)
        }
        val switches: List[(List[Int], Tree)] = cases flatMap {
          case CaseDef(Literal(Constant(x: Int)), _, body) => (x :: Nil, body) :: Nil
          case tree@CaseDef(Alternative(xs), _, body) =>
            //TODO(grek): debugging line as I failed to find a way to trigger Alternative from scala source code
            unit.warning(tree.pos, "Got alternative for CaseDef! Report your source code to jribble maintainers.")
            val constants = xs map { case Literal(Constant(x: Int)) => x }
            (constants, body) :: Nil
          case CaseDef(Ident(nme.WILDCARD), _, _) => Nil //this is handled separately
        }
        val default: Option[Tree] = {
          val trees = cases collect { case CaseDef(Ident(nme.WILDCARD), _, body) => body }
          assert(trees.size <= 1)
          trees.headOption
        }
        def printBody(body: Tree) = body match {
          case Block(stats, expr) if (expr equalsStructure Literal(())) =>
            print(" {");
            indent;
            println;
            printStats(stats)
            println; print("break;");
            undent; println;
            print("}"); println;
          case x => Predef.error("Unrecognized body in Match node " + x)
        }
        print("switch ("); print(ident); print(")");
        print(" {"); indent;
        switches foreach {
          case (constants, body) =>
            constants foreach { x => println; print(x.toString); print(":"); }
            printBody(body)
        }
        default foreach { body =>
          println; print("default:");
          printBody(body)
        }
        undent; println; print("}");
      }
      
      case Throw(expr) => 
        print("throw "); print(expr)
        
      case tree@TypeTree() =>
        print(tree.tpe)

      case tree@Apply(_: Ident, _) => unit.error(tree.pos, "Jumping to labels not handled by jribble")
      case Literal(Constant(x: Long)) =>
        print(x.toString); print("L")
      case Literal(Constant(x: Double)) =>
        print(x.toString); print("D")
      case Literal(Constant(x: Float)) =>
        print(x.toString); print("F")
      //handles classOf[Foo] expressions
      case Literal(c@Constant(x: Type)) if c.tag == ClassTag =>
        print(jribbleName(x)); print(".class")
      case tree: Literal => super.printRaw(tree)
      case tree: Ident if !tree.symbol.isPackage && tree.symbol.isModule =>
        printLoadModule(tree.symbol)
      case Return(expr) =>
        print("return");
        val unitLiteral = Literal(Constant())
        //in jribble unboxed unit is being returned by omitting return value altogether
        if (!(expr equalsStructure unitLiteral)) { print(" "); print(expr) }
      case tree: Ident => super.printRaw(tree)
      case tree: Assign => super.printRaw(tree)
      //access static field defined in java class
      case tree@Select(qualifier, name) if tree.symbol.isStaticMember =>
        print(jribbleName(tree.symbol.owner));
        print(".");
        print(symName(tree, name));
      case tree@Select(qualifier, name) =>
        print(qualifier);
        print(".("); print(jribbleName(tree.symbol.owner)); print(")");
        print(symName(tree, name));
      case tree: Apply => super.printRaw(tree)
      case ArrayValue(elemtpt, trees) =>
        print("<"); print(elemtpt); printRow(trees, ">{", ", ", "}")
      //TODO(grek): It looks that it's safe to just drop Typed but this should be double checked
      case Typed(expr, _) => print(expr)
        
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
      print(jribbleName(sym));
      if (!sym.isJavaDefined) {
        print("."); print(nme.MODULE_INSTANCE_FIELD)
      }
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

    override def printValueParams(ts: List[ValDef]) {
      print("(")
      printSeq(ts){printParam}{print(", ")}
      print(")")
    }

    //TODO(grek): Revisit this method and check it against GenJVM.javaFlags method
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
        //TODO(grek): Check comment in GenJVM's javaFlags method which says that
        //protected modifier shouldn't be emitted
        printFlag(PROTECTED, "protected")
      }
      if (sym.isStaticMember)
        fs += "static"
      if (!sym.enclClass.hasFlag(INTERFACE) && !sym.isClassConstructor)
        printFlag(FINAL, "final")
      if ((flags & (DEFERRED | ABSTRACT)) != 0 && !sym.enclClass.hasFlag(INTERFACE))
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

  def isInterface(sym: Symbol): Boolean = sym.hasFlag(INTERFACE)
}
