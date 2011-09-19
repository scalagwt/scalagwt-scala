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

  def rangePosition(pos: Position): Option[RangePosition] =
    if (pos.isInstanceOf[RangePosition]) Some(pos.asInstanceOf[RangePosition])
    else None


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
    
    def removeFromExtendsClause(idef: ImplDef, symbols: Symbol*) {
      val ss = Set(symbols: _*)
      /*  Filter out parents that do not have RangePosition. Roughly speaking, there are two cases for parent to not have RangePosition:
       *   
       *  a) it's been added implicitly by compiler (e.g. scala.ScalaObject is being added implicitly everywhere)
       *  b) we've stumbled upon compiler bug in positions logic
       * 
       * It's really hard to distinguish those two cases so we assume only a) here and keep fingers crossed. In case of
       * b) the rest of logic that follows will get wrong positions and will corrupt the output. It's not that bad
       * because corrupted output will be easily detected once patched source will be tried to compile.
       */ 
      val parents = idef.impl.parents.filter(_.pos.isInstanceOf[RangePosition])
      if (!parents.isEmpty) {
        //create a sequence of Symbols and positions attached to them
        //those positions are different from RangePositions and include things like keywords.
        //E.g. if we have `A with B with C` the sequence will look like this:
        //(A.symbol, (0, 1)), (B.symbol, (2, 8)), (C.symbol, (9, 15))
        val symbolsAndRanges: Seq[(Symbol, (Int, Int))] = parents.map(_.symbol) zip {
          val ranges = parents.map(_.pos.asInstanceOf[RangePosition])
          val ends = ranges.map(_.end)
          (ranges.head.start -> ranges.head.end) :: (ends.map(_+1) zip ends.tail)
        }
        symbolsAndRanges foreach {
          case (s, (start, end)) if ss contains s => patchtree.replace(start, end, "")
          case _ => ()
        }
      }
    }
    
    def removeTemplate(idef: ImplDef) {
      //annotation info is accessible only through symbol
      idef.symbol.annotations foreach removeAnnotation
      val range = idef.pos.asInstanceOf[RangePosition]
      patchtree.replace(range.start, range.end, "")
    }
    
    def removeAnnotation(x: AnnotationInfo) {
      val range = x.pos.asInstanceOf[RangePosition]
      //stat-1 because range doesn't include position for @ character
      patchtree.replace(range.start-1, range.end, "")
    }
    
    def removeDefDef(x: DefDef) {
      //annotation info is accessible only through symbol
      x.symbol.annotations foreach removeAnnotation
      val rangeOpt = rangePosition(x.pos)
      (rangeOpt
          map { range => patchtree.replace(range.start, range.end, "") }
          getOrElse warning(x.pos, "Unable to remove def " + x.name))

    }
    
    def removeValDef(x: ValDef) {
      //annotation info is accessible only through symbol
      x.symbol.annotations foreach removeAnnotation
      val rangeOpt = rangePosition(x.pos)
      (rangeOpt
          map { range => patchtree.replace(range.start, range.end, "") }
          getOrElse warning(x.pos, "Unable to remove val " + x.name))
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
    
    protected def methodRefersTo(m: Symbol)(p: Symbol => Boolean): Boolean = {
      def methodType(m: MethodType) = {
        val types = m.resultType :: m.params.map(_.tpe)
        //we need typeSymbolDirect instead of typeSymbol in order to not resolve type aliases
        val typeSymbols = types.map(_.typeSymbolDirect)
        typeSymbols.exists(p)
      }
      m.tpe match {
        case m: MethodType => methodType(m)
        case PolyType(_, result: MethodType) => methodType(result)
        case _ => false
      }
    }
    
    protected def enclosingClass(s: Symbol)(t: Tree)(f: PartialFunction[Tree, Unit]): Unit =
      if ((t.symbol != null) && (t.symbol.enclClass == s) && f.isDefinedAt(t)) f(t) else ()
      
    protected def within(s: Symbol)(t: Tree)(f: PartialFunction[Tree, Unit]): Unit =
      if ((t.symbol != null) && (t.symbol != NoSymbol) && (t.symbol.owner == s) && f.isDefinedAt(t)) f(t) else ()

  }

  /* ------------ individual patch-collectors ------------ */

  /** Removes par, parCombiner defs and parent traits mixed in to support parallel collections */
  private[Generating] class RemoveParallelCollections(patchtree: PatchTree) extends CallsiteUtils(patchtree) {
    
    private lazy val ParallelizableClass = definitions.getClass("scala.collection.Parallelizable")
    
    private lazy val CustomParallelizableClass = definitions.getClass("scala.collection.CustomParallelizable")
    
    private lazy val ParallelPackage = definitions.getModule("scala.collection.parallel")
    
    private val parallDefNames = Set("par", "parCombiner") map (newTermName) 
    
    private def enclTransPackage(sym: Symbol, encl: Symbol): Boolean =
      if (sym == NoSymbol) false
      else sym == encl || enclTransPackage(sym.enclosingPackage, encl)

    def collectPatches(tree: Tree) {
      tree match {
        case idef: ImplDef =>
          removeFromExtendsClause(idef, ParallelizableClass, CustomParallelizableClass)
          
        case imp: Import if enclTransPackage(imp.expr.symbol, ParallelPackage) =>
          val range = imp.pos.asInstanceOf[RangePosition]
          patchtree.replace(range.start, range.end, "")

        //TODO(grek): Improve accuracy of a condition by checking type arguments and return type
        case x: DefDef if parallDefNames contains x.name =>
          removeDefDef(x)

        case _ => ()
      }
    }

  }
  
  /** Removes any definitions related to serialization from scala collections classes */
  private[Generating] class RemoveSerializationSupport(patchtree: PatchTree) extends CallsiteUtils(patchtree) {
    private val proxy: Name = newTypeName("SerializationProxy")
    
    private val writeReplace = newTermName("writeReplace")
    
    private val serializableClass = definitions.getClass("scala.Serializable")
    
    private def proxyForRemoval(s: Symbol) = s.name == proxy
    
    private object WriteObjectMethod extends (Symbol => Boolean) {
      val name = newTermName("writeObject")
      val paramSymbol = definitions.getClass("java.io.ObjectOutputStream")
      
      def apply(s: Symbol): Boolean = s.name == name && (s.tpe match {
        case MethodType(param :: Nil, _) =>
          param.tpe.typeSymbol == paramSymbol
        case _ => false
      })
    }
    
    private object ReadObjectMethod extends (Symbol => Boolean) {
      val name = newTermName("readObject")
      val paramSymbol = definitions.getClass("java.io.ObjectInputStream")
      
      def apply(s: Symbol): Boolean = s.name == name && (s.tpe match {
        case MethodType(param :: Nil, _) =>
          param.tpe.typeSymbol == paramSymbol
        case _ => false
      })
    }
    
    def collectPatches(tree: Tree) {
      tree match {
        case x: ImplDef if proxyForRemoval(x.symbol) =>
          removeTemplate(x)
        //TODO(grek): Find out if we should remove scala.Serializable from parents
//        case x: ImplDef =>
//          removeFromExtendsClause(x, serializableClass)
        case x: DefDef if x.name == writeReplace =>
          removeDefDef(x)
        case x: DefDef if ReadObjectMethod(x.symbol) || WriteObjectMethod(x.symbol) =>
          removeDefDef(x)
        case _ => ()
      }
    }
  }
  
  /** Removes def that depend on VM, like exit, runtime, env, etc. */
  private[Generating] class CleanupSysPackageObject(patchtree: PatchTree) extends CallsiteUtils(patchtree) {
    
    //not sure why definitions.getPackageObjectClass doesn't work
    private val pkgObjectClass = definitions.getPackageObject("scala.sys").moduleClass
    
    private val defNames = Set("exit", "props", "runtime", "env", "allThreads", "addShutdownHook") map (newTermName)
    
    def collectPatches(tree: Tree) = enclosingClass(pkgObjectClass)(tree) {
      case x: DefDef if defNames contains x.name =>
        removeDefDef(x)
    }
    
  }
  
  private[Generating] class CleanupPredef(patchtree: PatchTree) extends CallsiteUtils(patchtree) {

    private val moduleClass = definitions.PredefModuleClass
    
    private val defNames = Set("exit") map (newTermName)
    
    def collectPatches(tree: Tree) = enclosingClass(moduleClass)(tree) {
      case x: DefDef if defNames contains x.name =>
        removeDefDef(x)
      //probably a call to Console.read*
      case x: DefDef if x.name startsWith "read" =>
        removeDefDef(x)
      case x: DefDef if x.name.toString == "refArrayOps" =>
        val range = rangePosition(x.pos).get
        patchtree.replace(range.start, range.end, "implicit def refArrayOps[T <: AnyRef: ClassManifest](xs: Array[T]): ArrayOps[T] = new ArrayOps.ofRef[T](xs)\n")
    }
    
  }
  
  /** Removes mirror val def that depends on part of reflect package that we excluded */
  private[Generating] class CleanupReflectPackageObject(patchtree: PatchTree) extends CallsiteUtils(patchtree) {

    private val pkgObjectClass = definitions.getPackageObject("scala.reflect").moduleClass
    
    //surprisingly enough ValDef have a trailing space in it's name and it is *not* a bug
    private val valNames = Set("mirror") map (_+" ") map (newTermName)
    
    def collectPatches(tree: Tree) = enclosingClass(pkgObjectClass)(tree) {
      case x: ValDef if valNames contains x.name =>
        removeValDef(x)
    }
    
  }
  
  /** Removes RoundingMode object that depends on Enumeration */
  private[Generating] class CleanupBigDecimal(patchtree: PatchTree) extends CallsiteUtils(patchtree) {
    
    private val bigDecimalModule = definitions.getModule("scala.math.BigDecimal")
    
    private val roundingModeModule = definitions.getMember(bigDecimalModule, "RoundingMode")
    
    private def withinRoundingMode(s: Symbol): Boolean =
      s.hasTransOwner(roundingModeModule.moduleClass)
    
    def collectPatches(tree: Tree) {
      tree match {
        case x: ModuleDef if x.symbol == roundingModeModule =>
          removeTemplate(x)
        case x: Import if x.expr.symbol == roundingModeModule =>
          val range = x.pos.asInstanceOf[RangePosition]
          patchtree.replace(range.start, range.end, "")
        case x: DefDef if methodRefersTo(x.symbol)(withinRoundingMode) =>
          removeDefDef(x)
        case _ => ()
      }
    }
    
  }

  private [Generating] class CleanupConsole(patchtree: PatchTree) extends CallsiteUtils(patchtree) {
    private val consoleModuleClass = definitions.getModule("scala.Console").moduleClass

    private val defNames = Set("in", "setIn", "withIn", "flush", "textComponents")

    private def shouldRemove(x: TermName): Boolean =
      ((x startsWith "read") || (defNames contains x.toString))

    def collectPatches(tree: Tree) {
      enclosingClass(consoleModuleClass)(tree) {
        case x: DefDef if shouldRemove(x.name) =>
          removeDefDef(x)
        case x: ValDef if x.name.toString == "inVar " =>
          removeValDef(x)
      }
    }
  }
  
  private[Generating] class RemoveNoStackTraceParent(patchtree: PatchTree) extends CallsiteUtils(patchtree) {
    
    private lazy val NoStackTraceClass = definitions.getClass("scala.util.control.NoStackTrace")

    def collectPatches(tree: Tree) {
      tree match {
        case idef: ImplDef =>
          removeFromExtendsClause(idef, NoStackTraceClass)
        case _ => ()
      }
    }

  }
  
  /** Removes references to I/O related classes from xml package */
  private[Generating] class CleanupXmlPackage(patchtree: PatchTree) extends CallsiteUtils(patchtree) {
    
    private lazy val XmlFactoryPackage = definitions.getModule("scala.xml.factory")
    private lazy val XmlParsingPackage = definitions.getModule("scala.xml.parsing")
    
    private val NoBindingFactoryAdapterName = newTermName("NoBindingFactoryAdapter")
    private val FactoryAdapterName = newTermName("FactoryAdapter")
    
    private lazy val XmlLoaderClass = definitions.getClass("scala.xml.factory.XMLLoader")
    private val XmlLoaderName = newTermName("XMLLoader")
    
    private lazy val XmlObjectClass = definitions.getModule("scala.xml.XML").moduleClass
    
    private lazy val XmlSource = definitions.getModule("scala.xml.Source")
    
    private val defNames = Set("withSAXParser", "saveFull", "save", "write") map (newTermName)

    def collectPatches(tree: Tree) {
      tree match {
        case idef: ImplDef if idef.symbol == XmlSource =>
          removeTemplate(idef)
        case x: Import if x.expr.symbol == XmlSource =>
          val range = x.pos.asInstanceOf[RangePosition]
          patchtree.replace(range.start, range.end, "")
        case idef: ImplDef =>
          removeFromExtendsClause(idef, XmlLoaderClass)
        case x: Import if x.expr.symbol == XmlFactoryPackage && x.selectors.exists(_.name == XmlLoaderName) =>
          val range = x.pos.asInstanceOf[RangePosition]
          patchtree.replace(range.start, range.end, "")
        //this probably could be removed upstream as it seems to be unused import
        case x: Import if x.expr.symbol == XmlParsingPackage && x.selectors.exists(_.name == NoBindingFactoryAdapterName) =>
          val range = x.pos.asInstanceOf[RangePosition]
          patchtree.replace(range.start, range.end, "")
        //this probably could be removed upstream as it seems to be unused import
        case x: Import if x.expr.symbol == XmlParsingPackage && x.selectors.exists(_.name == FactoryAdapterName) =>
          val range = x.pos.asInstanceOf[RangePosition]
          patchtree.replace(range.start, range.end, "")
        case _ => ()
      }
      enclosingClass(XmlObjectClass)(tree) {
        case x: DefDef if (defNames contains x.name) =>
          removeDefDef(x)
      }
    }

  }
  
  /** Removes imports of stuff from java.io that is not supported */
  private[Generating] class RemoveBadJavaImports(patchtree: PatchTree) extends CallsiteUtils(patchtree) {
    
    private lazy val JavaIoPackage = definitions.getModule("java.io")
    private val JavaIoNames = Set("BufferedReader", "Reader", "InputStream", "InputStreamReader",
        "File", "FileDescriptor", "FileInputStream", "FileOutputStream",
        "ObjectOutputStream", "ObjectInputStream", "StringReader", "Writer") map (newTermName)
    private lazy val JavaIoClasses = JavaIoNames map (x => definitions.getClass(JavaIoPackage.fullName + "." + x.toString))

    private lazy val JavaTextPackage = definitions.getModule("java.text")
    private lazy val JavaNioPackage = definitions.getModule("java.nio")
    
    private def enclTransPackage(sym: Symbol, encl: Symbol): Boolean =
      if (sym == NoSymbol) false
      else sym == encl || enclTransPackage(sym.enclosingPackage, encl)

    private def containsIoClass(x: Import): Boolean =
      (x.expr.symbol == JavaIoPackage) && (x.selectors exists (x => JavaIoNames contains x.name))

    private def removeImport(x: Import): Boolean =
      (enclTransPackage(x.expr.symbol, JavaNioPackage) || enclTransPackage(x.expr.symbol, JavaTextPackage))

    private def filterImport(x: Import, selectorsToRemove: Set[TermName]) = {
      val filteredSelectors = x.selectors map (_.name) filterNot (selectorsToRemove contains _)
      val range = x.pos.asInstanceOf[RangePosition]
      if (filteredSelectors isEmpty)
        patchtree.replace(range.start, range.end, "")
      else
        patchtree.replace(range.start, range.end,
          "import " + x.expr.toString + ".{" + (filteredSelectors mkString ", ") + "}\n")
    }

    private def badJavaClass(s: Symbol) = {
      JavaIoClasses exists (x => s hasTransOwner x)
    }

    def collectPatches(tree: Tree) {
      tree match {
        case x: Import if removeImport(x) =>
          val range = x.pos.asInstanceOf[RangePosition]
          patchtree.replace(range.start, range.end, "")
        case x: Import if containsIoClass(x) =>
          filterImport(x, JavaIoNames)
        case x: DefDef if methodRefersTo(x.symbol)(badJavaClass) =>
          removeDefDef(x)
        case _ => ()
      }
    }

  }
  
  /** Removes references to java.util.concurrent, java.util.Dictionary and java.util.Properties */
  private[Generating] class CleanupJavaConversions(patchtree: PatchTree) extends CallsiteUtils(patchtree) {
    
    private lazy val UtilPackage = definitions.getModule("java.util")
    private val concurrentName = newTermName("concurrent")
    
    private lazy val ConcurrentPackage = definitions.getModule("java.util.concurrent")
    
    private lazy val JavaConverionsObjectClass = definitions.getModule("scala.collection.JavaConversions").moduleClass
    
    private lazy val DictionaryClass = definitions.getClass("java.util.Dictionary")
    
    private lazy val PropertiesClass = definitions.getClass("java.util.Properties")
    
    private val templateNames: Set[Name] = Set("JConcurrentMapWrapper", "JDictionaryWrapper", "ConcurrentMapWrapper", "DictionaryWrapper", "JPropertiesWrapper") map (newTypeName)
    
    private def concurrentRef(s: Symbol): Boolean = s hasTransOwner ConcurrentPackage.moduleClass
    
    private def dictionaryRef(s: Symbol): Boolean = s hasTransOwner DictionaryClass
    
    private def propertiesRef(s: Symbol): Boolean = s hasTransOwner PropertiesClass
    
    private def badRef(s: Symbol): Boolean = concurrentRef(s) ||  dictionaryRef(s) || propertiesRef(s) 

    def collectPatches(tree: Tree) {
      tree match {
        case x: Import if x.expr.symbol == UtilPackage && x.selectors.exists(_.name == concurrentName) =>
          val range = x.pos.asInstanceOf[RangePosition]
          patchtree.replace(range.start, range.end, "")
        case _ => ()
      }
      within(JavaConverionsObjectClass)(tree) {
        case x: ClassDef if (templateNames contains x.name) =>
          removeTemplate(x)
      }
      enclosingClass(JavaConverionsObjectClass)(tree) {
        case x: DefDef if methodRefersTo(x.symbol)(badRef) =>
          removeDefDef(x)
      }
    }

  }
  
  /** Removes references to java.util.concurrent, java.util.Dictionary and java.util.Properties */
  private[Generating] class CleanupJavaConverters(patchtree: PatchTree) extends CallsiteUtils(patchtree) {
    
    private lazy val ConcurrentPackage = definitions.getModule("java.util.concurrent")
    
    private lazy val JavaConvertersObjectClass = definitions.getModule("scala.collection.JavaConverters").moduleClass
    
    private lazy val DictionaryClass = definitions.getClass("java.util.Dictionary")
    
    private lazy val PropertiesClass = definitions.getClass("java.util.Properties")
    
    private val defNames = Set("asJavaDictionaryConverter", "asJavaConcurrentMapConverter") map (newTermName)
    
    private val templateNames: Set[Name] = Set("AsJavaDictionary") map (newTypeName)
    
    private def concurrentRef(s: Symbol): Boolean = s hasTransOwner ConcurrentPackage.moduleClass
    
    private def dictionaryRef(s: Symbol): Boolean = s hasTransOwner DictionaryClass
    
    private def propertiesRef(s: Symbol): Boolean = s hasTransOwner PropertiesClass
    
    private def badRef(s: Symbol): Boolean = concurrentRef(s) ||  dictionaryRef(s) || propertiesRef(s) 

    def collectPatches(tree: Tree) {
      within(JavaConvertersObjectClass)(tree) {
        case x: ClassDef if (templateNames contains x.name) =>
          removeTemplate(x)
      }
      enclosingClass(JavaConvertersObjectClass)(tree) {
        case x: DefDef if methodRefersTo(x.symbol)(badRef) =>
          removeDefDef(x)
        case x: DefDef if defNames contains x.name =>
          removeDefDef(x)
      }
    }

  }
  
  /** Removes references to java.util.concurrent, java.util.Dictionary and java.util.Properties */
  private[Generating] class RemoveCloneMethod(patchtree: PatchTree) extends CallsiteUtils(patchtree) {
    
    private val cloneName: Name = newTermName("clone")

    def collectPatches(tree: Tree) {
      tree match {
//        case x: Apply if x.symbol.name == cloneName =>
//          val range = x.pos.asInstanceOf[RangePosition]
//          patchtree.replace(range.start, range.end-1, "sys.error(\"GWT doesn't support clone method.\")")
        case x: DefDef if x.name == cloneName =>
          if (x.symbol.nextOverriddenSymbol == definitions.Object_clone)
            //since Object.clone doesn't exist we should remove override modifier
            delOverrideModif(x)
          val range = x.rhs.pos.asInstanceOf[RangePosition]
          patchtree.replace(range.start, range.end, "{ sys.error(\"GWT doesn't support clone method.\") }\n")
        case _ => ()
      }
    }

  }
  
  /* ------------------------ The main patcher ------------------------ */

  class RephrasingTraverser(patchtree: PatchTree) extends Traverser {
    
    private lazy val removeParallelCollections = new RemoveParallelCollections(patchtree)
    
    private lazy val removeSerializationSupport = new RemoveSerializationSupport(patchtree)
    
    private lazy val cleanupSysPackage = new CleanupSysPackageObject(patchtree)
    
    private lazy val cleanupBigDecimal = new CleanupBigDecimal(patchtree)

    private lazy val cleanupConsole = new CleanupConsole(patchtree)
    
    private lazy val cleanupPredef = new CleanupPredef(patchtree)
    
    private lazy val cleanupReflectPackage = new CleanupReflectPackageObject(patchtree)
    
    private lazy val removeNoStackTraceParent = new RemoveNoStackTraceParent(patchtree)
    
    private lazy val cleanupXmlPackage = new CleanupXmlPackage(patchtree)
    
    private lazy val removeBadJavaImports = new RemoveBadJavaImports(patchtree)
    
    private lazy val cleanupJavaConversions = new CleanupJavaConversions(patchtree)
    
    private lazy val cleanupJavaConverters = new CleanupJavaConverters(patchtree)
    
    private lazy val removeCloneMethod = new RemoveCloneMethod(patchtree)

    override def traverse(tree: Tree): Unit = {
      
      removeParallelCollections collectPatches tree
      
      removeSerializationSupport collectPatches tree
      
      cleanupSysPackage collectPatches tree
      
      cleanupBigDecimal collectPatches tree

      cleanupConsole collectPatches tree
      
      cleanupPredef collectPatches tree
      
      cleanupReflectPackage collectPatches tree
      
      removeNoStackTraceParent collectPatches tree
      
      cleanupXmlPackage collectPatches tree
      
      removeBadJavaImports collectPatches tree
      
      cleanupJavaConversions collectPatches tree
      
      cleanupJavaConverters collectPatches tree
      
      removeCloneMethod collectPatches tree
      
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
