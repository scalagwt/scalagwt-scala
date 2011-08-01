/* jdk2ikvm -- Converts JDK-based Scala source files to use the IKVM library instead.
 * Copyright 2011 Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaNET/
 */

package scala.tools
package jdk2ikvm
package patchcmds

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

    private val Throwable_getStackTrace = definitions.getMember(ThrowableClass, "getStackTrace")

    def swallowing(tree: Tree, msg: String)(op: => Unit) {
      try { op }
      catch { case cce: ClassCastException =>
        warning(tree.pos, msg + "\n" + asString(tree)) /* nodeToString(tree), more detailed */
      }
    }

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

    val ienuCNames  = Set("java.util.AbstractCollection", 
                          "java.util.concurrent.CopyOnWriteArrayList", "java.util.LinkedList",
                          "java.util.ServiceLoader")
    val ienuClasses = ienuCNames map (cname => definitions.getClass(cname))   

    /** whether the idef ClassDef or ModuleDef:
     *   (a) extends one of the classes that IKVM endows with IEnumerable as extra interface, and
     *   (b) an IKVM's IterableEnumerator should be instantiated in the method to be added. */
    def shouldAddIterEnum(idef: ImplDef): Boolean = {
      ienuClasses exists (ienucsym => idef.symbol.ancestors contains ienucsym)
    }

    val juMap = definitions.getClass("java.util.Map")
    /** whether the idef ClassDef or ModuleDef :
     *   (a) extends one of the classes that IKVM endows with IEnumerable as extra interface, and
     *   (b) an IKVM's MapEnumerator should be instantiated in the method to be added. */
    def shouldAddMapEnum(idef: ImplDef): Boolean = {
      idef.symbol.ancestors contains juMap
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
    
    private def isjlObjOverrideOnly(dd: DefDef) : Boolean = { /* TODO dead code */
      val nonObjCestors = dd.symbol.owner.ancestors diff List(AnyClass, AnyRefClass, ObjectClass)
      nonObjCestors forall { csym => dd.symbol.overriddenSymbol(csym) == NoSymbol }
    }

    def couldntCoOverride(d: ValOrDefDef) {
      warning(d.pos, "j.l.Object override not co-overridden with .NET counterpart:\n"+asString(d))
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
    
    /*  Some callsites have to be rewritten to target static helper methods in IKVM,
     *  a rewriting dubbed 'detouring'. On entry to this method, those method symbols
     *  without a real assembly counterpart have been screened out already (e.g., String_+).
     *  Of those remaining, return only those declared for the first time as opposed to being csym overrides. */
    def detourables(msyms: List[Symbol], csym: Symbol) = {
      val publicInstanceMethods = msyms filter (m => m.isMethod && !m.isConstructor)
      publicInstanceMethods filter (m => m.overriddenSymbol(csym) == NoSymbol)
    }

    /*  Example:
     *        "abc".length
     *  -->
     *        java.lang.String.instancehelper_length("abc")
     */
    def instancehelper(rcvClass: String, app: Apply) {

      val Apply(fun, args) = app
      val Select(quali, originalName) = fun

      val appPos   = app.pos.asInstanceOf[RangePosition]
      val funPos   = fun.pos.asInstanceOf[RangePosition]

      /* Invocations on super.clone() are left as super calls, but to MemberwiseClone. */
      if(quali.isInstanceOf[Super]) {
        if(fun.symbol eq Object_clone) {
          // TODO discussion on cloning at http://www.scala-lang.org/node/169
          patchtree.replace(funPos.start, funPos.end - 1, "super.MemberwiseClone")
        } else if(fun.symbol eq Throwable_getStackTrace) {
          // TODO what's the JDK intent here 
          patchtree.replace(funPos.start, funPos.end - 1, "_root_.java.lang.Throwable.instancehelper_getStackTrace(this)")
        } else {
          warning(app.pos, "couldn't rewrite instancehelper on super " + asString(app))
        }
        return
      }

      /* Examples:

              x1 = getClass.getClassLoader
              x2 = wait(timeout - (current - start))
       */
      
      if(!quali.pos.isRange) {
        val newRcv = rcvClass + ".instancehelper_" + originalName
        if(args.isEmpty) {
          val tmpTo = if(patchtree.at(funPos.end) == '(') funPos.end + 1 /* e.g., originally this.wait() */
                      else funPos.end - 1                                /* e.g., originally this.wait   */
          patchtree.replace(funPos.start, tmpTo, newRcv + "(this)")
        } else {
          val lastArgPos = args.last.pos.asInstanceOf[RangePosition]
          val cmds = List(new Insert(newRcv + "( this , "),
                          new Patcheable(args.head.pos.start, lastArgPos.end - 1),
                          new Insert(" ) "))
          patchtree.tryPatch(appPos.start, appPos.end - 1, cmds, dropGapsOK = true)
        }
        return
      }

      val qualiPos = quali.pos.asInstanceOf[RangePosition]

      /* Examples:

              x1 = "abc".substring(0, AboutPositions.this.padding)
      app          |----------------------------------------------|[106:153]
      fun          |--------------|                                [106:121]
      quali        |----|                                          [106:111]
      arg0                         ||                              [122:123]
      arg1                            |--------------------------| [125:152]

              x2 = "def".length
      app          |-----------|[185:197]
      fun          |-----------|[185:197]
      quali        |----|       [185:190]

              x3 = "xyz".isEmpty()
      app          |--------------|[236:251]
      fun          |------------|  [236:249]
      quali        |----|          [236:241]

              x4 = "xyz" indexOf 'n'
      app          |----------------|[290:307]
      fun          |------------|    [290:303]
      quali        |----|            [290:295]
      arg0                       |--|[304:307]

              x5 = "abc" substring (0  ,   3)
      app          |-------------------------|[341:367]
      fun          |--------------|           [341:356]
      quali        |----|                     [341:346]
      arg0                          ||        [358:359]
      arg1                                 || [365:366]

      */

      val buf = mutable.ListBuffer.empty[PatchCmd]
      // new (static) receiver
      buf += new Insert(rcvClass + ".instancehelper_" + originalName + "(")
      // new first argument
      buf += new Patcheable(qualiPos.start, qualiPos.end - 1)
      // existing arguments if any
      if(!args.isEmpty) {
        buf += new Insert(", ")
        val headPos = args.head.pos.asInstanceOf[RangePosition]
        val lastPos = args.last.pos.asInstanceOf[RangePosition]
        buf += new Patcheable(headPos.start, lastPos.end - 1)
      }
      buf += new Insert(")")

      val afterQualiPos = patchtree.at(qualiPos.end)
      val beforeAppPos  = patchtree.at(appPos.start - 1) 
      val s = if( (beforeAppPos  == '(')  && (afterQualiPos == ')') ) appPos.start - 1
              else appPos.start
      val toRepl = patchtree.asString(s, appPos.end - 1)
      patchtree.tryPatch(s, appPos.end - 1, buf.toList, dropGapsOK = true)

    }


    protected lazy val jlCloneableClass    = definitions.getClass("java.lang.Cloneable")
    protected lazy val jlCharSequenceClass = definitions.getClass("java.lang.CharSequence")
    protected lazy val jlSerializable      = definitions.getClass("java.io.Serializable")
    protected lazy val ghostClasses        = List(jlCharSequenceClass, jlCloneableClass)

    val jlBoxedClasses = List(definitions.getClass("java.lang.Boolean"),
                                      definitions.getClass("java.lang.Byte"),
                                      definitions.getClass("java.lang.Character"),
                                      definitions.getClass("java.lang.Double"),
                                      definitions.getClass("java.lang.Float"),
                                      definitions.getClass("java.lang.Integer"),
                                      definitions.getClass("java.lang.Long"),
                                      definitions.getClass("java.lang.Short"))

    val classesStayingJavaLang = definitions.JavaLangPackage.tpe.decls filter (d => d.isClass) filterNot
      (c => ghostClasses.contains(c)) filterNot
      (c => c == jlSerializable)      filterNot
      (c => c == jlStringClass)       filterNot
      (c => c == jlObjectClass)       filterNot
      (c => jlBoxedClasses.contains(c)) toList

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

  private[Generating] class StringAndObjectContract(patchtree: PatchTree) extends CallsiteUtils(patchtree) {

    private def constructors(csym: Symbol) = { csym.info.decls.toList filter (m => m.isConstructor) }

    private val nonJavaJLObject = List( Object_## , Object_== , Object_!= , Object_eq , Object_ne , Object_synchronized ,
                                        Object_isInstanceOf , Object_asInstanceOf )
    private val jlObjectMembers = ObjectClass.info.decls.toList diff nonJavaJLObject
    private val jlObjectDetour = detourables(jlObjectMembers, AnyClass) filterNot (_ eq Object_clone) filterNot (_ eq Array_clone)
    private val jlObjectConstructors = constructors(ObjectClass) // just one constructor for Object

    private val jlStringMembers = StringClass.info.decls.toList filterNot (_ eq String_+)
    private val jlString_hashCode = getMember(StringClass, nme.hashCode_)
    private val jlString_toString = getMember(StringClass, nme.toString_)
    private val jlStringDetour = detourables(jlStringMembers, ObjectClass) // jlString_hashCode :: jlString_toString 
    private val jlStringConstructors = constructors(StringClass)

    private val JavaPackage       = getModule("java")
    private val JavaPackageClass  = JavaPackage.tpe.typeSymbol

    def collectPatches(tree: Tree) {
      instanceAndNewHelpers(tree)
      coOverrides(tree)
      callsitesToObjMethodsInAny(tree)
      addMissingJLObjOverrides(tree)
      // TODO rewrite super.hashCode
    }

    private def patchCallsite(app: Apply, txtFrom: String, txtTo: String, elimParens: Boolean) {
      val Apply(fun, args) = app 
      if(!fun.pos.isRange /* thus non-synthetic callsite */ ) {
        // TODO if not synthetic, warning(app.pos, "couldn't rewrite "+txtFrom+"() call " + asString(app))
        return
      }
      val funPos = fun.pos.asInstanceOf[RangePosition]
      val Select(quali, originalName) = fun
      val replStart =
        if(quali.pos.isRange) {
          val qualiPos = quali.pos.asInstanceOf[RangePosition]
          qualiPos.end
        } else {
          funPos.start
        }
      val toRepl = patchtree.asString(replStart, funPos.end - 1)
      val newText = toRepl.replaceAll(txtFrom, txtTo) // toRepl can be `.clone' or `clone', for example
      val tmpTo = if(elimParens && (patchtree.asString(funPos.end, funPos.end + 1) == "()"))
                    funPos.end + 1         /* e.g., originally this.clone() */
                  else funPos.end - 1      /* e.g., originally this.clone   */
      /* TODO save for parens elim, we still call toString
       * (for example, scala.collection.mutable.StringBuilder.toString)
       * Given that the DefDef for such `toString' methods is co-overriden with ToString,
       * shouldn't we just */
      patchtree.replace(replStart, tmpTo, newText)
    }

    private def hasTypeJavaLangCharSequence(tree: Tree): Boolean = {
      if(tree.tpe == null)   return false;
      if(tree.tpe == NoType) return false;
      tree.tpe.typeSymbol == jlCharSequenceClass
    }

    private def callsitesToObjMethodsInAny(tree: Tree) {
      tree match {
        case app @ Apply(fun, args) if(fun.symbol != NoSymbol) =>
          val msym   = fun.symbol
          val oInAny = msym.overriddenSymbol(AnyClass) 
          if ((msym == Any_toString) || (oInAny == Any_toString) ) {
            val leaveAsIs = hasTypeJavaLangCharSequence(fun.asInstanceOf[Select].qualifier)
            if(!leaveAsIs) {
              patchCallsite(app, "toString", "ToString", elimParens = true)
            }
          } else if ((msym == Any_hashCode) || (oInAny == Any_hashCode) ) {
            patchCallsite(app, "hashCode", "GetHashCode", elimParens = true)
          } else if ((msym == Any_equals) || (oInAny == Any_equals) ) {
            patchCallsite(app, "equals", "Equals", elimParens = false)
          } else if (    (msym == Array_clone)
                      || (msym.overriddenSymbol(ArrayClass) == Array_clone) ) {
            /* TODO at first sight it seems we would like to test for a clone() invocation on a native array only
               (in order to translate into System.Array::Clone())
                but there are overrides for cloning in MapLike, etc. that are rewritten into def MemberwiseClone(),
                and it's those we want to invoke. So for now this case is handled the same as the one below (Object_clone). */
            patchCallsite(app, "clone", "MemberwiseClone", elimParens = false)
          } else if (    (msym == Object_clone)
                      || (msym.overriddenSymbol(ObjectClass) == Object_clone) ) {
            // TODO discussion on cloning at http://www.scala-lang.org/node/169
            /* please notice that Object_clone isn't the same symbol as for
             *   scala.collection.mutable.Cloneable.clone
             * (which is arg-less) */
            patchCallsite(app, "clone", "MemberwiseClone", elimParens = true)
          }
        case _ => ()
      }
    }

    /*  Example:
     *        new String("abc")
     *  -->
     *        java.lang.String.newhelper("abc")
     **/
    private def newhelper(rcvClass: String, app : Apply) {
      val appPos = app.pos.asInstanceOf[RangePosition]
      val Apply(fun, args) = app
      val funPos = fun.pos.asInstanceOf[RangePosition]
      val buf = mutable.ListBuffer.empty[PatchCmd]
      buf += new Insert(rcvClass + ".newhelper")
      // preserve any text after (say) `new String'
      if(funPos.end <= appPos.end - 1) {
        buf += new Patcheable(funPos.end, appPos.end - 1)
        buf += new Insert(" ")
      }
      patchtree.tryPatch(appPos.start, appPos.end - 1, buf.toList, dropGapsOK = true)
    }

    private def instanceAndNewHelpers(tree: Tree) {
      tree match {

        case app @ Apply(fun, args) if (jlStringDetour contains app.symbol) => instancehelper("_root_.java.lang.String", app)

        case app @ Apply(fun, args) if (jlObjectDetour contains app.symbol) => instancehelper("_root_.java.lang.Object", app)

        case app @ Apply(fun @ Select(New(tpt), nme.CONSTRUCTOR), args) if (jlStringConstructors contains app.symbol) =>
          newhelper("java.lang.String", app)

        case app @ Apply(fun @ Select(New(tpt), nme.CONSTRUCTOR), args) if (jlObjectConstructors contains app.symbol) =>
          /* The pattern does not match super.<init> invocations in constructors, but only explicit instantiations a la new BlaBla(...) */
          val appPos = app.pos.asInstanceOf[RangePosition]
          patchtree.replace(appPos.start, appPos.end - 1, "new AnyRef")

        case _ => ()
      }
    }

    private val finalizeBody = """protected override void Finalize() {
              | if (!IKVM.Runtime.ByteCodeHelper.SkipFinalizer()) {
              |     try { this.finalize }
              |     catch { case _ => () }
              |   }
              | }""".stripMargin

    private def coOverrides(tree: Tree) {
      tree match {
        case dd: DefDef if (dd.symbol.isMethod) && (dd.symbol.isSourceMethod) && (!dd.symbol.isSynthetic)
           &&    (dd.symbol != Array_clone)
        =>
          if(dd.symbol.overriddenSymbol(AnyClass) != NoSymbol) {
            dd.symbol.name match {
              case nme.hashCode_ => renameOverride(dd, "hashCode", "GetHashCode")
              case nme.toString_ => renameOverride(dd, "toString", "ToString")
              case nme.equals_   => renameOverride(dd, "equals", "Equals")
              case _ => couldntCoOverride(dd)
            }
          } else if(dd.symbol.overriddenSymbol(ObjectClass) != NoSymbol) {
            dd.symbol.name match {
              case nme.clone_    => renameOverride(dd, "clone", "MemberwiseClone")
              case nme.finalize_ => renameOverride(dd, "hashCode", "Finalize")
              case _ => couldntCoOverride(dd)
            }
          }
        case vd: ValDef => 
          val g = vd.symbol.getter(vd.symbol.owner)
          if((g != NoSymbol) && (g.overriddenSymbol(AnyClass) != NoSymbol)) {
            g.name match {
              case nme.hashCode_ => renameOverride(vd, "hashCode", "GetHashCode")
              case nme.toString_ => renameOverride(vd, "toString", "ToString") // e.g. override lazy val toString 
              case _ => couldntCoOverride(vd)
            }
          }
        case _ => ()
      }
    }

    /* Given a template which extends an interface any of whose methods matches the signature of a j.l.Object method msym,
       (e.g., java.util.Map.Entry declares equals and hashCode)
       any local declarations (if any) will be rewritten to the System.Object signature,
       and thus the interface in question not fulfilled. Unless we add stubs as we do here.
     */
    private def addMissingJLObjOverrides(tree: Tree) {
      tree match {
        case cd: ClassDef =>
          val cestors       = cd.symbol.ancestors
          val nonObjCestors = cestors diff List(AnyClass, AnyRefClass, ObjectClass, ScalaObjectClass)
          val missingJLObjOverrides = for(a <- nonObjCestors; msym <- a.tpe.deferredMembers
                                          if jlObjectMembers contains msym.overriddenSymbol(ObjectClass)
                                          if msym.owner.ownerChain contains JavaPackageClass)
                                      yield msym
          var added : List[Name] = Nil
          for(msym <- missingJLObjOverrides.distinct; if !(added contains msym.name)) {
            added = msym.name :: added
            msym.name match {
              case nme.hashCode_ => addToBody(cd, "override def hashCode() = { this.GetHashCode() } /*addMissingJLObjOverrides*/ ")
              case nme.toString_ => addToBody(cd, "override def toString() = { this.ToString() } /*addMissingJLObjOverrides*/ ")
              case nme.equals_   => addToBody(cd, "override def equals(that: Any) = { this.Equals(that) } /*addMissingJLObjOverrides*/ ")
              case _             => warning(cd.pos, "addMissingJLObjOverrides found " + msym)
            }
          }
        case _ => ()
      }
    }

  }

  private[Generating] class MagicForInterfaces(patchtree: PatchTree) extends CallsiteUtils(patchtree) {

    private lazy val jlIterableClass   = definitions.getClass("java.lang.Iterable")
    private lazy val jioCloseableClass = definitions.getClass("java.io.Closeable")

    private lazy val ComparableClass_compareTo = definitions.getMember(ComparableClass, "compareTo")
    
    private lazy val ikvmExtendersNames = 
      Set("java.io.InputStream", "java.io.OutputStream", "java.io.PrintStream", "java.io.RandomAccessFile", 
          "java.io.Reader", "java.io.Writer", "java.nio.channels.DatagramChannel", "java.nio.channels.Pipe.SinkChannel", 
          "java.nio.channels.SocketChannel", "java.nio.channels.spi.AbstractInterruptibleChannel", "java.util.Formatter", 
          "javax.tools.ForwardingJavaFileManager")

    private def safeGetClass(name: String): Symbol = {
      try definitions.getClass(name)
      catch { case _: nsc.MissingRequirementError => NoSymbol }
    }

    private lazy val ikvmExtendersSym = ikvmExtendersNames map safeGetClass filterNot (_ == NoSymbol)

    private def extendsIDisposable(idef: ImplDef): Boolean = {
      idef.symbol.ancestors exists (ikvmExtendersSym contains _)
    }

    def collectPatches(tree: Tree) {
      tree match {
        case dd: DefDef if(dd.symbol.overriddenSymbol(ComparableClass) == ComparableClass_compareTo) =>
          // Extra Interface 1: System.IComparable is extended by IKVM's j.l.Comparable
          val p1 = dd.vparamss.head.head
          val ts1 = p1.tpt.tpe.typeSymbol
          if( (ts1 != AnyClass) ) {
            val noNeedToGuess = p1.tpt.pos.isInstanceOf[RangePosition]
            if(noNeedToGuess) {
              val pos1 = p1.tpt.pos.asInstanceOf[RangePosition]
              val txt1 = patchtree.asString(pos1.start, pos1.end - 1)
              val txtd = "override def CompareTo(that: Any) = { this.compareTo(that.asInstanceOf["+txt1+"]) }"
              spliceAfter(dd, txtd)
              // delOverrideModif(dd)
            } else {
              warning(dd.pos, "couldn't add CompareTo override: " + asString(dd))
            }
          }
        case idef: ImplDef =>

          // Extra Interface 2: System.Collections.IEnumerable is extended by a few IKVM classes
          if(shouldAddIterEnum(idef)) {
            addToBody(idef, "override def GetEnumerator = { new ikvm.lang.IterableEnumerator(this) }")
          } else if(shouldAddMapEnum(idef)) {
            addToBody(idef, "override def GetEnumerator = { new ikvm.lang.MapEnumerator(this) }")
          } else if (explicitlyExtends(idef, jlIterableClass)) {
            // Implied Interface 1
            addToExtendsClause(idef, "System.Collections.IEnumerable")
            addToBody(idef, "override def GetEnumerator = { new ikvm.lang.IterableEnumerator(this) }")
          }

          // Implied Interface 2
          if(explicitlyExtends(idef, jioCloseableClass)) {
            addToExtendsClause(idef, "System.IDisposable")
            addToBody(idef, "def Dispose { this.close }") 
          } else if(extendsIDisposable(idef)) {
            // don't add `System.IDisposable' to extends clause  
            addToBody(idef, "override def Dispose { this.close }") 
          }

        case _ => ()
      }
      /* Upcast to extra interface, part 1:

              receiver.compareTo(that)
        -->
              java.lang.Comparable.__Helper.compareTo(receiver, that)
       */
      tree match {
        case app @ Apply(fun, args) if (app.symbol eq ComparableClass_compareTo) =>

          val Apply(fun, List(arg)) = app
          val Select(quali, _) = fun

          val appPos   = app.pos.asInstanceOf[RangePosition]
          val qualiPos = quali.pos.asInstanceOf[RangePosition]
          val argPos   = arg.pos.asInstanceOf[RangePosition]

          val cmds = List( new Insert("java.lang.Comparable.__Helper.compareTo("),
                           new Patcheable(qualiPos.start, qualiPos.end - 1) ,
                           new Insert(", ") ,
                           new Patcheable(argPos.start, argPos.end - 1) ,
                           new Insert(")")
                         )

          patchtree.tryPatch(appPos.start, appPos.end - 1, cmds, dropGapsOK = true)

        case _ => ()
      }
      // Upcast to extra interface, part 2, done by IKVMUpcaster
    }

  }

  private[Generating] class GhostInterfaces(patchtree: PatchTree) extends CallsiteUtils(patchtree) {

    def collectPatches(tree: Tree) {
      // 5.4.1.a extends Cloneable, extends CharSequence
      rebaseFromTo(tree, jlCloneableClass,    "java.lang.Cloneable.__Interface")
      rebaseFromTo(tree, jlCharSequenceClass, "java.lang.CharSequence.__Interface")
      // 5.4.1.b new  Cloneable, new CharSequence
      newNew(tree, jlCloneableClass,    "java.lang.Cloneable.__Interface")
      newNew(tree, jlCharSequenceClass, "java.lang.CharSequence.__Interface")
      // TODO 5.4.3 Instance method invocations: for now handled through implicits
      // TODO 5.4.4 == and != check that IKVM's operators are invoked
      // 5.4.5 Type casts and checks
      isInstanceOfGhost(tree)
      // TODO the rest of serialization-related transforms
      rebaseFromTo(tree, jlSerializable, "java.io.Serializable.__Interface")
    }

    /*  Example:
     *        rcv.isInstanceOf[java.lang.CharSequence]
     *  -->
     *        java.lang.CharSequence.IsInstanceOf(rcv)
     *
     *  similarly for Cloneable
     */
    private def isInstanceOfGhost(tree: Tree) {
      tree match {
        // rewrites isInstanceOf[CharSequence] and isInstanceOf[Cloneable]
        case app @ TypeApply(fun, args) if (
            {
              val guard0 = (fun.symbol.overriddenSymbol(AnyClass) == Any_isInstanceOf)
              if (guard0 && (args.head.tpe != null)) {
                val typeArg = args.head.tpe.typeSymbol
                ghostClasses contains typeArg
              } else false
            }
          ) =>
          val typeArg = args.head.tpe.typeSymbol
          val staticUtil = if (typeArg == jlCharSequenceClass) "java.lang.CharSequence.IsInstanceOf"
                           else "java.lang.Cloneable.IsInstanceOf"
          val Select(quali, _) = fun
          val appPos   = app.pos.asInstanceOf[RangePosition]
          val qualiPos = quali.pos.asInstanceOf[RangePosition]
          val cmds = List( new Insert(staticUtil + "("),
                           new Patcheable(qualiPos.start, qualiPos.end - 1),
                           new Insert(")")
                         )
          patchtree.tryPatch(appPos.start, appPos.end - 1, cmds, dropGapsOK = true)
          // TODO what about asInstanceOf[] for CharSequence and Cloneable. Needed at all?
          // TODO rewrite isInstanceOf, asInstanceOf [array of CharSequence]
          // TODO rewrite isInstanceOf, asInstanceOf [array of Cloneable]
        
        case _ => ()
      }
    }

  }

  private[Generating] class ExceptionsRewritings(patchtree: PatchTree) extends CallsiteUtils(patchtree) {

    private val jlThrowableMembers = ThrowableClass.info.decls.toList
    private val jlThrowableDetour  = detourables(jlThrowableMembers, AnyClass)
    
    // type alias (and abstract types) are direct instances of TypeSymbol (and not ClassSymbol, which definitions.getClass returns)
    val sThrowable = ScalaPackageClass.info.decls.lookupEntry("Throwable".toTypeName).sym
    val sError     = ScalaPackageClass.info.decls.lookupEntry("Error".toTypeName).sym
    val sException = ScalaPackageClass.info.decls.lookupEntry("Exception".toTypeName).sym
    
    def collectPatches(tree: Tree) {
      tree match {
        case app @ Apply(fun, args) if (jlThrowableDetour contains app.symbol) => instancehelper("java.lang.Throwable", app)

        /* For example, scala.util.control.ControlThrowable is defined as: 
 
               trait ControlThrowable extends Throwable with NoStackTrace

           "Throwable" above refers to the type alias in the scala package object. 
           After jdk2ikvm has run, that alias points to System.Exception (similarly, scala.Exception and scala.Error are aliases)

           In order to preserve semantics, we reformulate the "extends" above to explicitly mention java.lang.Throwable.
           Similarly for j.l.Error and j.l.Exception. 
         */
        case cd: ClassDef if (!cd.symbol.isSynthetic) => 
          rebaseFromTo(cd, jlErrorClass,     "java.lang.Error")
          rebaseFromTo(cd, jlExceptionClass, "java.lang.Exception")
          rebaseFromTo(cd, ThrowableClass,   "java.lang.Throwable")
          rebaseFromTo(cd, sError,     "java.lang.Error")
          rebaseFromTo(cd, sException, "java.lang.Exception")
          rebaseFromTo(cd, sThrowable, "java.lang.Throwable")

        case Try(_, catches, _) if (!catches.isEmpty) =>
          // TODO Case (1) Originally catch Throwable
          // TODO Case (2) Originally catch Exception or catch Error
          // TODO Case (3) Otherwise

        case _ => ()
      }
    }
    
  }

  /** for example, Runtime.getRuntime() is rewritten to java.lang.Runtime.getRuntime() */
  private[Generating] class FullyQualifyJavaLang(patchtree: PatchTree) extends CallsiteUtils(patchtree) {

    private val jlSystemClass  = definitions.getClass("java.lang.System")

    def collectPatches(tree: Tree) {
      for(d <- definitions.JavaLangPackage.tpe.decls; if d.isClass) {
        fullyQualifyStaticAccesses(tree, d, "java.lang." + d.name)
      }
      if(tree.isInstanceOf[New]) {
        for(d <- classesStayingJavaLang) {
          newNew(tree, d, "java.lang." + d.name)
        }
      }
    }

    /** fully-qualify accesses to csym static members, otherwise won't be found on .NET.
       Example: String.format() is OK in forJVM mode, on .NET it means System.String.format()
     */
    private def fullyQualifyStaticAccesses(tree: Tree, csym: Symbol, fqn: String) {
      tree match {
        case Select(quali, name) if (quali.symbol eq csym.companionModule) =>
          if(quali.pos.isRange) {
            val qualiPos = quali.pos.asInstanceOf[RangePosition]
            patchtree.replace(qualiPos.start, qualiPos.end - 1, fqn)
          } else if(tree.pos.isRange)  {
            val treePos = tree.pos.asInstanceOf[RangePosition]
            patchtree.replace(treePos.start, treePos.end - 1, fqn + "." + name)
          } else {
            warning(tree.pos, "couldn't fqn static access " + asString(tree))
            /* TODO this happens for a static access as default argument
                 def touch(modTime: Long = System.currentTimeMillis) = ...
               One way to deal with it is upstream, at the handler for a formal param (ValDef).
               For now, warning is given. 
             */
          }
        case _ => ()
      }
    }

  }

  private[Generating] class ArgsForRepeated(patchtree: PatchTree) extends CallsiteUtils(patchtree) {

    private val JavaPackage       = getModule("java")
    private val JavaPackageClass  = JavaPackage.tpe.typeSymbol

    def isNonDetouredJDKMethod(msym: Symbol): Boolean = {
      if(msym == NoSymbol) return false
      if(msym.owner == ObjectClass) return false
      if(msym.owner == StringClass) return false
      val tsym = msym.owner.tpe.typeSymbol
      if(tsym == NoSymbol) return false
      val isJava  = (tsym.ownerChain contains JavaPackageClass)
      isJava
    }

    def hasJavaRepeatedParam(msym: Symbol): Boolean = {
      val jparams = msym.tpe.params
      if(jparams.isEmpty) return false;
      definitions.isJavaRepeatedParamType(jparams.last.tpe) 
    }

    def shouldPackRepeatedAsArray(msym: Symbol): Boolean =
      isNonDetouredJDKMethod(msym) && hasJavaRepeatedParam(msym) 

  }

  /** Makes explicit empty array as arg to repeated param
   *
   * For example, IKVM's Class.getMethod and Method.invoke require an explicit empty arg for the repeated param when empty. 
   * Same goes for all other JDK repeated params (they are declared as array types in IKVM method signatures). */
  private[Generating] class MissingArgForRepeated(patchtree: PatchTree) extends ArgsForRepeated(patchtree) {

    def collectPatches(tree: Tree) {
      tree match {
        case app @ Apply(fun, args)
          if (shouldPackRepeatedAsArray(app.symbol)) && (args.size == app.symbol.paramss.head.size - 1)
        =>
          val funPos = fun.pos.asInstanceOf[RangePosition]
          val addParens = !(patchtree.asString(funPos.end, args.head.pos.start - 1) contains '(')  
          val lastPos = args.last.pos.asInstanceOf[RangePosition]
          if(addParens) { patchtree.splice(args.head.pos.start, "(") }
          val closingParens = if(addParens) ")" else ""
          val elemtpe = definitions.elementType(JavaRepeatedParamClass, app.symbol.paramss.head.last.tpe)
          val tc = elemtpe.typeConstructor
          val strelemtpe = upcastings.get(tc.typeSymbol) getOrElse tc.toString
          patchtree.splice(lastPos.end, ", scala.Array.empty["+strelemtpe+"]" + closingParens)
        case _ => () 
      }
    }

  }

  /** The JDK-based version of invocations to Class.getMethod and Class.getConstructor can
   *  reel off zero, one, or more classOf[] arguments for the last (repeated) param in those methods.
   *  The IKVM version of those methods expects an Array[j.l.Class]. Therefore, arguments for it
   *  can't be omitted, nor reeled off. Visitor `MissingArgForRepeated' handles the case of zero args,
   *  and this one the case of one or more args.
   *
   *  Examples:
   *
   *      val m2 = clazz.getMethod("main", classOf[Array[String]])
   *
   *      val constr1 = clazz.getConstructor(classOf[String], classOf[Integer])
   *
   *      val constr2 = clazz getConstructor classOf[String]
   * 
   * */
  private[Generating] class OneOrMoreArgForRepeated(patchtree: PatchTree) extends ArgsForRepeated(patchtree) {

    def collectPatches(tree: Tree) {
      tree match {
        case app @ Apply(fun, args)
          if (shouldPackRepeatedAsArray(app.symbol)) && (args.size >= app.symbol.paramss.head.size)
        =>
          val firstRepeatedArg = app.symbol.paramss.head.size - 1
          val lastRepeatedArg  = args.size - 1
          val noNeedToGuess    = args(firstRepeatedArg).pos.isRange && args(lastRepeatedArg).pos.isRange
          if(noNeedToGuess) {
            val start = args(firstRepeatedArg).pos.asInstanceOf[RangePosition].start
            val end   = args(lastRepeatedArg).pos.asInstanceOf[RangePosition].end - 1
            // debug val toRepl = patchtree.asString(start, end)
            val cmds = List(new Insert("scala.Array( "),
                            new Patcheable(start, end),
                            new Insert(" ) "))
            patchtree.tryPatch(start, end, cmds, dropGapsOK = true)
          }
        case _ => ()
      }
    }

  }

  /** internals about annotations after typer in Sec. 3.2 of http://www.scala-lang.org/sid/5 */
  private[Generating] class AnnIgnorer(patchtree: PatchTree) extends CallsiteUtils(patchtree) {

    private val ignorables = List(ThrowsClass)  

    def collectPatches(tree: Tree) {
      tree match {
        case dd: DefDef =>
          for(anninfo <- dd.symbol.annotations; if (ignorables contains anninfo.atp.typeSymbol)) {
            val annPos = anninfo.pos.asInstanceOf[RangePosition]
            patchtree.splice(annPos.start - 1, "/*")
            patchtree.splice(annPos.end,       "*/")
          }
        case _ => ()
      }
    }

  }

  /* ------------------------ Type mapping and erasing ------------------------ */

  val jlErrorClass     = definitions.getClass("java.lang.Error")
  val jlExceptionClass = definitions.getClass("java.lang.Exception")
  val jlObjectClass    = definitions.getClass("java.lang.Object")
  val jlStringClass    = definitions.getClass("java.lang.String")

  val upcastings = Map(ThrowableClass   -> "System.Exception",
                       jlExceptionClass -> "System.Exception",
                       jlErrorClass     -> "System.Exception",
                       jlObjectClass    -> "Object",
                       jlStringClass    -> "String")
  /* TODO ComparableClass -> "System.IComparable" but only in `receiving' positions (method arg: yes, return type: no)
   * If the pair above were added to the `upcastings' map,
   * then occurrences of java.lang.Comparable[A] would be replaced by System.IComparable,
   * in which case two rewrite rules would interfere (upcasting and arg-type erasing).
   * Looks like one of them should be customized to account for the other. */

  val leaveAsIsNames = Map(jlObjectClass -> List("AnyRef"))

  private[Generating] class IKVMUpcaster(patchtree: PatchTree, csym: Symbol,
                                         newTypeRef: String, leaveAsIs: List[String])
                      extends CallsiteUtils(patchtree) {

    def collectPatches(tree: Tree) {

        def shouldSubst(sourceFrag: Tree): Boolean = {
          val t = sourceFrag.tpe
          val cond0 = (t != null) && (t != NoType) && (t.typeSymbol eq csym) && (sourceFrag.pos.isRange)
          if(cond0) {
            val rpos = sourceFrag.pos.asInstanceOf[RangePosition]
            val programText = patchtree.asString(rpos.start, rpos.end - 1)
            val doSubst = (newTypeRef != programText) && !(leaveAsIs contains programText)
            doSubst
          } else false
        }

        def trySubst(sourceFrag: Tree) {
          if(shouldSubst(sourceFrag)) {
            val rpos = sourceFrag.pos.asInstanceOf[RangePosition]
            patchtree.replace(rpos.start, rpos.end - 1, newTypeRef)
          }
        }

        /** precond: sourceFrag must have a TypeTree as dominator over the "AST node containment" hierarchy,
         *  ie. sourceFrag should *not* be visitable by a Tree traverser
         *  (which skips the `orig' node contained in a TypeTree, and its contained nodes, and so on. */
        def replSourceFragmentForASTType(sourceFrag: Tree) {
          sourceFrag match {
            case SingletonTypeTree(ref)              => replSourceFragmentForASTType(ref)
            case SelectFromTypeTree(qualifier, name) => replSourceFragmentForASTType(qualifier)
            case CompoundTypeTree(templ)             => replSourceFragmentForASTType(templ)
            case AppliedTypeTree(tpt, args) =>
              replSourceFragmentForASTType(tpt)
              for (arg <- args) { replSourceFragmentForASTType(arg) }
            case TypeBoundsTree(lo, hi) =>
              replSourceFragmentForASTType(lo)
              replSourceFragmentForASTType(hi)
            case ExistentialTypeTree(tpt, whereClauses) =>
              replSourceFragmentForASTType(tpt)
              for (wc <- whereClauses) { replSourceFragmentForASTType(wc) }
            case tt : TypeTree if (tt.original != null) =>
              replSourceFragmentForASTType(tt.original)  
            case Annotated(annot, arg) =>
              // TODO children not visited on purpose although I would like to know more about them 
              // warning(sourceFrag.pos, "Annotated(annot, arg) not visited in replSourceFragmentForASTType")
            case other if (shouldSubst(other)) => trySubst(other)
            case _ =>
              /* children of this node won't be visited by IKVMUpcaster.this.collectPatches,
               * because this node lives inside a TypeTree to start with.  
               * We shouldn't need to visit them anyway, but the warning is there to help discover overlooked cases. */
              val substCandidates = (new CollectRangedNodes apply sourceFrag) filter (rn => shouldSubst(rn))
              if(substCandidates.nonEmpty) {
                for (rn <- substCandidates) {
                  warning(rn.pos, "ranged node contained in a TypeTree not substituted by replSourceFragmentForASTType")
                }
              }
          }
        }

      tree match {
        case tt : TypeTree if (tt.original != null) => replSourceFragmentForASTType(tt.original)
        case _ => () // don't visit children: IKVMUpcaster.collectPatches is called inside Traverser.traverse
      }
    }

  }

  /** All IKVM types lack type params, whether its JDK counterpart (if any) takes type args or not. */
  private[Generating] class TypeArgEraser(patchtree: PatchTree) extends CallsiteUtils(patchtree) {

    private val JavaPackage       = getModule("java")
    private val JavaPackageClass  = JavaPackage.tpe.typeSymbol
    private val knownPos = collection.mutable.Set.empty[Symbol]
    private val knownNeg = collection.mutable.Set.empty[Symbol]

    /* erase(sourceFrag) is invoked > 1 for the same sourceFrag when it's a decl type in the prim constr of a case class.
     * To avoid that, a buffer is needed.  */
    private val erasedAlready = collection.mutable.Set.empty[Tree]    

        def shouldErase(t: Type): Boolean = {
          if(t != null && t != NoType) {
            val tsym = t.typeSymbol
            if(knownPos contains tsym) true
            else if(knownNeg contains tsym) false
            else {
              val isJava = (tsym.ownerChain contains JavaPackageClass)
              if(isJava && t.typeArgs.nonEmpty) { knownPos += tsym; assert(!(knownNeg contains tsym)); true }
              else { knownNeg += tsym; assert(!(knownPos contains tsym)); false }
            }
          } else false 
        }

        def erase(sourceFrag: AppliedTypeTree) {
          if(erasedAlready.contains(sourceFrag)) return
          erasedAlready += sourceFrag 
          val noNeedToGuess = sourceFrag.tpt.pos.isRange && sourceFrag.pos.isRange &&
                              sourceFrag.args.last.pos.isRange
                              /* we check this last position to find out whether that type param is synthetic. */ 
          if(noNeedToGuess) {
            val sourceFragPos = sourceFrag.pos.asInstanceOf[RangePosition]
            val tptPos = sourceFrag.tpt.pos.asInstanceOf[RangePosition]
            patchtree.splice(tptPos.end, "/*")
            patchtree.splice(sourceFragPos.end, "*/")
          } else {
            // this appears to happen only for the declared type of a value param
            patchtree.eraseTypeArg(sourceFrag.pos.startOrPoint)
            // DEBUG warning(sourceFrag.pos, "don't know where to erase type-args to AppliedTypeTree (a message brought to you by TypeArgEraser)")
          }
        }

        def erase(sourceFrag: ExistentialTypeTree) {
          if(sourceFrag.whereClauses forall (wc => wc.pos.isRange)) {
            // non-synthetic
            sourceFrag.tpt match {
              case att : AppliedTypeTree => att match {
                  case att2 : AppliedTypeTree if att2.pos.isRange =>
                    patchtree.splice(att2.tpt.pos.endOrPoint, "/*")
                    patchtree.splice(sourceFrag.pos.endOrPoint, "*/")
                    return 
                  case _ => ()
                }
              case _ => ()
            }
          }
          // synthetic
          if(sourceFrag.tpt.isInstanceOf[AppliedTypeTree]) {
            erase(sourceFrag.tpt.asInstanceOf[AppliedTypeTree])
          } else {
            warning(sourceFrag.pos, "don't know where to erase type-args to ExistentialTypeTree (a message brought to you by TypeArgEraser)")
          }
        }

      def eraseTypeArgs(sourceFrag: Tree, sourceFragTpe: Type) {
        sourceFrag match {
          case SingletonTypeTree(ref)              => eraseTypeArgs(ref, sourceFragTpe.widen)
          case SelectFromTypeTree(qualifier, name) =>
            // TODO in a type designator, how to find out what comes before the #
            eraseTypeArgs(qualifier, qualifier.tpe)
          case CompoundTypeTree(templ)             => eraseTypeArgs(templ, sourceFragTpe)
          case att @ AppliedTypeTree(tpt, args) =>
            if(shouldErase(sourceFragTpe)) erase(att)
            else {
              eraseTypeArgs(tpt, sourceFragTpe.typeConstructor)
              for ( (arg, tpe) <- args zip sourceFragTpe.typeArgs) { eraseTypeArgs(arg, tpe) }
            }
          case TypeBoundsTree(lo, hi) =>
            eraseTypeArgs(lo, sourceFragTpe.bounds.lo)
            eraseTypeArgs(hi, sourceFragTpe.bounds.hi)
          case ett @ ExistentialTypeTree(tpt, whereClauses) =>
            if(shouldErase(sourceFragTpe)) erase(ett)
            else {
              // TODO
              // eraseTypeArgs(tpt, sourceFragTpe.existentialSkolems)
              // for (wc <- whereClauses) { eraseTypeArgs(wc) }
            }
          case tt : TypeTree if (tt.original != null) =>
            eraseTypeArgs(tt.original, sourceFragTpe)
          case Annotated(annot, arg) =>
            // TODO children not visited on purpose although I would like to know more about them
            // warning(sourceFrag.pos, "Annotated(annot, arg) not visited in eraseTypeArgs")
          case _ =>
            /* children of this node won't be visited by IKVMUpcaster.this.collectPatches,
             * because this node lives inside a TypeTree to start with.
             * We shouldn't need to visit them anyway, but the warning is there to help discover overlooked cases. */
            val substCandidates = (new CollectRangedNodes apply sourceFrag) filter (rn => shouldErase(rn.tpe))
            for (rn <- substCandidates) {
              if (patchtree.asString(rn.pos.startOrPoint, rn.pos.endOrPoint - 1).contains('[')) {
                warning(rn.pos, "ranged node contained in a TypeTree not erased by eraseTypeArgs")
              }
            }
        }
    }

    def collectPatches(tree: Tree) {
      tree match {
        case tt : TypeTree if (tt.original != null) => eraseTypeArgs(tt.original, tt.tpe)
        case td : TypeDef  if (treeInfo.isAliasTypeDef(td)) && (!td.tparams.isEmpty) && (shouldErase(td.rhs.tpe)) =>
          // val nameStart = td.pos.point
          // val nameEnd   = nameStart + td.name.toString.size
          val paramSecStart = td.tparams.head.pos.start - 1 
          val paramSecEnd   = td.tparams.last.pos.end
          val toRepl = patchtree.asString(paramSecStart, paramSecEnd)
          if(toRepl.contains('[') && toRepl.contains(']')) {
            patchtree.eraseTypeArg(paramSecStart)
          } else {
            warning(td.pos, "couldn't erase type args in the left-hand side of type alias.")
          }
        case _ => ()
      }
    }

  }

  /* ------------- Goes hand in hand with TypeArgEraser  ------------- */

  private[Generating] class Downcaster(patchtree: PatchTree) extends CallsiteUtils(patchtree) {

    private val JavaPackage       = getModule("java")
    private val JavaPackageClass  = JavaPackage.tpe.typeSymbol
    private def isJava(tsym: Symbol) = (tsym.ownerChain contains JavaPackageClass)
    private def shouldDcast(app: Apply): Boolean = {
      if(app.symbol == NoSymbol) return false;
      if(!isJava(app.symbol.owner)) return false;
      if((app.tpe == null) || (app.tpe == NoType)) return false;
      val ts = app.tpe.resultType.typeSymbol
      if(ts == NoSymbol) return false;
      val res = ts.isType && ts.isParameter
      /* Please notice we can't downcast a (method or field) returning a polytype. For example,
       * java.util.jar.JarFile.entries can't be downcast as follows:
       *   .asInstanceOf[java.util.Enumeration[java.util.jar.jar.JarEntry]]
       * because under IKVM,
       *   java.util.Enumeration does not take type parameters.
       * Instead, one would have to track. In practice, this poses a problem only in cases like:
       *   jarFile.entries.asScala map (x => Fileish(x, () => getStream(x)))
       * (i.e., where no JDK method is explicitly invoked to obtain the type param of the polytype)   
       * which has to be rewritten (for now manually ) into: 
       *   jarFile.entries.asScala map (x => Fileish(x.asInstanceOf[java.util.jar.JarEntry], () => getStream(x.asInstanceOf[java.util.jar.JarEntry])))
       * (or more compactly, by downcasting the result of `asScala' above). */
      res
    }

    def collectPatches(tree: Tree) {
      tree match {
        case app @ Apply(fun, args) if (shouldDcast(app)) =>
          if (!(app.pos.isRange)) {
            warning(app.pos, "couldn't add asInstanceOf to " + asString(app))
          } else { 
            val aio = tree.tpe.toString
            if(aio != "_") {
              val pos = app.pos.asInstanceOf[RangePosition]
              patchtree.splice(pos.start, "(")
              patchtree.splice(pos.end, ").asInstanceOf["+aio+"]")
            }
          }
        case _ => ()
      }
    }

  }

  /* --------- fully-qualify occurrences of java.lang types (as opposed to accesses to static members) --------- */

  private[Generating] class TypesSubsttter(patchtree: PatchTree) extends CallsiteUtils(patchtree) {

    /* Explicitly listing which classes to substitute has the advantage that no unexpected substitution occurs.
       However, classesStayingJavaLang in visitor FullyQualifyJavaLang could replace the list below,
       because it filters out those java.lang types we don't want substituted. */
    val namesList = List("Appendable", "AssertionError", "CharSequence", "ClassLoader", "ClassNotFoundException",
                         "Exception" /* TODO pending more complete translation for exceptions */ , "ExceptionInInitializerError",
                         "IllegalStateException", "InterruptedException",
                         "NoClassDefFoundError", "NoSuchMethodError", "NoSuchMethodException", 
                         "Runtime", "SecurityException", "StackOverflowError", "StackTraceElement",
                         "Runnable", "StringBuffer", "Thread")

    val toReplace = for ( d <- namesList; fqn = "java.lang." + d ) yield Pair(definitions.getClass(fqn), fqn)

    def substType(sourceFrag: Tree, sourceFragTpe: Type, csym: Symbol, newFragment: String) {
      sourceFrag match {
        case SingletonTypeTree(ref)              =>
          substType(ref, sourceFragTpe.widen, csym, newFragment)
        case SelectFromTypeTree(qualifier, name) =>
          // TODO in a type designator, how to find out what comes before the #
          substType(qualifier, qualifier.tpe, csym, newFragment)
        case CompoundTypeTree(templ)             =>
          substType(templ, sourceFragTpe, csym, newFragment)
        case att @ AppliedTypeTree(tpt, args) =>
          substType(tpt, sourceFragTpe.typeConstructor, csym, newFragment)
          for ( (arg, tpe) <- args zip sourceFragTpe.typeArgs) { substType(arg, tpe, csym, newFragment) }
        case TypeBoundsTree(lo, hi) =>
          substType(lo, sourceFragTpe.bounds.lo, csym, newFragment)
          substType(hi, sourceFragTpe.bounds.hi, csym, newFragment)
        case ett @ ExistentialTypeTree(tpt, whereClauses) =>
          // TODO
          // eraseTypeArgs(tpt, sourceFragTpe.existentialSkolems)
          // for (wc <- whereClauses) { eraseTypeArgs(wc) }
        case tt : TypeTree if (tt.original != null) =>
          substType(tt.original, sourceFragTpe, csym, newFragment)
        case Annotated(annot, arg) =>
          // TODO children not visited on purpose although I would like to know more about them
          // warning(sourceFrag.pos, "Annotated(annot, arg) not visited in eraseTypeArgs")
        case _ =>
          if (shouldSubst(sourceFragTpe, csym)) {
            doSubst(sourceFrag, newFragment)
          }
      }
    }

    def shouldSubst(t: Type, csym: Symbol): Boolean = {
      if((t == null) || (t == NoType)) return false;
      val tsym = t.typeSymbol
      csym == tsym
    }

    def doSubst(ori: Tree, newFragment: String) {
      if(ori.pos.isRange) {
        val pos = ori.pos.asInstanceOf[RangePosition]
        patchtree.replace(pos.start, pos.end - 1, newFragment)
      } else {
        warning(ori.pos, "couldn't substitute type at " + asString(ori) + " with " + newFragment)
      }
    }

    def collectPatches(tree: Tree) {
      tree match {

        case tt: TypeTree if (tt.original != null) =>
          for( (csym, cname) <- toReplace ) substType(tt.original, tt.tpe, csym, cname)

        case ntree @ New(tpt) =>
          for( (csym, cname) <- toReplace ) newNew(ntree, csym, cname)

        case cd: ClassDef if (!cd.symbol.isSynthetic) =>
          for( (csym, cname) <- toReplace ) rebaseFromTo(cd, csym, cname)

        /* Example:
         *     classOf[SecurityException]
         * -->
         *    classOf[java.lang.SecurityException] 
         */
        case lit @ Literal(value) if (value.tag == ClassTag) =>
          /* actually this case handler is different from the others,
          *  because the replacement is based on `classesStayingJavaLang' and not on `toRelace'. */ 
           val ts = value.typeValue.typeSymbol
           if((ts != ObjectClass) && classesStayingJavaLang.contains(ts) && lit.pos.isRange) {
             val pos = lit.pos.asInstanceOf[RangePosition]
             val strCL = patchtree.asString(pos.start, pos.end - 1)
             val replStart = strCL.indexOf('[') + 1
             val replEnd   = strCL.lastIndexOf(']') - 1
             patchtree.replace(pos.start + replStart, pos.start + replEnd, "java.lang." + ts.name)
           }

        case _ => ()
      }
    }

  }

  /* --------- e.g., `implicit def Tuple8' instantiates Ordering and defines
               `compare' taking 2 Tuple8 arguments. That's not enough for IKVM,
                because an override expected there is compare(Any, Any) (because IKVM erases ... )
     --------- */

  private[Generating] class MissingErasedOverloads(patchtree: PatchTree) extends CallsiteUtils(patchtree) {
    
    /* TODO What this *visitor* does for j.u.Comparator could be generalized:
     *   whenever a DefDef D1 overrides a method D2 declared in a Java type, and D2 takes any param with generic type,
     *   and neither D1 nor a sibling overrides D2's erasure, then add such missing erased overload to call D1.
     *   That's a fair number of joins, even for a Prolog engine. */

    private val juComparator = definitions.getClass("java.util.Comparator")
    private val juC_compare  = definitions.getMember(juComparator, "compare")

    private val juCollection = definitions.getClass("java.util.Collection")
    private val juC_add      = definitions.getMember(juCollection, "add")

    private val juList  = definitions.getClass("java.util.List")
    private val juL_set = definitions.getMember(juList, "set")

    private val juMap_put = definitions.getMember(juMap, "put")

    private val juMapEntry = definitions.getClass("java.util.Map.Entry")
    private val juMapEntry_setValue = definitions.getMember(juMapEntry, "setValue")

    private val jucConcuMap = definitions.getClass("java.util.concurrent.ConcurrentMap")
    private val jucConcuMap_putIfAbsent = definitions.getMember(jucConcuMap, "putIfAbsent")
    private val jucCM_replace2 :: jucCM_replace3 :: _ = definitions.getMember(jucConcuMap, "replace").alternatives

    private val erasingDisambiguator = "NonErased"

    def addErasedOverload(dd: DefDef, jmsym: Symbol): Boolean = {
      val tsyms = dd.vparamss.head map {p => p.tpt.tpe.typeSymbol}
      if (tsyms forall {tsym => tsym == AnyClass}) { return false }
      val ascrPoss = dd.vparamss.head map {param => param.tpt.pos}
      val noNeedToGuess = ascrPoss forall {pos => pos.isRange}
      if(!noNeedToGuess) {
        warning(dd.pos, "couldn't add missing erased overload: " + asString(dd))
        return false 
      }
      val pNames = for (param <- dd.vparamss.head; val start = param.pos.start)
                   yield {
                     var end = start
                     while(Character.isJavaIdentifierPart(patchtree.at(end))) { end += 1 }
                     patchtree.asString(start, end - 1)
                   }
      val ascrTs    = ascrPoss map {pos => patchtree.asString(pos.start, pos.end - 1)}
      val javaParamTSyms = jmsym.tpe.params map {p => p.tpe.typeSymbol}
      val newParams = for (i <- 0 until pNames.size)
                      yield {
                        val vd   = dd.vparamss.head(i)
                        val tsym = javaParamTSyms(i) 
                        val ascr = if(tsym.isParameter) "Any" else ascrTs(i)
                        pNames(i) + ": " + ascr
                      }
      val newArgs   = for ((name, ascr) <- pNames zip ascrTs) yield name + ".asInstanceOf["+ascr+"]"
      val returnsTParam = jmsym.tpe.resultType.typeSymbol.isParameter
      val dcast = if(returnsTParam) ".asInstanceOf[System.Object]" else ""
      val newBody   = "this." + dd.name + erasingDisambiguator + newArgs.mkString("(", ", ", ")") + dcast
      val newMethod = "override def " + dd.name + newParams.mkString("(", ", ", ")") + " = { "+newBody+" }"  // TODO not always `= {'
      spliceAfter(dd, newMethod)
      delOverrideModif(dd)
      rename(dd, dd.name + erasingDisambiguator)
      true
    }

    def collectPatches(tree: Tree) {
      tree match {
        case dd: DefDef if(dd.symbol.overriddenSymbol(juComparator) == juC_compare) =>
          addErasedOverload(dd, juC_compare)
        case dd: DefDef if(dd.symbol.overriddenSymbol(juCollection) == juC_add)                 => addErasedOverload(dd, juC_add)
        case dd: DefDef if(dd.symbol.overriddenSymbol(juList)       == juL_set)                 => addErasedOverload(dd, juL_set)
        case dd: DefDef if(dd.symbol.overriddenSymbol(juMap)        == juMap_put)               => addErasedOverload(dd, juMap_put)
        case dd: DefDef if(dd.symbol.overriddenSymbol(juMapEntry)   == juMapEntry_setValue)     => addErasedOverload(dd, juMapEntry_setValue)
        case dd: DefDef if(dd.symbol.overriddenSymbol(jucConcuMap)  == jucConcuMap_putIfAbsent) => addErasedOverload(dd, jucConcuMap_putIfAbsent)
        case dd: DefDef if(dd.symbol.overriddenSymbol(jucConcuMap)  == jucCM_replace2)          => addErasedOverload(dd, jucCM_replace2)
        case dd: DefDef if(dd.symbol.overriddenSymbol(jucConcuMap)  == jucCM_replace3)          => addErasedOverload(dd, jucCM_replace3)
        case _ => ()
      }
    }
  }

  /* ---------
   The sources given as input to jdk2ikvm employ (for example) << for its JVM semantics.
   When porting those sources to .NET, jdk2ikvm has to make explicit at the source level that
   ``the shift distance actually used is therefore always in the range 0 to 63, inclusive'' (for Long, for Int it's 0 to 31).
   The Scala.NET compiler just compiles << as usual into CLR's shl. Sources written from the start to target .NET
   will not assume  ``the shift distance actually used is therefore always in the range ...'', nor will the compiler, nor will the CLR of course.

   Details at http://lamp.epfl.ch/~magarcia/ScalaNET/2011Q1/PostJDK2IKVM.pdf
   
   --------- */

  private[Generating] class ShiftSemantics(patchtree: PatchTree) extends CallsiteUtils(patchtree) {

    // TODO scalaPrimitives.init would be useful here but throws a duplicate symbol error. Interim solution: code duplication!   

    private val shiftPrims: mutable.Map[Symbol, Int] = new mutable.HashMap()

    for(csym <- List(ByteClass, ShortClass, CharClass, IntClass, LongClass)) {
      addPrimitives(csym, nme.LSL, scalaPrimitives.LSL) // x << y
      addPrimitives(csym, nme.LSR, scalaPrimitives.LSR) // x >>> y
      addPrimitives(csym, nme.ASR, scalaPrimitives.ASR) // x >> y
    }

    private def addPrimitives(cls: Symbol, method: Name, code: Int) {
      val tpe = cls.info
      val sym = tpe.member(method)
      if (sym == NoSymbol)
        inform("Unknown primitive method " + cls + "." + method)
      for (s <- sym.alternatives)
        addPrimitive(s, code)
    }

    /** Add a primitive operation to the map */
    private def addPrimitive(s: Symbol, code: Int) {
      assert(!(shiftPrims contains s), "Duplicate primitive " + s)
      shiftPrims(s) = code
    }

    private def couldntPerformPatch(app: Apply) {
      warning(app.pos, "couldn't make explicit actual-shift-amount semantics " + asString(app))
    }

    def collectPatches(tree: Tree) {
      tree match {
        
        case app @ Apply(fun, args) if (shiftPrims contains fun.symbol) =>
          if(!args.head.pos.isRange) { couldntPerformPatch(app); return }
          val argPos = args.head.pos.asInstanceOf[RangePosition]
          val toRepl = patchtree.asString(argPos.start, argPos.end - 1)
          if(toRepl == "_") { couldntPerformPatch(app); return; }
          // we have to logical bitwise AND the argument, no matter what shift operation. But for clarity made explicit.
          val code = shiftPrims(fun.symbol)
          code match {
            case   scalaPrimitives.LSL    // Shift(LSL, resKind) i.e. x << y
                 | scalaPrimitives.LSR    // Shift(LSR, resKind) i.e. x >>> y
                 | scalaPrimitives.ASR    // Shift(ASR, resKind) i.e. x >> y
            =>
              val Select(receiver, _) = fun
              val mask = if(receiver.tpe.typeSymbol == LongClass) "0x3f" else "0x1f"
              val buf = mutable.ListBuffer.empty[PatchCmd]
              // parens around rewritten arg
              buf += new Insert("(")
              // parens around original arg
              buf += new Insert("(")
              // original arg
              buf += new Patcheable(argPos.start, argPos.end - 1)
              buf += new Insert(") & " + mask + ")")
              patchtree.tryPatch(argPos.start, argPos.end - 1, buf.toList, dropGapsOK = false)

            case _                      => ()
          }

        case _ => ()
      }
    }

  }

  /* ---------
   Whenever a `final lazy val` definition is found in sources, it's rewritten leaving out the `final'. 
   Otherwise peverify complaints as in the example: 

   Error: [scalalib.dll : scala.util.regexp.WordExp+Letter::isNullable][offset 0x00000026] 
   Cannot change initonly field outside its .ctor.
   
   --------- */

  private[Generating] class DontFinalLazyVal(patchtree: PatchTree) extends CallsiteUtils(patchtree) {

    def collectPatches(tree: Tree) {
      import scala.reflect.generic.ModifierFlags
      tree match {
        case vd: ValDef if (hasModifier(vd, ModifierFlags.FINAL) && hasModifier(vd, ModifierFlags.LAZY) ) => delFinalModif(vd)
        case _ => ()
      }
    }

  }

  /* ------------------------ The main patcher ------------------------ */

  class RephrasingTraverser(patchtree: PatchTree) extends Traverser {

    private val stringAndObjectContract = new StringAndObjectContract(patchtree)
    private val missingErasedOverloads  = new MissingErasedOverloads(patchtree)
    private val magicForInterfaces      = new MagicForInterfaces(patchtree)
    private val ghostInterfaces         = new GhostInterfaces(patchtree)
    private val exceptionsRewritings    = new ExceptionsRewritings(patchtree)
    private val fullyQualifyJavaLang    = new FullyQualifyJavaLang(patchtree)
    private val missingArgForRepeated   = new MissingArgForRepeated(patchtree)
    private val oneOrMoreArgForRepeated = new OneOrMoreArgForRepeated(patchtree)
    private val annIgnorer              = new AnnIgnorer(patchtree)

    private val upcasters = for (
        u <- upcastings;
        val csym = u._1;
        val newTypeRef = u._2;
        val leaveAsIs = leaveAsIsNames.get(csym) getOrElse Nil
      ) yield new IKVMUpcaster(patchtree, csym, newTypeRef, leaveAsIs)

    private val typeArgEraser           = new TypeArgEraser(patchtree)
    private val downCaster              = new Downcaster(patchtree)
    private val typesSubsttter          = new TypesSubsttter(patchtree)

    private val shiftSemantics          = new ShiftSemantics(patchtree)
    private val dontFinalLazyVal        = new DontFinalLazyVal(patchtree)

    override def traverse(tree: Tree): Unit = {

      stringAndObjectContract  collectPatches  tree
      missingErasedOverloads   collectPatches  tree
      magicForInterfaces       collectPatches  tree
      ghostInterfaces          collectPatches  tree
      exceptionsRewritings     collectPatches  tree
      fullyQualifyJavaLang     collectPatches  tree

      missingArgForRepeated    collectPatches  tree
      oneOrMoreArgForRepeated  collectPatches  tree

      annIgnorer               collectPatches  tree
      upcasters foreach { uc => uc collectPatches  tree }
      typeArgEraser            collectPatches  tree
      downCaster               collectPatches  tree
      typesSubsttter           collectPatches  tree

      shiftSemantics           collectPatches  tree
      dontFinalLazyVal         collectPatches  tree

      // TODO type refs to java.lang types appear often unqualified, and IKVM won't like that. Notice this rewrite may interfere with others (upcasting? erasing)
      // TODO @serializable as per IKVM
      
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
