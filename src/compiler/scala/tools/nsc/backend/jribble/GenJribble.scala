/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 */


package scala.tools.nsc.backend.jribble

import java.io._

import scala.collection.{mutable=>mut}
import scala.collection.mutable.ListBuffer

import com.google.protobuf.GeneratedMessage
import com.google.protobuf.TextFormat
 
import scala.tools.nsc._
import scala.tools.nsc.symtab.Flags._


/** The Jribble backend. */
abstract class GenJribble 
extends SubComponent 
with JribbleAnalysis 
with JavaDefinitions
with JribbleNormalization
{
  val global: Global 
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
    
    def emitProto(proto: GeneratedMessage, symbol: Symbol, suffix: String) {
      val out = new BufferedOutputStream(new FileOutputStream(getFile(symbol, suffix)))
      if (settings.jribbleText.value) {
        val writer = new OutputStreamWriter(out, "UTF-8")
        TextFormat.print(proto, writer)
        writer.flush()
      } else {
        proto.writeTo(out)
      }
      out.close()
    }
      
    private def gen(tree: Tree, unit: CompilationUnit) {
      object converter extends ProtobufConverter {
         val global: GenJribble.this.global.type = GenJribble.this.global
         val typeKinds = GenJribble.this.typeKinds
         val scalaPrimitives = GenJribble.this.scalaPrimitives
         val freshNames = unit.fresh
      }
      def genClass(clazz: ClassDef) {
        val proto = converter.declaredType(clazz)

        // logic that adds static forwarders. Copied from GenJVM (see genClass method)
        val lmoc = clazz.symbol.companionModule
        // it must be a top level class (name contains no $s)
        def isCandidateForForwarders(sym: Symbol): Boolean =
          atPhase (currentRun.picklerPhase.next) {
            !(sym.name.toString contains '$') && (sym hasFlag MODULE) && !sym.isImplClass && !sym.isNestedClass
          }

        // add static forwarders if there are no name conflicts; see bugs #363 and #1735
        if (lmoc != NoSymbol && !isStaticModule(clazz.symbol) && !isInterface(clazz.symbol)) {
          if (isCandidateForForwarders(lmoc) && !settings.noForwarders.value) {
              log("Adding forwarders to existing class '%s' found in module '%s'".format(clazz.symbol, lmoc))
              converter.addForwarders(lmoc.moduleClass, proto)
          }
        }


        // print the main class
        emitProto(proto.build, clazz.symbol, converter.moduleSuffix(clazz.symbol) + ".jribble")
        
        // If it needs a mirror class, add one
        if (isStaticModule(clazz.symbol) && isTopLevelModule(clazz.symbol) && clazz.symbol.companionClass == NoSymbol) {
          val mirror = converter.mirrorClassFor(clazz.symbol)
          emitProto(mirror, clazz.symbol, ".jribble")
        }
      }
        
      def gen(tree: Tree) {
        tree match {
          case EmptyTree =>
          case PackageDef(_, stats) => stats foreach gen
          case clazz:ClassDef => genClass(clazz)
        }
      }  
      
      gen(tree)
    }
  }
}
