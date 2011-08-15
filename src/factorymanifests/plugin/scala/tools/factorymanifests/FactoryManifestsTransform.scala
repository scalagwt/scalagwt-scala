
package scala.tools.factorymanifests
import scala.collection.mutable
import scala.tools.nsc.transform.{Transform, TypingTransformers}
import scala.tools.nsc.plugins.PluginComponent

/**
 * Implements 'factorymanifests' compiler phase that provides alternative implementation of
 * Manifests that use static factories for Array creation. 
 *
 * Canonical Manifest implementation in Scala uses reflection for generic Array creation.
 * In environment where reflection is not supported (like GWT) this approach for generic
 * array handling is not feasible. This phase rewrites calls to ClassManifest.classType
 * methods into FactoryManifest.classType ones. Also, we generate anonymous classes that
 * implement FactoryManifest.Factory[T] trait for every concrete T <: AnyRef.
 * 
 * This way we have implementation of Manifests that does not depend on reflection. The price
 * we pay is increased number of classes needed to support generic arrays.
 * 
 * It's worth noting that this phase should be run just after 'refchecks' phase (and for sure
 * before flatten) so anonymous classes we generate are lifted to top-level ones. We want to
 * run after 'refcheck' because it means we are running after pickler. It's desirable because
 * we don't want pickling information for our anonymous classes to be generated.
 */
abstract class FactoryManifestsTransform extends PluginComponent with Transform with TypingTransformers {
  import global._

  val phaseName = "factorymanifests"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new Trans(unit)

  /**
   * The main transformer of this phase.
   */
  private class Trans(cunit: CompilationUnit) extends TypingTransformer(cunit) {
    
    /** How deeply Factories should be nested? Each level corresponds to one additional anonymous class */
    val nestingLevel = 1

    lazy val ClassManifestModule = definitions.getModule("scala.reflect.ClassManifest")
    
    lazy val classTypeMethod = definitions.getMember(ClassManifestModule, "classType") suchThat {
      //we want to match this one: def classType[T <: AnyRef](clazz: JClass[_]): ClassManifest[T]
      _.tpe.params.size == 1
    }
    
    lazy val FactoryManifestModule = definitions.getModule("scala.reflect.FactoryManifest") 
    
    lazy val classTypeFactoryMethod = definitions.getMember(FactoryManifestModule, "classType") suchThat {
      //we want to match this one: def classType[T <: AnyRef](clazz: JClass[_], factory: Factory[T]): ClassManifest[T]
      _.tpe.params.size == 2
    }
    
    lazy val factoryTrait = definitions.getClass("scala.reflect.FactoryManifest.Factory")
    
    override def transform(tree: Tree): Tree = tree match {
      case tree@Apply(fun, List(c: Literal)) if fun.symbol == classTypeMethod =>
        assert(c.value.tag == ClassTag)
        val forTpe = {
          val TypeApply(_, List(tp: TypeTree)) = fun
          tp.tpe
        }
        val factoryExpr = mkFactoryClass(forTpe, currentOwner, nestingLevel)
        localTyper.typedPos(tree.pos) {
          gen.mkMethodCall(classTypeFactoryMethod, List(forTpe), List(c, factoryExpr))
        }
      case x => super.transform(x)
    }
    
    def mkFactoryClass(param: Type, owner: Symbol, nest: Int): Tree = {
      import scala.reflect.internal.Flags._
      val arrayTpe: Type = appliedType(definitions.ArrayClass.tpe, List(param))
      def mkFactoryTpe(t: Type): Type = appliedType(factoryTrait.tpe, List(t))
      val factoryTpe = mkFactoryTpe(param)
      val arrayFactoryTpe = mkFactoryTpe(arrayTpe)
      
      def mkNewInstance(owner: Symbol): DefDef = {
        val name = newTermName("newInstance")
        val s = owner.newMethod(name)
        val len = {
          val ss = s.newValueParameter(NoPosition, "len") setInfo definitions.IntClass.tpe
          ValDef(ss) setType ss.tpe
        }
        s setInfo MethodType(s newSyntheticValueParams List(len.tpe), arrayTpe)
        val newArr = New(TypeTree(arrayTpe), List(List(Ident(len.symbol))))
        DefDef(Modifiers(FINAL), name, Nil, List(List(len)), TypeTree(arrayTpe), newArr) setSymbol s
      }
      def mkforArrayOf(owner: Symbol): DefDef = {
        val name = newTermName("forArrayOf")
        val s = owner.newMethod(name)
        s setInfo MethodType(Nil, arrayFactoryTpe)
        val body = if (nest == 0) 
          gen.mkSysErrorCall("FactoryManifests support limited depth of generic Array nesting.") else
          mkFactoryClass(arrayTpe, s, nest-1)
        DefDef(Modifiers(FINAL), name, Nil, Nil, TypeTree(arrayFactoryTpe), body) setSymbol s
      }
      
      val anonClass = owner newAnonymousClass owner.pos setFlag (FINAL | SYNTHETIC)
      val parents = List(definitions.ObjectClass.tpe, factoryTpe)
      anonClass setInfo ClassInfoType(parents, new Scope, anonClass)
      val members = List(mkNewInstance(anonClass), mkforArrayOf(anonClass))
      members foreach { x => anonClass.info.decls enter x.symbol }
      Block(ClassDef(anonClass, NoMods, List(Nil), List(Nil), members, owner.pos),
          Typed(New(TypeTree(anonClass.tpe), List(List())),
              TypeTree(factoryTpe)))
    }
  }
}
