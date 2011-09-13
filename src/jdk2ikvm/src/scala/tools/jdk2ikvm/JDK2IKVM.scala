/* jdk2ikvm -- Converts JDK-based Scala source files to use the IKVM library instead.
 * Copyright 2011 Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaNET/
 */

package scala.tools
package jdk2ikvm

import scala.tools.nsc.{ast, plugins, symtab, util, Global}
import plugins.Plugin
import java.io._
import nsc.util.{Position, SourceFile}

/** The actual work is done here. */
abstract class JDK2IKVM
  extends Plugin
  with gwt.patchcmds.Generating
{

  import global._

/* -------- PLUGIN OPTIONS ------------------------------------------- */    

  /** The directory to which the sources will be written. */
  var outputDirectory: Option[File]
  /** The directory where replacements are stored. */
  var replacementsDirectory: Option[File]
  /** The directory against which the input source paths will be relativized. */
  def baseDirectory: Option[File] = Some(new File(settings.sourcepath.value))

/* -------- TIME STATISTICS ------------------------------------------- */    

  var accTimeRewriteMs = 0L
  var accTimePrettyPrintMs = 0L

  private def hhmmss(milliseconds: Long): String = {
    val seconds : Long = ((milliseconds / 1000) % 60);
    val minutes : Long = ((milliseconds / 1000) / 60);
    String.format("%2d min, %2d sec", java.lang.Long.valueOf(minutes), java.lang.Long.valueOf(seconds))
  }

  private def trackDuration[T](updateAccumulated: (Long) => Unit)(action: => T): T = {
    val startTime = System.currentTimeMillis()
    try { action }
    finally {
      val delta = System.currentTimeMillis() - startTime
      updateAccumulated(delta)
    }
  }

/* -------- ADAPTING TO IKVM ------------------------------------------- */

	/** The entry method for producing a set of Scala files. */
	def generateOutput()
	{
    if(settings.debug.value) {
      settings.debug.value = false
      warning(NoPosition, "Because jdk2ikvm frequently gets the string representation of a type (as in `tree.tpe.toString`), " + 
                          "-Ydebug has been disabled. Otherwise, the resulting strings would contain more details " + 
                          "than one would expect in source code, making the sources non-compilable after conversion.")
    }
    val startTimeGenOutput = System.currentTimeMillis()
    for(unit <- currentRun.units) {

      // preparing the output file
      val sourceFile = unit.source.file.file
      val relativeSourcePath = getRelativeSourcePath(sourceFile)
      val outputFile = new File(outputDirectory.get, relativeSourcePath)
      outputFile.getParentFile.mkdirs()
      
      val replaced = {
        val basePath = replacementsDirectory.get
        assert(basePath.exists)
        val javaClasspath = System.getProperty("java.class.path")
        val f = new File(basePath, relativeSourcePath)
        if (f.exists()) {
          FileUtil.write(new java.io.FileInputStream(f), outputFile)
          true
        } else false
      }

      val shouldSkip = {
        val prefixes = Set(
            "scala/App.scala",
            "scala/Application.scala",
            "scala/collection/parallel",
            "scala/collection/Parallelizable.scala", 
            "scala/collection/CustomParallelizable.scala",
            "scala/collection/generic/GenericParCompanion.scala",
            "scala/collection/generic/GenericParTemplate.scala",
            "scala/collection/generic/GenericParTemplate.scala",
            "scala/collection/generic/ParFactory.scala",
            "scala/collection/generic/ParMapFactory.scala",
            "scala/collection/generic/ParSetFactory.scala",
            "scala/collection/generic/CanCombineFrom.scala",
            "scala/collection/generic/HasNewCombiner.scala",
            //breaks GWT due to bugs in scala (broken signatures) and bugs in jribble backend (broken jribble output)
            //excluding it for now
            "scala/collection/immutable/RedBlack.scala",
            //TreeMap and TreeSet depend on RedBlack.scala
            "scala/collection/immutable/TreeMap.scala",
            "scala/collection/immutable/TreeSet.scala",
            //depends on threads
            "scala/collection/generic/Signalling.scala",
            //depends on scala.io
            "scala/collection/immutable/PagedSeq.scala",
            "scala/collection/mutable/WeakHashMap.scala",
            "scala/concurrent/",
            "scala/parallel/",
            "scala/io/",
            //TODO(grek): Check if we can provide our own implementation of properties handling that is IO-free, e.g. with hardcoded map of values
            "scala/sys/BooleanProp.scala",
            "scala/sys/Prop.scala",
            "scala/sys/PropImpl.scala",
            "scala/sys/ShutdownHookThread.scala",
            "scala/sys/SystemProperties.scala",
            "scala/sys/process/",
            //soft/weak references, needed for structural types but we don't support them
            "scala/ref/",
            //we cannot exclude the whole reflect package, compiler depends on some classes and we need manifests
            "scala/reflect/generic/",
            "scala/reflect/Print.scala",
            //refers to reflection
            "scala/reflect/ScalaBeanInfo.scala",
            "scala/reflect/NameTransformer.scala",
            "scala/reflect/ReflectionUtils.scala",
            //refers to reflection
            "scala/runtime/MethodCache.scala",
            //depends on reflections, find out if we can do something about it
            "scala/Enumeration.scala",
            //depends on org.xml.* stuff, depends on I/O, etc.
            "scala/xml/package.scala",
            //not sure what it is, but doesn't seem we need it 
            "scala/text/",
            //not sure what it is, but doesn't seem we need it
            "scala/testing/",
            "scala/xml/include/sax/",
            //everything apart from XhmtlEntities and TokenTests, probably those two should be moved to some other package
            "scala/xml/parsing/ConstructingHandler.scala",
            "scala/xml/parsing/ConstructingParser.scala",
            "scala/xml/parsing/DefaultMarkupHandler.scala",
            "scala/xml/parsing/ExternalSources.scala",
            "scala/xml/parsing/FactoryAdapter.scala",
            "scala/xml/parsing/FatalError.scala",
            "scala/xml/parsing/MarkupHandler.scala",
            "scala/xml/parsing/MarkupParser.scala",
            "scala/xml/parsing/MarkupParserCommon.scala",
            "scala/xml/parsing/NoBindingFactoryAdapter.scala",
            "scala/xml/parsing/ValidatingMarkupHandler.scala",
            "scala/xml/parsing/XhtmlParser.scala",
            "scala/xml/pull/XMLEventReader.scala",
            "scala/xml/persistent/",
            "scala/xml/factory/",
            //we are removing this because it depends on sys/Prop.scala, so it might be included again once props are being handled
            "scala/util/control/NoStackTrace.scala",
            "scala/util/parsing/",
            "scala/util/Properties.scala",
            //depends on I/O (serialization)
            "scala/util/Marshal.scala",
            //depends on java.util.regex
            "scala/util/matching/",
            //depends on Console
            "scala/util/logging/")
        prefixes exists (x => relativeSourcePath startsWith x)
      }
      
      if (replaced) {
        scala.Console.println("[jdk2ikvm] replaced: " + unit.source.file.path)
      } else if(shouldSkip) {
        scala.Console.println("[jdk2ikvm] not writing: " + unit.source.file.path)
      } else if(unit.isJava) {
        // serialize as is
        val f: java.io.File = unit.source.file.file
        FileUtil.write(new java.io.FileInputStream(f), outputFile)
      } else {
        val billToRewriting: (Long) => Unit      = (elapsed => accTimeRewriteMs     += elapsed)
        val billToPrettyPrinting: (Long) => Unit = (elapsed => accTimePrettyPrintMs += elapsed)
                                                                                   
        val patchtree = trackDuration(billToRewriting)(collectPatches(unit)) 
        trackDuration(billToPrettyPrinting)(patchtree.serialize(outputFile))  

      }
		}
    scala.Console.println("[jdk2ikvm] time to prepare output: " + hhmmss(accTimeRewriteMs))
    scala.Console.println("[jdk2ikvm] time to serialize:      " + hhmmss(accTimePrettyPrintMs))
    scala.Console.println("[jdk2ikvm] wall-clock time:        " + hhmmss(System.currentTimeMillis - startTimeGenOutput))
	}

  /** Relativizes the path to the given Scala source file to the base directory. */
  private def getRelativeSourcePath(source: File): String =
  {
    baseDirectory match
    {
      case Some(base) =>
        FileUtil.relativize(base, source) match
        {
          case Some(relative) => relative
          case None => error("Source " + source + " not in base directory " + base); ""
        }
      case None => source.getName
    }
  }

  def collectPatches(unit: CompilationUnit): PatchTree = {
    val patchtree = new PatchTree(unit.source)
    if (!unit.isJava) {
      val rephrasingTraverser = new RephrasingTraverser(patchtree)
      try {
        rephrasingTraverser traverse unit.body
      } catch {
        case e: Exception =>
          scala.Console.err.println("Exception " + e + " thrown when traversing " + unit.source.file.path)
          throw e
      }
    }
    patchtree
  }

}
