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

      val shouldSkip = {
        val skips = List("Predef.scala") // , "Manifest.scala", "ClassManifest.scala"
        skips exists (fname => unit.source.file.path.endsWith(java.io.File.separator + fname))
        false
      }

      if(false && shouldSkip) {
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
      rephrasingTraverser traverse unit.body
    }
    patchtree
  }

}
