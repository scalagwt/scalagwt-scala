/* jdk2ikvm -- Converts JDK-based Scala source files to use the IKVM library instead.
 * Copyright 2011 Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaNET/
 */

package scala.tools
package jdk2ikvm

import scala.tools.nsc.{plugins, Global, Phase}
import plugins.PluginComponent
import java.io.File

object JDK2IKVMPlugin
{
	val PluginName = "jdk2ikvm"
  /** This is the name of the option that specifies the output directory.*/
  val OutputDirectoryOptionName = "output-directory:"
  val ReplacementsDirectoryOptionName = "replacements-directory:"
}
/* The standard plugin setup.  The main implementation is in JDK2IKVM.  The entry point is JDK2IKVM.generateOutput */
class JDK2IKVMPlugin(val global: nsc.Global) extends JDK2IKVM
{

  import global._
	import JDK2IKVMPlugin._

	val name = PluginName
	val description = "A plugin to process JDK-based Scala sources files to make them IKVM-compatible."
	lazy val components = List[PluginComponent](Component)

  /** The directory to which the converted sources will be written. */
	override var outputDirectory: Option[File] = None
	
	/** The directory where replacements are stored. */
  override var replacementsDirectory: Option[File] = None

  var afterWhichPhase  = "typer"
  var beforeWhichPhase = "superaccessors"

	override def processOptions(options: List[String], error: String => Unit)
	{
		for(option <- options)
		{
			if(option.startsWith(OutputDirectoryOptionName))
				outputDirectory = Some(new File(option.substring(OutputDirectoryOptionName.length)))
			else if (option.startsWith(ReplacementsDirectoryOptionName))
			  replacementsDirectory = Some(new File(option.substring(ReplacementsDirectoryOptionName.length)))
		}
    if(outputDirectory == None)
      error("Output directory option for " + name + " plugin not understood.")
    if(replacementsDirectory == None)
      error("Replacements directory option for " + name + " plugin not understood.")
    if(settings.sourcepath.isDefault)
      error("-sourcepath was not specified.")
	}
	override val optionsHelp: Option[String] =
	{
		val prefix = "  -P:" + name + ":"
    val msg = """|%1$s%2$s<name>            Set the directory to which the converted sources will be written.
      |%1$s%3$s<name>      Set the directory where replacement files are stored.
      |""".stripMargin.format(prefix, OutputDirectoryOptionName, ReplacementsDirectoryOptionName)
		Some(msg)
	}

	/* For source compatibility between 2.7.x and 2.8.x */
	private object runsBefore { def :: (s: String) = s }
  private abstract class CompatiblePluginComponent(afterPhase: String, beforePhase: String) extends PluginComponent
  {
    override val runsAfter  = List(afterPhase)
    override val runsBefore = List(beforePhase)
  }
  private object Component extends CompatiblePluginComponent(afterWhichPhase, beforeWhichPhase)
	{
		val global = JDK2IKVMPlugin.this.global
		val phaseName = JDK2IKVMPlugin.this.name
		def newPhase(prev: Phase) = new JDK2IKVMPhase(prev)
	}

	private class JDK2IKVMPhase(prev: Phase) extends Phase(prev)
	{
		val global = JDK2IKVMPlugin.this.global
    def name = JDK2IKVMPlugin.this.name
    def run = generateOutput() 
	}
  
}