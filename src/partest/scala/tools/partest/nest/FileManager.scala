/* NEST (New Scala Test)
 * Copyright 2007-2011 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{File, FilenameFilter, IOException, StringWriter,
                FileInputStream, FileOutputStream, BufferedReader,
                FileReader, PrintWriter, FileWriter}
import java.net.URI
import scala.tools.nsc.io.{ Path, Directory, File => SFile }
import sys.process._
import scala.collection.mutable

trait FileManager {  
  /**
   * Compares two files using a Java implementation of the GNU diff
   * available at http://www.bmsi.com/java/#diff.
   *
   * @param  f1  the first file to be compared
   * @param  f2  the second file to be compared
   * @return the text difference between the compared files
   */
  def compareFiles(f1: File, f2: File): String = {
    val diffWriter = new StringWriter
    val args = Array(f1.getAbsolutePath(), f2.getAbsolutePath())
    
    DiffPrint.doDiff(args, diffWriter)
    val res = diffWriter.toString
    if (res startsWith "No") "" else res
  }

  /**
   * Compares two directories using compareFiles method.
   */
  def compareDirectories(d1: Directory, d2: Directory): List[String] = {
    def explainNoCorresponding(x: Path, in: Path) =
      "'%s' has no corresponding file in '%s'".format(x, in)
    //obtain relative sets of relative paths that can be compared
    //so we can find files with missing corresponding file in another directory
    val fs1 = d1.deepFiles.map(d1.relativize(_)).toSet
    val fs2 = d2.deepFiles.map(d2.relativize(_)).toSet
    val noCorresponding1 = (fs2 -- fs1).toList.map(explainNoCorresponding(_, d1))
    val noCorresponding2 = (fs1 -- fs2).toList.map(explainNoCorresponding(_, d2))
    val noCorresponding = noCorresponding1 ++ noCorresponding2
    if (noCorresponding != Nil)
      noCorresponding
    else {
      val compare = for (x <- fs1.toList) yield (d1 / x, d2 / x)
      compare.flatMap {
        case (f1, f2) => compareFiles(f1.jfile, f2.jfile) match {
          case "" => Nil
          case msg => "Files '%s' and '%s' are different:\n%s".format(f1, f2, msg) :: Nil
        }
      }
    }
  }
  
  def testRootDir: Directory
  def testRootPath: String

  var JAVACMD: String
  var JAVAC_CMD: String

  var CLASSPATH: String
  var LATEST_LIB: String

  var showDiff = false
  var updateCheck = false
  var showLog = false
  var failed = false

  var SCALAC_OPTS = PartestDefaults.scalacOpts
  var JAVA_OPTS   = PartestDefaults.javaOpts
  var timeout     = PartestDefaults.timeout
  // how can 15 minutes not be enough? What are you doing, run/lisp.scala?
  // You complete in 11 seconds on my machine.
  var oneTestTimeout = 60 * 60 * 1000
  
  /** Only when --debug is given. */
  lazy val testTimings = new mutable.HashMap[String, Long]
  def recordTestTiming(name: String, milliseconds: Long) =
    synchronized { testTimings(name) = milliseconds }
  def showTestTimings() {
    testTimings.toList sortBy (-_._2) foreach { case (k, v) => println("%s: %s".format(k, v)) }
  }

  def getLogFile(dir: File, fileBase: String, kind: String): File =
    new File(dir, fileBase + "-" + kind + ".log")

  def getLogFile(file: File, kind: String): File = {
    val dir      = file.getParentFile
    val fileBase = basename(file.getName)

    getLogFile(dir, fileBase, kind)
  }

  def logFileExists(file: File, kind: String) =
    getLogFile(file, kind).canRead
  
  def overwriteFileWith(dest: File, file: File) =
    dest.isFile && copyFile(file, dest)
  
  def copyFile(from: File, dest: File): Boolean = {    
    if (from.isDirectory) {
      assert(dest.isDirectory, "cannot copy directory to file")
      val subDir:Directory = Path(dest) / Directory(from.getName)
      subDir.createDirectory()
      from.listFiles.toList forall (copyFile(_, subDir))
    }
    else {
      val to = if (dest.isDirectory) new File(dest, from.getName) else dest
      
      try {
        SFile(to) writeAll SFile(from).slurp()
        true
      }
      catch { case _: IOException => false }
    }
  }

  def mapFile(file: File, replace: String => String) {
    val f = SFile(file)
    
    f.printlnAll(f.lines.toList map replace: _*)
  }
}
