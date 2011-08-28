This directory contains jdk2ikvm tool written by Migeul Garcia. Source code of jdk2ikvm plug-in has been adapted to needs of Scala+GWT project.

Original source can be found at http://lampsvn.epfl.ch/svn-repos/scala/scala-experimental/trunk/jdk2ikvm/src/scala/tools/jdk2ikvm/

In a future it would make sense to share common code between Scala+GWT and Scala.NET projects. That would involve proper packaging jdk2ikvm as compiler plug-in with an option to provide custom patch commands. We might think about it once Scala moves to Sbt as there is a plan for having common infrastructure supporting compiler plug-ins used for Scala development.
