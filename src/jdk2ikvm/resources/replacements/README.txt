This folder contains replacements for some of .scala
files from standard library.

We need those replacements for two reasons:

  * there're problematic to process using automatic patching
    so we give up with that technique and just modify them by
    hand and put modified version here
  * original files are very much platform-dependent and we
    really have to provide our own implementation from scratch
    examples include: Console.scala, ScalaRunTime.scala, etc.
    
Below is a more detailed description of every file found in this directory.

scala/Array.scala
-----------------
Arrays are implemented using mix of reflection tricks and compile-time generated Manifests.
We have to analyze the whole thing and provide our own implementation of this logic without
using reflection. Since JavaScript doesn't care about the type of a thing we are putting
into an array we probably can skip the whole magic altogether.

scala/collection/immutable/StringLike.scala
-------------------------------------------
Depends on String.format. I found it easier to patch it by hand. Should be removed from here
ASAP.

scala/collection/mutable/BufferLike.scala
-----------------------------------------
Creepy one. Since in GWT there's one Object.clone() method Scala compiler gets really confused
when trying to compile this trait as it's uses clone() method in it's definition. I believe
the whole mess is caused by somehow broken code in a subtle way but I don't understand those
things enough. I'll ask other people for help.

scala/collection/mutable/StringBuilder.scala
--------------------------------------------
Depends on StringBuilder.reverse that is not supported by GWT. Maybe this can be fixed upstream?

scala/compat/Platform.scala
---------------------------
Platform is a keyword. We need to reimplement this from scratch in GWT environment.

scala/math/package.scala
------------------------
Depends on java.lang.Math.ulp method that is not supported by GWT. Probably should be patched
automatically if GWT cannot be fixed.

scala/MathCommon.scala
----------------------
See above.

scala/package.scala
-------------------
Depends on java.lang.AbstractMethodError. Maybe we could create it in GWT?
Depends on java.lang.Thread (currentThread method). Hopefully can be nuked or
moved to sys package.

scala/reflect/*Manifest.scala
-----------------------------
Goes without the comment. We need to reimplement this stuff to the extent
that is really needed. The only reason why we need Manifests is to support
Arrays. Once we figure this out, we should have a very simple implementation
of Manifests that doesn't provide any reflective functionality at all.

scala/runtime/RichChar.scala
----------------------------
Depends on java.lang.Character.* methods that are not supported in GWT.
Either fix in GWT or remove those things automatically.

scala/runtime/ScalaRunTime.scala
--------------------------------
Lots of platform specific stuff. We need to reimplement it from scratch.

scala/runtime/ArrayRuntime.java
-------------------------------
Depends on cloning.

scala/runtime/BoxesRunTime.java
-------------------------------
Depends on exception types unsupported by GWT.

scala/Symbol.scala
------------------
Depends on weak references for caching. JavaScript doesn't have anything
corresponding to weak references. We probably should move caching stuff
somewhere else (e.g. ScalaRuntime) so this could be left as is and
only caching implementation would be replaced. At the moment, it uses
simple HashMap for caching so Symbols are never gc'ed.

scala/util/control/Exception.scala
----------------------------------
Depends on JVM specific exception types that are not supported by GWT. Those include:

  * InvocationTargetException
  * ControlThrowable
  * InterruptedException
  
Also, depends on reflection. I believe this stuff should be refactored or not referenced
from the rest of library.
  
scala/xml/parsing/TokenTests.scala
----------------------------------
The whole XML stuff is a mess. We shouldn't support scala.xml.parsing in GWT at all.
However, this object is being referenced from other places in xml package.
This object has to be rewritten because of dependencies on methods in RichChar
that are removed. Check scala/runtime/RichChar for details.

