package scala.tools
package jdk2ikvm
package patchcmds

trait Patching {

  def pluginError(pos: nsc.util.Position, msg: String)
  def warning(pos: nsc.util.Position, msg: String)
  def info(pos: nsc.util.Position, msg: String) 

  type Bracket = Tuple2[Int, Int]

  /* ------- object Patch owns utility functions ---------------------  */

  object Patch {
    def allRanged(cmds: List[PatchCmd]): List[Ranged] = cmds collect { case r : Ranged => r }
    def boundaries(cmds: List[PatchCmd]): (Int, Int) = {
      val (starts, ends) = allRanged(cmds) map (r => (r.from, r.to)) unzip
      val a = starts reduceLeft math.min
      val b = ends reduceLeft math.max
      (a, b)
    }
    def encloses(outer: Bracket, inner: Bracket): Boolean = (outer._1 <= inner._1) && (inner._2 <= outer._2)
    def encloses(outer: Ranged, inner: Ranged): Boolean   = encloses(outer.bracket, inner.bracket)
    def partitions(a: Int, b: Int, cmds: List[PatchCmd]): Boolean = {
      // cmds not required to follow original order, i.e. cmds may shuffle fragments around (and in general do).  
      val rs = allRanged(cmds) sortBy (_.from)
      if(a != rs.head.from) return false
      if(b != rs.last.to) return false
      var prev = rs.head
      for(curr <- rs.tail) {
        if(prev.to + 1 != curr.from) return false
        prev = curr
      }
      return true
    }
  }

  /* ------- a patch command: an insert, or a bracketed command (patch, or replace) -------- */

  trait PatchCmd

  abstract class Ranged(val from: Int, val to: Int) extends PatchCmd {
    assert(0 <= from); assert(0 <= to); assert(from <= to)
    val bracket = (from, to)
    def showPos = "["+from+","+to+"]"
  }

  class Patcheable(from: Int, to: Int) extends Ranged(from, to) {
    private[Patching] var patches : List[PatchCmd] = Nil

    private[Patching] def tryPatch(a: Int, b: Int, cmds: List[PatchCmd], dropGapsOK: Boolean): Option[Patcheable] = {
      val ab = (a, b)
      if(!Patch.encloses(bracket, ab)) {
        return None
      }
      if(patches.isEmpty) {
        if(ab != bracket) {
          // for ab a strict subset, pad with before and after patches, then recurse to middlePatch
          val leftPatchL  = if(from < a) List(new Patcheable(from, a - 1)) else Nil
          val middlePatch = new Patcheable(a, b)
          val rightPatchL = if(b < to) List(new Patcheable(b + 1, to)) else Nil
          patches = leftPatchL ::: List(middlePatch) ::: rightPatchL
          middlePatch.tryPatch(a, b, cmds, dropGapsOK)
        } else {
          // add patches at this node
          assert(dropGapsOK || Patch.partitions(from, to, cmds)) 
          patches = cmds
          Some(this)
        }
      } else {
        val candidate = patches collect { case p : Patcheable => p } find (p => Patch.encloses(p.bracket, ab))
        if(candidate.isEmpty) {
          // special case: forgive the calling site for trying to replace identical contents
          val prevRepl = patches collect { case r : Replace => r } find (r => r.bracket == ab)
          if(prevRepl.isDefined) {
            cmds match {
              case List(r : Replace) if (r.text == prevRepl.get.text) => return Some(this)
              case _ => ()
            }
          }
        }
        // normal case: delegate to innermost enclosing Patcheable
        candidate flatMap (p => p.tryPatch(a, b, cmds, dropGapsOK))
      }
    }
  }

  class Replace(from: Int, to: Int, val text: String) extends Ranged(from, to)

  class Insert(val text: String) extends PatchCmd

  /* ------- Tree traversers interact with a PatchTree rather than instances of the above (Facade pattern) -------- */

  class PatchTree(val sf: nsc.util.SourceFile) {

    val root = new Patcheable(0, sf.content.size - 1)
    private var replQueue : List[Replace] = Nil

    def tryPatch(from: Int, to: Int, cmds: List[PatchCmd], dropGapsOK: Boolean) = {
      // val fname = sf.file.path
      val res = root.tryPatch(from, to, cmds, dropGapsOK)
      if(res.isEmpty) {
        val toRepl = asString(from, to)
        val p = new nsc.util.OffsetPosition(sf, from)
        pluginError(p, "got confused (likely by parens) trying to rewrite \n\t" + toRepl + 
                       "\ninto" + "\n\t" + asString(cmds) + "\nat" )
      }
    }

    /* ------- Text-for-text replacements (custom handling wrt tryPatch(Replace) but faster) -------- */

    def replace(from: Int, to: Int, text: String) {
      // val fname = sf.file.path
      val cmd = new Replace(from, to, text)
      replQueue = cmd :: replQueue
    }

    /* Motivation: in general we strive for non-conflicting replacements
     * and thus want to avoid (partially) overlapping replace commands.
     * However when mapping types to adapt to IKVM, a replacement (say, java.lang.Object --> Object)
     * may occur inside another (say, eliminating type arguments to IKVM classes whose JDK counterpart receives them).
     * In order to avoid *these* conflicting replacements, we should customize both rewriting rules.
     * It's easier instead to discard inner replacements here, being careful about not emitting
     * hiding replacements commands in other cases than the above mentioned. */
    private def discardingHiddenReplacements(): List[Replace] = {
      var toprepls : List[Replace] = Nil
      for(cmd <- replQueue sortWith ( (r1, r2) => r1.from <= r2.from) ) {
        val hiders = toprepls filter (r => Patch.encloses(r, cmd))
        assert(hiders.size <= 1)
        if(hiders.isEmpty) { toprepls = cmd :: toprepls }
        else {
          for(h <- hiders) {
            val p = new nsc.util.RangePosition(sf, cmd.from, cmd.from, cmd.to)
            info(p, "discardingHiddenReplacements()")
          }
        }
      }
      toprepls
    }

    /* ------- Text splicing (custom handling wrt tryPatch(Insert) but faster) -------- */

    private class Splice(val before: Int, val arrivalOrder: Int, val text: String) {
      override def toString() = "Splice("+before+","+arrivalOrder+","+text+")"
    }

    private implicit object spliceOrd extends Ordering[Splice] {
      /** text not a comparison key. */
      override def compare(x: Splice, y: Splice) : Int = {
        val compare1 = x.before compare y.before
        if(compare1 != 0) return compare1
        val compare2 = x.arrivalOrder compare y.arrivalOrder
        if(compare2 != 0) return compare2
        0
      }
    }

    private var splices = collection.immutable.TreeSet.empty[Splice]
    
    /** detects whether the splice would start a nested comment. If so will be discarded. */
    private def isDoubleComment(before: Int, text: String): Boolean = {
      if((text != "/*") && (text != "*/")) return false;
      val elem = new Splice(before, 0, text)
      val r = splices.from(elem)
      if(r.isEmpty) return false;
      val res = r.head.text == text
      res
    }

    def splice(before: Int, text: String) {
      // val fname = sf.file.path
      if(isDoubleComment(before, text)) return;
      var arrivalOrder = 0
      var elem : Splice = null 
      do {
        arrivalOrder += 1
        elem = new Splice(before, arrivalOrder, text)
      } while ({
        val r = splices.from(elem)
        !r.isEmpty && (r.head.before == before) && (r.head.arrivalOrder == arrivalOrder)
      })
      splices += elem
    }

    private def splicesInclusive(from: Int, to: Int) = {
      val fromSpl = new Splice(from, 0, "")
      val toSpl   = new Splice(to + 1, 0, "")
      splices.range(fromSpl, toSpl)
    }

    def eraseTypeArg(startingAt: Int) {
      var idx = startingAt
      var openBrackets = 0
      while(at(idx) != '[') { idx += 1 }; openBrackets += 1; splice(idx, "/*")
      while(openBrackets > 0) {
        idx += 1;
        at(idx) match { case ']' => openBrackets -= 1; case '[' => openBrackets += 1; case _ => () }
      }
      splice(idx+1, "*/")
    }

    /* ------- PatchTree to Text conversions -------- */

    def asString() : String = asString(root)
    def asString(from: Int, to: Int): String = asString(new Patcheable(from, to))
    def at(idx: Int): Char = sf.content(idx)

    def serialize(outputFile: java.io.File) {
      val out : java.io.PrintStream = new java.io.PrintStream(new java.io.FileOutputStream(outputFile), true, "UTF-8")
      // merge all queued replacements
      replQueue = discardingHiddenReplacements()
      for(cmd <- replQueue) { tryPatch(cmd.from, cmd.to, List(cmd), dropGapsOK = true) }
      // out.println("/* jdk2ikvm output for " + sf.toString.replace('\\', '/') + " */") // DEBUG
      out.print(txtSplicesAt(root.from - 1))
      patchedView(root, out)
      out.print(txtSplicesAt(root.to + 1))
      out.flush()
      out.close()
    }

    private def asString(node: PatchCmd) : String = {
      val baos = new java.io.ByteArrayOutputStream()
      val out = new java.io.PrintStream(baos, true, "UTF-8")
      patchedView(node, out)
      new String(baos.toByteArray(), "UTF-8")
    }

    /* lean and mean without visitor (too few node kinds and operations, really) */
    private def patchedView(node: PatchCmd, out: java.io.PrintStream ) {
      node match {
        case i : Insert => out.print(i.text)
        case r : Replace =>
          val hiddenSplices = splicesInclusive(r.from, r.to)
          assert(hiddenSplices.isEmpty)
          out.print(r.text)
        case p : Patcheable =>
          if(p.patches.isEmpty) {
            var visibleSplices = splicesInclusive(p.from, p.to) 
            for(current <- p.from to p.to) {
              while(!visibleSplices.isEmpty && visibleSplices.head.before == current) {
                out.print(visibleSplices.head.text)
                visibleSplices = visibleSplices.tail
              }
              out.print(sf.content(current))              
            }
          } else {
            // TODO check WFR: no splice or replacement in any gap (if any) left out between p.patches 
            for(c <- p.patches) patchedView(c, out)
          }
      }
    }

    /* lean and mean without visitor (too few node kinds and operations, really) */
    private def describe(node: PatchCmd) : String = {
      node match {
        case i : Insert => "Insert:"+asString(node)
        case r : Replace => "Replace"+r.showPos+":"+asString(node)
        case p : Patcheable =>
          if(p.patches.isEmpty) "Patcheable(Leaf,"+p.showPos+")"+asString(node) // TODO print splices in this range 
          else "Patcheable(InternalNode,"+p.showPos+"\n"+ (p.patches map (nestedP => "\t"+describe(nestedP))).mkString  +"\n)"
      }
    }

    private def txtSplicesAt(current: Int) = splicesInclusive(current, current) map (spl => spl.text) mkString

    /* ------- PatchTree debug helpers -------- */

    private def padRight(s: String, n: Int): String = java.lang.String.format("%1$-" + n + "s", s)
    private def padLeft (s: String, n: Int): String = java.lang.String.format("%1$#" + n + "s", s)
    private def times(n: Int, ch: Char) = List.fill(n)(ch).mkString

    def compare(tagsposs: List[Tuple2[String, nsc.util.RangePosition]]) {
      scala.Console.println(times(80,'_'))
      val (tags, poss) = tagsposs.unzip
      val outermostPos = poss.head
      val headerLength = (tags map (_.size) reduceLeft math.max)
      Console.print(times(headerLength, ' '))
      val arr = outermostPos.source.content
      val extraLead  = math.min(5, outermostPos.start)
      val extraTrail = math.min(5, (arr.size - 1) - outermostPos.end )
      Console.println(new String(arr.slice(outermostPos.start - extraLead, outermostPos.end + extraTrail + 1)))

      def helper(tag: String, pos: nsc.util.RangePosition) {
        Console.print(padRight(tag, headerLength))
        val strLead              = times(extraLead,' ')
        val strBeforeUnderlining = times(pos.start - outermostPos.start, ' ')
        val strUnderlining       = "|" + times(pos.end - pos.start + 1 - 2, '-') 
        val strAfterUnderlining  = (if(pos.start < pos.end) "|" else "") + times(outermostPos.end - pos.end, ' ') + pos.show
        Console.println(strLead + strBeforeUnderlining + strUnderlining + strAfterUnderlining)
      }

      for((tag, pos) <- tagsposs) { helper(tag, pos) }
            
    }

    private def printIndented(s: String) {
      s.split("\\r?\\n") foreach (txt => scala.Console.println("\t" + txt)) 
    }

    def asString(cmds: List[PatchCmd]) : String = cmds.toList map (cmd => asString(cmd)) mkString("")

    def console(treePos: nsc.util.RangePosition, treeConcise: String, tagscmds: List[(String, PatchCmd)]) {
      val (tags, cmds) = tagscmds.unzip

      scala.Console.println("BEFORE")
      printIndented(treeConcise)

      scala.Console.println("AFTER")
      printIndented(asString(cmds))
      scala.Console.println()

      scala.Console.println("ONE PATCHCMD AT A TIME")
      for((tag, cmd) <- tagscmds) { printIndented(tag + " = " + describe(cmd)) }
      scala.Console.println()
    }
    
  }

}