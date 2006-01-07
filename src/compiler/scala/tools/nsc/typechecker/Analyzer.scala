/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

/** The main attribution phase.
 */ 
mixin class Analyzer 
	 extends AnyRef
            with Contexts
    	    with Namers 
	    with Typers 
	    with Infer 
	    with Variances 
            with EtaExpansion 
	    with SyntheticMethods 
	    with Codification {

  val global: Global;
  import global._;

  object namerFactory extends SubComponent {
    val global: Analyzer.this.global.type = Analyzer.this.global;
    val phaseName = "namer";
    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      def apply(unit: CompilationUnit): unit =
	new Namer(startContext.make(unit)).enterSym(unit.body);
    }
  }

  object typerFactory extends SubComponent {
    val global: Analyzer.this.global.type = Analyzer.this.global;
    val phaseName = "typer";
    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      resetTyper;
      def apply(unit: CompilationUnit): unit =
	unit.body = newTyper(startContext.make(unit)).typed(unit.body)
    }
  }
}

