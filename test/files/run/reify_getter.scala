import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    class C {
      val x = 2
    }

    new C().x
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}
