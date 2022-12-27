import App1.b
import Macros.serialize

object App {
  val a = "abc"
  val f = () => { val x = "uvw"; a + b + x + "xyz"}
  serialize(f)
}