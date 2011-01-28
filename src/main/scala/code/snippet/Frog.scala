package code
package snippet

import net.liftweb._
import util.Helpers._

object Frog {
  def render = "span *" #> (1 to 10).map(i => now.toString) &
  "#foo *" #> "bar"
}
