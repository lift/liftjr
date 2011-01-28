package code
package lib

import net.liftweb._
import http._
import util._
import Helpers._

import org.jruby.embed._
import org.jruby._
import org.jruby.runtime.builtin._

import scala.xml.NodeSeq


case class JRubyCallable(inst: IRubyObject, meth: String) extends Function1[NodeSeq, NodeSeq] {
  import JRuby._

  import scala.collection.JavaConversions._

  def apply(in: NodeSeq): NodeSeq = {
    inst.callMethod(context, meth) match {
      case rh: RubyHash => {
        val lst: List[CssBindFunc] = 
          for {
            (k, v) <- rh.toList
          } yield k.toString #> v.toString
        
        lst match {
          case Nil => NodeSeq.Empty
          case xs => xs.reduceLeft(_ & _)(in)
        }
      }
    }
  }
}

object JRuby {
  lazy val jruby = new ScriptingContainer(LocalContextScope.THREADSAFE)
  def runtime = jruby.getProvider().getRuntime()
  def context = runtime.getCurrentContext()

  def init() {
    LiftRules.snippets.append {
      case JRuby(toCall) => toCall
    }
  }
  
  def unapply(in: List[String]): Option[JRubyCallable] = in match {
    case Nil => None
    case x :: xs => {
      for {
        parsed <- tryo(LiftRules.doWithResource("/"+x+".rb") {
          is =>
            jruby.parse(is, x+".rb").run()
        })
        
        clz <- tryo(runtime.fastGetClass(x))
        
        methName = xs.headOption getOrElse "render"
        
        inst <- tryo(clz.newInstance(context, Array(), null)) if {
          val ret = inst.callMethod(context, "respond_to?", 
                                    runtime.
                                    fastNewSymbol(methName)).
          toJava(classOf[Boolean])
          
          ret match {
            case b: _root_.java.lang.Boolean => b.booleanValue()
            case _ => false
          }
        }
      } yield {
        JRubyCallable(inst, methName)
      }
    }
  }
}
