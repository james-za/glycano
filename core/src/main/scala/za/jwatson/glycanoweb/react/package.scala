package za.jwatson.glycanoweb

import japgolly.scalajs.react.ReactComponentC.{ReqProps, DefaultProps, ConstProps}
import japgolly.scalajs.react.ScalazReact.SzRExt_SEvent
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{ReusableVar, Reusability}
import japgolly.scalajs.react.vdom.{ClassNameAttr, TagMod}
import org.scalajs.dom

import scala.annotation.tailrec
import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scalaz.effect.IO

package object react {
  abstract class Component[P, S, +B, +N <: TopNode](val component: ReqProps[P, S, B, N]) {
    def set(key: UndefOr[js.Any], ref: UndefOr[String]): ReqProps[P, S, B, N] = component.set(key, ref)
    def apply(props: P, children: ReactNode*): ReactComponentU[P, S, B, N] = component.apply(props, children)
    def withProps(p: => P): ConstProps[P, S, B, N] = component.withProps(p)
    def withDefaultProps(p: => P): DefaultProps[P, S, B, N] = component.withDefaultProps(p)
    def withKey(k: js.Any): ReqProps[P, S, B, N] = component.withKey(k)
    def withRef(r: String): ReqProps[P, S, B, N] = component.withRef(r)
  }

  def button(e: ReactMouseEvent): Int = e.dynamic[Int](_.button)

  implicit class StringClsContext(val sc: StringContext) extends AnyVal {
    def cls(args: Any*): TagMod = ClassNameAttr := sc.s(args: _*)
    def c(args: Any*): TagMod = ClassNameAttr := sc.s(args: _*)
    def div(args: Any*): vdom.ReactTag = vdom.Tags.div(ClassNameAttr := sc.s(args: _*))
  }

  @tailrec def hasParent(n: dom.Node, parent: dom.Node): Boolean = {
    if (n == null) false
    else if (parent.isEqualNode(n)) true
    else hasParent(n.parentNode, parent)
  }

  def preventingDefaultIO(io: => IO[Unit]) =
    (e: ReactEvent) => e.preventDefaultIO.flatMap(_ => io)

  def preventingDefaultIOF[N <: TopNode, E <: SyntheticEvent[N]](io: E => IO[Unit]) =
    (e: E) => e.preventDefaultIO.flatMap(_ => io(e))

  implicit class WithReusability[A](private val rv: ReusableVar[A]) extends AnyVal {
    def withReusability(r: Reusability[A]) = ReusableVar(rv.value)(rv.set)(r)
  }

  object RVarValue {
    def unapply[A](rv: ReusableVar[A]): Option[A] = Some(rv.value)

  }
}
