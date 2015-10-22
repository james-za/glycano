package za.jwatson.glycanoweb

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{Reusability, ReusableVar}
import japgolly.scalajs.react.vdom.{AttrValue, ClassNameAttr, TagMod}
import org.scalajs.dom

import scala.annotation.tailrec
import scala.scalajs.js
import scala.util.Try

package object react {
//  abstract class Component[P, S, +B, +N <: TopNode](val component: ReqProps[P, S, B, N]) {
//    def set(key: UndefOr[js.Any], ref: UndefOr[String]): ReqProps[P, S, B, N] = component.set(key, ref)
//    def apply(props: P, children: ReactNode*): ReactComponentU[P, S, B, N] = component.apply(props, children)
//    def withProps(p: => P): ConstProps[P, S, B, N] = component.withProps(p)
//    def withDefaultProps(p: => P): DefaultProps[P, S, B, N] = component.withDefaultProps(p)
//    def withKey(k: js.Any): ReqProps[P, S, B, N] = component.withKey(k)
//    def withRef(r: String): ReqProps[P, S, B, N] = component.withRef(r)
//  }

  implicit val tryOptionLike: OptionLike[Try] = new OptionLike[Try] {
    type O[A] = Try[A]
    def map     [A, B](o:       O[A])(f: A => B)         : O[B]      = o map f
    def fold    [A, B](o: O[A], b: => B)(f: A => B): B         = if (o.isSuccess) f(o.get) else b
    def foreach [A]   (o: O[A])(f: A => Unit)      : Unit      = o foreach f
    def isEmpty [A]   (o: O[A])                    : Boolean   = o.isFailure
    def toOption[A]   (o: O[A])                    : Option[A] = o.toOption
  }

  def button(e: ReactMouseEvent): Int = e.dynamic[Int](_.button)

  object StringClsContext {
  }

  val stringAttrX = new AttrValue[String] {
    def apply(v: String, b: js.Any => Unit): Unit = b(v)
  }

  implicit class StringClsContext(val sc: StringContext) extends AnyVal {
    implicit def stringAttr: AttrValue[String] = stringAttrX
    def cls(args: Any*): TagMod = ClassNameAttr := sc.s(args: _*)
    def c(args: Any*): TagMod = ClassNameAttr := sc.s(args: _*)
    def div(args: Any*): vdom.ReactTagOf[dom.html.Div] = vdom.Tags.div(ClassNameAttr := sc.s(args: _*))
  }

  @tailrec def hasParent(n: dom.Node, parent: dom.Node): Boolean = {
    if (n == null) false
    else if (parent.isEqualNode(n)) true
    else hasParent(n.parentNode, parent)
  }

  implicit class WithReusability[A](private val rv: ReusableVar[A]) extends AnyVal {
    def withReusability(r: Reusability[A]) = ReusableVar(rv.value)(rv.set)(r)
  }

  object RVarValue {
    def unapply[A](rv: ReusableVar[A]): Option[A] = Some(rv.value)

  }
}
