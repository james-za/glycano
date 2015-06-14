package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._
import org.scalajs.dom

import scalaz.effect.IO

package object semantic {

  object Dropdown {
    val Item = C("item", narrow = false)
    val Label = C("label", narrow = false)
    val NarrowToggle = ReactComponentB[String]("Dropdown")
      .initialState(false)
      .backend(_ => new OnUnmount.Backend)
      .render { $ =>
        val show = if ($.state) "visible" else "hidden"
        div"ui right dropdown item"(
          $.state ?= c"active visible",
          ^.width := 30.px, ^.minWidth := 30.px,
          ^.onClick ~~> $.setStateIO(true)
        )(
          $.props, <.i(c"dropdown icon"),
          div"menu transition $show"($.propsChildren)
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .configure(EventListener.installIO("click", _.setStateIO(false), _ => dom.document.body))
      .build
    def C(context: String, narrow: Boolean) = ReactComponentB[String]("Dropdown")
      .initialState(false)
      .render { $ =>
        val show = if ($.state) "visible" else "hidden"
        div"ui dropdown $context"(
          $.state ?= c"active visible",
          narrow ?= Seq(c"floating", ^.width := 30.px, ^.minWidth := 30.px),
          ^.onMouseOver ~~> $.setStateIO(true),
          ^.onMouseOut ~~> $.setStateIO(false)
        )(
          $.props, <.i(c"dropdown icon"),
          div"menu transition $show"($.propsChildren)
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
  }

  object RadioButtons {
    val defaultName: Any ~=> String = ReusableFn(_.toString)
    case class Props[A](selected: ReusableVar[Option[A]], choices: Seq[A], name: A ~=> String = defaultName, toggle: Boolean = false)

    class Backend[A]($: BackendScope[Props[A], Unit]) {
      def handleClick(a: A): IO[Unit] =
        $.props.selected.mod(s => if ($.props.toggle && s.contains(a)) None else Some(a))
    }

    implicit def reuseChoices[A]: Reusability[Seq[A]] = Reusability.by_==
    implicit def reuseProps[A]: Reusability[Props[A]] = Reusability.caseclass4(Props.unapply[A])

    def apply[A] = ReactComponentB[Props[A]]("RadioGroupMap")
      .stateless
      .backend(new Backend(_))
      .render { $ =>
        div"ui buttons"($.props.choices.map { value =>
          div"ui button"(
            $.props.selected.value.contains(value) ?= c"active",
            ^.onClick ~~> $.backend.handleClick(value),
            ^.key := value.##
          )($.props.name(value))
        })
      }
      .domType[dom.html.Div]
      .configure(Reusability.shouldComponentUpdate[Props[A], Unit, Backend[A], dom.html.Div])
      .build
  }
}
