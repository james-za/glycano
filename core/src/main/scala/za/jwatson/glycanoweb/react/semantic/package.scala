package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.extra.{ReusableFn, ~=>, ReusableVar, Reusability}
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._
import org.scalajs.dom

import scalaz.effect.IO

package object semantic {

  object Dropdown {
    val Item = C("item")
    val Label = C("label")
    def C(context: String) = ReactComponentB[String]("Dropdown")
      .initialState(false)
      .render { $ =>
        val show = if ($.state) "visible" else "hidden"
        <.div($.props)(
          ^.cls := s"ui dropdown $context",
          $.state ?= (^.cls := "active visible"),
          ^.onMouseOver ~~> $.setStateIO(true),
          ^.onMouseOut ~~> $.setStateIO(false)
        )(
          <.i(^.cls := "dropdown icon"),
          <.div(^.cls := s"menu transition $show")($.propsChildren)
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
  }

  object FormInput {
    case class Props(`type`: String, onChange: ReactEventI => Unit)

    implicit val reuseProps: Reusability[Props] = Reusability.by((_: Props).`type`)

    def apply(props: Props, children: ReactNode*) = component(props, children)
    val component = ReactComponentB[Props]("Input")
      .render { (P, C) =>
        <.input(
          ^.`type` := P.`type`,
          ^.cls := "form-control",
          ^.onChange ==> P.onChange
        )(C)
      }
      .domType[dom.html.Input]
      .configure(Reusability.shouldComponentUpdate)
      .build
  }

  object RadioGroupMap {
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
        <.div(^.cls := "btn-group")(
          for (value <- $.props.choices) yield {
            val selected = $.props.selected.value.contains(value)
            <.label($.props.name(value))(
              ^.cls := "btn btn-default",
              selected ?= (^.cls := "active"),
              ^.onClick ~~> $.backend.handleClick(value),
              ^.key := value.##
            )
          }
        )
      }
      .domType[dom.html.Div]
      .configure(Reusability.shouldComponentUpdate[Props[A], Unit, Backend[A], dom.html.Div])
      .build
  }
}
