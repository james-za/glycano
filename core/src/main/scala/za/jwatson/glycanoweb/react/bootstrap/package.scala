package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.extra.{ReusableFn, ~=>, ReusableVar, Reusability}
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._
import org.scalajs.dom
import za.jwatson.glycanoweb.react.bootstrap.{Bootstrap => bs}

import scalaz.effect.IO

package object bootstrap {

  object NavbarHeader {
    def apply(navbarId: String, children: ReactNode*) = component(navbarId, children)
    val component = ReactComponentB[String]("NavbarHeader")
      .render { (navbarId, C) =>
        <.div(^.cls:="navbar-header")(
          <.button(
            ^.`type` := "button",
            ^.cls := "navbar-toggle collapsed",
            "data-toggle".reactAttr := "collapse",
            "data-target".reactAttr := s"#$navbarId"
          )(
            <.span(^.cls := "sr-only", "Toggle Navigation"),
            <.span(^.cls := "icon-bar"),
            <.span(^.cls := "icon-bar"),
            <.span(^.cls := "icon-bar")
          ),
          <.a(^.cls := "navbar-brand", ^.href := "#")(C)
        )
      }
      .domType[dom.html.Div]
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

  object Button {
    case class Props(onClick: () => Unit,
                     style: bs.Style = bs.Default,
                     size: bs.Size = bs.Md,
                     block: Boolean = false,
                     nav: Boolean = false,
                     pressed: Boolean = false)

    implicit val reuseBSStyle: Reusability[bs.Style] = Reusability.by_==
    implicit val reuseBSSize: Reusability[bs.Size] = Reusability.by_==
    implicit val reuseProps: Reusability[Props] = Reusability.by((p: Props) => (p.style, p.size, p.block, p.nav, p.pressed))

    def apply(props: Props, children: ReactNode*) = component(props, children)
    def withKey(key: scalajs.js.Any) = component.withKey(key)
    val component = ReactComponentB[Props]("Button")
      .initialState(false)
      .noBackend
      .render((P, C, S, B) => {
        val block = if (P.block) " btn-block" else ""
        val nav = if (P.nav) " navbar-btn" else ""
        <.button(
          ^.cls := s"btn btn-${P.style.name} btn-${P.size.name}$block$nav",
          ^.onClick --> P.onClick()
        )(C)
      })
      .domType[dom.html.Button]
      .configure(Reusability.shouldComponentUpdate)
      .build
  }

  object GlyphIcon {
    def apply(icon: String, children: ReactNode*) = component(icon, children)
    val component = ReactComponentB[String]("GlyphIcon")
      .render { (props, children) =>
        <.span(^.cls := "glyphicon glyphicon-" + props)(children)
      }
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
