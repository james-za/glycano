package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.vdom.Attr
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._
import org.scalajs.dom

package object bootstrap {

  object NavbarHeader {
    def apply(navbarId: String, children: ReactNode*) = component(navbarId, children)
    val component = ReactComponentB[String]("NavbarHeader")
      .render { (navbarId, C) =>
        <.div(^.cls:="navbar-header")(
          <.button(
            ^.`type` := "button",
            ^.cls := "navbar-toggle collapsed",
            Attr("data-toggle") := "collapse",
            Attr("data-target") := s"#$navbarId"
          )(
              <.span(^.cls := "sr-only", "Toggle Navigation"),
              <.span(^.cls := "icon-bar"),
              <.span(^.cls := "icon-bar"),
              <.span(^.cls := "icon-bar")
            ),
          <.a(^.cls := "navbar-brand", ^.href := "#")(C)
        )
      }
      .domType[dom.HTMLDivElement]
      .build
  }

  object FormInput {
    case class Props(`type`: String, onChange: ReactEventI => Unit)

    def apply(props: Props, children: ReactNode*) = component(props, children)
    val component = ReactComponentB[Props]("Input")
      .render { (P, C) =>
        <.input(
          ^.`type` := P.`type`,
          ^.cls := "form-control",
          ^.onChange ==> P.onChange
        )(C)
      }
      .shouldComponentUpdate((T, P, S) => T.props.`type` != P.`type`)
      .domType[dom.HTMLInputElement]
      .build
  }

}
