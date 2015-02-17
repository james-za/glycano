package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._
import za.jwatson.glycanoweb.react.bootstrap.{Button, FormInput, GlyphIcon, NavbarHeader}

object Navbar {
  case class Props(B: GlycanoApp.Backend, bondLabels: Boolean)

  def apply(props: Props, children: ReactTag*) = component(props, children)
  val component = ReactComponentB[Props]("Navbar")
    .render((P, C) => {
      <.nav(^.cls := "navbar navbar-default", ^.role := "navigation")(<.div(^.cls := "container-fluid")(
        NavbarHeader("glycano-navbar-collapse", "Glycano"),
        <.div(^.cls := "collapse navbar-collapse", ^.id := "glycano-navbar-collapse")(
          <.p(^.cls := "navbar-text", "Load:"),
          <.form(^.cls := "navbar-form navbar-left")(
            <.div(^.cls := "form-group")(
              FormInput(FormInput.Props("file", e => println(e.target.files(0).name)))
            )
          ),
          <.p(^.cls := "navbar-text")("Filename:"),
          <.form(^.cls := "navbar-form navbar-left")(
            <.div(^.cls := "form-group")(
              <.input(
                ^.ref := "filename",
                ^.`type` := "text",
                ^.cls := "form-control",
                ^.placeholder := "Filename",
                ^.value := "glycano",
                ^.readOnly := "true")
            )
          ),
          <.ul(^.cls := "nav navbar-nav")(
            //saveDropdown
          ),
          <.form(^.cls := "navbar-form navbar-left")(
            <.div(^.cls := "form-group")(
              <.label(^.cls := "checkbox-inline")(
                <.input(
                  ^.checked := P.bondLabels,
                  ^.`type` := "checkbox",
                  ^.onChange --> P.B.toggleBondLabels()
                ),
                "Bond Labels"
              )
            )
          ),
          " ",
          Button.withKey("b00")(Button.Props(() => P.B.clearAll(), nav = true), "Clear All"), " ",
          Button.withKey("b01")(Button.Props(() => P.B.delete(), nav = true), "Delete"), " ",
          Button.withKey("b02")(Button.Props(() => P.B.cut(), nav = true), "Cut"), " ",
          Button.withKey("b03")(Button.Props(() => P.B.copy(), nav = true), "Copy"), " ",
          Button.withKey("b04")(Button.Props(() => P.B.paste(), nav = true), "Paste"), " ",
          Button.withKey("b05")(Button.Props(() => P.B.undo(), nav = true), GlyphIcon("chevron-left"), " Undo"), " ",
          Button.withKey("b06")(Button.Props(() => P.B.redo(), nav = true), GlyphIcon("chevron-right"), " Redo"), " ",
          Button.withKey("b07")(Button.Props(() => P.B.addAnnotation(), nav = true), GlyphIcon("font"), " Add Annotation"), " ",
          Button.withKey("b08")(Button.Props(() => P.B.zoomOut(), nav = true), GlyphIcon("zoom-out")), " ",
          Button.withKey("b09")(Button.Props(() => P.B.zoomReset(), nav = true), "Reset Zoom"), " ",
          Button.withKey("b10")(Button.Props(() => P.B.zoomIn(), nav = true), GlyphIcon("zoom-in")), " ",
          C
        )
      ))
    })
    .shouldComponentUpdate((T, P, S) => T.props.bondLabels != P.bondLabels)
    .build
}
