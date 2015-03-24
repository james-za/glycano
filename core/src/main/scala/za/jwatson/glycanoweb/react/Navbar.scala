package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.extra._
import japgolly.scalajs.react._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import za.jwatson.glycanoweb.react.GlycanoApp.{Mode, AppStateL, AppState}
import za.jwatson.glycanoweb.react.GlycanoCanvas.View
import za.jwatson.glycanoweb.react.bootstrap.{Button, FormInput, GlyphIcon, NavbarHeader}
import za.jwatson.glycanoweb.structure.{ResidueId, AnnotId, RGraph}

import scala.util.Try
import scalaz.effect.IO

object Navbar {
  def navbtn(name: String, action: => IO[Unit]) =
    <.button(
      name,
      ^.cls := "btn btn-default navbar-btn",
      ^.onClick ~~> ((e: ReactEvent) => for {
        _ <- e.preventDefaultIO
        _ <- action
      } yield ())
    )

  def navbtn(icon: String, name: String, action: => IO[Unit]) =
    <.button(
      <.i(^.cls := "fa fa-lg fa-" + icon), " ", name,
      ^.cls := "btn btn-default navbar-btn",
      ^.onClick ~~> ((e: ReactEvent) => for {
        _ <- e.preventDefaultIO
        _ <- action
      } yield ())
    )

  class Backend(val t: BackendScope[ExternalVar[AppState], Unit]) {
    def clickCenter = t.props.modL(AppState.view) { v =>
      val (x, y, w, h) = t.props.value.fitBounds
      println(s"$x $y $w $h")
      println(v)
      val sx = v.width / w
      val sy = v.height / h
      val scale = math.min(sx, sy)
      println(s"$sx $sy $scale")
      View(x + w / 2, y + h / 2, scale * 0.975, v.width, v.height)
    }
  }

  def apply(props: ExternalVar[AppState], children: ReactNode*) = component(props, children: _*)
  val component = ReactComponentB[ExternalVar[AppState]]("Navbar")
    .stateless
    .backend(new Backend(_))
    .render($ => {
      val appState = $.props
      val zoom = appState.value.view.scale * 100

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
          <.form(^.cls := "form-inline")(
            <.div(^.cls := "form-group")(
              <.label(^.cls := "checkbox-inline")(
                <.input(
                  ^.checked := appState.value.bondLabels,
                  ^.`type` := "checkbox",
                  ^.onChange ~~> appState.modL(GlycanoApp.AppState.bondLabels)(!_)
                ),
                "Bond Labels"
              )
            ),
            " ", navbtn("Clear All", appState.setL(AppStateL.graphL)(RGraph())),
            " ", navbtn("trash", "Delete", appState.mod(GlycanoApp.cutS.exec)),
            " ", navbtn("cut", "Cut", appState.mod(GlycanoApp.copyS.exec)),
            " ", navbtn("copy", "Copy", appState.mod(GlycanoApp.pasteS.exec)),
            " ", navbtn("paste", "Paste", appState.mod(GlycanoApp.deleteS.exec)),
            " ", navbtn("undo", "Undo", appState.mod(GlycanoApp.undo)),
            " ", navbtn("repeat", "Redo", appState.mod(GlycanoApp.redo)),
            " ", navbtn("edit", "Add Annotation", appState.mod(AppState.mode set Mode.PlaceAnnotation)),
            " ", <.div(^.cls := "form-group")(
              <.label("Annotation Font Size", ^.paddingRight := 5.px),
              <.input(
                ^.cls := "form-control",
                ^.value := appState.value.annotationFontSize,
                ^.`type` := "number",
                ^.onChange ~~> ((e: ReactEventI) => appState.setL(GlycanoApp.AppState.annotationFontSize)(Try(e.target.value.toDouble).getOrElse(24))),
                ^.width := 80.px
              )
            ),
            " ", navbtn("search-minus", "", appState.modL(AppState.view ^|-> View.scale)(_ / 1.1)),
            " ", <.div(^.cls := "form-group")(<.input(
              ^.cls := "form-control",
              ^.`type` := "range",
              "min".reactAttr := 0,
              "max".reactAttr := 200,
              ^.step := 0.01,
              ^.value := zoom,
              ^.onChange ~~> ((e: ReactEventI) => appState.setL(AppState.view ^|-> View.scale)(Try(e.target.value.toDouble / 100.0).getOrElse(1.0)))
            )),
            " ", <.span(f"$zoom%.2f" + "%"),
            " ", navbtn("search-plus", "", appState.modL(AppState.view ^|-> View.scale)(_ * 1.1)),
            " ", navbtn("Reset Zoom", appState.setL(AppState.view ^|-> View.scale)(1.0)),
            " ", navbtn("Center", $.backend.clickCenter),
            " ", $.propsChildren
          )
        )
      ))
    })
    .shouldComponentUpdate((T, P, S) => T.props.value != P.value)
    .build
}
