package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.extra.ExternalVar
import japgolly.scalajs.react.{ReactEvent, ReactEventI, ReactNode, ReactComponentB}
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.prefix_<^._
import za.jwatson.glycanoweb.react.GlycanoApp.{Mode, AppStateL, AppState}
import za.jwatson.glycanoweb.react.GlycanoCanvas.View
import za.jwatson.glycanoweb.react.bootstrap.{Button, FormInput, GlyphIcon, NavbarHeader}
import za.jwatson.glycanoweb.structure.{ResidueId, AnnotId, RGraph}

import scala.util.Try
import scalaz.effect.IO

object Navbar {
  case class Props(
    state: ExternalVar[GlycanoApp.AppState]
//    bondLabels: ExternalVar[Boolean],
//    zoom: ExternalVar[Double],
//    graph: ExternalVar[RGraph],
//    selection: ExternalVar[(Set[ResidueId], Set[AnnotId])]
  )

  def navbtn(name: String, action: IO[Unit]) =
    <.button(
      name,
      ^.cls := "btn btn-default navbar-btn",
      ^.onClick ~~> ((e: ReactEvent) => for {
        _ <- e.preventDefaultIO
        _ <- action
      } yield ())
    )
  def navbtn(icon: String, name: String, action: IO[Unit]) =
    <.button(
      <.i(^.cls := "fa fa-lg fa-" + icon), " ", name,
      ^.cls := "btn btn-default navbar-btn",
      ^.onClick ~~> ((e: ReactEvent) => for {
        _ <- e.preventDefaultIO
        _ <- action
      } yield ())
    )

  def apply(props: Props, children: ReactNode*) = component(props, children: _*)
  val component = ReactComponentB[Props]("Navbar")
    .stateless
    .render($ => {
      val state = $.props.state
      val zoom = state.value.view.scale * 100

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
                  ^.checked := state.value.bondLabels,
                  ^.`type` := "checkbox",
                  ^.onChange ~~> state.modL(GlycanoApp.AppState.bondLabels)(!_)
                ),
                "Bond Labels"
              )
            ),
            " ", navbtn("Clear All", state.setL(AppStateL.graphL)(RGraph())),
            " ", navbtn("trash", "Delete", state.mod(GlycanoApp.cutS.exec)),
            " ", navbtn("cut", "Cut", state.mod(GlycanoApp.copyS.exec)),
            " ", navbtn("copy", "Copy", state.mod(GlycanoApp.pasteS.exec)),
            " ", navbtn("paste", "Paste", state.mod(GlycanoApp.deleteS.exec)),
            " ", navbtn("undo", "Undo", state.mod(GlycanoApp.undo)),
            " ", navbtn("repeat", "Redo", state.mod(GlycanoApp.redo)),
            " ", navbtn("edit", "Add Annotation", state.mod(AppState.mode set Mode.PlaceAnnotation)),
            " ", <.div(^.cls := "form-group")(
              <.label("Annotation Font Size", ^.paddingRight := 5.px),
              <.input(
                ^.cls := "form-control",
                ^.value := state.value.annotationFontSize,
                ^.`type` := "number",
                ^.onChange ~~> ((e: ReactEventI) => state.setL(GlycanoApp.AppState.annotationFontSize)(Try(e.target.value.toDouble).getOrElse(24))),
                ^.width := 80.px
              )
            ),
            " ", navbtn("search-minus", "", state.modL(AppState.view ^|-> View.scale)(_ / 1.1)),
            " ", <.div(^.cls := "form-group")(<.input(
              ^.cls := "form-control",
              ^.`type` := "range",
              "min".reactAttr := 0,
              "max".reactAttr := 200,
              ^.step := 0.01,
              ^.value := zoom,
              ^.onChange ~~> ((e: ReactEventI) => state.setL(AppState.view ^|-> View.scale)(Try(e.target.value.toDouble / 100.0).getOrElse(1.0)))
            )),
            " ", <.span(f"$zoom%.2f" + "%"),
            " ", navbtn("search-plus", "", state.modL(AppState.view ^|-> View.scale)(_ * 1.1)),
            " ", navbtn("Reset Zoom", state.setL(AppState.view ^|-> View.scale)(1.0)),
            " ", $.propsChildren
          )
        )
      ))
    })
    .shouldComponentUpdate((T, P, S) => T.props.state.value != P.state.value)
    .build
}
