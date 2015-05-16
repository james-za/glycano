package za.jwatson.glycanoweb.react

import importedjs.filereaderjs.{FileReaderJS, Opts}
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import za.jwatson.glycanoweb.Gly
import za.jwatson.glycanoweb.react.GlycanoApp.{Mode, AppStateL, AppState}
import za.jwatson.glycanoweb.react.GlycanoCanvas.View
import za.jwatson.glycanoweb.react.bootstrap.{Button, FormInput, GlyphIcon, NavbarHeader}
import za.jwatson.glycanoweb.structure.{ResidueId, AnnotId, RGraph}
import scala.collection.immutable.NumericRange
import scalajs.js

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

  def navbtn_(name: String, action: () => Unit) =
    <.button(
      name,
      ^.cls := "btn btn-default navbar-btn",
      ^.onClick ==> { (e: ReactEvent) => e.preventDefault(); action() }
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

  def navcheckbox(name: String, checked: Boolean, action: => IO[Unit]): ReactTag =
    <.div(^.cls := "form-group")(
      <.label(^.cls := "checkbox-inline")(
        <.input(
          ^.checked := checked,
          ^.`type` := "checkbox",
          ^.onChange ~~> action
        ),
        name
      )
    )

  def navcheckbox[A](name: String, ev: ExternalVar[A], lens: monocle.Lens[A, Boolean]): ReactTag =
    navcheckbox(name, lens.get(ev.value), ev.modL(lens)(!_))

  def navnumber(name: String, value: Double, action: Double => IO[Unit]): ReactTag = {
    <.div(^.cls := "form-group")(
      <.label(name, ^.paddingRight := 5.px),
      <.input(
        ^.cls := "form-control",
        ^.value := value,
        ^.`type` := "number",
        ^.onChange ~~> ((e: ReactEventI) => action(Try(e.target.value.toDouble).getOrElse(value))),
        ^.width := 80.px
      )
    )
  }

  def navnumber[A](name: String, ev: ExternalVar[A], lens: monocle.Lens[A, Double]): ReactTag =
    navnumber(name, lens.get(ev.value), ev.setL(lens))

  def navrange(range: NumericRange[Double], value: Double, action: Double => IO[Unit]): ReactTag =
    <.div(^.cls := "form-group")(<.input(
      ^.cls := "form-control",
      ^.`type` := "range",
      "min".reactAttr := range.start,
      "max".reactAttr := range.end,
      ^.step := range.step,
      ^.value := value,
      ^.onChange ~~> ((e: ReactEventI) => action(Try(e.target.value.toDouble).getOrElse(value)))
    ))

  def navrange[A](range: NumericRange[Double], ev: ExternalVar[A], lens: monocle.Lens[A, Double]): ReactTag =
    navrange(range, lens.get(ev.value), ev.setL(lens))

  class Backend(val t: BackendScope[ExternalVar[AppState], String]) {
    def clickCenter = t.props.modL(AppState.view) { v =>
      t.props.value.bounds.fold(v) { b =>
        val sx = v.width / b.width
        val sy = v.height / b.height
        val scale = math.min(sx, sy)
        View(b.x + b.width / 2, b.y + b.height / 2, scale * 0.975, v.width, v.height)
      }
    }

    def loadGly(name: String, gly: Gly): Unit = {
      (for {
        _ <- t.props.setL(AppStateL.graphL)(gly.toRGraph)
        _ <- t.setStateIO(name)
      } yield ()).unsafePerformIO()
    }

    def saveSvg(): Unit = {
      import js.Dynamic.{global => g}
      for (fn <- t.refs[dom.html.Input]("filename").map(_.getDOMNode())) {
        val base = if (fn.value.isEmpty) "glycano" else fn.value
        val name = if (base.endsWith(".gly")) base.dropRight(3) + "svg" else base + ".svg"
        val svg = dom.document.getElementById("canvas").outerHTML
        val base64 = dom.window.btoa(g.unescape(g.encodeURIComponent(svg)).asInstanceOf[String])
        val dataUrl = "data:image/svg+xml;base64," + base64
        val a = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
        a.asInstanceOf[js.Dynamic].download = name
        a.asInstanceOf[js.Dynamic].href = dataUrl
        dom.document.body.appendChild(a)
        a.click()
      }
    }

    def saveGly(): Unit = {
      import js.Dynamic.{global => g}, upickle._, Gly._
      for (fn <- t.refs[dom.html.Input]("filename").map(_.getDOMNode())) {
        val base = if (fn.value.isEmpty) "glycano" else fn.value
        val name = if (base.endsWith(".gly")) base else base + ".gly"
        val graph = AppStateL.graph(t.props.value)
        val gly = write[Gly](Gly.from(graph))
        val base64 = dom.window.btoa(g.unescape(g.encodeURIComponent(gly)).asInstanceOf[String])
        val dataUrl = "data:text/plain;base64," + base64
        val a = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
        a.asInstanceOf[js.Dynamic].download = name
        a.asInstanceOf[js.Dynamic].href = dataUrl
        dom.document.body.appendChild(a)
        a.click()
      }
    }

    def savePng(): Unit = {
      import js.Dynamic.{global => g}
      for (fn <- t.refs[dom.html.Input]("filename").map(_.getDOMNode())) {
        val base = if (fn.value.isEmpty) "glycano" else fn.value
        val name = if (base.endsWith(".gly")) base.dropRight(3) + "png" else base + ".png"
        g.saveSvgAsPng(dom.document.getElementById("canvas"), name)
      }
    }
  }

  def apply(props: ExternalVar[AppState], children: ReactNode*) = component(props, children: _*)
  val component = ReactComponentB[ExternalVar[AppState]]("Navbar")
    .initialState("glycano")
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
              <.input(
                ^.ref := "loadfile",
                ^.`type` := "file",
                ^.cls := "form-control",
                ^.onChange ==> { (e: ReactEventI) =>
                  println(e.target.files(0).name)
                }
              )
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
                ^.value := $.state,
                ^.onChange ~~> ((e: ReactEventI) => $.setStateIO(e.target.value))
              )
            )
          ),
          <.form(^.cls := "form-inline")(
            " ", navbtn_("Save .gly", { () => $.backend.saveGly() }),
            " ", navbtn_("Save .svg", { () => $.backend.saveSvg() }),
            " ", navbtn_("Save .png", { () => $.backend.savePng() }),
            " ", navcheckbox("Bond Labels", appState, AppState.bondLabels),
            " ", navbtn("Clear All", appState.setL(AppStateL.graphL)(RGraph())),
            " ", navbtn("trash", "Delete", appState.mod(GlycanoApp.cutS.exec)),
            " ", navbtn("cut", "Cut", appState.mod(GlycanoApp.copyS.exec)),
            " ", navbtn("copy", "Copy", appState.mod(GlycanoApp.pasteS.exec)),
            " ", navbtn("paste", "Paste", appState.mod(GlycanoApp.deleteS.exec)),
            " ", navbtn("undo", "Undo", appState.mod(GlycanoApp.undo)),
            " ", navbtn("repeat", "Redo", appState.mod(GlycanoApp.redo)),
            " ", navbtn("edit", "Add Annotation", appState.mod(AppState.mode set Mode.PlaceAnnotation)),
            " ", navnumber("Annotation Font Size", appState, AppState.annotationFontSize),
            " ", navbtn("search-minus", "", appState.modL(AppState.view ^|-> View.scale)(_ / 1.1)),
            " ", navrange(0.0 to 200.0 by 0.01, appState, AppState.view ^|-> View.scale ^<-> monocle.Iso[Double, Double](_ * 100.0)(_ / 100.0)),
            " ", <.span(f"$zoom%.2f" + "%"),
            " ", navbtn("search-plus", "", appState.modL(AppState.view ^|-> View.scale)(_ * 1.1)),
            " ", navbtn("Reset Zoom", appState.setL(AppState.view ^|-> View.scale)(1.0)),
            " ", navbtn("Center", $.backend.clickCenter),
            " ", navcheckbox("Snap To Grid", appState, AppState.snapToGrid),
            " ", navcheckbox("Show Grid", appState, AppState.showGrid),
            " ", navnumber("Grid Width", appState, AppState.gridWidth),
            " ", navcheckbox("Snap Rotation", appState, AppState.snapRotation),
            " ", navrange(0.0 to 180.0 by 1.0, appState, AppState.snapRotationDegrees),
            " ", <.span(f"${appState.value.snapRotationDegrees}%.0fº"),
            " ", $.propsChildren
          )
        )
      ))
    })
    .shouldComponentUpdate((T, P, S) => T.props.value != P.value || T.state != S)
    .componentDidMount($ => {
      for (in <- $.refs[dom.html.Input]("loadfile").map(_.getDOMNode())) {
        val fileReaderOpts = Opts.load((e: dom.ProgressEvent, file: dom.File) => {
          import upickle._, Gly._
          val str = e.target.asInstanceOf[js.Dynamic].result.asInstanceOf[String]
          try {
            val gly = read[Gly](str)(rwGly)
            $.backend.loadGly(file.name, gly)
          } catch {
            case e: Exception =>
          }
        })
        fileReaderOpts.readAsDefault = "Text"
        FileReaderJS.setupInput(in, fileReaderOpts)
      }
    })
    .build
}
