package za.jwatson.glycanoweb.react

import importedjs.filereaderjs.{FileReaderJS, Opts}
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.Iso
import org.scalajs.dom
import za.jwatson.glycanoweb.Gly
import za.jwatson.glycanoweb.react.GlycanoApp.{Mode, AppStateL, AppState}
import za.jwatson.glycanoweb.react.GlycanoCanvas.{Bounds, View}
import za.jwatson.glycanoweb.react.bootstrap.{Button, FormInput, GlyphIcon, NavbarHeader}
import za.jwatson.glycanoweb.structure.{ResidueId, AnnotId, RGraph}
import scala.collection.immutable.NumericRange
import scalajs.js

import scala.util.Try
import scalaz.effect.IO

object Navbar {
  def clampIso(min: Double, max: Double): Iso[Double, Double] =
    Iso[Double, Double](x => x)(x => math.max(min, math.min(max, x)))
  def multIso(mult: Double): Iso[Double, Double] =
    Iso[Double, Double](_ * mult)(_ / mult)

  def navbtn(name: String, action: => IO[Unit], disabled: Boolean = false) =
    <.button(
      name,
      ^.cls := "btn btn-default navbar-btn",
      ^.onClick ~~> ((e: ReactEvent) => for {
        _ <- e.preventDefaultIO
        _ <- action
      } yield ()),
      disabled ?= (^.disabled := true)
    )

  def navbtni(icon: String, name: String, action: => IO[Unit], disabled: Boolean = false) =
    <.button(
      <.i(^.cls := "fa fa-lg fa-" + icon), " ", name,
      ^.cls := "btn btn-default navbar-btn",
      ^.onClick ~~> ((e: ReactEvent) => for {
        _ <- e.preventDefaultIO
        _ <- action
      } yield ()),
      disabled ?= (^.disabled := true)
    )

  def navcheckbox(name: String, checked: Boolean, action: => IO[Unit], disabled: Boolean): ReactTag =
    <.div(^.cls := "form-group")(
      <.label(^.cls := "checkbox-inline")(
        <.input(
          ^.checked := checked,
          ^.`type` := "checkbox",
          ^.onChange ~~> action,
          disabled ?= (^.disabled := true)
        ),
        name
      )
    )

  def navcheckbox[A](name: String, rv: ReusableVar[A], lens: monocle.Lens[A, Boolean], disabled: Boolean = false): ReactTag =
    navcheckbox(name, lens.get(rv.value), rv.modL(lens)(!_), disabled)

  def navnumber(name: String, value: Double, action: Double => IO[Unit], disabled: Boolean): ReactTag = {
    <.div(^.cls := "form-group")(
      <.label(name, ^.paddingRight := 5.px),
      <.input(
        ^.cls := "form-control",
        ^.value := value,
        ^.`type` := "number",
        ^.onChange ~~> ((e: ReactEventI) => action(Try(e.target.value.toDouble).getOrElse(value))),
        ^.width := 80.px,
        disabled ?= (^.disabled := true)
      )
    )
  }

  def navnumber[A](name: String, rv: ReusableVar[A], lens: monocle.Lens[A, Double], disabled: Boolean = false): ReactTag =
    navnumber(name, lens.get(rv.value), rv.setL(lens), disabled)

  def navrange(range: NumericRange[Double], value: Double, action: Double => IO[Unit], disabled: Boolean): ReactTag =
    <.div(^.cls := "form-group")(<.input(
      ^.cls := "form-control",
      ^.`type` := "range",
      "min".reactAttr := range.start,
      "max".reactAttr := range.end,
      ^.step := range.step,
      ^.value := value,
      ^.onChange ~~> ((e: ReactEventI) => action(Try(e.target.value.toDouble).getOrElse(value))),
      disabled ?= (^.disabled := true)
    ))

  def navrange[A](range: NumericRange[Double], rv: ReusableVar[A], lens: monocle.Lens[A, Double], disabled: Boolean = false): ReactTag =
    navrange(range, lens.get(rv.value), rv.setL(lens), disabled)

  class Backend(val t: BackendScope[ReusableVar[AppState], String]) {
    def clickCenter = t.props.modL(AppState.view) { v =>
      t.props.value.bounds.fold(v) {
        case Bounds(x, y, width, height) =>
          val sx = v.width / width
          val sy = v.height / height
          val scale = math.min(sx, sy)
          View(x + width / 2, y + height / 2, scale * 0.975, v.width, v.height)
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
        val graph = t.props.value.graph
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

  implicit val reuseAppState: Reusability[AppState] =
    Reusability.by((a: AppState) => (
      a.annotationFontSize,
      a.bondLabels,
      a.gridWidth,
      a.showGrid,
      a.snapRotation,
      a.snapRotationDegrees,
      a.snapToGrid,
      a.view,
      a.undoPosition,
      a.history.length,
      a.buffer.isEmpty,
      a.selection match { case (rs, as) => rs.isEmpty && as.isEmpty }
    ))(Reusability.by_==)

  val C = ReactComponentB[ReusableVar[AppState]]("Navbar")
    .initialState("glycano")
    .backend(new Backend(_))
    .render { $ =>
      val appState = $.props
      val s = appState.value
      val zoom = appState.value.view.scale * 100
      val emptySelection = s.selection match {
        case (rs, as) => rs.isEmpty && as.isEmpty
      }

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
            " ", navbtn("Save .gly", IO($.backend.saveGly())),
            " ", navbtn("Save .svg", IO($.backend.saveSvg())),
            " ", navbtn("Save .png", IO($.backend.savePng())),
            " ", navcheckbox("Bond Labels", appState, AppState.bondLabels),
            " ", navbtn("Clear All", appState.setL(AppStateL.graphL)(RGraph()), s.graph.isEmpty),
            " ", navbtni("cut", "Cut", appState.mod(GlycanoApp.cutS.exec), emptySelection),
            " ", navbtni("copy", "Copy", appState.mod(GlycanoApp.copyS.exec), emptySelection),
            " ", navbtni("paste", "Paste", appState.mod(GlycanoApp.pasteS.exec), s.buffer.isEmpty),
            " ", navbtni("trash", "Delete", appState.mod(GlycanoApp.deleteS.exec), emptySelection),
            " ", navbtni("undo", "Undo", appState.mod(GlycanoApp.undo), s.undoPosition + 1 >= s.history.length),
            " ", navbtni("repeat", "Redo", appState.mod(GlycanoApp.redo), s.undoPosition == 0),
            " ", navbtni("edit", "Add Annotation", appState.mod(AppState.mode set Mode.PlaceAnnotation)),
            " ", navnumber("Annotation Font Size", appState, AppState.annotationFontSize),
            " ", navbtni("search-minus", "", appState.modL(AppState.view ^|-> View.scale)(_ / 1.1)),
            " ", navrange(0.0 to 200.0 by 0.01, appState, AppState.view ^|-> View.scale ^<-> multIso(100)),
            " ", <.span(f"$zoom%.2f" + "%"),
            " ", navbtni("search-plus", "", appState.modL(AppState.view ^|-> View.scale)(_ * 1.1)),
            " ", navbtn("Reset Zoom", appState.setL(AppState.view ^|-> View.scale)(1.0)),
            " ", navbtn("Center", $.backend.clickCenter),
            " ", navcheckbox("Snap To Grid", appState, AppState.snapToGrid),
            " ", navcheckbox("Show Grid", appState, AppState.showGrid),
            " ", navnumber("Grid Width:", appState, AppState.gridWidth),
            " ", navcheckbox("Snap Rotation To:", appState, AppState.snapRotation),
            " ", navnumber("", appState, AppState.snapRotationDegrees ^<-> clampIso(1, 180)),
            " ", <.span("º"),
            " ", $.propsChildren
          )
        )
      ))
    }
    .configure(Reusability.shouldComponentUpdate)
    .componentDidMount { $ =>
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
    }
    .build
}
