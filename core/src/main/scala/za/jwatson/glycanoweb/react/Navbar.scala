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

  class Backend(val t: BackendScope[ExternalVar[AppState], String]) {
    def clickCenter = t.props.modL(AppState.view) { v =>
      val (x, y, w, h) = t.props.value.fitBounds
      val sx = v.width / w
      val sy = v.height / h
      val scale = math.min(sx, sy)
      View(x + w / 2, y + h / 2, scale * 0.975, v.width, v.height)
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
        val a = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
        a.setAttribute("download", name)
        val svg = dom.document.getElementById("canvas").outerHTML
        val base64 = dom.window.btoa(g.unescape(g.encodeURIComponent(svg)).asInstanceOf[String])
        val dataUrl = "data:image/svg+xml;base64," + base64
        a.setAttribute("href", dataUrl)
        dom.document.body.appendChild(a)
        a.click()
      }
    }

    def saveGly(): Unit = {
      import js.Dynamic.{global => g}
      import upickle._, Gly._
      for (fn <- t.refs[dom.html.Input]("filename").map(_.getDOMNode())) {
        val base = if (fn.value.isEmpty) "glycano" else fn.value
        val name = if (base.endsWith(".gly")) base else base + ".gly"
        val a = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
        a.setAttribute("download", name)
        val graph = AppStateL.graph(t.props.value)
        val gly = write[Gly](Gly.from(graph))
        val base64 = dom.window.btoa(g.unescape(g.encodeURIComponent(gly)).asInstanceOf[String])
        val dataUrl = "data:image/svg+xml;base64," + base64
        a.setAttribute("href", dataUrl)
        dom.document.body.appendChild(a)
        a.click()
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
          <.ul(^.cls := "nav navbar-nav")(
            //saveDropdown
          ),
          <.form(^.cls := "form-inline")(
            " ", navbtn_("Save .gly", { () => $.backend.saveGly() }),
            " ", navbtn_("Save .svg", { () => $.backend.saveSvg() }),
            " ", <.div(^.cls := "form-group")(
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
