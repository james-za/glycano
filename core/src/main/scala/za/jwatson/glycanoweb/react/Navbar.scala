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

import js.Dynamic.{global => g}

import scala.util.Try
import scalaz.effect.IO

object Navbar {
//  def clampIso(min: Double, max: Double): Iso[Double, Double] =
//    Iso[Double, Double](x => x)(x => math.max(min, math.min(max, x)))
//  def multIso(mult: Double): Iso[Double, Double] =
//    Iso[Double, Double](_ * mult)(_ / mult)

  def navrange(range: NumericRange[Double], value: Double, action: Double => IO[Unit], disabled: Boolean): ReactTag =
    div"form-group"(<.input(
      c"form-control",
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

  class Backend(val $: BackendScope[ReusableVar[RGraph], Unit]) {
//    def clickCenter = $.props.modL(AppState.view) { v =>
//      $.props.value.bounds.fold(v) {
//        case Bounds(x, y, width, height) =>
//          val sx = v.width / width
//          val sy = v.height / height
//          val scale = math.min(sx, sy)
//          View(x + width / 2, y + height / 2, scale * 0.975, v.width, v.height)
//      }
//    }

    val dataUrlSvg = IO {
      val svg = dom.document.getElementById("canvas").outerHTML
      val base64 = dom.window.btoa(g.unescape(g.encodeURIComponent(svg)).asInstanceOf[String])
      "data:image/svg+xml;base64," + base64
    }

    val dataUrlGly = IO {
      import upickle._, Gly._
      val graph = $.props.value
      val gly = write[Gly](Gly.from(graph))
      val base64 = dom.window.btoa(g.unescape(g.encodeURIComponent(gly)).asInstanceOf[String])
      "data:text/plain;base64," + base64
    }

    def nameInput = $.refs[dom.html.Input]("filename").map(_.getDOMNode())

    val baseName = IO(nameInput.map(_.value).filter(_.nonEmpty).getOrElse("glycano"))

    def save(extension: String, dataUrlIO: IO[String]) = for {
      baseName <- baseName
      dataUrl <- dataUrlIO
      _ <- IO {
        val a = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
        a.asInstanceOf[js.Dynamic].download = baseName + extension
        a.asInstanceOf[js.Dynamic].href = dataUrl
        dom.document.body.appendChild(a)
        a.click()
      }
    } yield ()

    val saveSvg = save(".svg", dataUrlSvg)
    val saveGly = save(".gly", dataUrlGly)
    val savePng = for {
      name <- baseName
      _ <- IO(g.saveSvgAsPng(dom.document.getElementById("canvas"), name + ".png"))
    } yield ()

    def loadGly(name: String, gly: Gly) = for {
      _ <- $.props.set(gly.toRGraph)
      _ <- IO(nameInput.foreach(_.value = if (name.endsWith(".gly")) name.dropRight(3) else name))
    } yield ()
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

  val C = ReactComponentB[ReusableVar[RGraph]]("Navbar")
    .stateless
    .backend(new Backend(_))
    .render { $ =>
      implicit val graph: RGraph = $.props.value

      <.nav(c"navbar navbar-default", ^.role := "navigation")(div"container-fluid"(
        NavbarHeader("glycano-navbar-collapse", "Glycano"),
        div"collapse navbar-collapse"(^.id := "glycano-navbar-collapse")(
          <.p(c"navbar-text", "Load:"),
          <.form(c"navbar-form navbar-left")(
            div"form-group"(
              <.input(
                ^.ref := "loadfile",
                ^.`type` := "file",
                c"form-control",
                ^.onChange ==> { (e: ReactEventI) =>
                  println(e.target.files(0).name)
                }
              )
            )
          ),
          <.p(c"navbar-text", "Filename:"),
          <.form(c"navbar-form navbar-left")(
            div"form-group"(
              div"input-group"(
                <.input(
                  ^.ref := "filename",
                  ^.`type` := "text",
                  c"form-control",
                  ^.placeholder := "Filename"
                ),
                div"input-group-btn"(
                  <.button(c"btn btn-default dropdown-toggle")("Save As...", <.span(c"caret")),
                  <.ul(c"dropdown-menu dropdown-menu-right")(
                    <.li(<.a("Glycano (.gly)", ^.onClick ~~> $.backend.saveGly)),
                    <.li(c"divider"),
                    <.li(<.a("Vector (.svg)", ^.onClick ~~> $.backend.saveSvg)),
                    <.li(<.a("Image (.png)", ^.onClick ~~> $.backend.savePng))
                  )
                )
              )
            )
          ),
          <.form(c"form-inline")(
//            " ", navbtn("Save .gly", $.backend.saveGly),
//            " ", navbtn("Save .svg", $.backend.saveSvg),
//            " ", navbtn("Save .png", $.backend.savePng)
//            " ", navbtni("search-minus", "", appState.modL(AppState.view ^|-> View.scale)(_ / 1.1)),
//            " ", navrange(0.0 to 200.0 by 0.01, appState, AppState.view ^|-> View.scale ^<-> multIso(100)),
//            " ", <.span(f"$zoom%.2f" + "%"),
//            " ", navbtni("search-plus", "", appState.modL(AppState.view ^|-> View.scale)(_ * 1.1)),
//            " ", navbtn("Reset Zoom", appState.setL(AppState.view ^|-> View.scale)(1.0)),
//            " ", navbtn("Center", $.backend.clickCenter)
          )
        )
      ))
    }
    .configure(Reusability.shouldComponentUpdate)
    .componentDidMount { $ =>
      for {
        load <- $.refs[dom.html.Input]("loadfile")
        drop <- $.refs[dom.html.Label]("dropfile")
      } {
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
        fileReaderOpts.dragClass = "blue"
        FileReaderJS.setupInput(load.getDOMNode(), fileReaderOpts)
        FileReaderJS.setupDrop(drop.getDOMNode(), fileReaderOpts)
      }
    }
    .build
}
