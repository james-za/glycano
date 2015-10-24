package za.jwatson.glycanoweb.react

import importedjs.filereaderjs.{FileReaderJS, Opts}
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
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

  class Backend(val $: BackendScope[ReusableVar[RGraph], Boolean]) extends OnUnmount {
    val dataUrlSvg = CallbackTo {
      val svg = dom.document.getElementById("canvas").outerHTML
      val base64 = dom.window.btoa(g.unescape(g.encodeURIComponent(svg)).asInstanceOf[String])
      "data:image/svg+xml;base64," + base64
    }

    val dataUrlGly = for (rvGraph <- $.props) yield {
      import upickle._, Gly._
      val graph = rvGraph.value
      val gly = write[Gly](Gly.from(graph))
      val base64 = dom.window.btoa(g.unescape(g.encodeURIComponent(gly)).asInstanceOf[String])
      "data:text/plain;base64," + base64
    }

    def nameInput = $.refs[dom.html.Input]("filename").map(_.getDOMNode())

    val baseName = CallbackTo(nameInput.map(_.value).filter(_.nonEmpty).getOrElse("glycano"))

    def save(extension: String, dataUrlCB: CallbackTo[String]) = for {
      baseName <- baseName
      dataUrl <- dataUrlCB
      _ <- Callback {
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
      _ <- Callback(g.saveSvgAsPng(dom.document.getElementById("canvas"), name + ".png"))
    } yield ()

    def loadGly(name: String, gly: Gly) = for {
      rvGraph <- $.props
      _ <- rvGraph.set(gly.toRGraph)
      _ <- Callback(nameInput.foreach(_.value = if (name.endsWith(".gly")) name.dropRight(4) else name))
    } yield ()
  }

  val C = ReactComponentB[ReusableVar[RGraph]]("Navbar")
    .initialState(false)
    .backend(new Backend(_))
    .render { $ =>
      implicit val graph: RGraph = $.props.value

      <.nav(c"navbar navbar-default", ^.role := "navigation")(
        div"container-fluid"(
          NavbarHeader("glycano-navbar-collapse", Icon.C.withKey("icon")(), <.span("Glycano", ^.key := "text")),
          div"collapse navbar-collapse"(
            <.form(c"navbar-form")(
              div"form-group"(
                <.label("Load:")(^.marginLeft := 5.px),
                <.input(c"form-control", ^.tpe := "file", ^.ref := "loadfile")(^.marginLeft := 5.px)
              ),
              div"form-group"(
                <.label("Filename:")(^.marginLeft := 5.px),
                div"input-group"(^.marginLeft := 5.px)(
                  <.input(c"form-control", ^.tpe := "text", ^.ref := "filename", ^.placeholder := "Filename"),
                  div"input-group-btn"(
                    $.state ?= c"open",
                    <.button(
                      c"btn btn-default dropdown-toggle", ^.ref := "toggle",
                      ^.onClick ==> (preventDefault(_: ReactMouseEvent) >> $.modState(!_))
                    )("Save As...", <.span(c"caret")),
                    <.ul(c"dropdown-menu dropdown-menu-right", ^.ref := "dropmenu")(
                      <.li(<.a(c"btn", "Glycano (.gly)", ^.onClick --> $.backend.saveGly)),
                      <.li(c"divider"),
                      <.li(<.a(c"btn", "Vector (.svg)", ^.onClick --> $.backend.saveSvg)),
                      <.li(<.a(c"btn", "Image (.png)", ^.onClick --> $.backend.savePng))
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .configure(EventListener[dom.Event].install(
      "click",
      $ => e => {
        val node = e.target.asInstanceOf[dom.Node]
        for {
          toggleElement <- CallbackOption.liftOptionLike($.refs[dom.Element]("toggle"))
          if !hasParent(node, toggleElement.getDOMNode())
          dropMenu <- CallbackOption.liftOptionLike($.refs[dom.Element]("dropmenu"))
          if !hasParent(node, dropMenu.getDOMNode())
          _ <- $.setState(false)
        } yield ()
      },
      _ => dom.document.body
    ))
    .componentDidMount { $ =>
      CallbackOption.liftOptionLike($.refs[dom.html.Input]("loadfile")).map { load =>
        val fileReaderOpts = Opts.load((e: dom.ProgressEvent, file: dom.File) => {
          import upickle._, Gly._
          val str = e.target.asInstanceOf[js.Dynamic].result.asInstanceOf[String]
          Try($.backend.loadGly(file.name, read[Gly](str)(rwGly)).runNow())
        })
        fileReaderOpts.readAsDefault = "Text"
        fileReaderOpts.dragClass = "blue"
        val node = load.getDOMNode()
        FileReaderJS.setupInput(node, fileReaderOpts)
        FileReaderJS.setupDrop(node, fileReaderOpts)
      }
    }
    .build
}
