package za.jwatson.glycanoweb.react

import importedjs.filereaderjs.{FileReaderJS, Opts}
import japgolly.scalajs.react.{BackendScope, ReactEventI, ReactComponentB}
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.ReusableVar
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import za.jwatson.glycanoweb.Gly
import za.jwatson.glycanoweb.react.GlycanoApp.AppStateL
import za.jwatson.glycanoweb.react.semantic.Dropdown
import za.jwatson.glycanoweb.structure.RGraph

import scala.scalajs.js
import scalaz.effect.IO
import js.Dynamic.{global => g}

object MainMenu {
  type P = ReusableVar[RGraph]
  class B($: BackendScope[P, Unit]) {

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

  val Icon = ReactComponentB[Unit]("Icon")
    .render(_ => <.svg.svg(c"ui avatar image", ^.svg.width := "64px", ^.svg.height := "64px", ^.svg.viewBox := "-3 -3 106 86", ^.svg.xmlns := "http://www.w3.org/2000/svg")(
      <.svg.g(
        <.svg.polygon(^.svg.points := "90,40 65,80 25,80 0,40 25,0 65,0", ^.svg.fill := "#0000FF"),
        <.svg.polygon(^.svg.points := "90,40 65,80 25,80 0,40 25,0 65,0", ^.svg.fill := "none", ^.svg.stroke := "#000000", ^.svg.strokeWidth := "3")
      ),
      <.svg.rect(
        ^.svg.x := "80", ^.svg.y := "30", ^.svg.width := "20", ^.svg.height := "20", ^.svg.rx := "5", ^.svg.ry := "5",
        ^.svg.fill := "#FFFFFF", ^.svg.stroke := "#000000", ^.svg.strokeWidth := "1"
      )
    ))
    .buildU

  val C = ReactComponentB[P]("MainMenu")
    .stateless
    .backend(new B(_))
    .render { $ =>
      div"ui main menu"(
        div"ui item"(<.h3(Icon(), "Glycano")),
        div"ui item"(
          <.div(
            <.label(c"ui button", ^.ref := "dropfile")(
              <.input(^.ref := "loadfile", ^.tpe := "file", ^.display.none, ^.height := 0.px, ^.width := 0.px),
              "Open..."
            )
          )
        ),
        div"ui item"(
          div"ui right labeled input"(
            <.input(^.tpe := "text", ^.placeholder := "glycano", ^.ref := "filename"),
            Dropdown.Label("Save As...", Seq(
              <.a("Glycano (.gly)")(c"item", ^.onClick ~~> $.backend.saveGly, ^.key := "gly"),
              <.a("Vector (.svg)")(c"item", ^.onClick ~~> $.backend.saveSvg, ^.key := "svg"),
              <.a("Image (.png)")(c"item", ^.onClick ~~> $.backend.savePng, ^.key := "png")
            ))
          )
        )
      )
    }
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
