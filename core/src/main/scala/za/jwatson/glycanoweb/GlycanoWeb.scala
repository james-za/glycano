package za.jwatson.glycanoweb

import org.scalajs.dom
import za.jwatson.glycanoweb.react.GlycanoApp
import za.jwatson.glycanoweb.render.DisplayConv

import scala.scalajs.js.JSApp

object GlycanoWeb extends JSApp {
  def main(): Unit = {
    val props = GlycanoApp.Props(conventions = DisplayConv.conventions)
    GlycanoApp.C(props).render(dom.document.body)
  }
}