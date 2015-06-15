package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.extra.ReusableVar
import japgolly.scalajs.react.{BackendScope, ReactComponentB}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.ScalazReact._
import za.jwatson.glycanoweb.react.semantic.Dropdown
import za.jwatson.glycanoweb.structure.RGraph
import scalajs.js

import scalaz.effect.IO

object ToolBar {
  type P = (ReusableVar[RGraph])
  class B($: BackendScope[P, Unit]) {
    val clearAll = IO.putStrLn("clearAll")//$.props.set(RGraph())
    val cut = IO.putStrLn("cut")
    val copy = IO.putStrLn("copy")
    val paste = IO.putStrLn("paste")
    val undo = IO.putStrLn("undo")
    val redo = IO.putStrLn("redo")
    val addAnnotation = IO.putStrLn("addAnnotation")
  }

  val C = ReactComponentB[P]("ToolBar")
    .stateless
    .backend(new B(_))
    .render { $ =>
      div"sixteen wide column"(
        div"ui small labeled icon floated menu"(
          <.a(c"item", <.i(c"file icon"), "Clear All", ^.onClick ~~> $.backend.clearAll)
        ),
        div"ui small labeled icon floated menu"(
          <.a(c"item", <.i(c"cut icon"), "Cut", ^.onClick ~~> $.backend.cut),
          <.a(c"item", <.i(c"copy icon"), "Copy", ^.onClick ~~> $.backend.copy),
          <.a(c"item", <.i(c"paste icon"), "Paste", ^.onClick ~~> $.backend.paste)
        ),
        div"ui small labeled icon floated menu"(
          <.a(c"item", <.i(c"undo icon"), "Undo", ^.onClick ~~> $.backend.undo),
          <.a(c"item", <.i(c"repeat icon"), "Redo", ^.onClick ~~> $.backend.redo)
        ),
        div"ui small labeled icon floated menu"(
          <.a(c"item", <.i(c"edit icon"), "Add Annotation", ^.onClick ~~> $.backend.addAnnotation),
          Dropdown.NarrowToggle(("", true),
            div"header"("Font Size:", ^.key := 0),
            div"ui fluid input header"(^.key := 1)(
              <.input(^.tpe := "number", ^.paddingRight := "1em !important", ^.width := 70.px)
            )
          )
        )
      )
    }
    .build
}
