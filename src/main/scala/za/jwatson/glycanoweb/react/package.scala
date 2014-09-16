package za.jwatson.glycanoweb

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._

import org.scalajs.dom.HTMLButtonElement

import za.jwatson.glycanoweb.render.DisplayConv

package object react {

  def setConv(c: DisplayConv): Unit = {}

  class Backend(t: BackendScope[Map[String, DisplayConv], DisplayConv]) {
    def setDisplayConv(dc: DisplayConv)(e: SyntheticEvent[HTMLButtonElement]): Unit =
      t.setState(dc)
  }

  class RadioGroupBackend[A](t: BackendScope[Map[String, A], Option[(String, A)]], toggle: Boolean) {
    def handleClick(a: (String, A))(e: SyntheticEvent[HTMLButtonElement]): Unit =
      t.modState(old => if (toggle && old.contains(a)) None else Some(a))
  }

  def RadioGroup[A](name: String, toggle: Boolean = false) = ReactComponentB[Map[String, A]](name)
    .initialState[Option[(String, A)]](None)
    .backend(new RadioGroupBackend(_, toggle))
    .render((P, S, B) =>
      div(cls:="btn-group", "data-toggle".attr:="buttons")(
        for (a <- P.toSeq) yield button(a._1)(
          cls := (if (S.contains[(String, A)](a)) "btn btn-default active" else "btn btn-default"),
          onclick ==> B.handleClick(a)
        )
      )
    ).create

  val ConventionGroup = RadioGroup[DisplayConv]("ConventionGroup", toggle = true)(Map(
    "UCT" -> DisplayConv.UCT,
    "CFG" -> DisplayConv.UCT,
    "Oxford" -> DisplayConv.UCT
  ))


}
