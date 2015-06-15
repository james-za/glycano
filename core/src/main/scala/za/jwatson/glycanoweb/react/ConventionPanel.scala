package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import za.jwatson.glycanoweb.react.semantic.RadioButtons
import za.jwatson.glycanoweb.render.DisplayConv

object ConventionPanel {
  val getNameDisplayConvFn: DisplayConv ~=> String = ReusableFn(_.name)
  val ConventionChoice = RadioButtons[DisplayConv](fluid = true)
  val C = ReactComponentB[ReusableVar[Option[DisplayConv]]]("ConventionPanel")
    .render { props =>
      div"ui segment"(
        <.h5(c"ui center aligned header", div"content"("Convention")),
        ConventionChoice(RadioButtons.Props[DisplayConv](
          props, DisplayConv.conventions.values.toSeq,
          getNameDisplayConvFn, toggle = false
        ))
      )
    }
    .build
}
