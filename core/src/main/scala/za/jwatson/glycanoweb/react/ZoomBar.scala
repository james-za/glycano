package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.extra.ReusableVar
import japgolly.scalajs.react.vdom.prefix_<^._
import za.jwatson.glycanoweb.react.GlycanoCanvas.View

object ZoomBar {
  val C = ReactComponentB[ReusableVar[View]]("ZoomBar")
    .render(_ => div"")
    .build
}
