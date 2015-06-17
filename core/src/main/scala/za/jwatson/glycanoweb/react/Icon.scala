package za.jwatson.glycanoweb.react

import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._

object Icon {
  val C = ReactComponentB[Unit]("Icon")
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
}
