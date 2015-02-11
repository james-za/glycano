package za.jwatson.glycanoweb.render

import japgolly.scalajs.react.vdom.prefix_<^._
import za.jwatson.glycanoweb.structure.{SubstituentType => ST}

import scalajs.js

object SubstituentShape {
  def apply(st: ST, scale: Double) = st match {
    case ST.n => <.svg.svg(^.svg.width := 24 * scale, ^.svg.height := 30 * scale, ^.svg.viewBox := "0 0 24 30")(
      <.svg.rect(^.svg.width := 24, ^.svg.height := 30, ^.svg.rx := 5, ^.svg.ry := 5, ^.svg.fill := "#86CEFF", ^.svg.stroke := "black"),
      <.svg.text(^.svg.textAnchor := "middle", "textShadow".reactStyle := "none", ^.svg.fontSize := 30, ^.svg.x := 12, ^.svg.y := 26, ^.svg.fill := "black")("N")
    )
    case ST.cooh => <.svg.svg(^.svg.width := 26 * scale, ^.svg.height := 26 * scale, ^.svg.viewBox := "0 0 26 26")(
      <.svg.circle(^.svg.cx := 13, ^.svg.cy := 13, ^.svg.r := 13, ^.svg.fill := "white", ^.svg.stroke := "black"),
      <.svg.path(^.svg.transform := "scale(0.65) translate(4, 4)", ^.svg.fill := "black", ^.svg.d := """M 0,14.355469 2.2460938,7.421875 C 7.4218645,9.2448552 11.181626,10.82363 13.525391,12.158203 12.906885,6.2663426 12.581365,2.2136123 12.548828,0 l 7.080078,0 c -0.09768,3.2227258 -0.472027,7.2591801 -1.123047,12.109375 3.35284,-1.692646 7.193982,-3.2551444 11.523438,-4.6875 l 2.246094,6.933594 c -4.134146,1.367244 -8.186877,2.278702 -12.158204,2.734375 1.985652,1.725314 4.785129,4.801483 8.398438,9.228515 L 22.65625,30.46875 C 20.768205,27.89718 18.53839,24.397835 15.966797,19.970703 13.557926,24.560595 11.442043,28.059941 9.6191406,30.46875 L 3.8574219,26.318359 C 7.6334528,21.663463 10.335273,18.587294 11.962891,17.089844 7.763661,16.276098 3.7760348,15.364641 0,14.355469""")
    )
    case ST.methyl => <.svg.svg(^.svg.width := 20 * scale, ^.svg.height := 36 * scale, ^.svg.viewBox := "0 0 20 36")(
      <.svg.rect(^.svg.width := 20, ^.svg.height := 36, ^.svg.fill := "white", ^.svg.stroke := "black"),
      <.svg.path(^.svg.fill := "black", ^.svg.transform := "translate(10, 2) scale(22)", ^.svg.d := """M0,0 L-0.25,1 C-0.375,1.5 0.375,1.5 0.25,1 Z""")
    )
    case ST.deoxy => <.svg.svg(^.svg.width := 20 * scale, ^.svg.height := 20 * scale, ^.svg.viewBox := "0 0 20 20")(
      <.svg.circle(^.svg.cx := 10, ^.svg.cy := 10, ^.svg.r := 10, ^.svg.fill := "white", ^.svg.stroke := "black"),
      <.svg.line(^.svg.x1 := 4, ^.svg.y1 := 4, ^.svg.x2 := 16, ^.svg.y2 := 16, ^.svg.strokeWidth := 3, ^.svg.stroke := "black"),
      <.svg.line(^.svg.x1 := 4, ^.svg.y1 := 16, ^.svg.x2 := 16, ^.svg.y2 := 4, ^.svg.strokeWidth := 3, ^.svg.stroke := "black")
    )
    case ST.s => <.svg.svg(^.svg.width := 30 * scale, ^.svg.height := 30 * scale, ^.svg.viewBox := "0 0 30 30")(
      <.svg.circle(^.svg.cx := 15, ^.svg.cy := 15, ^.svg.r := 15, ^.svg.fill := "#FFFF00", ^.svg.stroke := "black"),
      <.svg.text(^.svg.textAnchor := "middle", "textShadow".reactStyle := "none", ^.svg.fontSize := 30, ^.svg.x := 15, ^.svg.y := 26, ^.svg.fill := "black")("S")
    )
    case ST.p => <.svg.svg(^.svg.width := 30 * scale, ^.svg.height := 30 * scale, ^.svg.viewBox := "0 0 30 30")(
      <.svg.circle(^.svg.cx := 15, ^.svg.cy := 15, ^.svg.r := 15, ^.svg.fill := "#8E008E", ^.svg.stroke := "black"),
      <.svg.text(^.svg.textAnchor := "middle", "textShadow".reactStyle := "none", ^.svg.fontSize := 30, ^.svg.x := 15, ^.svg.y := 26, ^.svg.fill := "white")("P")
    )
    case ST.ac => <.svg.svg(^.svg.width := 44 * scale, ^.svg.height := 40 * scale, ^.svg.viewBox := "0 0 44 40")(
      <.svg.polygon(^.svg.points := "22,0 0,40 44,40", ^.svg.fill := "white", ^.svg.stroke := "black"),
      <.svg.text(^.svg.textAnchor := "middle", "textShadow".reactStyle := "none", ^.svg.fontSize := 24, ^.svg.x := 22, ^.svg.y := 36, ^.svg.fill := "black")("Ac")
    )
  }
}