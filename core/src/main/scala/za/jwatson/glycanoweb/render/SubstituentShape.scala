package za.jwatson.glycanoweb.render

import japgolly.scalajs.react.vdom.prefix_<^._
import za.jwatson.glycanoweb.structure.{SubstituentType => ST}

import scala.scalajs.js

object SubstituentShape {
  def apply(st: ST): (ReactTag, (Double, Double)) = memo.getOrElseUpdate(st.symbol, of(st))
  val memo = js.Dictionary.empty[(ReactTag, (Double, Double))]
  def of(st: ST): (ReactTag, (Double, Double)) = st match {
    case ST.n => (<.svg.g(
      <.svg.rect(^.svg.width := 24, ^.svg.height := 30, ^.svg.rx := 5, ^.svg.ry := 5, ^.svg.fill := "#86CEFF", ^.svg.stroke := "black"),
      <.svg.text(^.svg.pointerEvents := "none", ^.svg.textAnchor := "middle", "textShadow".reactStyle := "none", ^.svg.fontSize := 30, ^.svg.x := 12, ^.svg.y := 26, ^.svg.fill := "black")("N")
    ), (24, 30))
    case ST.cooh => (<.svg.g(
      <.svg.circle(^.svg.cx := 13, ^.svg.cy := 13, ^.svg.r := 13, ^.svg.fill := "white", ^.svg.stroke := "black"),
      <.svg.path(^.svg.transform := "scale(0.65) translate(4, 4)", ^.svg.fill := "black", ^.svg.d := """M 0,14.355469 2.2460938,7.421875 C 7.4218645,9.2448552 11.181626,10.82363 13.525391,12.158203 12.906885,6.2663426 12.581365,2.2136123 12.548828,0 l 7.080078,0 c -0.09768,3.2227258 -0.472027,7.2591801 -1.123047,12.109375 3.35284,-1.692646 7.193982,-3.2551444 11.523438,-4.6875 l 2.246094,6.933594 c -4.134146,1.367244 -8.186877,2.278702 -12.158204,2.734375 1.985652,1.725314 4.785129,4.801483 8.398438,9.228515 L 22.65625,30.46875 C 20.768205,27.89718 18.53839,24.397835 15.966797,19.970703 13.557926,24.560595 11.442043,28.059941 9.6191406,30.46875 L 3.8574219,26.318359 C 7.6334528,21.663463 10.335273,18.587294 11.962891,17.089844 7.763661,16.276098 3.7760348,15.364641 0,14.355469""")
    ), (26, 26))
    case ST.methyl => (<.svg.g(
      <.svg.rect(^.svg.width := 20, ^.svg.height := 36, ^.svg.fill := "white", ^.svg.stroke := "black"),
      <.svg.path(^.svg.fill := "black", ^.svg.transform := "translate(10, 2) scale(22)", ^.svg.d := """M0,0 L-0.25,1 C-0.375,1.5 0.375,1.5 0.25,1 Z""")
    ), (20, 36))
    case ST.deoxy => (<.svg.g(
      <.svg.circle(^.svg.cx := 15, ^.svg.cy := 15, ^.svg.r := 15, ^.svg.fill := "black"),
      <.svg.line(^.svg.x1 := 6, ^.svg.y1 := 6, ^.svg.x2 := 24, ^.svg.y2 := 24, ^.svg.strokeWidth := 6, ^.svg.stroke := "white"),
      <.svg.line(^.svg.x1 := 6, ^.svg.y1 := 24, ^.svg.x2 := 24, ^.svg.y2 := 6, ^.svg.strokeWidth := 6, ^.svg.stroke := "white")
    ), (30, 30))
    case ST.s => (<.svg.g(
      <.svg.circle(^.svg.cx := 15, ^.svg.cy := 15, ^.svg.r := 15, ^.svg.fill := "#FFFF00", ^.svg.stroke := "black"),
      <.svg.text(^.svg.pointerEvents := "none", ^.svg.textAnchor := "middle", "textShadow".reactStyle := "none", ^.svg.fontSize := 30, ^.svg.x := 15, ^.svg.y := 26, ^.svg.fill := "black")("S")
    ), (30, 30))
    case ST.p => (<.svg.g(
      <.svg.circle(^.svg.cx := 15, ^.svg.cy := 15, ^.svg.r := 15, ^.svg.fill := "#8E008E", ^.svg.stroke := "black"),
      <.svg.text(^.svg.pointerEvents := "none", ^.svg.textAnchor := "middle", "textShadow".reactStyle := "none", ^.svg.fontSize := 30, ^.svg.x := 15, ^.svg.y := 26, ^.svg.fill := "white")("P")
    ), (30, 30))
    case ST.ac => (<.svg.g(
      <.svg.polygon(^.svg.points := "22,0 0,40 44,40", ^.svg.fill := "#FF8080", ^.svg.stroke := "black", ^.svg.strokeWidth := 3),
      <.svg.text(^.svg.pointerEvents := "none", ^.svg.textAnchor := "middle", "textShadow".reactStyle := "none", ^.svg.fontSize := 20, ^.svg.x := 19.5, ^.svg.y := 37, ^.svg.fill := "black")("Ac")
        ("fontWeight".reactStyle := "bold", "fontStyle".reactStyle := "italic")
    ), (44, 40))
    case ST.am => (<.svg.g(
      <.svg.polygon(^.svg.points := "22,0 0,40 44,40", ^.svg.fill := "#00D1D1", ^.svg.stroke := "black", ^.svg.strokeWidth := 3),
      <.svg.text(^.svg.pointerEvents := "none", ^.svg.textAnchor := "middle", "textShadow".reactStyle := "none", ^.svg.fontSize := 20, ^.svg.x := 20.5, ^.svg.y := 37, ^.svg.fill := "black")("Am")
        ("fontWeight".reactStyle := "bold", "fontStyle".reactStyle := "italic")
    ), (44, 40))
    case ST.r => (<.svg.g(
      <.svg.rect(^.svg.width := 20, ^.svg.height := 24, ^.svg.fill := "white", ^.svg.stroke := "black", ^.svg.strokeWidth := 0.8),
      <.svg.path(^.svg.fill := "none", ^.svg.stroke := "black", ^.svg.strokeWidth := 1.5, ^.svg.transform := "translate(1, 1) rotate(90 9 9)", ^.svg.d := sinePath(22, 18, 2, 24 * 2))
    ), (20, 24))
  }
  def sinePath(w: Double, h: Double, n: Double, lines: Int): String = {
    val parts = for (i <- 0 until lines) yield {
      val t = i.toDouble / lines
      val x = t * w
      val y = (math.sin(t * 2 * math.Pi * n) + 1) / 2 * h
      f"L$x%.3f,$y%.3f"
    }
    f"M0,${h / 2}%.3f" + parts.mkString(" ")
  }
}