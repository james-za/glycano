package za.jwatson.glycanoweb.render

import japgolly.scalajs.react.vdom.prefix_<^._
import za.jwatson.glycanoweb.structure.{SubstituentType => ST}

object SubstituentShape {
  def apply(st: ST) = substShapes(st)
  val substShapes = Map(
    ST.n -> <.svg.rect(^.svg.width := 20, ^.svg.height := 30, ^.svg.rx := 5, ^.svg.fill := "#86CEFF", ^.svg.stroke := "black")(
      <.svg.text(^.svg.fontSize := 30, ^.svg.fill := "black")("N")
    ),
    ST.cooh -> <.svg.circle(^.svg.radius := 10, ^.svg.fill := "white", ^.svg.stroke := "black")(
      <.svg.path(^.svg.fill := "black", ^.svg.d := """M 0,14.355469 2.2460938,7.421875 C 7.4218645,9.2448552 11.181626,10.82363 13.525391,12.158203 12.906885,6.2663426 12.581365,2.2136123 12.548828,0 l 7.080078,0 c -0.09768,3.2227258 -0.472027,7.2591801 -1.123047,12.109375 3.35284,-1.692646 7.193982,-3.2551444 11.523438,-4.6875 l 2.246094,6.933594 c -4.134146,1.367244 -8.186877,2.278702 -12.158204,2.734375 1.985652,1.725314 4.785129,4.801483 8.398438,9.228515 L 22.65625,30.46875 C 20.768205,27.89718 18.53839,24.397835 15.966797,19.970703 13.557926,24.560595 11.442043,28.059941 9.6191406,30.46875 L 3.8574219,26.318359 C 7.6334528,21.663463 10.335273,18.587294 11.962891,17.089844 7.763661,16.276098 3.7760348,15.364641 0,14.355469""")
    ),
    ST.methyl -> <.svg.rect(^.svg.width := 10, ^.svg.height := 30, ^.svg.fill := "white", ^.svg.stroke := "black")(
      <.svg.path(^.svg.fill := "black", ^.svg.transform := "scale(22)", ^.svg.d := """M0,0 L-0.25,1 C-0.375,1.5 0.375,1.5 0.25,1 Z""")
    ),
    ST.deoxy -> <.svg.circle(^.svg.radius := 8, ^.svg.fill := "white", ^.svg.stroke := "black")(
      <.svg.path(^.svg.fill := "black", ^.svg.d := """M 476.82,418.45 L 486.73,428.41 C 487.28,428.96 487.38,429.06 487.38,429.46 C 487.38,430.01 486.93,430.46 486.39,430.46 C 485.99,430.46 485.79,430.26 485.34,429.81 L 475.38,419.85 L 465.36,429.81 C 464.82,430.36 464.72,430.46 464.32,430.46 C 463.82,430.46 463.32,430.01 463.32,429.46 C 463.32,429.06 463.52,428.86 463.97,428.41 L 473.88,418.45 L 463.97,408.54 C 463.47,408.04 463.32,407.74 463.32,407.45 C 463.32,406.9 463.82,406.45 464.32,406.45 C 464.72,406.45 464.82,406.55 465.36,407.1 L 475.33,417.06 L 485.29,407.1 C 485.79,406.6 486.09,406.45 486.39,406.45 C 486.98,406.45 487.38,406.9 487.38,407.45 C 487.38,407.84 487.28,407.94 486.73,408.49 L 476.82,418.45 z """)
    ),
    ST.s -> <.svg.circle(^.svg.radius := 15, ^.svg.fill := "#FFFF00", ^.svg.stroke := "black")(
      <.svg.text(^.svg.fontSize := 30, ^.svg.fill := "black")("S")
    ),
    ST.p -> <.svg.circle(^.svg.radius := 15, ^.svg.fill := "#8E008E", ^.svg.stroke := "black")(
      <.svg.text(^.svg.fontSize := 30, ^.svg.fill := "white")("P")
    ),
    ST.ac -> <.svg.polygon(^.svg.points := "0,20 -20,20 20,20", ^.svg.fill := "white", ^.svg.stroke := "black")(
      <.svg.text(^.svg.fontSize := 24, ^.svg.fill := "black")("Ac")
    )
  )
}