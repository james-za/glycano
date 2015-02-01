package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import monocle.macros.Lenses
import za.jwatson.glycanoweb.GlyAnnot
import za.jwatson.glycanoweb.render.GlycanoCanvas.InputState
import za.jwatson.glycanoweb.structure.RGraph.GraphEntry

import za.jwatson.glycanoweb.structure._

import scalajs.js

object GlycanoCanvas {
  case class Props(B: GlycanoApp.Backend, width: Int = 800, height: Int = 600, graph: RGraph, selection: (Set[Residue], Set[GlyAnnot]), view: View = View())
  @Lenses case class State(hoverHandle: Option[Residue] = None, inputState: InputState = InputState.Default)
  @Lenses case class View(x: Double = 0, y: Double = 0, scale: Double = 1)
  class Backend(t: BackendScope[Props, State]) {
    def hoverHandle(r: Residue): Unit = t.modState(State.hoverHandle set Some(r))
    def leaveHandle(r: Residue): Unit = t.modState(State.hoverHandle set None)
  }

  val defaultPoints = "90,40 65,80 25,80 0,40 25,0 65,0"
  def defaultShape(B: Backend, r: Residue, hh: Boolean) = <.svg.g(
    <.svg.polygon(
      ^.svg.points := defaultPoints,
      ^.svg.fill := "blue",
      ^.svg.stroke := "black",
      "strokeWidth".reactAttr := 3
    ),
    <.svg.rect(
      ^.svg.x := 90 - 10, ^.svg.y := 40 - 10,
      ^.svg.width := 20, ^.svg.height := 20,
      ^.svg.rx := 5, ^.svg.ry := 5,
      ^.svg.fill := "white",
      ^.svg.stroke := (if (hh) "blue" else "black"),
      "strokeWidth".reactAttr := (if (hh) 2 else 1),
      ^.onMouseOver --> B.hoverHandle(r),
      ^.onMouseOut --> B.leaveHandle(r)
    )
  )
  def defaultShape(ano: Anomer, abs: Absolute, rt: ResidueType, scale: Double = 1) = <.svg.g(
    ^.svg.transform := s"scale($scale)",
    <.svg.polygon(
      ^.svg.points := defaultPoints,
      ^.svg.fill := "blue",
      ^.svg.stroke := "black",
      "strokeWidth".reactAttr := 3
    ),
    <.svg.rect(
      ^.svg.x := 90 - 10, ^.svg.y := 40 - 10,
      ^.svg.width := 20, ^.svg.height := 20,
      ^.svg.rx := 5, ^.svg.ry := 5,
      ^.svg.fill := "white",
      ^.svg.stroke := "black",
      "strokeWidth".reactAttr := 1
    )
  )
  val defaultOutline = defaultPoints.split("[, ]").map(_.toDouble).grouped(2).map(a => (a(0), a(1))).toIndexedSeq
  def outlinePos(r: Residue, ge: GraphEntry, i: Int): (Double, Double) = {
    val (x, y) = defaultOutline(i)
    import importedjs.{paper => p}
    val point = p.Point(x, y).rotate(ge.rotation, p.Point(0, 0))
    (ge.x + point.x, ge.y + point.y)
  }

  def apply(props: Props, children: ReactNode*) = component.apply(props, children)
  val component = ReactComponentB[Props]("GlycanoCanvas")
    .initialState(State())
    .backend(new Backend(_))
    .render((P, C, S, B) => {
      val View(vx, vy, vs) = P.view

      <.svg.svg(
        ^.svg.width := P.width,
        ^.svg.height := P.height
      )(
        <.svg.g(^.svg.transform := s"translate($vx $vy) scale($vs)")(
          for {
            (r @ Residue(_, rt, ano, abs), ge) <- P.graph.entries
            Link(to, i) <- ge.parent
          } yield {
            val (x1, y1) = outlinePos(r, ge, 0)
            val (x2, y2) = outlinePos(to, P.graph.entries(to), i)
            <.svg.line(
              ^.svg.x1 := x1, ^.svg.y1 := y1,
              ^.svg.x2 := x2, ^.svg.y2 := y2,
              ^.svg.stroke := "black", "strokeWidth".reactAttr := "7"
            )
          },
          for ((r @ Residue(_, rt, ano, abs), GraphEntry(x, y, rot, _, _, subs)) <- P.graph.entries) yield {
            <.svg.g(^.svg.transform := s"translate($x $y) rotate($rot)")(
              defaultShape(B, r, S.hoverHandle.contains(r)),
              <.svg.text(
                ^.svg.fill := "red",
                "textAnchor".reactAttr := "middle"
              )(r.desc, P.selection._1.contains(r) ?= " (selected)")
            )
          }
        )
      )
    })
    //.domType[dom.SVGSVGElement]
    .build

}
