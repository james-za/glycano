package za.jwatson.glycanoweb.react

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import monocle.macros.Lenses
import za.jwatson.glycanoweb.GlyAnnot
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure.RGraph.GraphEntry

import za.jwatson.glycanoweb.structure._

import scalajs.js

object GlycanoCanvas {
  case class Props(B: GlycanoApp.Backend, dc: DisplayConv, width: Int = 800, height: Int = 600, graph: RGraph, selection: (Set[Residue], Set[GlyAnnot]), view: View = View())

  @Lenses case class State(hoverHandle: Option[Residue] = None, inputState: InputState = InputState.Default)

  @Lenses case class View(x: Double = 0, y: Double = 0, scale: Double = 1)

  class Backend(t: BackendScope[Props, State]) {
  }

  sealed trait InputState
  object InputState {
    case object Default extends InputState
    case object PlaceResidue extends InputState
    case object AddSubstituent extends InputState
    case class BoxSelect(down: (Double, Double)) extends InputState
    case class Drag(last: (Double, Double)) extends InputState
    case object CreateBond extends InputState
    case object PostCreateBond extends InputState
    case class Hit(down: (Double, Double), item: Residue) extends InputState
    case class Rotate(item: Residue) extends InputState
    case object AddAnnotation extends InputState
  }


//  val defaultPoints = "90,40 65,80 25,80 0,40 25,0 65,0"
//  def defaultShape(B: Backend, r: Residue, hh: Boolean) = <.svg.g(
//    <.svg.polygon(
//      ^.svg.points := defaultPoints,
//      ^.svg.fill := "blue",
//      ^.svg.stroke := "black",
//      "strokeWidth".reactAttr := 3
//    ),
//    <.svg.rect(
//      ^.svg.x := 90 - 10, ^.svg.y := 40 - 10,
//      ^.svg.width := 20, ^.svg.height := 20,
//      ^.svg.rx := 5, ^.svg.ry := 5,
//      ^.svg.fill := "white",
//      ^.svg.stroke := (if (hh) "blue" else "black"),
//      "strokeWidth".reactAttr := (if (hh) 2 else 1),
//      ^.onMouseOver --> B.hoverHandle(r),
//      ^.onMouseOut --> B.leaveHandle(r)
//    )
//  )
//  def defaultShape(ano: Anomer, abs: Absolute, rt: ResidueType, scale: Double = 1) = <.svg.g(
//    ^.svg.transform := s"scale($scale)",
//    <.svg.polygon(
//      ^.svg.points := defaultPoints,
//      ^.svg.fill := "blue",
//      ^.svg.stroke := "black",
//      "strokeWidth".reactAttr := 3
//    ),
//    <.svg.rect(
//      ^.svg.x := 90 - 10, ^.svg.y := 40 - 10,
//      ^.svg.width := 20, ^.svg.height := 20,
//      ^.svg.rx := 5, ^.svg.ry := 5,
//      ^.svg.fill := "white",
//      ^.svg.stroke := "black",
//      "strokeWidth".reactAttr := 1
//    )
//  )
//  val defaultOutline = polygonOutline(defaultPoints)

  def outlinePos(outline: IndexedSeq[(Double, Double)], r: Residue, ge: GraphEntry, i: Int): (Double, Double) = {
    val (x, y) = rotatePoint(outline(i), ge.rotation)
    (ge.x + x, ge.y + y)
  }

  def rotatePoint(p: (Double, Double), a: Double, c: (Double, Double) = (0, 0)): (Double, Double) = {
    val sin = math.sin(a)
    val cos = math.cos(a)

    val tx = p._1 - c._1
    val ty = p._2 - c._2

    val nx = tx * cos - ty * sin
    val ny = tx * sin + ty * cos

    (nx + c._1, ny + c._2)
  }

  def polygonOutline(points: String) = points.split("[, ]").map(_.toDouble).grouped(2).map(a => (a(0), a(1))).toIndexedSeq

  def apply(props: Props, children: ReactNode*) = component.apply(props, children)
  val component = ReactComponentB[Props]("GlycanoCanvas")
    .initialState(State())
    .backend(new Backend(_))
    .render((P, C, S, B) => {
      val View(vx, vy, vs) = P.view

      val outlines = for ((r, ge) <- P.graph.entries) yield {
        r -> P.dc.outline(r, ge.subs)
      }

      <.svg.svg(
        ^.svg.width := P.width,
        ^.svg.height := P.height
      )(
        <.svg.g(^.svg.transform := s"translate($vx $vy) scale($vs)")(
          for {
            (r @ Residue(_, rt, ano, abs), ge) <- P.graph.entries
            Link(to, i) <- ge.parent
          } yield {
            val (x1, y1) = outlinePos(outlines(r), r, ge, 0)
            val (x2, y2) = outlinePos(outlines(r), to, P.graph.entries(to), i)
            <.svg.line(
              ^.svg.x1 := x1, ^.svg.y1 := y1,
              ^.svg.x2 := x2, ^.svg.y2 := y2,
              ^.svg.stroke := "black", "strokeWidth".reactAttr := "7"
            )
          },
          for ((r, ge) <- P.graph.entries) yield SVGResidue(SVGResidue.Props(r, ge, P.dc))
        )
      )
    })
    //.domType[dom.SVGSVGElement]
    .build

}
