package za.jwatson.glycanoweb.render

import za.jwatson.glycanoweb.structure.{SubstituentType, ResidueType, Substituent, Residue}
import ResidueShape._

import japgolly.scalajs.react.vdom.prefix_<^._

case class ResidueShape(residue: Residue, dc: DisplayConv, subs: Map[Int, Vector[SubstituentType]] = Map.empty) {
  val group = residue.rt match {
    case ResidueType.Begin => pathGroup((0, 0), (10, 0), (10, 4), (4, 4), (4, 28), (10, 28), (10, 32), (0, 32))
    case ResidueType.End => pathGroup((0, 0), (10, 0), (10, 32), (0, 32), (0, 28), (6, 28), (6, 4), (0, 4))
    case _ => dc.group(residue, subs, handleHover = false, () => (), () => (), _ => ())
  }
}

object ResidueShape {

  def group(residue: Residue, subs: Map[Int, Vector[SubstituentType]], dc: DisplayConv) = residue.rt match {
    case ResidueType.Begin => pathGroup((0, 0), (10, 0), (10, 4), (4, 4), (4, 28), (10, 28), (10, 32), (0, 32))
    case ResidueType.End => pathGroup((0, 0), (10, 0), (10, 32), (0, 32), (0, 28), (6, 28), (6, 4), (0, 4))
    case _ => dc.group(residue, subs, handleHover = false, () => (), () => (), _ => ())
  }

  def pathGroup(pts: (Double, Double)*) = {
    val path = pts.map { case (x, y) => s"${x * 3},${y * 3}" }
    val points = path.mkString(" ")
    <.svg.g(<.svg.polygon(^.svg.points := points, ^.svg.fill := "black"))
  }
}