package za.jwatson.glycanoweb.render

import importedjs.{paper => p}
import importedjs.paper.{Point => P}
import importedjs.paper.Implicits._
import za.jwatson.glycanoweb.structure.{ResidueType, Substituent, Residue}
import ResidueShape._
import scalajs.js

case class ResidueShape(residue: Residue, dc: DisplayConv, subs: Map[Int, Vector[Substituent]] = Map.empty) {
  val group = residue.rt match {
    case ResidueType.Begin => pathGroup(P(0,0),P(10,0),P(10,4),P(4,4),P(4,28),P(10,28),P(10,32),P(0,32))
    case ResidueType.End => pathGroup(P(0,0),P(10,0),P(10,32),P(0,32),P(0,28),P(6,28),P(6,4),P(0,4))
    case _ => dc.group(residue, subs)
  }
  group.applyMatrix = false

  val outline = group.children
    .find(_.name.getOrElse("") == "outline")
    .map(_.asInstanceOf[p.Path])
    .getOrElse(p.Path.RegularPolygon(group.bounds.center, residue.rt.linkage, 0))
  val handle = group.children
    .find(_.name.getOrElse("") == "handle")
    .map(_.asInstanceOf[p.Path])

  for (h <- handle)
    h.bounds.center = outline.firstSegment.point
}

object ResidueShape {
  def pathGroup(pts: p.Point*) = {
    val path = new p.Path(js.Array(pts.map(pt => pt.multiply(3.0): p.Segment): _*))
    path.closePath()
    path.fillColor = "#000000"
    new p.Group(js.Array(path))
  }
}