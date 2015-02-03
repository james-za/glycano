package za.jwatson.glycanoweb.render

import importedjs.{paper => p}
import importedjs.paper.{Point => P}
import importedjs.paper.Implicits._
import za.jwatson.glycanoweb.structure.{ResidueType, Substituent, Residue}
import ResidueShape._
import scalajs.js

import japgolly.scalajs.react.vdom.prefix_<^._

case class ResidueShape(residue: Residue, dc: DisplayConv, subs: Map[Int, Vector[Substituent]] = Map.empty) {
  val group = residue.rt match {
    case ResidueType.Begin => pathGroup((0,0),(10,0),(10,4),(4,4),(4,28),(10,28),(10,32),(0,32))
    case ResidueType.End => pathGroup((0,0),(10,0),(10,32),(0,32),(0,28),(6,28),(6,4),(0,4))
    case _ => dc.group(residue, subs, hh = false, () => (), () => ())
  }
//  group.applyMatrix = false
//
//  val outline = group.children
//    .find(_.name.getOrElse("") == "outline")
//    .map(_.asInstanceOf[p.Path])
//    .getOrElse(p.Path.RegularPolygon(group.bounds.center, residue.rt.linkage, 0))
//  val handle = group.children
//    .find(_.name.getOrElse("") == "handle")
//    .map(_.asInstanceOf[p.Path])
//
//  for (h <- handle)
//    h.bounds.center = outline.firstSegment.point
}

object ResidueShape {
  def pathGroup(pts: (Double, Double)*) = {
    val path = pts.map { case (x, y) => s"${x * 3},${y * 3}" }
    val points = path.mkString(" ")
//    path.closePath()
//    path.fillColor = "#000000"
//    new p.Group(js.Array(path))
    <.svg.g(<.svg.polygon(^.svg.points := points))
  }
}