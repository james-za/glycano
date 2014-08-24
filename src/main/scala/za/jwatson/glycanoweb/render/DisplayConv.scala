package za.jwatson.glycanoweb.render

import za.jwatson.glycanoweb.ConventionEditor._
import za.jwatson.glycanoweb.structure.{Residue, ResidueType, Absolute, Anomer}

import scalajs.js

import importedjs.{paper => p}

import scala.util.Try

class DisplayConv(conv: Conv) {
  val shapeToItem: Shape => p.Item = {
    case DefinedShape(position, name) =>
      throw new UnsupportedOperationException("ShapeDef cannot refer to another ShapeDef")
    case Polygon(points) =>
      new p.Path(points)
    case Rect(ToDouble(x), ToDouble(y),
    ToDouble(width), ToDouble(height),
    ToDouble(rx), ToDouble(ry))
      if rx > 0 || ry > 0 =>
      p.Path.RoundRectangle(new p.Rectangle(x, y, width, height), new p.Size(rx, ry))
  }
  def name = conv.name
  val shapeDefs = conv.shapeDefs.mapValues(shapeToItem)
  def item(r: Residue): p.Item = {
    val matched = conv.rules.filter(_.conds.forall(_.matches(r)))
    val mods = matched.flatMap(_.mods)
    val styles = mods.foldLeft(Map[String, Map[String, String]]()) {
      case (map, StyleMod(style, content)) =>
        map + (style -> map.get(style).map(_ ++ content).getOrElse(content))
      case (map, _) => map
    }
    val shapes = mods.collect {
      case ShapeMod(priority, classes, shape) =>
        val item = shape match {
          case DefinedShape(_, name) => shapeDefs(name)
          case _ => shapeToItem(shape)
        }
        item.name = classes.mkString(" ")
        val stylePairs = styles.foldLeft(Map[String, String]()) {
          case (z, (style, pairs)) if classes contains style =>
            z ++ pairs
        }
        stylePairs foreach {
          case ("fill", "none") => item.fillColor = new p.Color(0, 0, 0, 0)
          case ("fill", fill) => item.fillColor = new p.Color(fill)
          case ("stroke", stroke) => item.strokeColor = new p.Color(stroke)
          case ("stroke-width", sw) => item.strokeWidth = sw.toDouble
          case (attr, value) => throw new IllegalArgumentException(s"""Unsupported attribute "$attr" with value "$value"""")
        }
        priority -> item
    }.sortBy(_._1).map(_._2)
    new p.Group(js.Array(shapes: _*))
  }
}

object ToInt {
  def unapply(str: String): Option[Int] = Try(str.toInt).toOption
}

object ToDouble {
  def unapply(str: String): Option[Double] = Try(str.toDouble).toOption
}