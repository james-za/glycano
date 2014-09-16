package za.jwatson.glycanoweb.render

import za.jwatson.glycanoweb.ConventionEditor
import za.jwatson.glycanoweb.ConventionEditor._
import za.jwatson.glycanoweb.structure.{Residue, ResidueType, Absolute, Anomer}

import scalajs.js

import importedjs.{paper => p}

import scala.util.Try
import scalaz.Monoid

object DisplayConv {
  implicit val DisplayInstance = Monoid.instance[DisplayConv]({
    case (a, b) =>
      val name = a.conv.name
      val shapeDefs = a.conv.shapeDefs ++ b.conv.shapeDefs
      val rules = a.conv.rules ++ b.conv.rules
      DisplayConv(Conv(name, shapeDefs, rules))
  }, DisplayConv(Conv("")))

  def fromString(str: String): Option[DisplayConv] = {
    val result = new ConventionEditor.ConventionParser(str).conventions.run()
    val conv = result.toOption.flatMap(_.headOption)
    conv.map(new DisplayConv(_))
  }

  import scalaz.syntax.std.option._
  val UCT = fromString(ConventionEditor.testText).orZero
}

case class DisplayConv(conv: Conv) {
  def translateColor(color: String): p.Color = color match {
    case "none" => new p.Color(0, 0, 0, 0)
//    case "black" => new p.Color("#000000")
//    case "white" => new p.Color("#FFFFFF")
//    case "blue" => new p.Color("#0000FF")
//    case "yellow" => new p.Color("#FFFF00")
//    case "lime" => new p.Color("#00FF00")
//    case "red" => new p.Color("#FF0000")
    case fill => new p.Color(fill)
  }
  val shapeToItem: Shape => p.Item = {
    case DefinedShape(position, name) =>
      throw new UnsupportedOperationException("ShapeDef cannot refer to another ShapeDef")
    case Polygon(points) =>
      val poly = new p.Path()
      for (Array(x, y) <- points.split("[,;\\s]").grouped(2)) {
        poly.add(new p.Segment(p.Point(x.toDouble, y.toDouble)))
      }
      poly.closePath()
      poly
    case Rect(ToDouble(x), ToDouble(y),
      ToDouble(width), ToDouble(height),
      ToDouble(rx), ToDouble(ry))
      if rx > 0 || ry > 0 =>
      p.Path.RoundRectangle(new p.Rectangle(x, y, width, height), new p.Size(rx, ry))
  }
  def name = conv.name
  //val shapeDefs = conv.shapeDefs.mapValues(shapeToItem)
  def group(r: Residue): p.Group = {
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
          case DefinedShape(_, name) => shapeToItem(conv.shapeDefs(name))
          case _ => shapeToItem(shape)
        }
        if (classes contains "links") item.name = "outline"
        if (classes contains "handle") item.name = "handle"
        val stylePairs = styles.foldLeft(Map[String, String]()) {
          case (z, (style, pairs)) if classes contains style =>
            z ++ pairs
          case (z, _) =>
            z
        }
        stylePairs foreach {
          case ("fill", fill) =>
            val c = translateColor(fill)
            if (r.rt == ResidueType.Glc && r.anomer == Anomer.Alpha && r.absolute == Absolute.D) {
              println(c)
            }
            item.fillColor = c
          case ("stroke", stroke) => item.strokeColor = translateColor(stroke)
          case ("stroke-width", sw) => item.strokeWidth = sw.toDouble
          case ("x", x) => item.position.x = x.toDouble
          case ("y", y) => item.position.y = y.toDouble
          case (attr, value) => throw new IllegalArgumentException(s"""Unsupported attribute "$attr" with value "$value"""")
        }
        priority -> item
    }.sortBy(_._1).map(_._2)
//    if (r.rt == ResidueType.Glc && r.anomer == Anomer.Alpha && r.absolute == Absolute.D) {
//      println(r)
////      println(conv.rules)
////      println(conv.shapeDefs)
//      println("MATCHED=============")
//      println(matched.mkString("\n"))
//      println("MODS================")
//      println(mods.mkString("\n"))
//      println("ITEMS===============")
//      println(shapes.map(s =>
//        js.JSON.stringify(s, space = 2: js.Any)
//      ).mkString("\n"))
//      show = false
//    }
    new p.Group(js.Array(shapes: _*))
  }
}

object ToInt {
  def unapply(str: String): Option[Int] = Try(str.toInt).toOption
}

object ToDouble {
  def unapply(str: String): Option[Double] = Try(str.toDouble).toOption
}