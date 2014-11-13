package za.jwatson.glycanoweb.render

import org.parboiled2.ParseError
import za.jwatson.glycanoweb.{GlycanoWeb, ConventionParser, ConventionEditor}
import za.jwatson.glycanoweb.ConventionEditor.RuleCond.{DefaultCond, ResCond}
import za.jwatson.glycanoweb.ConventionEditor._
import za.jwatson.glycanoweb.structure._

import scalajs.js

import importedjs.{paper => p}

import scala.util.{Success, Failure, Try}

class DisplayConv(val conv: Conv) {
  def translateColor(color: String): p.Color = color match {
    case "none" => new p.Color(0, 0, 0, 0)
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
    case Circle(ToDouble(x), ToDouble(y), ToDouble(r)) =>
      p.Path.Circle(p.Point(x, y), r)
    case Star(ToDouble(x), ToDouble(y), ToDouble(n), ToDouble(r1), ToDouble(r2)) =>
      p.Path.Star(p.Point(x, y), n, r1, r2)
    case Rect(ToDouble(x), ToDouble(y),
    ToDouble(width), ToDouble(height),
    ToDouble(rx), ToDouble(ry)) =>
      val rect = new p.Rectangle(x, y, width, height)
      if (rx > 0 || ry > 0) p.Path.RoundRectangle(rect, new p.Size(rx, ry))
      else p.Path.Rectangle(rect)
  }
  def name = conv.name
  //val shapeDefs = conv.shapeDefs.mapValues(shapeToItem)
  def group(r: Residue, subs: Map[Int, Vector[Substituent]]): p.Group = {
    val matched = conv.rules.filter(_.conds.forall(_.matches(r, subs)))
    val shapeRules = matched.filter(_.mods.exists(_.isInstanceOf[ShapeMod]))
    val rtDefined = shapeRules.flatMap(_.conds).exists(_.isInstanceOf[ResCond])

    val mods = if (rtDefined) matched.flatMap(_.mods) else
      conv.rules.filter(_.conds.contains(DefaultCond)).flatMap(_.mods)
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
          case ("fill", fill) => item.fillColor = translateColor(fill)
          case ("stroke", stroke) => item.strokeColor = translateColor(stroke)
          case ("stroke-width", sw) => item.strokeWidth = sw.toDouble
          case ("x", x) => item.position.x += x.toDouble
          case ("y", y) => item.position.y += y.toDouble
          case (attr, value) => throw new IllegalArgumentException( s"""Unsupported attribute "$attr" with value "$value"""")
        }
        priority -> item
    }.sortBy(_._1).map(_._2)
    new p.Group(js.Array(shapes: _*))
  }
}

object DisplayConv {
  def parseTextConv(text: String): Option[DisplayConv] = {
    val parser = new ConventionParser(text)
    val convs = parser.conventions.run()
    convs match {
      case Success(c)             ⇒ //Expression is valid\n" + c.mkString("\n")
      case Failure(e: ParseError) ⇒ println("Expression is not valid: " + parser.formatError(e))
      case Failure(e)             ⇒ println("Unexpected error during parsing run: " + e)
    }
    convs.getOrElse(Seq.empty).headOption.map(new DisplayConv(_))
  }

  val convDefault = new DisplayConv(Conv(""))

  import org.scalajs.dom

  val conventions = rx.Var[collection.mutable.Map[String, DisplayConv]](js.Dictionary[DisplayConv]())
  def convs = dom.localStorage("glycano.conventions").asInstanceOf[js.UndefOr[String]].fold {
    js.Dictionary[String]()
  } {
    c => js.JSON.parse(c).asInstanceOf[js.Dictionary[String]]
  }

  def refresh(): Unit = {
    conventions() = for {
      (k, v) <- convs
      c <- parseTextConv(v)
    } yield k -> c
//    for (c <- conventions().get("UCT")) convUCT() = c
//    for (c <- conventions().get("CFG")) convCFG() = c
  }

  refresh()

  for {
    (k, v) <- Map(
      "UCT" -> ConventionEditor.textUCT,
      "CFG" -> ConventionEditor.textCFG
    )
    c <- parseTextConv(v)
  } {
    //println(s"conv '$k':\n$v\n\n")
    conventions()(k) = c
  }

  def convUCT = conventions().getOrElse("UCT", convDefault)
  def convCFG = conventions().getOrElse("CFG", convDefault)
}

object ToInt {
  def unapply(str: String): Option[Int] = Try(str.toInt).toOption
}

object ToDouble {
  def unapply(str: String): Option[Double] = Try(str.toDouble).toOption
}