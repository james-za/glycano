package za.jwatson.glycanoweb.render

import org.parboiled2.ParseError
import za.jwatson.glycanoweb.{GlycanoWeb, ConventionParser, ConventionEditor}
import za.jwatson.glycanoweb.ConventionEditor.RuleCond.{DefaultCond, ResCond}
import za.jwatson.glycanoweb.ConventionEditor._
import za.jwatson.glycanoweb.structure._

import scalajs.js

import japgolly.scalajs.react.vdom.prefix_<^._

import scala.util.{Success, Failure, Try}

class DisplayConv(val conv: Conv) {
  val shapeToItem: Shape => ReactTag = {
    case DefinedShape(position, name) =>
      throw new UnsupportedOperationException("ShapeDef cannot refer to another ShapeDef")
    case Polygon(points) =>
      <.svg.polygon(^.svg.points := points)
    case Circle(ToDouble(x), ToDouble(y), ToDouble(r)) =>
      <.svg.circle(^.svg.cx := x, ^.svg.cy := y, ^.svg.r := r)
    case Star(ToDouble(x), ToDouble(y), ToDouble(n), ToDouble(r1), ToDouble(r2)) =>
      val count = n.toInt * 2
      val dt = 2 * math.Pi / count
      val points = for (i <- 0 until count) yield {
        val angle = i * dt
        val out = i % 2 == 0
        val r = if (out) r2 else r1
        val x = r * math.cos(angle)
        val y = r * math.sin(angle)
        s"$x,$y"
      }
      <.svg.polygon(^.svg.points := points.mkString(" "))
    case Rect(ToDouble(x), ToDouble(y),
    ToDouble(width), ToDouble(height),
    ToDouble(rx), ToDouble(ry)) =>
      <.svg.rect(
        ^.svg.x := x, ^.svg.y := y,
        ^.svg.width := width, ^.svg.height := height,
        ^.svg.rx := rx, ^.svg.ry := ry
      )
  }
  def name = conv.name
  //val shapeDefs = conv.shapeDefs.mapValues(shapeToItem)
  
  def residueMods(r: Residue, subs: Map[Int, Vector[Substituent]]): Seq[RuleMod] = {
    val matched = conv.rules.filter(_.conds.forall(_.matches(r, subs)))
    val shapeRules = matched.filter(_.mods.exists(_.isInstanceOf[ShapeMod]))
    val rtDefined = shapeRules.flatMap(_.conds).exists(_.isInstanceOf[ResCond])

    if (rtDefined) matched.flatMap(_.mods) else
      conv.rules.filter(_.conds.contains(DefaultCond)).flatMap(_.mods)
  }

  def polygonOutline(points: String) = points.split("[, ]").map(_.toDouble).grouped(2).map(a => (a(0), a(1))).toIndexedSeq

  def outline(r: Residue, subs: Map[Int, Vector[Substituent]]): IndexedSeq[(Double, Double)] =
    outline(residueMods(r, subs), r, subs)

  def outline(mods: Seq[RuleMod], r: Residue, subs: Map[Int, Vector[Substituent]]): IndexedSeq[(Double, Double)] = {
    mods.collect {
      case ShapeMod(priority, classes, Polygon(points)) =>
        polygonOutline(points)
    }.headOption.getOrElse(IndexedSeq.fill(r.rt.linkage)((0.0, 0.0)))
  }

  def group(r: Residue, subs: Map[Int, Vector[Substituent]], hh: Boolean, hOver: () => Unit, hOut: () => Unit): ReactTag = {
    val mods = residueMods(r, subs)

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
        val outlineMod = classes contains "links" ?= (^.svg.`class` := "outline")
        val handleMod = classes contains "handle" ?= Seq(
          hh ?= Seq(
            "strokeWidth".reactAttr := "3",
            ^.svg.stroke := "blue"
          ),
          ^.onMouseOver --> hOver(),
          ^.onMouseOut --> hOut(),
          ^.svg.`class` := "handle"
        )
        val stylePairs = styles.foldLeft(Map[String, String]()) {
          case (z, (style, pairs)) if classes contains style =>
            z ++ pairs
          case (z, _) =>
            z
        }
        val styleMods = stylePairs.collect({
          case ("fill", fill) => ^.svg.fill := fill
          case ("stroke", stroke) => ^.svg.stroke := stroke
          case ("stroke-width", sw) => "strokeWidth".reactAttr := sw
          case ("x", x) => ^.svg.x := x
          case ("y", y) => ^.svg.y := y
          //case (attr, value) => throw new IllegalArgumentException( s"""Unsupported attribute "$attr" with value "$value"""")
        }).toSeq
        priority -> item(outlineMod, styleMods, handleMod)
    }.sortBy(_._1).map(_._2)
    <.svg.g(shapes)
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
  def convs = dom.localStorage.getItem("glycano.conventions").asInstanceOf[js.UndefOr[String]].fold {
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