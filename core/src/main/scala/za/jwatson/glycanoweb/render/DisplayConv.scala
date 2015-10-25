package za.jwatson.glycanoweb.render

import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.prefix_<^._
import org.parboiled2.ParseError
import za.jwatson.glycanoweb.convention.Convention.RuleCond.{DefaultCond, ResCond}
import za.jwatson.glycanoweb.convention.Convention._
import za.jwatson.glycanoweb.convention.{CFG, Convention, ConventionParser, UCT}
import za.jwatson.glycanoweb.structure.RGraph.GraphEntry
import za.jwatson.glycanoweb.structure._

import scala.scalajs.js
import scala.util.{Failure, Success, Try}

class DisplayConv(val conv: Conv) {
  val (defaultRules, rules) = conv.rules.partition(_.conds.contains(DefaultCond))
  val defaultMods = defaultRules.flatMap(_.mods)

  val palettes = conv.palettes.map(p => p.name -> p).toMap

  val shapeToItem: Shape => ReactTag = {
    case DefinedShape(position, name) =>
      throw new UnsupportedOperationException("ShapeDef cannot refer to another ShapeDef")
    case Path(d) =>
      <.svg.path(^.svg.d := d)
    case Polygon(points) =>
      <.svg.polygon(^.svg.points := points)
    case Circle(ToDouble(x), ToDouble(y), ToDouble(r)) =>
      <.svg.circle(^.svg.cx := x, ^.svg.cy := y, ^.svg.r := r)
    case Star(ToDouble(x), ToDouble(y), ToDouble(n), ToDouble(r1), ToDouble(r2)) =>
      val count = n.toInt * 2
      val dt = 2 * math.Pi / count
      val points = for (i <- 0 until count) yield {
        val angle = i * dt - math.Pi / 2.0
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

  def residueMods(residue: Residue) = residueModsMemo.getOrElseUpdate(residue.symbol, residueModsInner(residue))
  val residueModsMemo = js.Dictionary.empty[Seq[RuleMod]]
  def residueModsInner(residue: Residue): Seq[RuleMod] = {
    val matched = rules.filter(_.conds.forall(_.matches(residue)))

    val shapeRules = matched.filter(_.mods.exists(_.isInstanceOf[ShapeMod]))
    val rtDefined = shapeRules.exists(_.conds.exists(_.isInstanceOf[ResCond]))

    if (rtDefined) matched.flatMap(_.mods) else defaultMods
  }

  def polygonOutline(points: String) = points.split("[, ]").map(_.toDouble).grouped(2).map(a => (a(0), a(1))).toIndexedSeq

  def outline(residue: Residue) = outlineMemo.getOrElseUpdate(residue.symbol, outlineInner(residueMods(residue), residue))
  val outlineMemo = js.Dictionary.empty[IndexedSeq[(Double, Double)]]
  def outlineInner(mods: Seq[RuleMod], residue: Residue): IndexedSeq[(Double, Double)] = residue.rt match {
    case ResidueType.Begin => IndexedSeq((12.0, 48.0))
    case ResidueType.End => IndexedSeq((30.0, 48.0), (18.0, 48.0))
    case _ =>
      mods.flatMap {
        case ShapeMod(_, classes, shape) if classes contains "outline" =>
          outlineFromShape(shape)
        case _ => None
      }.headOption.getOrElse(IndexedSeq.fill(residue.rt.linkage)((0.0, 0.0)))
  }

  def outlineFromShape(shape: Shape): Option[IndexedSeq[(Double, Double)]] = shape match {
    case Polygon(points) =>
      Some(polygonOutline(points))
    case DefinedShape(_, shapeName) =>
      for {
        innerShape <- conv.shapeDefs.get(shapeName)
        outline <- outlineFromShape(innerShape)
      } yield outline
    case Circle(x, y, r) =>
      val cx = x.toDouble
      val cy = y.toDouble
      val radius = math.abs(r.toDouble)
      Some(IndexedSeq[(Double, Double)]((cx - radius, cy - radius), (cx + radius, cy + radius)))
    case Rect(x, y, w, h, _, _) =>
      val rx = x.toDouble
      val ry = y.toDouble
      val rw = w.toDouble
      val rh = h.toDouble
      Some(IndexedSeq[(Double, Double)]((rx, ry), (rx + rw, ry + rh)))
    case Star(x, y, n, r1, r2) =>
      val cx = x.toDouble
      val cy = y.toDouble
      val radius = math.max(math.abs(r1.toDouble), math.abs(r2.toDouble))
      Some(IndexedSeq[(Double, Double)]((cx - radius, cy - radius), (cx + radius, cy + radius)))
    case _ => None
  }

  def links(residue: Residue) = linksMemo.getOrElseUpdate(residue.symbol, linksInner(residueMods(residue), residue))
  val linksMemo = js.Dictionary.empty[IndexedSeq[(Double, Double)]]
  def linksInner(mods: Seq[RuleMod], residue: Residue): IndexedSeq[(Double, Double)] = residue.rt match {
    case ResidueType.Begin => IndexedSeq((12.0, 48.0))
    case ResidueType.End => IndexedSeq((30.0, 48.0), (18.0, 48.0))
    case _ =>
      mods.flatMap {
        case ShapeMod(_, classes, Polygon(points)) if classes.contains("links") =>
          Some(polygonOutline(points))
        case ShapeMod(_, classes, DefinedShape(_, shapeName)) if classes.contains("links") =>
          for {
            Polygon(points) <- conv.shapeDefs.get(shapeName)
          } yield polygonOutline(points)
        case _ => None
      }.headOption.getOrElse(IndexedSeq.fill(residue.rt.linkage)((0.0, 0.0)))
  }

  def bounds(residue: Residue) = boundsMemo.getOrElseUpdate(residue.symbol, boundsInner(residue))
  val boundsMemo = js.Dictionary.empty[((Double, Double), Double, Double)]
  def boundsInner(residue: Residue): ((Double, Double), Double, Double) = residue.rt match {
    case ResidueType.Begin => ((0.0, 0.0), 30.0, 96.0)
    case ResidueType.End => ((0.0, 0.0), 30.0, 96.0)
    case _ =>
      val (xs, ys) = outline(residue).unzip
      val x = xs.min
      val y = ys.min
      val width = xs.max - x
      val height = ys.max - y
      ((x, y), width, height)
  }

  def linkPos(outline: IndexedSeq[(Double, Double)], ge: GraphEntry, i: Int): (Double, Double) = {
    val ((x0, y0), w, h) = bounds(ge.residue)
    val offset = (x0 + w / 2.0, y0 + h / 2.0)
    val (x, y) = rotatePointRadians(outline(i - 1), math.toRadians(ge.rotation), offset)
    (ge.x + x, ge.y + y)
  }

  def rotatePointRadians(p: (Double, Double), a: Double, o: (Double, Double) = (0, 0)): (Double, Double) = {
    val sin = math.sin(a)
    val cos = math.cos(a)

    val tx = p._1 - o._1
    val ty = p._2 - o._2

    val nx = tx * cos - ty * sin
    val ny = tx * sin + ty * cos

    (nx, ny)//(nx + o._1, ny + o._2)
  }

  def shapes(residue: Residue) = shapesMemo.getOrElseUpdate(residue.symbol, shapesInner(residue))
  val shapesMemo = js.Dictionary.empty[(ReactTag, Option[ReactTag])]
  def shapesInner(residue: Residue) = residue.rt match {
    case ResidueType.Begin => pathGroup((0, 0), (10, 0), (10, 4), (4, 4), (4, 28), (10, 28), (10, 32), (0, 32))
    case ResidueType.End => pathGroup((0, 0), (10, 0), (10, 32), (0, 32), (0, 28), (6, 28), (6, 4), (0, 4))
    case rt => shapesFromConv(residue)
  }

  def pathGroup(pts: (Double, Double)*) = {
    val path = pts.map { case (x, y) => s"${x * 3},${y * 3}" }
    val points = path.mkString(" ")
    (<.svg.g(<.svg.polygon(^.svg.points := points, ^.svg.fill := "black")), None)
  }

  def shapesFromConv(residue: Residue): (ReactTag, Option[ReactTag]) = {
    val mods = residueMods(residue)

    val styles = mods.foldLeft(Map[String, Map[String, String]]()) {
      case (map, StyleMod(style, content)) =>
        map + (style -> map.get(style).map(_ ++ content).getOrElse(content))
      case (map, _) => map
    }

    val (handleShapeMods, residueShapeMods) = mods.collect {
      case ShapeMod(priority, classes, shape) =>
        val item = shape match {
          case DefinedShape(_, name) => shapeToItem(conv.shapeDefs(name))
          case _ => shapeToItem(shape)
        }

        val styleMods = for {
          (style, pairs) <- styles.toSeq if classes contains style
          mod <- pairs.collect {
            case ("fill", fill) => ^.svg.fill := fill
            case ("stroke", stroke) => ^.svg.stroke := stroke
            case ("stroke-width", sw) => ^.svg.strokeWidth := sw
            case ("x", x) => ^.svg.x := x
            case ("y", y) => ^.svg.y := y
          }
        } yield mod

        val linksMod = classes contains "links" ?= (^.cls := "links")
        val outlineMod = classes contains "outline" ?= (^.cls := "outline")
        val isHandle = classes contains "handle"
        val handleMod = isHandle ?= (^.cls := "handle")

        (priority, isHandle, item(linksMod, outlineMod, styleMods, handleMod))
    }.sortBy(_._1).partition(_._2)
    (<.svg.g(residueShapeMods.map(_._3)), handleShapeMods.map(_._3).headOption)
  }

}

object DisplayConv {
  implicit val reusability: Reusability[DisplayConv] = Reusability.byRef

  def parseTextConv(text: String): Option[DisplayConv] = {
    val parser = new ConventionParser(text)
    val convs = parser.conventions.run()
    convs match {
      case Success(c)             ⇒ //Expression is valid\n" + c.mkString("\n")
      case Failure(e: ParseError) ⇒ println("Expression is not valid: " + parser.formatError(e))
      case Failure(e)             ⇒ println("Unexpected error during parsing run: " + e.printStackTrace())
    }
    convs.getOrElse(Seq.empty).headOption.map(new DisplayConv(_))
  }

  val convDefault = new DisplayConv(Conv(""))

  val conventions = for {
    (k, v) <- Map(
      "UCT" -> UCT.text,
      "CFG" -> CFG.text
    )
    c <- parseTextConv(v)
  } yield k -> c

  def convUCT = conventions.getOrElse("UCT", convDefault)
  def convCFG = conventions.getOrElse("CFG", convDefault)
}

object ToInt {
  def unapply(str: String): Option[Int] = Try(str.toInt).toOption
}

object ToDouble {
  def unapply(str: String): Option[Double] = Try(str.toDouble).toOption
}