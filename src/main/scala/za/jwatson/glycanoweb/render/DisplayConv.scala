package za.jwatson.glycanoweb.render

import japgolly.scalajs.react.ReactMouseEvent
import org.parboiled2.ParseError
import za.jwatson.glycanoweb.{ConventionParser, ConventionEditor}
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

  val residueModsMemo = scalaz.Memo.mutableHashMapMemo((residueModsInner _).tupled)
  def residueModsInner(ano: Anomer, abs: Absolute, rt: ResidueType, subs: Map[Int, Vector[SubstituentType]]): Seq[RuleMod] = {
    val matched = conv.rules.filter(_.conds.forall(_.matches(ano, abs, rt, subs)))
    val shapeRules = matched.filter(_.mods.exists(_.isInstanceOf[ShapeMod]))
    val rtDefined = shapeRules.flatMap(_.conds).exists(_.isInstanceOf[ResCond])

    if (rtDefined) matched.flatMap(_.mods) else
      conv.rules.filter(_.conds.contains(DefaultCond)).flatMap(_.mods)
  }

  def polygonOutline(points: String) = points.split("[, ]").map(_.toDouble).grouped(2).map(a => (a(0), a(1))).toIndexedSeq

  def outline(ano: Anomer, abs: Absolute, rt: ResidueType, subs: Map[Int, Vector[SubstituentType]]): IndexedSeq[(Double, Double)] =
    outlineMemo(residueModsMemo(ano, abs, rt, subs), ano, abs, rt, subs)

  val outlineMemo = scalaz.Memo.mutableHashMapMemo((outlineInner _).tupled)
  def outlineInner(mods: Seq[RuleMod], ano: Anomer, abs: Absolute, rt: ResidueType, subs: Map[Int, Vector[SubstituentType]]): IndexedSeq[(Double, Double)] = {
    mods.flatMap {
      case ShapeMod(_, classes, Polygon(points)) if classes.contains("links") =>
        Some(polygonOutline(points))
      case ShapeMod(_, classes, DefinedShape(_, shapeName)) if classes.contains("links") =>
        for {
          Polygon(points) <- conv.shapeDefs.get(shapeName)
        } yield polygonOutline(points)
      case _ => None
    }.headOption.getOrElse(IndexedSeq.fill(rt.linkage)((0.0, 0.0)))
  }

  val boundsMemo = scalaz.Memo.mutableHashMapMemo((boundsInner _).tupled)
  def boundsInner(ano: Anomer, abs: Absolute, rt: ResidueType, subs: Map[Int, Vector[SubstituentType]]): ((Double, Double), Double, Double) = {
    val (xs, ys) = outline(ano, abs, rt, subs).unzip
    val x = xs.min
    val y = ys.min
    val width = xs.max - x
    val height = ys.max - y
    ((x, y), width, height)
  }

//  def group(r: Residue, subs: Map[Int, Vector[SubstituentType]], handleHover: Boolean,
//            handleMouseOver: () => Unit,
//            handleMouseOut: () => Unit,
//            handleMouseDown: ReactMouseEvent => Unit): ReactTag = {
//    val mods = residueModsMemo(r.anomer, r.absolute, r.rt, subs)
//
//    val styles = mods.foldLeft(Map[String, Map[String, String]]()) {
//      case (map, StyleMod(style, content)) =>
//        map + (style -> map.get(style).map(_ ++ content).getOrElse(content))
//      case (map, _) => map
//    }
//
////    val styleModPairs = for (StyleMod(style, content) <- mods) yield {
////      style -> content
////    }
////    val styles = styleModPairs.groupBy(_._1).mapValues(_.map(_._2).reduce(_ ++ _))
//
//    val shapes = mods.collect {
//      case ShapeMod(priority, classes, shape) =>
//        val item = shape match {
//          case DefinedShape(_, name) => shapeToItem(conv.shapeDefs(name))
//          case _ => shapeToItem(shape)
//        }
//
//        val styleMods = for {
//          (style, pairs) <- styles.toSeq if classes contains style
//          mod <- pairs.collect {
//            case ("fill", fill) => ^.svg.fill := fill
//            case ("stroke", stroke) => ^.svg.stroke := stroke
//            case ("stroke-width", sw) => ^.svg.strokeWidth := sw
//            case ("x", x) => ^.svg.x := x
//            case ("y", y) => ^.svg.y := y
//          }
//        } yield mod
//
//        val outlineMod = classes contains "links" ?= (^.cls := "outline")
//        val handleMod = classes contains "handle" ?= Seq(
//          handleHover ?= Seq(
//            ^.svg.strokeWidth := "3",
//            ^.svg.stroke := "blue"
//          ),
//          ^.onMouseOver --> handleMouseOver(),
//          ^.onMouseOut --> handleMouseOut(),
//          ^.onMouseDown ==> handleMouseDown,
//          ^.cls := "handle"
//        )
//
//        priority -> item(outlineMod, styleMods, handleMod)
//    }.sortBy(_._1).map(_._2)
//    <.svg.g(shapes)
//  }

  def shapes(ano: Anomer, abs: Absolute, rt: ResidueType, subs: Map[Int, Vector[SubstituentType]]): (ReactTag, ReactTag) =
    shapesMemo.getOrElseUpdate((ano, abs, rt, subs).##.toString, shapesInner(ano, abs, rt, subs))
  val shapesMemo = js.Dictionary.empty[(ReactTag, ReactTag)]
  def shapesInner(ano: Anomer, abs: Absolute, rt: ResidueType, subs: Map[Int, Vector[SubstituentType]]): (ReactTag, ReactTag) = {
    val mods = residueModsMemo(ano, abs, rt, subs)

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

        val outlineMod = classes contains "links" ?= (^.cls := "outline")
        val isHandle = classes contains "handle"
        val handleMod = isHandle ?= (^.cls := "handle")

        (priority, isHandle, item(outlineMod, styleMods, handleMod))
    }.sortBy(_._1).partition(_._2)
    (<.svg.g(residueShapeMods.map(_._3)), handleShapeMods.map(_._3).headOption.getOrElse(<.svg.g()))
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[DisplayConv]

  override def equals(other: Any): Boolean = other match {
    case that: DisplayConv =>
      (that canEqual this) &&
        conv == that.conv
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(conv)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
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

//  val conventions = rx.Var[collection.mutable.Map[String, DisplayConv]](js.Dictionary[DisplayConv]())
//  def convs = dom.localStorage.getItem("glycano.conventions").asInstanceOf[js.UndefOr[String]].fold {
//    js.Dictionary[String]()
//  } {
//    c => js.JSON.parse(c).asInstanceOf[js.Dictionary[String]]
//  }

  val conventions = for {
    (k, v) <- Map(
      "UCT" -> ConventionEditor.textUCT,
      "CFG" -> ConventionEditor.textCFG
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