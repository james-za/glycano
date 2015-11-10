package za.jwatson.glycanoweb.convention

import japgolly.scalajs.react.extra.Reusability
import za.jwatson.glycanoweb.structure._

object Convention {
  case class Conv(name: String, shapeDefs: Map[String, Shape] = Map.empty, rules: Seq[ConvRule] = Seq.empty, palettes: Seq[Palette] = Seq.empty)

  case class ConvRule(conds: Seq[RuleCond], mods: Seq[RuleMod])
  sealed trait RuleCond { def matches(residue: Residue): Boolean }
  object RuleCond {
    case class AnoCond(allowed: Anomer) extends RuleCond {
      def matches(residue: Residue) = allowed == residue.ano
    }
    case class AbsCond(allowed: Absolute) extends RuleCond {
      def matches(residue: Residue) = allowed == residue.abs
    }
    case class ResCond(allowed: Seq[String]) extends RuleCond {
      def matches(residue: Residue) = allowed contains residue.rt.symbol
    }
    case class SubCond(allowed: Seq[(Option[Int], String)]) extends RuleCond {
      def matches(residue: Residue) = {
        val subsFlat = for ((p, sts) <- residue.subs.toSeq; st <- sts) yield p -> st.symbol.toLowerCase
        val subsSet: Set[String] = subsFlat.map(_._2)(collection.breakOut)
        allowed.forall {
          case (Some(p), st) =>
            residue.subs.get(p).exists(_.exists(_.symbol.toLowerCase == st.toLowerCase))
          case (None, st) =>
            subsSet.contains(st.toLowerCase)
        } && subsFlat.forall {
          case (i, str) =>
            allowed.exists(c => c._2.toLowerCase == str && c._1.forall(_ == i))
        }
      }
    }
    case object DefaultCond extends RuleCond {
      def matches(residue: Residue) = false
    }
  }
  sealed trait RuleMod
  case class ShapeMod(priority: Int, classes: Seq[String], shape: Shape) extends RuleMod
  case class StyleMod(style: String, content: Map[String, String]) extends RuleMod

  sealed trait Shape
  case class DefinedShape(position: Int, name: String) extends Shape
  case class Path(d: String) extends Shape
  case class Polygon(points: String) extends Shape
  case class RegularPolygon(cx: Double = 0.0, cy: Double = 0.0, n: Int, radius: Double) extends Shape
  case class Rect(x: Double = 0.0, y: Double = 0.0, width: Double, height: Double, rx: Double = 0.0, ry: Double = 0.0) extends Shape
  case class Circle(x: Double = 0.0, y: Double = 0.0, radius: Double) extends Shape
  case class Star(x: Double = 0.0, y: Double = 0.0, n: Int, r1: Double, r2: Double) extends Shape
  object Rect {
    val fromMap = (map: Map[String, String]) => Rect(
      map.get("x").fold(0.0)(_.toDouble), map.get("y").fold(0.0)(_.toDouble),
      map.get("width").fold(0.0)(_.toDouble), map.get("height").fold(0.0)(_.toDouble),
      map.get("rx").fold(0.0)(_.toDouble), map.get("ry").fold(0.0)(_.toDouble)
    )
  }
  object Circle {
    val fromMap = (map: Map[String, String]) => Circle(
      map.get("x").fold(0.0)(_.toDouble), map.get("y").fold(0.0)(_.toDouble),
      map("r").toDouble
    )
  }
  object Star {
    val fromMap = (map: Map[String, String]) => Star(
      map.get("x").fold(0.0)(_.toDouble), map.get("y").fold(0.0)(_.toDouble),
      map("n").toInt, map("r1").toDouble, map("r2").toDouble
    )
  }
  object RegularPolygon {
    val fromMap = (map: Map[String, String]) => RegularPolygon(
      map.get("x").fold(0.0)(_.toDouble), map.get("y").fold(0.0)(_.toDouble),
      map("n").toInt, map("r").toDouble
    )
  }
  class ConvBuilder(name: String) {
    val shapeDefs = Map.newBuilder[String, Shape]
    val rules = Seq.newBuilder[ConvRule]
    val palettes = Seq.newBuilder[Palette]
    def +=(sd: (String, Shape)): ConvBuilder = { shapeDefs += sd; this }
    def +=(cr: ConvRule): ConvBuilder = { rules += cr; this }
    def +=(pl: Palette): ConvBuilder = { palettes += pl; this }
    def result(): Conv = Conv(name, shapeDefs.result(), rules.result(), palettes.result())
  }

  case class Palette(name: String, residues: Seq[(ResidueType, Map[Int, Vector[SubstituentType]])])

  object Palette {
    val Repeat = Palette(
      "Repeat",
      Seq[(ResidueType, Map[Int, Vector[SubstituentType]])](
        (ResidueType.Begin, Map.empty),
        (ResidueType.End, Map.empty)
      )
    )

    implicit val reusability: Reusability[Palette] = Reusability.byRef
  }
}


