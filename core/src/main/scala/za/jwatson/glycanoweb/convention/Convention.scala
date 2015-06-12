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
        val subsSet = (for ((p, sts) <- residue.subs; st <- sts) yield st.symbol.toLowerCase).toSet
        allowed.forall {
          case (Some(p), st) =>
            residue.subs.get(p).exists(_.exists(_.symbol.toLowerCase == st.toLowerCase))
          case (None, st) =>
            subsSet.contains(st.toLowerCase)
        } && subsSet.forall(str => allowed.exists(_._2.toLowerCase == str))
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
  case class Rect(x: String = "0", y: String = "0", width: String, height: String, rx: String = "0", ry: String = "0") extends Shape
  case class Circle(x: String = "0", y: String = "0", radius: String) extends Shape
  case class Star(x: String = "0", y: String = "0", n: String, r1: String, r2: String) extends Shape
  object Rect {
    val fromMap = (map: Map[String, String]) => Rect(
      map.getOrElse("x", "0"), map.getOrElse("y", "0"),
      map("width"), map("height"),
      map.getOrElse("rx", "0"), map.getOrElse("ry", "0")
    )
  }
  object Circle {
    val fromMap = (map: Map[String, String]) =>
      Circle(map.getOrElse("x", "0"), map.getOrElse("y", "0"), map("r"))
  }
  object Star {
    val fromMap = (map: Map[String, String]) =>
      Star(map.getOrElse("x", "0"), map.getOrElse("y", "0"), map("n"), map("r1"), map("r2"))
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
    implicit val reusability: Reusability[Palette] = Reusability.byRef
  }
}


