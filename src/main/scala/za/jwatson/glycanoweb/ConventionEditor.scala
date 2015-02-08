package za.jwatson.glycanoweb

import org.parboiled2.ParseError
import org.scalajs.jquery.{JQuery, jQuery => jQ}
import rx._
import za.jwatson.glycanoweb.BootstrapScalatags._
import za.jwatson.glycanoweb.ConventionEditor._
import za.jwatson.glycanoweb.render.DisplayConv
import za.jwatson.glycanoweb.structure._

import scala.scalajs.js
import scala.util.{Failure, Success}
import scalatags.JsDom._
import scalatags.JsDom.all._
import scalaz.syntax.std.option._

//class ConventionEditor(val modalId: String) {
//  val text = Var("")
//  val textAreaId = modalId + "Text"
//  def createNavButton = navBtn(size = Md)(onclick:=(() => jQ("#" + modalId).modal("show")))("Convention Editor")
//  def renderModal = {
//    val r = modal(modalId,
//      header = span("Convention Editor").some,
//      body = div(
//        form("role".attr:="form")(
//          formGroup(
//            label(`for`:=textAreaId),
//            textarea(cls:="form-control", rows:=30, id:=textAreaId)
//          )
//        )
//      ).some,
//      footer = div(
//        btn(Default)("data-dismiss".attr:="modal")("Close"),
//        btn(Primary)(onclick:=(() => parseText()))("Save")
//      ).some
//    ).render
//    r
//  }
//  def parseText(): Unit = {
//    val inText = jQ("#" + textAreaId).`val`().asInstanceOf[String]
//    println("parsing: " + inText.take(20) + "...")
//    val parser = new ConventionParser(inText)
//    parser.conv.run() match {
//      case Success(c)             =>
//        //println(c)
//        DisplayConv.convs(c.name) = inText
//        org.scalajs.dom.localStorage.setItem("glycano.conventions", js.JSON.stringify(DisplayConv.convs))
//        DisplayConv.refresh()
//      case Failure(e: ParseError) => println("Expression is not valid: " + parser.formatError(e))
//      case Failure(e)             => println("Unexpected error during parsing run: " + e)
//    }
//    println("...done")
//    text() = inText
//  }
//  def setText(text: String): Unit = {
//    jQ("#" + textAreaId).`val`(text)
//  }
//}

object ConventionEditor {
  val textUCT =
    """
      |convention "UCT" {
      |   def Triangle = Polygon(points="40,35 0,70 0,0")
      |   def Diamond = Polygon(points="80,40 40,80 0,40 40,0")
      |   def Arrow = Polygon(points="90,30 60,60 0,60 0,0 60,0")
      |   def Hexagon = Polygon(points="90,40 65,80 25,80 0,40 25,0 65,0")
      |   def Seven = Polygon(points="90,40 65,80 25,80 0,40 25,0 45,15 65,0")
      |   def Eight = Polygon(points="90,40 65,80 45,65 25,80 0,40 25,0 45,15 65,0")
      |
      |   def LShape = Polygon(points="0,0 0,44 36,44 36,40 4,40 4,0")
      |
      |   default
      |   -> #1 [primary] Hexagon
      |   -> #3 [outline, links] Hexagon
      |   -> #5 [handle] Rect(x="80", y="30", width="20", height="20", rx="5", ry="5")
      |   -> style [primary] { fill: #FFFFFF }
      |   -> style [outline] { fill: none; stroke: #000000; stroke-width: 3 }
      |   -> style [handle] { fill: #FFFFFF; stroke: #000000; stroke-width: 1 }
      |
      |   (Glycero)
      |   -> #1 [primary] Triangle
      |   -> #3 [outline, links] Triangle
      |   -> #5 [handle] Rect(x="30", y="25", width="20", height="20", rx="5", ry="5")
      |   (Erythro, Threo)
      |   -> #1 [primary] Diamond
      |   -> #3 [outline, links] Diamond
      |   -> #5 [handle] Rect(x="70", y="30", width="20", height="20", rx="5", ry="5")
      |   (Ara, Lyx, Rib, Xyl)
      |   -> #1 [primary] Arrow
      |   -> #3 [outline, links] Arrow
      |   -> #5 [handle] Rect(x="80", y="20", width="20", height="20", rx="5", ry="5")
      |   (Ido, All, Alt, Gal, Glc, Gul, Man, Tal)
      |   -> #1 [primary] Hexagon
      |   -> #3 [outline, links] Hexagon
      |   -> #5 [handle] Rect(x="80", y="30", width="20", height="20", rx="5", ry="5")
      |
      |   (Ara) -> #2 [secondary] Polygon(points="0,0 0,60 30,60 30,0")
      |   (Rib) -> #2 [secondary] Polygon(points="0,0 0,30 90,30 60,0")
      |   (Ido) -> #2 [secondary] Polygon(points="25,0 65,0 25,80 65,80")
      |   (All) -> #2 [secondary] Polygon(points="0,40 25,0 65,80 25,80")
      |   (Alt) -> #2 [secondary] Polygon(points="45,0 65,0 90,40 45,40")
      |   (Gal) -> #2 [secondary] Polygon(points="0,40 90,40 65,80 25,80")
      |   (Gul) -> #2 [secondary] Polygon(points="25,0 65,0 25,80 0,40 90,40 65,80")
      |   (Man) -> #2 [secondary] Polygon(points="0,40 25,0 45,0 45,80 25,80")
      |
      |   (Glycero) -> style [primary] { fill: #FFFFFF }
      |   (Erythro) -> style [primary] { fill: #FFFFFF }
      |   (Threo) -> style [primary] { fill: #000000 }
      |   D (Ara) -> style [primary] { fill: #FFFFFF } -> style [secondary] { fill: #000000 }
      |   L (Ara) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #FFFFFF }
      |   (Lyx) -> style [primary] { fill: #000000 }
      |   D (Rib) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #FFFFFF }
      |   L (Rib) -> style [primary] { fill: #FFFFFF } -> style [secondary] { fill: #000000 }
      |   (Xyl) -> style [primary] { fill: #FFA0A0 }
      |   D (Ido) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #BF6000 }
      |   L (Ido) -> style [primary] { fill: #BF6000 } -> style [secondary] { fill: #000000 }
      |   D (All) -> style [primary] { fill: #FFFFFF } -> style [secondary] { fill: #000000 }
      |   L (All) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #FFFFFF }
      |   D (Alt) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #FFFFFF }
      |   L (Alt) -> style [primary] { fill: #FFFFFF } -> style [secondary] { fill: #000000 }
      |   D (Gal) -> style [primary] { fill: #FFFF00 } -> style [secondary] { fill: #000000 }
      |   L (Gal) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #FFFF00 }
      |   (Glc) -> style [primary] { fill: #0000FF }
      |   D (Gul) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #FFFFFF }
      |   L (Gul) -> style [primary] { fill: #FFFFFF } -> style [secondary] { fill: #000000 }
      |   D (Man) -> style [primary] { fill: #00FF00 } -> style [secondary] { fill: #000000 }
      |   L (Man) -> style [primary] { fill: #000000 } -> style [secondary] { fill: #00FF00 }
      |   (Tal) -> style [primary] { fill: #000000 }
      |
      |   L (Glycero)
      |     -> #4 [lshape] LShape
      |     -> style [lshape] { x: 4; y: 14 }
      |   L (Erythro, Threo)
      |     -> #4 [lshape] LShape
      |     -> style [lshape] { x: 22; y: 18 }
      |   L (Ara, Lyx, Rib, Xyl)
      |     -> #4 [lshape] LShape
      |     -> style [lshape] { x: 22; y: 8 }
      |   L (Ido, All, Alt, Gal, Glc, Gul, Man, Tal,
      |      Fru, Psi, Sor, Tag)
      |     -> #4 [lshape] LShape
      |     -> style [lshape] { x: 27; y: 18 }
      |
      |   a * -> style [handle] { fill: #FFFFFF; stroke: #000000; stroke-width: 1 }
      |   b * -> style [handle] { fill: #000000; stroke: #000000; stroke-width: 1 }
      |
      |   * -> style [outline] { fill: none; stroke: #000000; stroke-width: 3 }
      |   * -> style [lshape] { fill: #000000; stroke: #FFFFFF; stroke-width: 1 }
      |
      |   (Three)
      |   -> #0 [primary] Triangle
      |   -> #2 [outline, links] Triangle
      |   -> #3 [outlinefront] Triangle
      |   -> #5 [handle] Rect(x="30", y="25", width="20", height="20", rx="5", ry="5")
      |
      |   (Four)
      |   -> #0 [primary] Diamond
      |   -> #2 [outline, links] Diamond
      |   -> #3 [outlinefront] Diamond
      |   -> #5 [handle] Rect(x="70", y="30", width="20", height="20", rx="5", ry="5")
      |
      |   (Rul, Xul)
      |   -> #0 [primary] Arrow
      |   -> #2 [outline, links] Arrow
      |   -> #3 [outlinefront] Arrow
      |   -> #5 [handle] Rect(x="80", y="20", width="20", height="20", rx="5", ry="5")
      |
      |   (Fru, Psi, Sor, Tag)
      |   -> #0 [primary] Hexagon
      |   -> #2 [outline, links] Hexagon
      |   -> #3 [outlinefront] Hexagon
      |   -> #5 [handle] Rect(x="80", y="30", width="20", height="20", rx="5", ry="5")
      |
      |   (AltHep)
      |   -> #0 [primary] Seven
      |   -> #2 [outline, links] Seven
      |   -> #3 [outlinefront] Seven
      |   -> #5 [handle] Rect(x="80", y="30", width="20", height="20", rx="5", ry="5")
      |
      |   (ManOct)
      |   -> #0 [primary] Eight
      |   -> #2 [outline, links] Eight
      |   -> #3 [outlinefront] Eight
      |   -> #5 [handle] Rect(x="80", y="30", width="20", height="20", rx="5", ry="5")
      |
      |   (Sor) -> #1 [secondary] Polygon(points="0,40 90,40 65,80 25,80")
      |   (Tag) -> #1 [secondary] Polygon(points="90,40 65,0 45,0 45,80 65,80")
      |
      |   (Three, Four, Rul, Fru, Sor, Tag, AltHep, ManOct) -> style [primary] { fill: #FFFFFF }
      |   (Xul, Psi) -> style [primary] { fill: #808080 }
      |   (Sor) -> style [secondary] { fill: #808080 }
      |   (Tag) -> style [secondary] { fill: #808080 }
      |
      |   (Three, Four, Rul, Xul, Fru, Psi, Sor, Tag, AltHep, ManOct)
      |   -> style [outline] { stroke: #000000; stroke-width: 11 }
      |   -> style [outlinefront] { stroke: #FFFFFF; stroke-width: 9 }
      |}
    """.stripMargin

  val textCFG =
    """
      |convention "CFG" {
      |   def CFGCircle = Circle(r="30")
      |   def CFGSquare = Rect(width="60", height="60")
      |   def CFGSquareTR = Polygon(points="0,0 60,0 60,60")
      |   def CFGDiamond = Polygon(points="30,0 60,30 30,60 0,30")
      |   def CFGDiamondT = Polygon(points="0,30 30,0 60,30")
      |   def CFGDiamondB = Polygon(points="60,30 30,60 0,30")
      |   def CFGDiamondL = Polygon(points="30,60 0,30 30,0")
      |   def CFGDiamondR = Polygon(points="30,0 60,30 30,60")
      |   def CFGTriangle = Polygon(points="0,60 30,0 60,60")
      |   def CFGStar = Star(n="5", r1="-10", r2="-30")
      |
      |   default
      |   -> #1 [primary] CFGCircle
      |   -> #2 [outline, links] CFGCircle
      |   -> style [primary] { fill: #FFFFFF }
      |   -> style [outline] { stroke: #000000; stroke-width: 3 }
      |
      |   (Gal, Glc, Man) <>
      |   -> #1 [primary] CFGCircle
      |   -> #2 [outline, links] CFGCircle
      |
      |   (Gal, Glc, Man) <N, Ac>
      |   -> #1 [primary] CFGSquare
      |   -> #2 [outline, links] CFGSquare
      |
      |   (Gal, Glc, Man) <COOH>
      |   -> #1 [secondary] CFGSquare
      |   -> #2 [primary] CFGSquareTR
      |   -> #3 [thin] CFGSquareTR
      |   -> #4 [outline, links] CFGSquare
      |
      |   (Fuc) <>
      |   -> #1 [primary] CFGTriangle
      |   -> #2 [outline, links] CFGTriangle
      |
      |   (Xyl) <>
      |   -> #1 [primary] CFGStar
      |   -> #2 [outline, links] CFGStar
      |
      |   (Gal) -> style [primary] { fill: #FFFF00 }
      |   (Glc) -> style [primary] { fill: #0000FA }
      |   (Man) -> style [primary] { fill: #00C832 }
      |   (Fuc) -> style [primary] { fill: #FA0000 }
      |   (Xyl) -> style [primary] { fill: #FAEAD5 }
      |
      |   * -> style [outline] { stroke: #000000; stroke-width: 3 }
      |   * -> style [thin] { stroke: #000000; stroke-width: 1 }
      |   * -> style [secondary] { fill: #FFFFFF }
      |}
    """.stripMargin
  import scala.language.implicitConversions
  implicit def toRichJQuery(jQuery: JQuery): RichJQuery = jQuery.asInstanceOf[RichJQuery]
  trait RichJQuery extends js.Object {
    def modal(command: String): JQuery = js.native
  }

  import org.parboiled2._

  case class Conv(name: String, shapeDefs: Map[String, Shape] = Map.empty, rules: Seq[ConvRule] = Seq.empty)

  case class ConvRule(conds: Seq[RuleCond], mods: Seq[RuleMod])
  sealed trait RuleCond { def matches(ano: Anomer, abs: Absolute, rt: ResidueType, subs: Map[Int, Vector[SubstituentType]]): Boolean }
  object RuleCond {
    case class AnoCond(allowed: Anomer) extends RuleCond {
      def matches(ano: Anomer, abs: Absolute, rt: ResidueType, subs: Map[Int, Vector[SubstituentType]]) = allowed == ano
    }
    case class AbsCond(allowed: Absolute) extends RuleCond {
      def matches(ano: Anomer, abs: Absolute, rt: ResidueType, subs: Map[Int, Vector[SubstituentType]]) = allowed == abs
    }
    case class ResCond(allowed: Seq[String]) extends RuleCond {
      def matches(ano: Anomer, abs: Absolute, rt: ResidueType, subs: Map[Int, Vector[SubstituentType]]) = allowed contains rt.symbol
    }
    case class SubCond(allowed: Seq[String]) extends RuleCond {
      def matches(ano: Anomer, abs: Absolute, rt: ResidueType, subs: Map[Int, Vector[SubstituentType]]) = {
        //allowed.forall(a => subs.values.flatten.exists(_.st.symbol == a))
        val allowedSet = allowed.map(_.toLowerCase).toSet
        val subsSet = subs.values.toVector.flatten.map(_.symbol.toLowerCase).toSet
        allowedSet == subsSet
      }
    }
    case object DefaultCond extends RuleCond {
      def matches(ano: Anomer, abs: Absolute, rt: ResidueType, subs: Map[Int, Vector[SubstituentType]]) = false
    }
  }
  sealed trait RuleMod
  case class ShapeMod(priority: Int, classes: Seq[String], shape: Shape) extends RuleMod
  case class StyleMod(style: String, content: Map[String, String]) extends RuleMod

  sealed trait Shape
  case class DefinedShape(position: Int, name: String) extends Shape
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
    def +=(sd: (String, Shape)): ConvBuilder = { shapeDefs += sd; this }
    def +=(cr: ConvRule): ConvBuilder = { rules += cr; this }
    def result(): Conv = Conv(name, shapeDefs.result(), rules.result())
  }

}


