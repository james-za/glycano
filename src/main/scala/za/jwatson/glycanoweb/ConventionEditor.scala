package za.jwatson.glycanoweb

import org.parboiled2.{ParseError, Rule0, Parser, ParserInput}
import rx._
import shapeless.HNil
import za.jwatson.glycanoweb.ConventionEditor.RuleCond.{SubCond, ResCond, AbsCond, AnoCond}
import za.jwatson.glycanoweb.render.Convention
import za.jwatson.glycanoweb.structure.Absolute.{L, D}
import za.jwatson.glycanoweb.structure.Anomer._
import za.jwatson.glycanoweb.structure._
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.{Success, Failure}
import scalajs.js
import scalatags.JsDom._
import all._
import BootstrapScalatags._
import scalaz.syntax.std.option._
import scalaz.std.option._
import org.scalajs.jquery.{jQuery => jQ, JQuery}
import ConventionEditor._

class ConventionEditor(val modalId: String) {
  val text = Var("")
  val textAreaId = modalId + "Text"
  def createNavButton = navBtn(size = Lg)(onclick:=(() => jQ("#" + modalId).modal("show")))("Convention Editor")
  def renderModal = {
    modal(modalId,
      header = span("Convention Editor").some,
      body = div(
        form("role".attr:="form")(
          formGroup(
            label(`for`:=textAreaId),
            textarea(cls:="form-control", rows:=3, id:=textAreaId)
          )
        )
      ).some,
      footer = div(
        btn(Default)("data-dismiss".attr:="modal")("Close"),
        btn(Primary)(onclick:=(() => parseText()))("Save")
      ).some
    ).render
  }
  def parseText(): Unit = {
    val inText = jQ("#" + textAreaId).`val`().asInstanceOf[String]
    println("parsing: " + inText.take(20) + "...")
    val parser = new ConventionParser(inText)
    val result = parser.conventions.run() match {
      case Success(c)             ⇒ "Expression is valid\n" + c.mkString("\n")
      case Failure(e: ParseError) ⇒ "Expression is not valid: " + parser.formatError(e)
      case Failure(e)             ⇒ "Unexpected error during parsing run: " + e
    }
    println(result)
    println("...done")
    text() = inText
  }
  jQ("#" + textAreaId).`val`(testText)
}

object ConventionEditor {
  val testText =
    """
      |convention "UCT" {
      |    def Triangle = Polygon(points="40,35 0,70 0,0")
      |    def Diamond = Polygon(points="80,40 40,80 0,40 40,0")
      |    def Arrow = Polygon(points="90,30 60,60 0,60 0,0 60,0")
      |    def Hexagon = Polygon(points="90,40 65,80 25,80 0,40 25,0 65,0")
      |
      |    def LShape = Polygon(points="0,0 0,44 36,44 36,40 4,40 4,0")
      |
      |    (Glycero)
      |    -> #1 [primary] Triangle
      |    -> #3 [outline, links] Triangle
      |    (Erythro, Threo)
      |    -> #1 [primary] Diamond
      |    -> #3 [outline, links] Diamond
      |    (Ara, Lyx, Rib, Xyl)
      |    -> #1 [primary] Arrow
      |    -> #3 [outline, links] Arrow
      |    (Ido, All, Alt, Gal, Glc, Gul, Man, Tal)
      |    -> #1 [primary] Hexagon
      |    -> #3 [outline, links] Hexagon
      |
      |    (Ara) -> #2 [secondary] Polygon(points="0,0 0,60 30,60 30,0")
      |    (Rib) -> #2 [secondary] Polygon(points="0,0 0,30 90,30 60,0")
      |    (Ido) -> #2 [secondary] Polygon(points="25,0 65,0 25,80 65,80")
      |    (All) -> #2 [secondary] Polygon(points="0,40 25,0 65,80 25,80")
      |    (Alt) -> #2 [secondary] Polygon(points="45,0 65,0 90,40 45,40")
      |    (Gal) -> #2 [secondary] Polygon(points="0,40 90,40 65,80 25,80")
      |    (Gul) -> #2 [secondary] Polygon(points="25,0 65,0 25,80 0,40 90,40 65,80")
      |    (Man) -> #2 [secondary] Polygon(points="0,40 25,0 45,0 45,80 25,80")
      |
      |    (Glycero) -> style [primary] { fill: white }
      |    (Erythro) -> style [primary] { fill: white }
      |    (Threo) -> style [primary] { fill: black }
      |    D (Ara) -> style [primary] { fill: white } -> style [secondary] { fill: black }
      |    L (Ara) -> style [primary] { fill: black } -> style [secondary] { fill: white }
      |      (Lyx) -> style [primary] { fill: black }
      |    D (Rib) -> style [primary] { fill: black } -> style [secondary] { fill: white }
      |    L (Rib) -> style [primary] { fill: white } -> style [secondary] { fill: black }
      |      (Xyl) -> style [primary] { fill: #FFA0A0 }
      |    D (Ido) -> style [primary] { fill: black } -> style [secondary] { fill: #BF6000 }
      |    L (Ido) -> style [primary] { fill: #BF6000 } -> style [secondary] { fill: black }
      |    D (All) -> style [primary] { fill: white } -> style [secondary] { fill: black }
      |    L (All) -> style [primary] { fill: black } -> style [secondary] { fill: white }
      |    D (Alt) -> style [primary] { fill: black } -> style [secondary] { fill: white }
      |    L (Alt) -> style [primary] { fill: white } -> style [secondary] { fill: black }
      |    D (Gal) -> style [primary] { fill: yellow } -> style [secondary] { fill: black }
      |    L (Gal) -> style [primary] { fill: black } -> style [secondary] { fill: yellow }
      |      (Glc) -> style [primary] { fill: blue }
      |    D (Gul) -> style [primary] { fill: black } -> style [secondary] { fill: white }
      |    L (Gul) -> style [primary] { fill: white } -> style [secondary] { fill: black }
      |    D (Man) -> style [primary] { fill: lime } -> style [secondary] { fill: black }
      |    L (Man) -> style [primary] { fill: black } -> style [secondary] { fill: lime }
      |    (Tal) -> style [primary] { fill: black }
      |
      |    L (Glycero)                                -> #4 [lshape] LShape -> style [lshape] { x: 22; y: 18 }
      |    L (Erythro, Threo)                         -> #4 [lshape] LShape -> style [lshape] { x: 22; y: 18 }
      |    L (Ara, Lyx, Rib, Xyl)                     -> #4 [lshape] LShape -> style [lshape] { x: 22; y: 18 }
      |    L (Ido, All, Alt, Gal, Glc, Gul, Man, Tal) -> #4 [lshape] LShape -> style [lshape] { x: 22; y: 18 }
      |
      |    * -> #5 [handle] Rect(width="20", height="20", rx="5", ry="5")
      |    a * -> style [handle] { fill: white; stroke: black; stroke-width: 1 }
      |    b * -> style [handle] { fill: black; stroke: black; stroke-width: 1 }
      |
      |    * -> style [outline] { fill: none; stroke: black; stroke-width: 3 }
      |}
    """.stripMargin
  import scala.language.implicitConversions
  implicit def toRichJQuery(jQuery: JQuery): RichJQuery = jQuery.asInstanceOf[RichJQuery]
  trait RichJQuery extends js.Object {
    def modal(command: String): JQuery = ???
  }

  import org.parboiled2._

  case class Conv(name: String, shapeDefs: Map[String, Shape] = Map.empty, rules: Seq[ConvRule] = Seq.empty)

  case class ConvRule(conds: Seq[RuleCond], mods: Seq[RuleMod])
  sealed trait RuleCond { def matches(r: Residue): Boolean }
  object RuleCond {
    case class AnoCond(allowed: Anomer) extends RuleCond { def matches(r: Residue) = allowed == r.anomer }
    case class AbsCond(allowed: Absolute) extends RuleCond { def matches(r: Residue) = allowed == r.absolute }
    case class ResCond(allowed: Seq[String]) extends RuleCond { def matches(r: Residue) = allowed contains r.rt.symbol }
    case class SubCond(allowed: Seq[String]) extends RuleCond { def matches(r: Residue) = allowed contains "" }
  }
  sealed trait RuleMod
  case class ShapeMod(priority: Int, classes: Seq[String], shape: Shape) extends RuleMod
  case class StyleMod(style: String, content: Map[String, String]) extends RuleMod

  sealed trait Shape
  case class DefinedShape(position: Int, name: String) extends Shape
  case class Polygon(points: String) extends Shape
  case class Rect(x: String = "0", y: String = "0", width: String, height: String, rx: String = "0", ry: String = "0") extends Shape
  object Rect {
    val fromMap = (map: Map[String, String]) => Rect(
      map.getOrElse("x", "0"), map.getOrElse("y", "0"),
      map("width"), map("height"),
      map.getOrElse("rx", "0"), map.getOrElse("ry", "0")
    )
  }
  class ConvBuilder(name: String) {
    val shapeDefs = Map.newBuilder[String, Shape]
    val rules = Seq.newBuilder[ConvRule]
    def +=(sd: (String, Shape)): ConvBuilder = { shapeDefs += sd; this }
    def +=(cr: ConvRule): ConvBuilder = { rules += cr; this }
    def result(): Conv = Conv(name, shapeDefs.result(), rules.result())
  }

  class ConventionParser(val input: ParserInput) extends Parser {
    import shapeless._
    import scalaz.syntax.std.option._

    val WhiteSpaceChar = CharPredicate.from((c: Char) => " \n\r\t\f".contains(c))
    def ws: Rule0 = rule { zeroOrMore(WhiteSpaceChar) }
    def ws(c: Char): Rule0 = rule { c ~ ws }
    def ws(s: String): Rule0 = rule { str(s) ~ ws }
    def conventions = rule { ws ~ oneOrMore(convention) ~ EOI }
    def convention = rule {
      conventionName ~> (new ConvBuilder(_)) ~ ws('{') ~ conventionInner ~ ws('}') ~> (_.result())
    }
    def conventionName = rule { ws("convention") ~ stringLiteral }
    def conventionInner: Rule[ConvBuilder :: HNil, ConvBuilder :: HNil] = rule { zeroOrMore(shapeDef | convRule) }

    def shapeDef = rule { ws("def ") ~ identifier ~ ws('=') ~ shape ~> ((_: ConvBuilder) += _ -> _) }
    def convRule = rule { ruleCond ~ modifiers ~> ConvRule ~> ((_: ConvBuilder) += _) }
    type RuleCondBuilder = mutable.Builder[RuleCond, Seq[RuleCond]]
    def ruleCond = rule {
      push(Seq.newBuilder[RuleCond]: RuleCondBuilder) ~
      anoCond ~> ruleCondBuild ~
      absCond ~> ruleCondBuild ~
      resCond ~> ruleCondBuild ~
      subCond ~> ruleCondBuild ~> (_.result())
    }
    val ruleCondBuild = (b: RuleCondBuilder, co: Option[RuleCond]) => {
      for (c <- co) b += c
      b
    }

    def modifiers: Rule1[Seq[RuleMod]] = rule { oneOrMore(ws("->") ~ (styleMod | shapeMod)) }

    def shapeMod: Rule1[ShapeMod] = rule { priority ~ classList ~ shape ~> ShapeMod }
    def priority = rule { optional('#' ~ intLiteral) ~> (_.getOrElse(0)) }
    def classList = rule { optional(ws('[') ~ oneOrMore(identifier).separatedBy(ws(',')) ~ ws(']')) ~> (_.toSeq.flatten) }

    def styleMod: Rule1[StyleMod] = rule { styleName ~ ws('{') ~ styleRules ~ ws('}') ~> StyleMod }
    def styleName = rule { ws("style") ~ ws('[') ~ identifier ~ ws(']') }
    def styleRules = rule { zeroOrMore(cssIdent ~ ws(':') ~ cssValue ~> (_ -> _) ~ optional(ws(';'))) ~> (_.toMap) }
    def cssIdent = rule { capture(oneOrMore(CharPredicate.AlphaNum | ch('-'))) ~ ws }
    def cssValue = rule { capture(oneOrMore(noneOf(";{}"))) }

    def anoCond = rule { optional(anomer ~> AnoCond) }
    def absCond = rule { optional(absolute ~> AbsCond) }
    def resCond = rule { (residueTypeList ~> ResCond ~> (_.some)) | (ws('*') ~ push(none)) }
    def subCond = rule { optional(ws('<') ~ oneOrMore(identifier).separatedBy(ws(',')) ~ ws('>') ~> SubCond) }
    
    def residueTypeList: Rule1[Seq[String]] = rule { ws('(') ~ oneOrMore(identifier).separatedBy(ws(',')) ~ ws(')') }

    def anomer: Rule1[Anomer] = rule { ano(Alpha, "a", "alpha", "α") | ano(Beta, "b", "beta", "ß") }
    def ano(anomer: Anomer, short: String, long: String, unicode: String) = rule {
      (ignoreCase(short.toLowerCase) | ignoreCase(long.toLowerCase) | unicode) ~ ws ~ push(anomer)
    }

    def absolute: Rule1[Absolute] = rule { abs(D, "d", "ᴅ") | abs(L, "l", "ʟ") }
    def abs(abs: Absolute, char: String, unicode: String) = rule {
      (ignoreCase(char) | unicode) ~ ws ~ push(abs)
    }

    def shape = rule { polygon | rect | dummyShape }
    def namedShape(name: String) = rule { ws(name) ~ ws('(') ~ namedArgList ~ ws(')') }
    def polygon: Rule1[Shape] = rule { namedShape("Polygon") ~> ((map: Map[String, String]) => Polygon(map("points"))) }
    def rect: Rule1[Shape] = rule { namedShape("Rect") ~> Rect.fromMap }
    def dummyShape = rule { push(cursor) ~ ((identifier ~ ws('(') ~ namedArgList ~ drop[Map[String, String]] ~ ws(')')) | identifier) ~> DefinedShape }
    def definedShape = rule { push(cursor) ~ identifier ~> DefinedShape }
    
    def namedArgList = rule { (zeroOrMore(namedArg) separatedBy ws(',')) ~> (_.toMap) }
    def namedArg = rule { identifier ~ ws('=') ~ stringLiteral ~> (_ -> _) }

    def identifier = rule { capture(oneOrMore(CharPredicate.AlphaNum)) ~ ws }
    def stringLiteral = rule { '"' ~ capture(zeroOrMore(noneOf("\""))) ~ '"' ~ ws }
    def intLiteral = rule { capture(optional(ch('+')|'-') ~ oneOrMore(CharPredicate.Digit)) ~> (_.toInt) ~ ws }
  }
}
