package za.jwatson.glycanoweb

import org.parboiled2.{ParseError, Rule0, Parser, ParserInput}
import rx._
import shapeless.HNil
import za.jwatson.glycanoweb.render.Convention
import za.jwatson.glycanoweb.structure.Absolute.{L, D}
import za.jwatson.glycanoweb.structure.{ResidueType, Absolute, Anomer}
import za.jwatson.glycanoweb.structure.Anomer.{Beta, Alpha}
import scala.util.{Success, Failure}
import scalajs.js
import scalatags.JsDom._
import all._
import BootstrapScalatags._
import scalaz.syntax.std.option._
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
    val result = parser.Conventions.run() match {
      case Success(c)             ⇒ "Expression is valid\n" + c.mkString("\n")
      case Failure(e: ParseError) ⇒ "Expression is not valid: " + parser.formatError(e)
      case Failure(e)             ⇒ "Unexpected error during parsing run: " + e
    }
    println(result)
    println("...done")
    text() = inText
  }
}

object ConventionEditor {
  import scala.language.implicitConversions
  implicit def toRichJQuery(jQuery: JQuery): RichJQuery = jQuery.asInstanceOf[RichJQuery]
  trait RichJQuery extends js.Object {
    def modal(command: String): JQuery = ???
  }

  import org.parboiled2._

  case class Conv(name: String, shapeDefs: Map[String, Shape] = Map.empty, rules: Map[ConvCond, ConvRule] = Map.empty)
  case class ShapeDef(name: String, shape: Shape)
  case class ConvCond(anomers: Seq[Anomer], absolutes: Seq[Absolute], residueTypes: Seq[String], substituents: Seq[String])
  case class ConvRule(shapes: Seq[ShapeRule] = Seq.empty, styles: Map[String, String] = Map.empty)
  case class ShapeRule(priority: Int, classes: Seq[String], shape: Shape)
  sealed trait Shape
  case class DummyShape(name: String) extends Shape
  case class Polygon(points: String) extends Shape
  case class Rect(x: String = "0", y: String = "0", width: String, height: String, rx: String = "0", ry: String = "0") extends Shape
  object Rect {
    val fromMap = (map: Map[String, String]) => Rect(
      map.getOrElse("x", "0"), map.getOrElse("y", "0"),
      map("width"), map("height"),
      map.getOrElse("rx", "0"), map.getOrElse("ry", "0")
    )
  }

  class ConventionParser(val input: ParserInput) extends Parser {
    import shapeless._

    val convShapeDefsL: Lens[Conv, Map[String, Shape]] = lens[Conv] >> 'shapeDefs
    val convRulesL: Lens[Conv, Map[ConvCond, ConvRule]] = lens[Conv] >> 'rules

    val convRuleShapesL: Lens[ConvRule, Seq[ShapeRule]] = lens[ConvRule] >> 'shapes
    val convRuleStylesL: Lens[ConvRule, Map[String, String]] = lens[ConvRule] >> 'styles

    val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
    def ws: Rule0 = rule { zeroOrMore(WhiteSpaceChar) }
    def ws(c: Char): Rule0 = rule { c ~ ws }
    def ws(s: String): Rule0 = rule { str(s) ~ ws }
    def Conventions = rule { ws ~ oneOrMore(ConventionOuter) ~ EOI }
    def ConventionOuter = rule { ws("convention") ~ StringLiteral ~> (Conv(_)) ~ ws('{') ~ ConventionInner ~ ws('}') }
    def ConventionInner: Rule[Conv :: HNil, Conv :: HNil] = rule {
      zeroOrMore(
        (CShapeDef ~> ((c: Conv, sd: (String, Shape)) => convShapeDefsL.modify(c)(_ + sd))) |
        CRule ~> ((c: Conv, rule: (ConvCond, ConvRule)) => convRulesL.modify(c)(_ + rule))
      )
    }

    def CShapeDef = rule { ws("def ") ~ Identifier ~ ws('=') ~ CShape ~> (_ -> _) }
    def CRule = rule { CCond ~ push(ConvRule()) ~ oneOrMore(ws("->") ~ CMod) ~> (_ -> _) }
    def CCond: Rule1[ConvCond] = rule {
      optional(CAnomer) ~> (_.fold(Seq[Anomer](Alpha, Beta))(Seq(_))) ~
      optional(CAbsolute) ~> (_.fold(Seq[Absolute](D, L))(Seq(_))) ~
      ResidueTypeList ~
      push(Seq("")) ~>
      (ConvCond(_, _, _, _))
    }
    
    def ResidueTypeList = rule {
      ws('(') ~ (
        (ws('*') ~ push(ResidueType.ResidueTypes.map(_.symbol))) |
        oneOrMore(Identifier).separatedBy(ws(','))
      ) ~ ws(')')
    }

    def CAnomer = rule { CAlpha | CBeta }
    def CAlpha = rule { (ignoreCase("a") | ignoreCase("alpha") | "α") ~ push(Alpha: Anomer) ~ ws }
    def CBeta = rule { (ignoreCase("b") | ignoreCase("beta") | "ß") ~ push(Beta: Anomer) ~ ws }

    def CAbsolute = rule { CD | CL }
    def CD = rule { (ignoreCase("d") | "ᴅ") ~ push(D: Absolute) ~ ws }
    def CL = rule { (ignoreCase("l") | "ʟ") ~ push(L: Absolute) ~ ws }

    def CMod: Rule[ConvRule :: HNil, ConvRule :: HNil] = rule {
      StyleMod ~> ((cr: ConvRule, style: (String, String)) => {println(style); convRuleStylesL.modify(cr)(_ + style)}) |
      ShapeMod ~> ((cr: ConvRule, sr: ShapeRule) => {println(sr); convRuleShapesL.modify(cr)(_ :+ sr)})
    }

    def CShape = rule { SPolygon | SRect | SDummy }
    def ShapeMod: Rule1[ShapeRule] = rule {
      optional('#' ~ IntLiteral) ~> (_.getOrElse(0)) ~
      optional(ws('[') ~ oneOrMore(Identifier).separatedBy(ws(',')) ~ ws(']')) ~> (_.toSeq.flatten) ~
      CShape ~>
      (ShapeRule(_, _, _))
    }
    def StyleMod = rule { ws("style") ~ ws('[') ~ Identifier ~ ws(']') ~ '{' ~ ws('}') ~> (_ -> "tempstyle") }

    def SDummy = rule { ((Identifier ~ ws('(') ~ NamedArgList ~> (_ => ()) ~ ws(')')) | Identifier) ~> DummyShape }

    def shape(name: String) = rule { ws(name) ~ ws('(') ~ NamedArgList ~ ws(')') }
    def SPolygon: Rule1[Shape] = rule { shape("Polygon") ~> ((map: Map[String, String]) => Polygon(map("points"))) }
    def SRect: Rule1[Shape] = rule { shape("Rect") ~> Rect.fromMap }

    def NamedArgList = rule { zeroOrMore(NamedArg).separatedBy(ws(',')) ~> (_.toMap) }
    def NamedArg = rule { Identifier ~ ws('=') ~ StringLiteral ~> (_ -> _) }

    def Identifier: Rule1[String] = rule { capture(oneOrMore(CharPredicate.AlphaNum)) ~ ws }
    def StringLiteral = rule { '"' ~ capture(zeroOrMore(noneOf("\""))) ~ '"' ~ ws }
    def IntLiteral = rule { capture(optional(ch('+')|'-') ~ oneOrMore(CharPredicate.Digit)) ~> (_.toInt) ~ ws }
  }
}
