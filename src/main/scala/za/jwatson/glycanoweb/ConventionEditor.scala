package za.jwatson.glycanoweb

import org.parboiled2.{Rule0, Parser, ParserInput}
import rx._
import shapeless.HNil
import za.jwatson.glycanoweb.render.Convention
import scalajs.js
import scalatags.JsDom._
import all._
import BootstrapScalatags._
import scalaz.syntax.std.option._
import org.scalajs.jquery.{jQuery => jQ, JQuery}
import ConventionEditor._

class ConventionEditor(val modalId: String) {
  val text = Var("")
  def createNavButton = navBtn(size = Lg)(onclick:=(() => jQ("#" + modalId).modal("show")))("Convention Editor")
  def renderModal = {
    val textAreaId = modalId + "Text"
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
        btn(Primary)(onclick:=parseText)("Save")
      ).some
    ).render
  }
  def parseText(): Unit = {
    val inText = jQ("#" + modalId).text()



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

  case class Conv(name: String, shapeDefs: Map[String, Shape], rules: Seq[ConvRule])
  case class ConvRule(priority: Int, shapes: Map[Shape, Seq[String]], styles: Seq[String])
  case class ShapeDef(name: String, shape: Shape)
  sealed trait Shape
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
    val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
    def ws: Rule0 = rule { zeroOrMore(WhiteSpaceChar) }
    def ws(c: Char): Rule0 = rule { c ~ ws }
    def ws(s: String): Rule0 = rule { str(s) ~ ws }
    def Conventions = rule { ws ~ zeroOrMore(ConventionOuter) }
    def ConventionOuter = rule { ws("convention") ~ capture(oneOrMore(CharPredicate.AlphaNum)) ~> Conv ~ ws('{') ~ ConventionInner ~ ws('}') }
    def ConventionInner = rule { zeroOrMore(CShapeDef | CRule) }

    def CShapeDef = rule { ws("def ") ~ Identifier ~ ws('=') ~ ShapeMod }
    def CRule = rule { CCond ~ oneOrMore(ws("->") ~ CMod) }
    def CCond = rule { ws }
    def CMod = rule { ShapeMod | StyleMod }

    def ShapeMod = rule {
      optional('#' ~ IntLiteral) ~
      optional(ws('[') ~ oneOrMore(Identifier).separatedBy(ws(',')) ~ ws(']')) ~> (_.toSeq.flatten) ~
      (SPolygon | SRect) ~>
      ((priority: Option[Int], classes: Option[Seq[String]], shape: Shape) => {
        1
      })
    }
    def StyleMod = rule { ws }

    def shape(name: String) = rule { ws(name) ~ ws('(') ~ NamedArgList ~ ws(')') }
    def SPolygon: Rule1[Shape] = rule { shape("Polygon") ~> (map => Polygon(map("points"))) }
    def SRect: Rule1[Shape] = rule { shape("Rect") ~> Rect.fromMap }

    def NamedArgList = rule { zeroOrMore(NamedArg).separatedBy(ws(',')) ~> (_.toMap) }
    def NamedArg = rule { Identifier ~ ws('=') ~ StringLiteral ~> (_ -> _) }

    def Identifier: Rule1[String] = rule { capture(oneOrMore(CharPredicate.AlphaNum)) ~ ws }
    def StringLiteral = rule { '"' ~ capture(zeroOrMore(noneOf("\""))) ~ '"' ~ ws }
    def IntLiteral = rule { capture(optional(ch('+')|'-') ~ oneOrMore(CharPredicate.Digit)) ~> (_.toInt) }
  }
}
