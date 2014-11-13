package za.jwatson.glycanoweb

import za.jwatson.glycanoweb.ConventionEditor.RuleCond._
import za.jwatson.glycanoweb.structure.Absolute.{L, D}
import za.jwatson.glycanoweb.structure.{Absolute, Anomer}
import za.jwatson.glycanoweb.structure.Anomer.{Beta, Alpha}

import org.parboiled2._
import ConventionEditor._

class ConventionParser(val input: ParserInput) extends Parser {

  def conv: Rule1[Conv] = rule { ws ~ convention ~ EOI }

  def ws: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
  def ws(c: Char): Rule0 = rule { ch(c) ~ ws }
  def ws(s: String): Rule0 = rule { str(s) ~ ws }
  def conventions: Rule1[Seq[Conv]] = rule { ws ~ oneOrMore(convention) ~ EOI }
  def convention: Rule1[Conv] = rule {
    conventionName ~> (new ConvBuilder(_)) ~ ws('{') ~ zeroOrMore(shapeDef | convRule) ~> (_.result()) ~ ws('}')
  }
  def conventionName = rule { ws("convention") ~ stringLiteral }

  def shapeDef = rule { ws("def ") ~ identifier ~ ws('=') ~ shape ~> ((_: ConvBuilder) += _ -> _) }
  def convRule = rule { (ruleCond | defaultCond) ~ modifiers ~> ConvRule ~> ((_: ConvBuilder) += _) }
  type RuleCondBuilder = collection.mutable.Builder[RuleCond, Seq[RuleCond]]
  def defaultCond = rule { ws("default") ~ push(Seq(DefaultCond)) }
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

  def anoCond: Rule1[Option[RuleCond]] = rule { optional(anomer ~> AnoCond) }
  def absCond: Rule1[Option[RuleCond]] = rule { optional(absolute ~> AbsCond) }
  def resCond: Rule1[Option[RuleCond]] = rule { (residueTypeList ~> ResCond ~> ((rc: ResCond) => Some(rc): Option[ResCond])) | (ws('*') ~ push(None: Option[ResCond])) }
  def subCond: Rule1[Option[RuleCond]] = rule { optional(ws('<') ~ zeroOrMore(identifier).separatedBy(ws(',')) ~ ws('>') ~> SubCond) }

  def residueTypeList: Rule1[Seq[String]] = rule { ws('(') ~ oneOrMore(identifier).separatedBy(ws(',')) ~ ws(')') }

  def anomer: Rule1[Anomer] = rule { ano(Alpha, "a", "alpha", "α") | ano(Beta, "b", "beta", "ß") }
  def ano(anomer: Anomer, short: String, long: String, unicode: String) = rule {
    (ignoreCase(short.toLowerCase) | ignoreCase(long.toLowerCase) | unicode) ~ ws ~ push(anomer)
  }

  def absolute: Rule1[Absolute] = rule { abs(D, "d", "ᴅ") | abs(L, "l", "ʟ") }
  def abs(abs: Absolute, char: String, unicode: String) = rule {
    (ignoreCase(char) | unicode) ~ ws ~ push(abs)
  }

  def shape = rule { polygon | rect | circle | star | definedShape }
  def namedShape(name: String) = rule { ws(name) ~ ws('(') ~ namedArgList ~ ws(')') }
  def polygon: Rule1[Shape] = rule { namedShape("Polygon") ~> ((map: Map[String, String]) => Polygon(map("points"))) }
  def rect: Rule1[Shape] = rule { namedShape("Rect") ~> Rect.fromMap }
  def circle: Rule1[Shape] = rule { namedShape("Circle") ~> Circle.fromMap }
  def star: Rule1[Shape] = rule { namedShape("Star") ~> Star.fromMap }
  def dummyShape: Rule1[DefinedShape] = rule { push(cursor) ~ ((identifier ~ ws('(') ~ namedArgList ~ drop[Map[String, String]] ~ ws(')')) | identifier) ~> DefinedShape }
  def definedShape: Rule1[DefinedShape] = rule { push(cursor) ~ identifier ~> DefinedShape }

  def namedArgList: Rule1[Map[String, String]] = rule { (zeroOrMore(namedArg) separatedBy ws(',')) ~> ((_: Seq[(String, String)]).toMap) }
  def namedArg: Rule1[(String, String)] = rule { identifier ~ ws('=') ~ stringLiteral ~> ((_: String) -> (_: String)) }

  def identifier: Rule1[String] = rule { capture(oneOrMore(CharPredicate.AlphaNum)) ~ ws }
  def stringLiteral: Rule1[String] = rule { '"' ~ capture(zeroOrMore(noneOf("\"\r\n"))) ~ '"' ~ ws }
  def intLiteral: Rule1[Int] = rule { capture(optional(ch('+')|'-') ~ oneOrMore(CharPredicate.Digit)) ~> (_.toInt) ~ ws }
}