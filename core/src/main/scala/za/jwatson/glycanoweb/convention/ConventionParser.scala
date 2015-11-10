package za.jwatson.glycanoweb.convention

import org.parboiled2._
import shapeless.{HNil, ::}
import za.jwatson.glycanoweb.convention.Convention.RuleCond._
import za.jwatson.glycanoweb.convention.Convention._
import za.jwatson.glycanoweb.structure.Absolute.{D, L}
import za.jwatson.glycanoweb.structure.Anomer.{Alpha, Beta}
import za.jwatson.glycanoweb.structure.{ResidueType, SubstituentType, Absolute, Anomer}

import scala.annotation.tailrec

class ConventionParser(val input: ParserInput) extends Parser {

  def conv: Rule1[Conv] = rule { ws ~ convention ~ EOI }

  def ws: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
  def ws(c: Char): Rule0 = rule { ch(c) ~ ws }
  def ws(s: String): Rule0 = rule { str(s) ~ ws }
  def conventions: Rule1[Seq[Conv]] = rule { ws ~ oneOrMore(convention) ~ EOI }
  def convention: Rule1[Conv] = rule {
    conventionName ~> (new ConvBuilder(_)) ~ ws('{') ~ zeroOrMore(shapeDef | convRule | paletteRule) ~> (_.result()) ~ ws('}')
  }
  def conventionName = rule { ws("convention") ~ stringLiteral }

  def shapeDef: Rule[ConvBuilder :: HNil, ConvBuilder :: HNil] =
    rule { (ws("def ") ~ identifier ~ ws('=') ~ shape) ~> ((_: ConvBuilder) += (_: String) -> (_: Shape)) }
  def convRule: Rule[ConvBuilder :: HNil, ConvBuilder :: HNil] =
    rule { (ruleCond | defaultCond) ~ modifiers ~> ConvRule ~> ((_: ConvBuilder) += (_: ConvRule)) }
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
  def classList = rule { optional(ws('[') ~ oneOrMore(identifier).separatedBy(ws(',')) ~ ws(']')) ~> ((_: Option[Seq[String]]).toSeq.flatten) }

  def styleMod: Rule1[StyleMod] = rule { styleName ~ ws('{') ~ styleRules ~ ws('}') ~> StyleMod }
  def styleName = rule { ws("style") ~ ws('[') ~ identifier ~ ws(']') }
  def styleRules = rule { zeroOrMore(cssIdent ~ ws(':') ~ cssValue ~> (_ -> _) ~ optional(ws(';'))) ~> ((_: Seq[(String, String)]).toMap) }
  def cssIdent = rule { capture(oneOrMore(CharPredicate.AlphaNum | ch('-'))) ~ ws }
  def cssValue = rule { capture(oneOrMore(noneOf(";{}"))) }

  def anoCond: Rule1[Option[RuleCond]] = rule { optional(anomer ~> AnoCond) }
  def absCond: Rule1[Option[RuleCond]] = rule { optional(absolute ~> AbsCond) }
  def resCond: Rule1[Option[RuleCond]] = rule { (residueTypeList ~> ResCond ~> ((rc: ResCond) => Some(rc): Option[ResCond])) | (ws('*') ~ push(None: Option[ResCond])) }
  def subCond: Rule1[Option[RuleCond]] = rule { optional(ws('<') ~ zeroOrMore(subMatcher).separatedBy(ws(',')) ~> SubCond ~ ws('>')) }
  def subMatcher: Rule1[(Option[Int], String)] = rule { optional(naturalNumber) ~ alphaString ~> ((_: Option[Int]) -> (_: String)) }

  def residueTypeList: Rule1[Seq[String]] = rule { ws('(') ~ oneOrMore(identifier).separatedBy(ws(',')) ~ ws(')') }

  def anomer: Rule1[Anomer] = rule { ano(Alpha, "a", "alpha", "α") | ano(Beta, "b", "beta", "ß") }
  def ano(anomer: Anomer, short: String, long: String, unicode: String) = rule {
    (ignoreCase(short.toLowerCase) | ignoreCase(long.toLowerCase) | unicode) ~ ws ~ push(anomer)
  }

  def absolute: Rule1[Absolute] = rule { abs(D, "d", "ᴅ") | abs(L, "l", "ʟ") }
  def abs(abs: Absolute, char: String, unicode: String) = rule {
    (ignoreCase(char) | unicode) ~ ws ~ push(abs)
  }

  def shape = rule { path | polygon | rect | circle | star | regularPolygon | definedShape }
  def namedShape(name: String) = rule { ws(name) ~ ws('(') ~ namedArgList ~ ws(')') }
  def path: Rule1[Shape] = rule { namedShape("Path") ~> ((map: Map[String, String]) => Path(map("d"))) }
  def polygon: Rule1[Shape] = rule { namedShape("Polygon") ~> ((map: Map[String, String]) => Polygon(map("points"))) }
  def rect: Rule1[Shape] = rule { namedShape("Rect") ~> Rect.fromMap }
  def circle: Rule1[Shape] = rule { namedShape("Circle") ~> Circle.fromMap }
  def star: Rule1[Shape] = rule { namedShape("Star") ~> Star.fromMap }
  def regularPolygon: Rule1[Shape] = rule { namedShape("RegularPolygon") ~> RegularPolygon.fromMap }
  def dummyShape: Rule1[DefinedShape] = rule { push(cursor) ~ ((identifier ~ ws('(') ~ namedArgList ~ drop[Map[String, String]] ~ ws(')')) | identifier) ~> DefinedShape }
  def definedShape: Rule1[DefinedShape] = rule { push(cursor) ~ identifier ~> DefinedShape }

  def namedArgList: Rule1[Map[String, String]] = rule { (zeroOrMore(namedArg) separatedBy ws(',')) ~> ((_: Seq[(String, String)]).toMap) }
  def namedArg: Rule1[(String, String)] = rule { (identifier ~ ws('=') ~ stringLiteral) ~> ((_: String) -> (_: String)) }

  def identifier: Rule1[String] = rule { capture(oneOrMore(CharPredicate.AlphaNum)) ~ ws }
  def stringLiteral: Rule1[String] = rule { '"' ~ stringLiteralInner ~ '"' ~ ws }
  def stringLiteralInner: Rule1[String] = rule { capture(zeroOrMore(noneOf("\"\r\n"))) }
  def intLiteral: Rule1[Int] = rule { capture(optional(ch('+')|'-') ~ oneOrMore(CharPredicate.Digit)) ~> ((_: String).toInt) ~ ws }

  def paletteRule: Rule[ConvBuilder :: HNil, ConvBuilder :: HNil] = rule { palette ~> ((_: ConvBuilder) += (_: Palette)) }
  def palette: Rule1[Palette] = rule { (ws("palette") ~ stringLiteral ~ ws('{') ~ zeroOrMore(paletteResidue).separatedBy(ws(',')) ~ ws ~ ws('}')) ~> (Palette.apply _) }
  def paletteResidue: Rule1[(ResidueType, Map[Int, Vector[SubstituentType]])] = rule { (paletteRT ~ subStacks) ~> ((_: ResidueType) -> (_: Map[Int, Vector[SubstituentType]])) }
  def paletteRT: Rule1[ResidueType] = rule { alphaString ~> ((s: String) => ResidueType.ResidueTypeMap(s)) }
  def alphaString: Rule1[String] = rule { capture(oneOrMore(CharPredicate.Alpha)) ~ ws }
  def subStacks: Rule1[Map[Int, Vector[SubstituentType]]] = rule { zeroOrMore(subStack) ~> ((_: Seq[(Int, Vector[SubstituentType])]).toMap) }
  def subStack: Rule1[(Int, Vector[SubstituentType])] = rule { (naturalNumber ~ ws ~ subStackParts) ~> ((_: Int) -> (_: Vector[SubstituentType])) }
  def subStackParts: Rule1[Vector[SubstituentType]] = rule { oneOrMore(alphaString) ~> ((_: Seq[String]).toVector.flatMap(p => ConventionParser.takeSubsPart(p))) }
  def naturalNumber: Rule1[Int] = rule { capture(CharPredicate.Digit19 ~ zeroOrMore(CharPredicate.Digit)) ~> ((_: String).toInt) ~ ws }
}

object ConventionParser {
  @tailrec def takeSubsPart(part: String, acc: Vector[SubstituentType] = Vector.empty): Vector[SubstituentType] =
    SubstituentType.SubstituentTypes.find(st => part.startsWith(st.symbol)) match {
      case Some(st) => takeSubsPart(part.drop(st.symbol.length), acc :+ st)
      case None => acc
    }
}