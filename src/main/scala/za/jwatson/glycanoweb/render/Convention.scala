package za.jwatson.glycanoweb.render

import importedjs.paper.Implicits._
import importedjs.paper._
import importedjs.{paper => p}
import org.scalajs.dom.SVGElement
import za.jwatson.glycanoweb.render.Convention.CanvasItemMod
import za.jwatson.glycanoweb.structure.Absolute.{D, L}
import za.jwatson.glycanoweb.structure.Anomer._
import za.jwatson.glycanoweb.structure._

import scala.scalajs.js
import scalaz.syntax.std.option._

class Convention(scope: p.PaperScope) {

  import importedjs.paper.{Point => P}

  def createIcon(rt: ResidueType, abs: Absolute, ano: Anomer, bounds: p.Rectangle): SVGElement = {
    val rs = ResidueShape(Residue.next(rt, ano, abs))
    val group = rs.group
    group.remove()

    val sx = bounds.width / group.bounds.width
    val sy = bounds.height / group.bounds.height
    val scale = sx min sy
    group.fitBounds(bounds)

    for(item <- group.getItems(null)) {
      item.strokeWidth *= scale
    }

    val svg = group.exportSVG()
    svg
  }

  def createIcon(st: SubstituentType, bounds: p.Rectangle): SVGElement = {
    val ss = SubstituentShape(Substituent.next(st))
    val group = ss.item
    group.remove()

    val sx = bounds.width / group.bounds.width
    val sy = bounds.height / group.bounds.height
    val scale = sx min sy
    group.fitBounds(bounds)

    for(item <- group.getItems(null)) {
      item.strokeWidth *= scale
    }

    val svg = group.exportSVG()
    svg
  }

  def closedPath(pts: p.Segment*): p.Path = {
    val path = new p.Path(js.Array[p.Segment](pts: _*))
    path.closePath()
    path
  }

  def closedPathScaled(scale: Double, pts: p.Segment*): p.Path = {
    val path = new p.Path(js.Array[p.Segment](pts: _*))
    path.closePath()
    path.scale(scale, scale)
    path
  }

  val triangle = closedPath(P(40, 0), P(0, 35), P(0, -35))
  val diamond = closedPath(P(80,0), P(40,40), P(0,0), P(40,-40))
  val arrow = closedPath(P(90,0), P(60,30), P(0,30), P(0,-30), P(60,-30))
  val hexagon = closedPath(P(90,0), P(65,40), P(25,40), P(0,0), P(25,-40), P(65,-40))

  val details = {
    import za.jwatson.glycanoweb.structure.ResidueType._
    Map[ResidueType, (p.Path, String, String, p.Path)](
      Glycero -> (triangle, "white", "white", new p.Path()),
      Erythro -> (diamond, "white", "white", new p.Path()),
      Threo -> (diamond, "black", "black", new p.Path()),
      Ara -> (arrow, "white", "black", closedPath(P(0,-30),P(0,30),P(30,30),P(30,-30))),
      Lyx -> (arrow, "black", "black", new p.Path()),
      Rib -> (arrow, "black", "white", closedPath(P(0,-30),P(0,0),P(90,0),P(60,-30))),
      Xyl -> (arrow, "#FFA0A0", "#FFA0A0", new p.Path()),
      Ido -> (hexagon, "black", "#BF6000", closedPath(P(25,-40), P(65,-40), P(25,40), P(65,40))),
      All -> (hexagon, "white", "black", closedPath(P(0,0), P(25,-40), P(65,40), P(25,40))),
      Alt -> (hexagon, "black", "white", closedPath(P(45,-40),P(65,-40),P(90,0),P(45,0))),
      Gal -> (hexagon, "yellow", "black", closedPath(P(0,0), P(90,0), P(65,40), P(25,40))),
      Glc -> (hexagon, "blue", "blue", new p.Path()),
      Gul -> (hexagon, "black", "white", closedPath(P(25,-40), P(65,-40), P(25,40), P(0,0), P(90,0), P(65,40))),
      Man -> (hexagon, "lime", "black", closedPath(P(0,0), P(25,-40), P(45,-40), P(45,40), P(25,40))),
      Tal -> (hexagon, "black", "black", new p.Path()),

      Rul -> (arrow, "white", "white", new p.Path()),
      Fru -> (hexagon, "white", "white", new p.Path())
    ) withDefault {
      case _ => (hexagon, "red", "red", new p.Path())
    }
  }

  val handleRect = new p.Rectangle(-10, -10, 20, 20)
  val handleBase: p.Path = p.Path.RoundRectangle(handleRect, new p.Size(5, 5))
  private val cw = new p.Color("white")
  private val cb = new p.Color("black")
  def gs(c: p.Color, n: js.Number) = new p.GradientStop(c, n)
  val gr = new p.Gradient(js.Array(
    gs(cw, 0),
    gs(cw, 0.5 - Double.MinPositiveValue),
    gs(cb, 0.5),
    gs(cb, 1)
  ), "")
  val halfGradientWB = new p.GradientColor(gr, P(-10, 0), P(10, 0), false)
  handleBase.strokeColor = "black"
  handleBase.remove()

//  val parser = new ConventionParser(ConventionEditor.testText)
//  val convs = parser.conventions.run()
//  val result = convs match {
//    case Success(c)             ⇒ "Expression is valid\n" + c.mkString("\n\n")
//    case Failure(e: ParseError) ⇒ "Expression is not valid: " + parser.formatError(e)
//    case Failure(e)             ⇒ "Unexpected error during parsing run: " + e
//  }
//  println(result)
//  val dcOpt = convs.getOrElse(Seq.empty).headOption.map(new DisplayConv(_))
//  val dc = dcOpt getOrElse new DisplayConv(Conv("Empty Conv"))

  case class ResidueShape(residue: Residue) {
//    val group = dc.group(residue)
//    val outline = group.getItem(js.Dynamic.literal(name = "outline": js.Any)).asInstanceOf[p.Path]
//    val handle = group.getItem(js.Dynamic.literal(name = "handle": js.Any)).asInstanceOf[p.Path]
//    if (outline == null) {
//      println(js.JSON.stringify(group, space = 2: js.Any))
//      println(residue)
//    }
//    handle.position = outline.firstSegment.point
    
    private val (o, b, f, d) = details(residue.rt)
    val (back, fill) = residue.absolute match {
      case D => (b, f)
      case L => (f, b)
    }
    val base = o.clonePath(insert = false)
    val detail = d.clonePath(insert = false)
    val outline = o.clonePath(insert = false)
    val handle = handleBase.clonePath(insert = false)
    handle.fillColor = residue.anomer match {
      case Alpha => new p.Color("white")
      case Beta => new p.Color("black")
      //case Unknown => halfGradientWB
    }
    handle.name = "handle"
    handle.position = outline.firstSegment.point
    val group = residue.rt.category match {
      case ResidueCategory.Ketose =>
        val border1 = o.clonePath(insert = false)
        val border2 = o.clonePath(insert = false)
        border1.strokeColor = new p.Color("black")
        border1.strokeWidth = 10.5
        border2.strokeColor = new p.Color("white")
        border2.strokeWidth = 8.5
        new p.Group(js.Array(base, detail, outline, border1, border2, handle))
      case _ =>
        new p.Group(js.Array(base, detail, outline, handle))
    }
    if(residue.absolute == L) {
      val text = new p.PointText(P(0,0))
      text.content = "L"
      text.fillColor = new p.Color("black")
      text.strokeColor = new p.Color("white")
      text.strokeWidth = 1
      text.asInstanceOf[js.Dynamic].fontSize = 40
      text.asInstanceOf[js.Dynamic].fontWeight = "900"
      //        val x = text.point.x - text.bounds.width / 2.0
      //        val y = text.point.y + (20 / 1.2)
      //        text.point = P(x, y)
      text.position = outline.bounds.center
      group.addChild(text)
    }
    outline.strokeColor = "black"
    outline.strokeWidth = 3
    base.fillColor = new p.Color(back)
    detail.fillColor = new p.Color(fill)

    group.transformContent = false
    //outline.curves = js.Array[p.Curve]()
  }

  def pulseTime = 0.8

  implicit class PulseItem(item: p.Item) {
    def pulseDuration_=(t: Double): Unit =
      item.asInstanceOf[js.Dynamic].pulseDuration = t
    def pulseDuration: Double =
      item.asInstanceOf[js.Dynamic].pulseDuration.asInstanceOf[js.UndefOr[Double]].getOrElse(0.0)
  }

//  val substBacks = {
//    import za.jwatson.glycanoweb.structure.{SubstituentType => ST}
//    Map(
//      ST.n -> p.Path.RoundRectangle(new p.Rectangle(-10, -15, 20, 30), new p.Size(5, 5)),
//      ST.cooh -> p.Path.Circle(P(0, 0), 10),
//      ST.methyl -> p.Path.Rectangle(new p.Rectangle(-5, -15, 10, 30)),
//      ST.deoxy -> p.Path.Circle(P(0, 0), 8),
//      ST.s -> p.Path.Circle(P(0, 0), 15),
//      ST.p -> p.Path.Circle(P(0, 0), 15),
//      ST.ac -> closedPath(P(0, 20), P(-20, 20), P(20, 20))
//    )
//  }

  //val substs = SubstituentType.substituentTypes.map(st => st -> subst(st)).toMap

  for ((a, _, _, b) <- details.values) {
    a.remove()
    b.remove()
  }
  //for (a <- substs.values) a.remove()

  def subst(st: SubstituentType): p.Item = {
    import za.jwatson.glycanoweb.structure.{SubstituentType => ST}
    def pt(s: Int, c: String, t: String) = {
      val d = new p.PointText(P(0, 0))
      d.fontSize = s
      d.fillColor = new p.Color(c)
      d.content = t
      d.position = P(0, 0)
      d
    }
    def rrb(db: p.Rectangle, s: Double, r: Double, fill: String, stroke: String) = {
      val rect = new p.Rectangle(db.x - s, db.y - s, db.width + s * 2, db.height + s * 2)
      val b = p.Path.RoundRectangle(rect, new p.Size(r, r))
      b.fillColor = new p.Color(fill)
      b.strokeColor = stroke
      b
    }
    def cb(db: p.Rectangle, s: Double, fill: String, stroke: String) = {
      val b = p.Path.Circle(db.center, math.max(db.width, db.height) / 2 + s)
      b.fillColor = new p.Color(fill)
      b.strokeColor = stroke
      b
    }
    def tb(db: p.Rectangle, fill: String, stroke: String) = {
      val c = db.center
      val b = closedPath(P(c.x, c.y - 30), P(c.x - 25, c.y + 15), P(c.x + 25, c.y + 15))
      b.fillColor = new p.Color(fill)
      b.strokeColor = stroke
      b
    }
    val d = st match {
      case ST.n => pt(30, "black", "N")
      case ST.cooh =>
        val path = new p.Path("""M 0,14.355469 2.2460938,7.421875 C 7.4218645,9.2448552 11.181626,10.82363 13.525391,12.158203 12.906885,6.2663426 12.581365,2.2136123 12.548828,0 l 7.080078,0 c -0.09768,3.2227258 -0.472027,7.2591801 -1.123047,12.109375 3.35284,-1.692646 7.193982,-3.2551444 11.523438,-4.6875 l 2.246094,6.933594 c -4.134146,1.367244 -8.186877,2.278702 -12.158204,2.734375 1.985652,1.725314 4.785129,4.801483 8.398438,9.228515 L 22.65625,30.46875 C 20.768205,27.89718 18.53839,24.397835 15.966797,19.970703 13.557926,24.560595 11.442043,28.059941 9.6191406,30.46875 L 3.8574219,26.318359 C 7.6334528,21.663463 10.335273,18.587294 11.962891,17.089844 7.763661,16.276098 3.7760348,15.364641 0,14.355469""")
        path.fillColor = new p.Color("black")
        path
      case ST.methyl =>
        //val path1 = closedPathScaled(scale = 22, P(0, 0), P(-0.25, 1), new p.Segment(P(0.25, 1), P(-0.375, 1.5), P(0.375, 1.5)))
        val path = new p.Path("""M0,0 L-0.25,1 C-0.375,1.5 0.375,1.5 0.25,1 Z""")
        path.fillColor = new p.Color("black")
        path.scale(22, 22)
        path
      case ST.deoxy =>
        val path = new p.Path("""M 476.82,418.45 L 486.73,428.41 C 487.28,428.96 487.38,429.06 487.38,429.46 C 487.38,430.01 486.93,430.46 486.39,430.46 C 485.99,430.46 485.79,430.26 485.34,429.81 L 475.38,419.85 L 465.36,429.81 C 464.82,430.36 464.72,430.46 464.32,430.46 C 463.82,430.46 463.32,430.01 463.32,429.46 C 463.32,429.06 463.52,428.86 463.97,428.41 L 473.88,418.45 L 463.97,408.54 C 463.47,408.04 463.32,407.74 463.32,407.45 C 463.32,406.9 463.82,406.45 464.32,406.45 C 464.72,406.45 464.82,406.55 465.36,407.1 L 475.33,417.06 L 485.29,407.1 C 485.79,406.6 486.09,406.45 486.39,406.45 C 486.98,406.45 487.38,406.9 487.38,407.45 C 487.38,407.84 487.28,407.94 486.73,408.49 L 476.82,418.45 z """)
        path.fillColor = new p.Color("black")
//        path.strokeWidth = 12
//        path.strokeCap = "square"
        path
      case ST.s => pt(30, "black", "S")
      case ST.p => pt(30, "white", "P")
      case ST.ac => pt(24, "black", "Ac")
    }
    val db = d.strokeBounds
    val b = st match {
      case ST.n => rrb(db, 5, 5, fill = "#86CEFF", stroke = "black")
      case ST.cooh => cb(db, 5, fill = "white", stroke = "")
      case ST.methyl => rrb(db, 5, 5, fill = "white", stroke = "")
      case ST.deoxy => cb(db, 6, fill = "white", stroke = "black")
      case ST.s => cb(db, 5, fill = "#FFFF00", stroke = "black")
      case ST.p => cb(db, 5, fill = "#8E008E", stroke = "black")
      case ST.ac => tb(db, fill = "white", stroke = "black")
    }
    new p.Group(js.Array[p.Item](b, d))
  }

  case class SubstituentShape(s: Substituent) {
    val item = subst(s.st)
    item.scale(0.7, 0.7)
  }

  val itemResidueShapes = collection.mutable.Map[js.Number, ResidueShape]()
  val residueResidueShapes = collection.mutable.Map[Residue, ResidueShape]()

  val itemSubstShapes = collection.mutable.Map[js.Number, SubstituentShape]()
  val substSubstShapes = collection.mutable.Map[Substituent, SubstituentShape]()

  def items = residueResidueShapes.values.map(_.group)

  implicit class RichResidueRS(residue: Residue) {
    def residueShape: ResidueShape = residueResidueShapes(residue)
    def outline: p.Path = residueShape.outline
    def handle: p.Path = residueShape.handle
    def group: p.Group = residueShape.group

    def getResidueShape: Option[ResidueShape] = residueResidueShapes.get(residue)
    def getOutline: Option[p.Path] = getResidueShape.map(_.outline)
    def getHandle: Option[p.Path] = getResidueShape.map(_.handle)
    def getGroup: Option[p.Group] = getResidueShape.map(_.group)
  }
  implicit class RichSubstituentRS(substituent: Substituent) {
    def substituentShape: SubstituentShape = substSubstShapes(substituent)
    def group: p.Item = substituentShape.item

    def getSubstituentShape: Option[SubstituentShape] = substSubstShapes.get(substituent)
    def getGroup: Option[p.Item] = getSubstituentShape.map(_.item)
  }
  implicit class RichItemRS(item: p.Item) {
    def residueShape: ResidueShape = itemResidueShapes(item.id)
    def residue: Residue = residueShape.residue

    def substituentShape: SubstituentShape = itemSubstShapes(item.id)
    def substituent: Substituent = substituentShape.s

    def getResidueShape: Option[ResidueShape] = itemResidueShapes.get(item.id)
    def getResidue: Option[Residue] = getResidueShape.map(_.residue)
  }

  def getResidue(item: Item): Option[Residue] = item.getResidue
  def getItem(residue: Residue): Option[Item] = residue.getGroup
  def getItem(substituent: Substituent): Option[Item] = substituent.getGroup

  val bonds = collection.mutable.Map[js.Number, Residue]()
  val bondItems = collection.mutable.Map[Residue, p.Path]()

  def addResidue(r: Residue, pos: p.Point, rot: Double = 0.0): Unit = {
    val rs = ResidueShape(r)
    residueResidueShapes(r) = rs
    itemResidueShapes(rs.group.id) = rs
    for(item <- rs.group.children) {
      itemResidueShapes(item.id) = rs
    }
    rs.group.position = pos
    rs.group.rotate(rot, rs.group.bounds.center)
  }

  def removeResidue(residue: Residue): Unit = {
    val rs = residueResidueShapes(residue)
    for (item <- rs.group.children) {
      itemSubstShapes.remove(item.id)
    }
    itemResidueShapes.remove(rs.group.id)
    residueResidueShapes.remove(residue)
    rs.group.remove()
  }

  def addSubstituent(subst: Substituent, pos: p.Point, mid: Boolean): Unit = {
    val ss = SubstituentShape(subst)
    substSubstShapes(subst) = ss
    itemSubstShapes(ss.item.id) = ss
    for (item <- ss.item.children) {
      itemSubstShapes(item.id) = ss
    }
    ss.item.position = if(mid) pos else pos.add(0, ss.item.bounds.height / 2)
  }

  def removeSubstituent(subst: Substituent): Unit = {
    val ss = substSubstShapes(subst)
    for (item <- ss.item.children) {
      itemSubstShapes.remove(item.id)
    }
    itemSubstShapes.remove(ss.item.id)
    substSubstShapes.remove(subst)
    ss.item.remove()
  }

  lazy val x = {
    val x = new p.Path("""M 476.82,418.45 L 486.73,428.41 C 487.28,428.96 487.38,429.06 487.38,429.46 C 487.38,430.01 486.93,430.46 486.39,430.46 C 485.99,430.46 485.79,430.26 485.34,429.81 L 475.38,419.85 L 465.36,429.81 C 464.82,430.36 464.72,430.46 464.32,430.46 C 463.82,430.46 463.32,430.01 463.32,429.46 C 463.32,429.06 463.52,428.86 463.97,428.41 L 473.88,418.45 L 463.97,408.54 C 463.47,408.04 463.32,407.74 463.32,407.45 C 463.32,406.9 463.82,406.45 464.32,406.45 C 464.72,406.45 464.82,406.55 465.36,407.1 L 475.33,417.06 L 485.29,407.1 C 485.79,406.6 486.09,406.45 486.39,406.45 C 486.98,406.45 487.38,406.9 487.38,407.45 C 487.38,407.84 487.28,407.94 486.73,408.49 L 476.82,418.45 z """)
    x.scale(0.45, 0.45)
    x.fillColor = new p.Color(0.2, 0.2, 0.2)
    x.name = "delete"
    x.strokeColor = new p.Color(0.2, 0.2, 0.2)
    x.strokeWidth = 4
    x.remove()
    x
  }

  def highlightResidue(residue: Residue): Unit = {
    //val hl = residue.outline.clonePath()
    val rc = residue.outline.strokeBounds
    val rct = new p.Rectangle(rc.x - 5, rc.y - 5, rc.width + 10, rc.height + 10)
    val hl = p.Path.RoundRectangle(rct, new p.Size(5, 5))
    hl.strokeWidth = 1
    hl.strokeColor = new p.Color(0, 0, 0, 1)
    hl.fillColor = new p.Color(0.5, 0.5, 1, 0.5)

    val box = p.Path.RoundRectangle(new p.Rectangle(hl.bounds.topRight.subtract(10, 10), new p.Size(20, 20)), new p.Size(6, 6))
    box.name = "delete"
    box.fillColor = new p.Color("white")
    box.strokeColor = new p.Color("black")
    box.strokeWidth = 2

    val x2 = x.clonePath()
    x2.position = hl.bounds.topRight

    val rotHeight = 30
    val rotRadius = 10

    val rotHandle = p.Path.Circle(hl.bounds.topCenter.subtract(0, rotHeight), rotRadius)
    rotHandle.name = "rotate"
    rotHandle.fillColor = "grey"
    rotHandle.strokeColor = "black"
    rotHandle.strokeWidth = 1

    val rotConnector = p.Path.Line(hl.bounds.topCenter, rotHandle.position)
    rotConnector.strokeColor = "black"
    rotConnector.strokeWidth = 1.5

    val gr = new p.Group(js.Array(hl, box, x2, rotConnector, rotHandle))
    gr.name = "highlight"
    residue.group.insertChild(0, gr)
    //residue.group.addChild(hl)

    itemResidueShapes(rotHandle.id) = residue.residueShape
    itemResidueShapes(x2.id) = residue.residueShape
  }

  def unhighlightResidue(residue: Residue): Unit = {
    val hls = residue.group getItems js.Dynamic.literal(name = "highlight": js.Any)
    hls foreach (_.remove())
  }

  def addBond(from: Residue, to: Residue, i: Int): Unit = {

    val item = new p.Path()
    item.strokeColor = "black"
    item.strokeWidth = 7
    if(from.anomer == Beta) {
      item.dashArray = js.Array(15, 10)
    }
    item.project.layers(0) addChild item
    bonds(item.id) = from
    bondItems(from) = item
    updateBond(from, to, i)
  }

  def removeBond(r: Residue): Unit = {
    for(item <- bondItems.get(r)) {
      item.remove()
      bondItems.remove(r)
      bonds.remove(item.id)
    }
  }

  def updateBond(from: Residue, to: Residue, i: Int): Unit = {
    for(path <- bondItems.get(from)) {
      path.removeSegments()
      path.add(from.group.localToGlobal(from.outline.segments(0).point))
      path.add(to.group.localToGlobal(to.outline.segments(i - 1).point))
    }
  }

  def linkValid(from: p.Point, to: p.Point, parent: Boolean, angle: Double): Boolean = {
    onRightOf(from, to, angle) == parent
  }

  def onRightOf(from: p.Point, to: p.Point, angle: Double): Boolean = {
    p.Point(1, 0).rotate(angle, p.Point(0, 0)).dot(to subtract from) > 0
  }

  def getClosestLinkAnyFilter(resShapes: Iterable[ResidueShape] = residueResidueShapes.values, linkFilter: Link => Boolean, from: p.Point, parent: Option[Boolean] = None, angle: Double = 0, threshold: Double = 50 * 50): Option[Link] = {
    val points = for {
      residueShape <- resShapes
      segment <- residueShape.outline.segments.toSeq
      link <- getLink(segment)
      if linkFilter(link)
      segPt = residueShape.group.localToGlobal(segment.point)
      if parent.fold(true)(linkValid(from, segPt, _, angle))
      distsq = segPt.getDistance(from, squared = true)
      if distsq < threshold
    } yield (link, distsq)

    if(points.nonEmpty) Some(points.minBy(_._2.doubleValue())._1) else None
  }

  def getClosestLinkAnyFilterFrom(resShapes: Iterable[Residue], linkFilter: Link => Boolean, from: p.Point, parent: Option[Boolean] = None, angle: Double = 0, threshold: Double = 50 * 50): Option[Link] =
  getClosestLinkAnyFilter(resShapes.map(_.residueShape), linkFilter, from, parent, angle, threshold)


  def getClosestLinkAnyFrom(residues: Iterable[Residue], from: p.Point, parent: Option[Boolean] = None, angle: Double = 0, threshold: Double = 50 * 50): Option[Link] =
    getClosestLinkAnyFilter(residues.map(_.residueShape), _ => true, from, parent, angle, threshold)

  def getClosestLinkAny(from: p.Point, parent: Option[Boolean] = None, angle: Double = 0, threshold: Double = 50 * 50): Option[Link] =
    getClosestLinkAnyFilter(residueResidueShapes.values, _ => true, from, parent, angle, threshold)

  def getClosestLink(from: p.Point, to: Residue, parent: Boolean, angle: Double = 0): Option[Link] = {
    val points = for {
      residueShape <- residueResidueShapes.get(to).toSeq
      segment <- residueShape.outline.segments.toSeq
      segPt = residueShape.group.localToGlobal(segment.point)
      if linkValid(from, segPt, parent, angle)
      distsq = segPt.getDistance(from, squared = true)
      if distsq < threshold
    } yield (segment, distsq)

    val segment = if(points.nonEmpty) Some(points.minBy[Double](_._2)._1) else None

    segment flatMap getLink
  }

  def getLink(segment: p.Segment): Option[Link] = {
    val residue = segment.path.parent.getResidue
    residue.map(Link(_, segment.index.intValue() + 1))
  }

  def linkPosition(link: Link): p.Point = {
    link.residue.group.localToGlobal(link.residue.outline.segments(link.position - 1).point)
  }

  val threshold = 50 * 50

  var selectionBox: Option[p.Path] = None

  def drawBoxSelection(box: Option[Rectangle]): Unit = {
    selectionBox.map(_.remove())
    selectionBox = box.map { rect =>
      val sb = p.Path.Rectangle(rect)
      sb.fillColor = new p.Color(0.5, 0.5, 1.0, 0.5)
      sb.strokeColor = "black"
      sb
    }
  }

  def testSelection(rect: Rectangle, residue: Residue): Boolean = {
    rect.contains(residue.group.position)
  }

  var residueTemplate: Option[(p.Item, p.Path)] = None

  def showResidueTemplate(residue: Option[Residue], pos: p.Point): Unit = {
    residue.fold {
      for ((item, _) <- residueTemplate) item.remove()
      residueTemplate = None
    } { r =>
      if (residueTemplate.isEmpty) {
        val rs = ResidueShape(r)
        residueTemplate = (rs.group, rs.outline).some
      }
      for ((item, _) <- residueTemplate) item.position = pos
    }
  }

  def residueTemplateItem(): Option[p.Item] = {
    residueTemplate.map(_._1)
  }

  def residueTemplateLinkPosition(i: Int): Option[p.Point] = {
    residueTemplate.map(rtmp => rtmp._1.localToGlobal(rtmp._2.segments(i - 1).point))
  }

  var substituentTemplate: Option[p.Item] = None

  def showSubstituentTemplate(stOpt: Option[SubstituentType], pos: p.Point, mid: Boolean = true): Unit = {
    stOpt.fold {
      for (item <- substituentTemplate) item.remove()
      substituentTemplate = None
    } { st =>
      if (substituentTemplate.isEmpty)
        substituentTemplate = SubstituentShape(Substituent.next(st)).item.some
      for (item <- substituentTemplate) {
        item.position = if(mid) pos else pos.add(0, item.bounds.height / 2)
      }
    }
  }

  def hitHandle(residue: Residue, point: P): Boolean = {
    residue.getHandle.exists(_.contains(residue.group.globalToLocal(point)))
  }

  def finishBond(from: Link, to: p.Point): Option[Link] = {
    getClosestLinkAny(to, parent = Some(true))
  }

  val handleHL: CanvasItemMod[p.Path, Residue] = new CanvasItemMod[p.Path, Residue] {
    override def create(r: Residue): p.Path = r.handle
    override def update(h: p.Path, r: Residue): Unit = {h.strokeColor = "blue"}
    override def revert(h: p.Path, r: Residue): Unit = {h.strokeColor = "black"}
  }

  val handlePress: CanvasItemMod[p.Path, Residue] = new CanvasItemMod[p.Path, Residue] {
    override def create(r: Residue): p.Path = r.handle
    override def update(h: p.Path, r: Residue): Unit = {h.strokeWidth = 2}
    override def revert(h: p.Path, r: Residue): Unit = {h.strokeWidth = 1}
  }
}

object Convention {
  trait CanvasItemMod[T, S] {
    def update(t: T, s: S): Unit
    def create(s: S): T
    def revert(t: T, s: S): Unit

    var item: Option[T] = None
    var itemSource: Option[S] = None

    def apply(source: Option[S]): Unit = {
      for (t <- item) {
        revert(t, itemSource.get)
        item = None
      }
      for(s <- source) {
        item = Some(create(s))
        update(item.get, s)
      }
//      source match {
//        case Some(s) =>
//          if(source != itemSource) {
//            for (t <- item) revert(t, s)
//            item = Some(create(s))
//          }
//          update(item.get, s)
//        case None =>
//          for(t <- item) {
//            revert(t, itemSource.get)
//            item = None
//          }
//      }
      itemSource = source
    }
  }
}