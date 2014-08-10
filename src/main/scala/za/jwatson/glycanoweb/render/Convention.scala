package za.jwatson.glycanoweb.render

import importedjs.paper.Implicits._
import importedjs.paper._
import importedjs.{paper => p}
import org.scalajs.dom.SVGElement
import za.jwatson.glycanoweb.render.Convention.CanvasItemMod
import za.jwatson.glycanoweb.structure.Absolute.{D, L}
import za.jwatson.glycanoweb.structure.Anomer.{Alpha, Beta}
import za.jwatson.glycanoweb.structure.Residue.Link
import za.jwatson.glycanoweb.structure._

import scala.scalajs.js
import scalaz.Alpha.P

class Convention(scope: p.PaperScope) {

  import importedjs.paper.{Point => P}

  def createIcon(rt: ResidueType, abs: Absolute, ano: Anomer, bounds: p.Rectangle): SVGElement = {
    val rs = ResidueShape(Residue(rt, ano, abs))
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
    val ss = SubstituentShape(st)
    val group = ss.sub
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
      Rib -> (arrow, "black", "white", closedPath(P(0,-30),P(0,30),P(30,30),P(30,-30))),
      Xyl -> (arrow, "#FFA0A0", "#FFA0A0", new p.Path()),
      Ido -> (hexagon, "black", "#BF6000", closedPath(P(25,-40), P(65,-40), P(25,40), P(65,40))),
      All -> (hexagon, "white", "black", closedPath(P(0,0), P(25,-40), P(65,40), P(25,40))),
      Alt -> (hexagon, "black", "white", closedPath(P(45,-40),P(65,-40),P(90,0),P(45,0))),
      Gal -> (hexagon, "yellow", "black", closedPath(P(0,0), P(90,0), P(65,40), P(25,40))),
      Glc -> (hexagon, "blue", "blue", new p.Path()),
      Gul -> (hexagon, "black", "white", closedPath(P(25,-40), P(65,-40), P(25,40), P(0,0), P(90,0), P(65,40))),
      Man -> (hexagon, "lime", "black", closedPath(P(0,0), P(25,-40), P(45,-40), P(45,40), P(25,40))),
      Tal -> (hexagon, "black", "black", new p.Path()))
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

  case class ResidueShape(residue: Residue) {
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
    val group = new p.Group(js.Array(base, detail, outline, handle))
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

    //outline.curves = js.Array[p.Curve]()
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

  val substs = SubstituentType.substituentTypes.map(st => st -> subst(st)).toMap

  for ((a, _, _, b) <- details.values) {
    a.remove()
    b.remove()
  }
  for (a <- substs.values) a.remove()

  def subst(st: SubstituentType): p.Item = {
    import za.jwatson.glycanoweb.structure.{SubstituentType => ST}
    def pt(s: Int, c: String, t: String) = {
      val d = new p.PointText(new p.Point())
      d.fontSize = s
      d.fillColorString = c
      d.content = t
      d.position = P(0, 0)
      d
    }
    def rrb(i: p.Item, s: Double, r: Double, fill: String, stroke: String) = {
      val db = i.bounds
      val rect = new p.Rectangle(db.x - s, db.y - s, db.width + s * 2, db.height + s * 2)
      val b = p.Path.RoundRectangle(rect, new p.Size(r, r))
      b.fillColorString = fill
      b.strokeColor = stroke
      b
    }
    def cb(i: p.Item, s: Double, fill: String, stroke: String) = {
      val db = i.bounds
      val b = p.Path.Circle(i.bounds.center, math.max(db.width, db.height) / 2 + s)
      b.fillColorString = fill
      b.strokeColor = stroke
      b
    }
    def tb(i: p.Item, fill: String, stroke: String) = {
      val c = i.bounds.center
      val b = closedPath(P(c.x, c.y + 20), P(c.x - 20, c.y + 20), P(c.x + 20, c.y + 20))
      b.fillColorString = fill
      b.strokeColor = stroke
      b
    }
    val d = st match {
      case ST.n => pt(30, "black", "N")
      case ST.cooh => pt(60, "black", "*")
      case ST.methyl => closedPathScaled(scale = 22, P(0, 0), P(-0.25, 1), new p.Segment(P(0.25, 1), P(-0.375, 1.5), P(0.375, 1.5)))
      case ST.deoxy => pt(30, "black", "×")
      case ST.s => pt(30, "black", "S")
      case ST.p => pt(30, "white", "P")
      case ST.ac => pt(24, "black", "Ac")
    }
    val b = st match {
      case ST.n => rrb(d, 5, 5, fill = "#86CEFF", stroke = "black")
      case ST.cooh => cb(d, 5, fill = "white", stroke = "")
      case ST.methyl => rrb(d, 5, 5, fill = "white", stroke = "")
      case ST.deoxy => cb(d, 2, fill = "white", stroke = "black")
      case ST.s => cb(d, 5, fill = "#FFFF00", stroke = "black")
      case ST.p => cb(d, 5, fill = "#8E008E", stroke = "black")
      case ST.ac => tb(d, "white", "black")
    }
    new p.Group(js.Array(b, d))
  }

  case class SubstituentShape(st: SubstituentType) {
    val sub = substs(st).cloneItem()
  }

  val itemResidueShapes = collection.mutable.Map[js.Number, ResidueShape]()
  val residueResidueShapes = collection.mutable.Map[Residue, ResidueShape]()

  def items = residueResidueShapes.values.map(_.group)

  implicit class RichResidue(residue: Residue) {
    def residueShape: ResidueShape = residueResidueShapes(residue)
    def base: p.Path = residueShape.base
    def detail: p.Path = residueShape.detail
    def outline: p.Path = residueShape.outline
    def handle: p.Path = residueShape.handle
    def group: p.Group = residueShape.group

    def getResidueShape: Option[ResidueShape] = residueResidueShapes.get(residue)
    def getBase: Option[p.Path] = getResidueShape.map(_.base)
    def getDetail: Option[p.Path] = getResidueShape.map(_.detail)
    def getOutline: Option[p.Path] = getResidueShape.map(_.outline)
    def getHandle: Option[p.Path] = getResidueShape.map(_.handle)
    def getGroup: Option[p.Group] = getResidueShape.map(_.group)
  }
  implicit class RichItem(item: p.Item) {
    def residueShape: ResidueShape = itemResidueShapes(item.id)
    def residue: Residue = residueShape.residue

    def getResidueShape: Option[ResidueShape] = itemResidueShapes.get(item.id)
    def getResidue: Option[Residue] = getResidueShape.map(_.residue)
  }

  def getResidue(item: Item): Option[Residue] = item.getResidue
  def getItem(residue: Residue): Option[Item] = residue.getGroup

  val bonds = collection.mutable.Map[js.Number, Residue]()
  val bondItems = collection.mutable.Map[Residue, p.Path]()

  def addResidue(r: Residue, pos: p.Point): Unit = {
    val rs = ResidueShape(r)
    residueResidueShapes(r) = rs
    itemResidueShapes(rs.group.id) = rs
    for(item <- rs.group.children) {
      itemResidueShapes(item.id) = rs
    }
    rs.group.position = pos
  }

  def removeResidue(residue: Residue): Unit = {
    val rs = residueResidueShapes(residue)
    itemResidueShapes.remove(rs.group.id)
    itemResidueShapes.remove(rs.handle.id)
    itemResidueShapes.remove(rs.outline.id)
    itemResidueShapes.remove(rs.base.id)
    itemResidueShapes.remove(rs.detail.id)
    residueResidueShapes.remove(residue)
    rs.group.remove()
  }

  def highlightResidue(residue: Residue): Unit = {
    //val hl = residue.outline.clonePath()
    val rc = residue.outline.strokeBounds
    val rct = new p.Rectangle(rc.x - 5, rc.y - 5, rc.width + 10, rc.height + 10)
    val hl = p.Path.RoundRectangle(rct, new p.Size(5, 5))
    hl.strokeWidth = 1
    hl.name = "highlight"
    hl.strokeColor = new p.Color(0, 0, 0, 1)
    hl.fillColor = new p.Color(0.5, 0.5, 1, 0.5)
    residue.group.insertChild(0, hl)
    //residue.group.addChild(hl)
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
      path.add(from.outline.segments(0).point)
      path.add(to.outline.segments(i - 1).point)
    }
  }

  def linkValid(from: p.Point, to: p.Point, parent: Boolean, angle: Double): Boolean = {
    onRightOf(from, to, angle) == parent
  }

  def onRightOf(from: p.Point, to: p.Point, angle: Double): Boolean = {
    p.Point(1, 0).rotate(angle, p.Point(0, 0)).dot(to subtract from) > 0
  }

  def getClosestLinkAny(from: p.Point, parent: Option[Boolean] = None, angle: Double = 0, threshold: Double = 50 * 50): Option[Link] = {
    val points = for {
      residueShape <- residueResidueShapes.values
      segment <- residueShape.outline.segments.toSeq
      if parent.fold(true)(linkValid(from, segment.point, _, angle))
      distsq = segment.point.getDistance(from, squared = true)
      if distsq < threshold
    } yield (segment, distsq)

    val segment = if(points.nonEmpty) Some(points.minBy(_._2.doubleValue())._1) else None

    segment flatMap getLink
  }

  def getClosestLink(from: p.Point, to: Residue, parent: Boolean, angle: Double = 0): Option[Link] = {
    val points = for {
      residueShape <- residueResidueShapes.get(to).toSeq
      segment <- residueShape.outline.segments.toSeq
      if linkValid(from, segment.point, parent, angle)
      distsq = segment.point.getDistance(from, squared = true)
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
    link.residue.outline.segments(link.position - 1).point
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
    residue match {
      case Some(r) =>
        if(residueTemplate.isEmpty) {
          val rs = ResidueShape(r)
          residueTemplate = Some((rs.group, rs.outline))
        }
        residueTemplate.map(_._1.position = pos)
      case None =>
        residueTemplate.map(_._1.remove())
        residueTemplate = None
    }
  }

  def residueTemplateItem(): Option[p.Item] = {
    residueTemplate.map(_._1)
  }

  def residueTemplateLinkPosition(i: Int): Option[p.Point] = {
    residueTemplate.map(_._2.segments(i - 1).point)
  }

  def hitHandle(residue: Residue, point: P): Boolean = {
    residue.getHandle.exists(_.contains(point))
  }

  def finishBond(from: Link, to: p.Point): Option[Link] = {
    getClosestLinkAny(to, parent = Some(true))
  }

  val handleHL = new CanvasItemMod[p.Path, Residue] {
    override def create(r: Residue): p.Path = r.handle
    override def update(h: p.Path, r: Residue) = h.strokeColor = "blue"
    override def revert(h: p.Path, r: Residue) = h.strokeColor = "black"
  }

  val handlePress = new CanvasItemMod[p.Path, Residue] {
    override def create(r: Residue): p.Path = r.handle
    override def update(h: p.Path, r: Residue) = h.strokeWidth = 2
    override def revert(h: p.Path, r: Residue) = h.strokeWidth = 1
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