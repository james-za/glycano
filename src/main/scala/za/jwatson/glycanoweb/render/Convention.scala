package za.jwatson.glycanoweb.render

import importedjs.paper.Implicits._
import importedjs.{paper => p}
import org.parboiled2.ParseError
import org.scalajs.dom.SVGElement
import za.jwatson.glycanoweb.{GlycanoWeb, ConventionEditor}
import za.jwatson.glycanoweb.ConventionEditor.{Conv, ConventionParser}
import za.jwatson.glycanoweb.render.Convention.CanvasItemMod
import za.jwatson.glycanoweb.structure.Absolute.{D, L}
import za.jwatson.glycanoweb.structure.Anomer._
import za.jwatson.glycanoweb.structure._
import za.jwatson.glycanoweb.structure.RGraph._

import scala.scalajs.js
import scala.util.{Failure, Success}
import scalaz.syntax.std.option._

class Convention(scope: p.PaperScope) {
  def dc = GlycanoWeb.displayConv()

  import importedjs.paper.{Point => P}

  def createIcon(rt: ResidueType, abs: Absolute, ano: Anomer, bounds: p.Rectangle): SVGElement = {
    val rs = ResidueShape(Residue.next(rt, ano, abs), dc)
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

  //val dc = DisplayConv.convUCT
  //println(dc.conv)


  def pulseTime = 0.8

  implicit class PulseItem(item: p.Item) {
    def pulseDuration_=(t: Double): Unit =
      item.asInstanceOf[js.Dynamic].pulseDuration = t
    def pulseDuration: Double =
      item.asInstanceOf[js.Dynamic].pulseDuration.asInstanceOf[js.UndefOr[Double]].getOrElse(0.0)
  }

  val itemResidueShapes = collection.mutable.Map[js.Number, ResidueShape]()
  val residueResidueShapes = collection.mutable.Map[Residue, ResidueShape]()

  val itemSubstShapes = collection.mutable.Map[js.Number, SubstituentShape]()
  val substSubstShapes = collection.mutable.Map[Substituent, SubstituentShape]()

  def items = residueResidueShapes.values.map(_.group)

  implicit class RichResidueRS(residue: Residue) {
    def residueShape: ResidueShape = residueResidueShapes(residue)
    def outline: p.Path = residueShape.outline
    def handle: p.Path = residueShape.handle.get
    def group: p.Group = residueShape.group

    def getResidueShape: Option[ResidueShape] = residueResidueShapes.get(residue)
    def getOutline: Option[p.Path] = getResidueShape.map(_.outline)
    def getHandle: Option[p.Path] = getResidueShape.flatMap(_.handle)
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

  def getResidue(item: p.Item): Option[Residue] = item.getResidue
  def getItem(residue: Residue): Option[p.Item] = residue.getGroup
  def getItem(substituent: Substituent): Option[p.Item] = substituent.getGroup

  val bonds = collection.mutable.Map[js.Number, Residue]()
  val bondItems = collection.mutable.Map[Residue, p.Path]()

  def addResidue(r: Residue, pos: p.Point, rot: Double = 0.0, subs: Map[Int, Vector[Substituent]] = Map.empty): Unit = {
    val rs = ResidueShape(r, dc, subs)
    residueResidueShapes(r) = rs
    itemResidueShapes(rs.group.id) = rs
    for(item <- rs.group.children) {
      itemResidueShapes(item.id) = rs
    }
    rs.group.position = pos
    rs.group.rotate(rot, rs.group.bounds.center)
  }

  def removeResidue(residue: Residue): Unit = {
    for (rs <- residueResidueShapes.get(residue)) {
      for (item <- rs.group.children) {
        itemResidueShapes.remove(item.id)
      }
      itemResidueShapes.remove(rs.group.id)
      residueResidueShapes.remove(residue)
      rs.group.remove()
    }
  }

  def addSubstituent(subst: Substituent, pos: p.Point, mid: Boolean): SubstituentShape = {
    val ss = SubstituentShape(subst)
    substSubstShapes(subst) = ss
    itemSubstShapes(ss.item.id) = ss
    for (item <- ss.item.children) {
      itemSubstShapes(item.id) = ss
    }
    ss.item.position = if(mid) pos else pos.add(0, ss.item.bounds.height / 2)
    ss
  }

  def removeSubstituent(subst: Substituent): Unit = {
    for (ss <- substSubstShapes.get(subst)) {
      for (item <- ss.item.children) {
        itemSubstShapes.remove(item.id)
      }
      itemSubstShapes.remove(ss.item.id)
      substSubstShapes.remove(subst)
      ss.item.remove()
    }
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

      path.add(linkPosition(Link(from, 1)))
      path.add(linkPosition(Link(to, i)))
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
    if (dc == DisplayConv.convUCT)
      link.residue.group.localToGlobal(link.residue.outline.segments(link.position - 1).point)
    else
      link.residue.group.localToGlobal(link.residue.outline.bounds.center)
  }

  val threshold = 50 * 50

  var selectionBox: Option[p.Path] = None

  def drawBoxSelection(box: Option[p.Rectangle]): Unit = {
    selectionBox.map(_.remove())
    selectionBox = box.map { rect =>
      val sb = p.Path.Rectangle(rect)
      sb.fillColor = new p.Color(0.5, 0.5, 1.0, 0.5)
      sb.strokeColor = "black"
      sb
    }
  }

  def testSelection(rect: p.Rectangle, residue: Residue): Boolean = {
    rect.contains(residue.group.position)
  }

  var residueTemplate: Option[(p.Item, p.Path)] = None

  def showResidueTemplate(residue: Option[Residue], pos: p.Point): Unit = {
    residue.fold {
      for ((item, _) <- residueTemplate) item.remove()
      residueTemplate = None
    } { r =>
      if (residueTemplate.isEmpty) {
        val rs = ResidueShape(r, dc)
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