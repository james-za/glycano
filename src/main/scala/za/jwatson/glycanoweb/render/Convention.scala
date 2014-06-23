package za.jwatson.glycanoweb.render

import scala.scalajs.js
import za.jwatson.glycanoweb.structure.Anomer.{Alpha, Beta}
import importedjs.{paper => p}
import p.Implicits._
import za.jwatson.glycanoweb.structure.Absolute.{L, D}
import importedjs.paper._
import za.jwatson.glycanoweb.structure._
import org.scalajs.dom.SVGElement
import za.jwatson.glycanoweb.structure.Bond
import scala.Some
import za.jwatson.glycanoweb.render.GlycanoCanvas.TempBond
import za.jwatson.glycanoweb.structure.Link
import za.jwatson.glycanoweb.render.Convention.CanvasItemMod

abstract class Convention(scope: p.PaperScope) {

  val handleHL: CanvasItemMod[p.Path, Residue]
  val handlePress: CanvasItemMod[p.Path, Residue]

  def finishBond(from: Link, to: p.Point): Option[Link]

  def hitHandle(residue: Residue, point: Point): Boolean

  val tempBond: CanvasItemMod[p.Path, TempBond]

  def showResidueTemplate(residue: Option[Residue], pos: p.Point): Unit

  def testSelection(rect: Rectangle, residue: Residue): Boolean

  def drawBoxSelection(box: Option[Rectangle]): Unit

  def removeResidue(residue: Residue): Unit
  def removeBond(b: Bond): Unit

  def highlightResidue(residue: Residue): Unit
  def unhighlightResidue(residue: Residue): Unit

  def addResidue(r: Residue, pos: p.Point): Unit
  def addBond(b: Bond): Unit

  def residueMoved(e: p.ToolEvent, r: Residue, o: p.Point): Unit
  def updateBond(b: Bond): Unit
  
  def getResidue(item: p.Item): Option[Residue]
  def getItem(residue: Residue): Option[p.Item]
  
  def items: Iterable[p.Item]
  
  def getClosestLink(point: p.Point): Option[Link]
  def highlightLink(link: Option[Link]): Unit
  def linkPosition(link: Link): p.Point

  def createIcon(rt: ResidueType, abs: Absolute, ano: Anomer, bounds: p.Rectangle): SVGElement
}

object Convention {
  class UCT(scope: p.PaperScope) extends Convention(scope) {

    import p.{Point => P}

    override def createIcon(rt: ResidueType, abs: Absolute, ano: Anomer, bounds: p.Rectangle): SVGElement = {
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

    def closedPath(pts: p.Segment*): p.Path = {
      val path = new p.Path(js.Array[p.Segment](pts: _*))
      path.closePath()
      path
    }

    val triangle = closedPath(P(40, 0), P(0, 35), P(0, -35))
    val diamond = closedPath(P(80,0), P(40,40), P(0,0), P(40,-40))
    val arrow = closedPath(P(90,0), P(60,30), P(0,30), P(0,-30), P(60,-30))
    val hexagon = closedPath(P(90,0), P(65,40), P(25,40), P(0,0), P(25,-40), P(65,-40))

    val details = {
      import ResidueType._
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
      handle.fillColor = residue.anomeric match {
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

    val itemResidueShapes = collection.mutable.Map[js.Number, ResidueShape]()
    val residueResidueShapes = collection.mutable.Map[Residue, ResidueShape]()

    override def items = residueResidueShapes.values.map(_.group)

    implicit class RichResidue(residue: Residue) {
      /** Only use if `residueShape` is known to exist */
      def residueShape: ResidueShape = residueResidueShapes(residue)
      /** Only use if `base` is known to exist */
      def base: p.Path = residueShape.base
      /** Only use if `detail` is known to exist */
      def detail: p.Path = residueShape.detail
      /** Only use if `outline` is known to exist */
      def outline: p.Path = residueShape.outline
      /** Only use if `handle` is known to exist */
      def handle: p.Path = residueShape.handle
      /** Only use if `group` is known to exist */
      def group: p.Group = residueShape.group
      def getResidueShape: Option[ResidueShape] = residueResidueShapes.get(residue)
      def getBase: Option[p.Path] = getResidueShape.map(_.base)
      def getDetail: Option[p.Path] = getResidueShape.map(_.detail)
      def getOutline: Option[p.Path] = getResidueShape.map(_.outline)
      def getHandle: Option[p.Path] = getResidueShape.map(_.handle)
      def getGroup: Option[p.Group] = getResidueShape.map(_.group)
    }
    implicit class RichItem(item: p.Item) {
      /** Only use if `residueShape` is known to exist */
      def residueShape: ResidueShape = itemResidueShapes(item.id)
      /** Only use if `residue` is known to exist */
      def residue: Residue = residueShape.residue
      def getResidueShape: Option[ResidueShape] = itemResidueShapes.get(item.id)
      def getResidue: Option[Residue] = getResidueShape.map(_.residue)
    }

    override def getResidue(item: Item): Option[Residue] = item.getResidue
    override def getItem(residue: Residue): Option[Item] = residue.getGroup

    val bonds = collection.mutable.Map[js.Number, Bond]()
    val bondItems = collection.mutable.Map[Bond, p.Path]()

    override def addResidue(r: Residue, pos: p.Point): Unit = {
      val rs = ResidueShape(r)
      residueResidueShapes(r) = rs
      itemResidueShapes(rs.group.id) = rs
      for(item <- rs.group.children) {
        itemResidueShapes(item.id) = rs
      }
      rs.group.position = pos
    }

    override def removeResidue(residue: Residue): Unit = {
      val rs = residueResidueShapes(residue)
      itemResidueShapes.remove(rs.group.id)
      itemResidueShapes.remove(rs.handle.id)
      itemResidueShapes.remove(rs.outline.id)
      itemResidueShapes.remove(rs.base.id)
      itemResidueShapes.remove(rs.detail.id)
      residueResidueShapes.remove(residue)
      rs.group.remove()
    }

    override def highlightResidue(residue: Residue): Unit = {
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

    override def unhighlightResidue(residue: Residue): Unit = {
      val hls = residue.group getItems js.Dynamic.literal(name = "highlight": js.Any)
      hls foreach (_.remove())
    }

    override def addBond(b: Bond): Unit = {
      val item = new p.Path()
      item.strokeColor = "black"
      item.strokeWidth = 7
      if(b.from.residue.anomeric == Beta) {
        item.dashArray = js.Array(15, 10)
      }
      item.project.layers(0) addChild item
      bonds(item.id) = b
      bondItems(b) = item
      updateBond(b)
    }

    override def removeBond(b: Bond): Unit = {
      val item = bondItems(b)
      item.remove()
      bondItems.remove(b)
      bonds.remove(item.id)
    }

    override def updateBond(b: Bond): Unit = {
      for(path <- bondItems.get(b)) {
        path.removeSegments()
        path.add(b.from.residue.outline.segments(b.from.position - 1).point)
        path.add(b.to.residue.outline.segments(b.to.position - 1).point)
      }
    }

    def residueMoved(e: p.ToolEvent, r: Residue, o: p.Point): Unit = {
      for(item <- r.getGroup) item.position = e.point add o
      for {
        b <- r.bonds.values
      } updateBond(b)
    }

    override def getClosestLink(point: p.Point): Option[Link] = {
      val points = for {
        residueShape <- residueResidueShapes.values
        segment <- residueShape.outline.segments.toSeq
        distsq = segment.point.getDistance(point, true)
        if distsq < threshold
      } yield (segment, distsq)

      val segment = if(points.nonEmpty) Some(points.minBy(_._2.doubleValue())._1) else None

      segment flatMap getLink
    }

    def getLink(segment: p.Segment): Option[Link] = {
      val residue = segment.path.parent.getResidue
      residue.map(Link(_, segment.index.intValue() + 1))
    }

    override def linkPosition(link: Link): p.Point = {
      link.residue.outline.segments(link.position - 1).point
    }

    val threshold = 50 * 50

    var highlightedLink: Option[Link] = None
    var linkHighlight: Option[p.Item] = None

    override def highlightLink(link: Option[Link]): Unit = {
      if(highlightedLink != link) {
        linkHighlight.map(_.remove())
        linkHighlight = None
        highlightedLink = link
        linkHighlight = link.map { l =>
          val num = new p.PointText(linkPosition(l))
          num.content = s"${l.position}"
          num.fillColor = new p.Color("white")
          num.strokeColor = new p.Color("black")
          num.strokeWidth = 1
          num.asInstanceOf[js.Dynamic].fontSize = 30
          num
        }
      }
      for {
        lh <- linkHighlight
        l <- link
      } lh.position = linkPosition(l)
    }

    var selectionBox: Option[p.Path] = None

    override def drawBoxSelection(box: Option[Rectangle]): Unit = {
      selectionBox.map(_.remove())
      selectionBox = box.map { rect =>
        val sb = p.Path.Rectangle(rect)
        sb.fillColor = new p.Color(0.5, 0.5, 1.0, 0.5)
        sb.strokeColor = "black"
        sb
      }
    }

    override def testSelection(rect: Rectangle, residue: Residue): Boolean = {
      rect.contains(residue.group.position)
    }
    
    var residueTemplate: Option[p.Item] = None

    override def showResidueTemplate(residue: Option[Residue], pos: p.Point): Unit = {
      residue match {
        case Some(r) =>
          if(residueTemplate.isEmpty)
            residueTemplate = Some(ResidueShape(r).group)
          residueTemplate.map(_.position = pos)
        case None =>
          residueTemplate.map(_.remove())
          residueTemplate = None
      }
    }

    override val tempBond = new CanvasItemMod[p.Path, TempBond] {
      override def create(s: TempBond): p.Path = {
        val line = p.Path.Line(linkPosition(s.from), s.to)
        line.strokeWidth = 10
        line.strokeColor = "#AAAAAA"
        line.dashArray = js.Array(1, 15)
        line.strokeCap = "round"
        line
      }
      override def update(t: p.Path, s: TempBond): Unit = {
        t.segments(1).point = s.to
      }
      override def revert(t: p.Path, s: TempBond): Unit = t.remove()
    }

    override def hitHandle(residue: Residue, point: P): Boolean = {
      residue.getHandle.exists(_.contains(point))
    }

    override def finishBond(from: Link, to: p.Point): Option[Link] = {
      getClosestLink(to)
    }

    override val handleHL = new CanvasItemMod[p.Path, Residue] {
      override def create(r: Residue) = r.handle
      override def update(h: p.Path, r: Residue) = h.strokeColor = "blue"
      override def revert(h: p.Path, r: Residue) = h.strokeColor = "black"
    }
    
    override val handlePress = new CanvasItemMod[p.Path, Residue] {
      override def create(r: Residue) = r.handle
      override def update(h: p.Path, r: Residue) = h.strokeWidth = 2
      override def revert(h: p.Path, r: Residue) = h.strokeWidth = 1
    }
  }

  trait CanvasItemMod[T, S] {
    def update(t: T, s: S): Unit
    def create(s: S): T
    def revert(t: T, s: S): Unit

    var item: Option[T] = None
    var itemSource: Option[S] = None

    def apply(source: Option[S]): Unit = {
      source match {
        case Some(s) =>
          if(source != itemSource) {
            for(t <- item) revert(t, s)
            item = Some(create(s))
          }
          update(item.get, s)
        case None =>
          for(t <- item) {
            revert(t, itemSource.get)
            item = None
          }
      }
      itemSource = source
    }
  }
}