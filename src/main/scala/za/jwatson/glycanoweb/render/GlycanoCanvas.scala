package za.jwatson.glycanoweb.render

import org.scalajs.dom.HTMLCanvasElement
import za.jwatson.glycanoweb.structure.Residue
/*
class GlycanoCanvas(canvas: HTMLCanvasElement) {
  val undoLimit = 50
  var undoPosition = 0

  def dc = GlycanoWeb.displayConv()

  def loadGly(gly: Gly): Unit = {
    clearAll()

    val added = for (GlyRes(ano, abs, rt, x, y, rot, _, _, subs) <- gly.residues) yield {
      val res = addResidue(ano, abs, rt, p.Point(x, y), rot)
      for {
        (pos, sts) <- subs
        st <- sts
      } addSubstituent(Link(res, pos), st)
      res
    }
    val pr = added.toIndexedSeq
    for ((GlyRes(_, _, _, _, _, _, tr, tp, _), i) <- gly.residues.zipWithIndex if tr != -1) {
      addBond(Bond(pr(i), Link(pr(tr), tp)))
    }

    annotations() = (for (GlyAnnot(_, x, y, rot, text, size) <- gly.annotations) yield {
      val a = GlyAnnot.next(x, y, rot, text, size)
      a.id -> a
    }).toMap

    scope.view.draw()
  }

  def clearAll(): Unit = {
    updateSelection(Set.empty)
    for {
      r <- residues()
      (_, subs) <- r.substituents
      sub <- subs
    } ctx.removeSubstituent(sub)
    bonds() foreach ctx.removeBond
    residues() foreach ctx.removeResidue
    graph() = RGraph()
    annotations() = Map.empty
  }

  val graph = Var(RGraph())
  implicit def _graph: RGraph = graph()
  val residues = Rx { graph().entries.keySet }
  val bonds = Rx { residues().filter(_.hasParent) }

//  def update(r: Residue, placement: Placement): Unit = {
//    graph() = graph().updated(r, placement)
//  }

  val annotations = Var(Map.empty[Int, GlyAnnot])

  Obs(annotations, skipInitial = true) {
    ctx.annots := annotations().values.toSeq
  }

  val history = Var(Vector((graph(), annotations())))
  def addToHistory(): Unit = {
    history() = (graph(), annotations()) +: history().drop(undoPosition) take undoLimit
    undoPosition = 0
  }

  def setState(target: (RGraph, Map[Int, GlyAnnot])): Unit = {
    clearAll()
//    for ((r, ge) <- target.entries) {
//      addResidue(r.anomer, r.absolute, r.rt, p.Point(ge.x, ge.y), ge.rotation)
//      for {
//        (pos, subs) <- ge.subs
//        sub <- subs
//      } addSubstituent(Link(r, pos), sub)
//    }
//    for {
//      (r, ge) <- target.entries
//      link <- ge.parent
//    } addBond(Bond(r, link))
    val (g, a) = target
    loadGly(Gly.from(g, a))
    redraw()
  }

  val scope = new p.PaperScope()
  scope.setup(canvas)
  implicit val ctx = new PaperJSContext(scope)

  val layerBack = scope.project.activeLayer
  val layerFront = new p.Layer()
  val layerAnnot = new p.Layer()
  layerFront.activate()

  val zoomLevel = Var(0)
  val zoom = Rx { math.pow(1.2, zoomLevel()) }
  Obs(zoomLevel, skipInitial = true) {
    scope.view.zoom = zoom()
  }

  def redraw() = scope.view.draw()

  def toggleAddAnnotation(): Unit = {
    updateSelection(Set.empty)
    state() match {
      case Default => state() = AddAnnotation
      case AddAnnotation => state() = Default
      case _ =>
    }
  }

  rx.Obs(GlycanoWeb.displayConv, skipInitial = true) {
    for ((s, ss) <- ctx.substSubstShapes) {
      ctx.removeSubstituent(s)
    }
    for ((r, rs) <- ctx.residueResidueShapes if r.rt.category != ResidueCategory.Repeat) {
      val pos = rs.group.position
      val rot = rs.group.rotation
      ctx.removeResidue(r)
      if (dc.name == "UCT") {
        ctx.addResidue(r, pos, rot)
        for {
          (p, subs) <- r.substituents
          top = ctx.linkPosition(Link(r, p))
        } {
          subs.zipWithIndex.foldLeft(top) {
            case (t, (sub, i)) =>
              val first = i == 0
              val ss = ctx.addSubstituent(sub, t, first)
              val h = ss.item.bounds.height
              t.add(0, if (first) h / 2 else h)
          }
        }
      } else {
        ctx.addResidue(r, pos, rot, r.substituents)
      }
    }
    updateResidueSetBonds(residues())
    scope.view.draw()
  }

  def addResidue(ano: Anomer, abs: Absolute, rt: ResidueType, pos: p.Point, rot: Double = 0.0): Residue = {
    val added = addResidue(Residue.next(rt, ano, abs), pos, rot)
    added
  }

  def addResidue(residue: Residue, pos: p.Point, rot: Double): Residue = {
    graph() += residue
    graph() = graph().updated(residue, Placement(pos.x, pos.y, rot))
    ctx.addResidue(residue, pos, rot)
    residue
  }

  def removeResidue(residue: Residue): Unit = {
    for {
      (i, subs) <- residue.substituents
      sub <- subs
    } removeSubstituent(Link(residue, i), sub)

    for (b <- residue.bond) removeBond(b)
    for {
      m <- residue.children
      (i, from) <- m
    } removeBond(Bond(from, Link(residue, i)))

    graph() -= residue
    ctx.removeResidue(residue)
  }

  def addBond(bond: Bond): Unit = {
    graph() += bond
    ctx.addBond(bond.from, bond.to.residue, bond.to.position)
  }

  def removeBond(bond: Bond): Unit = {
    graph() -= bond
    ctx.removeBond(bond.from)
  }

  def removeChildBond(link: Link): Unit = {
    graph() -= link
    for (r <- link.residue.child(link.position))
      ctx.removeBond(r)
  }

  def addSubstituent(link: Link, st: SubstituentType): Unit = {
    if (link.residue.rt.category != ResidueCategory.Repeat) {
      addSubstituent(link, Substituent.next(st))
    }
  }

  def addSubstituent(link: Link, substituent: Substituent): Unit = {
    if (link.residue.rt.category != ResidueCategory.Repeat) {
      val (pos, mid) = nextSubstPos(link)
      graph() += (link -> substituent)
      ctx.addSubstituent(substituent, pos, mid)
    }
  }

  def removeSubstituent(link: Link, subst: Substituent): Unit = {
    graph() -= (link, subst)
    ctx.removeSubstituent(subst)
    for {
      subs <- link.residue.substituents.get(link.position)
      top = ctx.linkPosition(link)
    } {
      subs.flatMap(_.getItem).zipWithIndex.foldLeft(top) {
        case (t, (item, i)) =>
          val first = i == 0
          val h = item.bounds.height
          item.position = if (first) t else t.add(0, h / 2)
          t.add(0, if (first) h / 2 else h)
      }
    }
  }

  implicit class RichResidue(residue: Residue) {
    def getItem: Option[p.Item] = ctx.getItem(residue)
  }
  implicit class RichSubstituent(substituent: Substituent) {
    def getItem: Option[p.Item] = ctx.getItem(substituent)
  }
  implicit class RichAnnotation(annot: GlyAnnot) {
    def getItem: Option[p.Item] = ctx.getItem(annot)
  }
  implicit class RichItem(item: p.Item) {
    def getResidue: Option[Residue] = ctx.getResidue(item)
    def getAnnotation: Option[GlyAnnot] = ctx.getAnnotation(item)
  }

  scope.project.layers.push(new p.Layer())
  val bondLayer = scope.project.layers(0)
  val residueLayer = scope.project.layers(1)
  scope.project.activeLayer = residueLayer

  scope.tool = new p.Tool()
  scope.tool.onMouseDown = (e: p.ToolEvent) => {
  }


//  canvas.onmousedown = mouseDown _
//  canvas.onmouseup = mouseUp _
//  canvas.onmousemove = mouseMove _

  scope.tool.onMouseDown = mouseDown _
  scope.tool.onMouseUp = mouseUp _
  scope.tool.onMouseMove = mouseMove _

  jQ(org.scalajs.dom.window).keydown((e: JQueryEventObject) => {
    val shift = e.asInstanceOf[js.Dynamic].shiftKey.asInstanceOf[js.UndefOr[Boolean]].getOrElse(false)
    val ctrl = e.asInstanceOf[js.Dynamic].ctrlKey.asInstanceOf[js.UndefOr[Boolean]]
    val meta = e.asInstanceOf[js.Dynamic].metaKey.asInstanceOf[Boolean]
    val mod = meta || ctrl.getOrElse(false)
    e.which match {
      case 46 =>
        deleteSelection()
        redraw()
      case 27 | 32 =>
        if(GlycanoWeb.showModeSelect()) {
          cancelPlace()
        }
      case 88 /*X*/ if mod =>
        copySelection()
        deleteSelection()
        redraw()
      case 67 /*C*/ if mod =>
        copySelection()
      case 86 /*V*/ if mod =>
        pasteSelection()
        redraw()
      case 90 /*Z*/ if mod =>
        if (shift) redo() else undo()
      case 89 /*Y*/ if mod =>
        redo()
      case _ =>
    }
  })

  def undo(): Unit = {
    if (undoPosition < math.min(undoLimit, history().size) - 1) {
      undoPosition += 1
      setState(history()(undoPosition))
    }
  }

  def redo(): Unit = {
    if (undoPosition > 0) {
      undoPosition -= 1
      setState(history()(undoPosition))
    }
  }

  val buffer = Var[Map[Residue, GraphEntry]](Map.empty)

  def deleteSelection(): Unit = {
    for (annotId <- selectedAnnotation()) {
      selectedAnnotation() = None
      annotations() -= annotId
    }

    val deleted = selection()
    updateSelection(Set.empty)
    deleted foreach removeResidue

    addToHistory()
  }

  def copySelection(): Unit = {
    buffer() = graph().entries.filterKeys(selection().contains)
  }

  def pasteSelection(): Unit = {
    updateSelection(Set.empty)

    val addedResidues = for ((src, ge) <- buffer()) yield {
      val added = addResidue(src.anomer, src.absolute, src.rt, p.Point(ge.x, ge.y), ge.rotation)
      for {
        (pos, subs) <- ge.subs
        sub <- subs
      } addSubstituent(Link(added, pos), sub.st)
      added
    }
    val lookup = (buffer().keys zip addedResidues).toMap
    for {
      (src, ge) <- buffer()
      Link(srcParent, position) <- ge.parent
      added <- lookup.get(src)
      addedParent <- lookup.get(srcParent)
    } addBond(Bond(added, Link(addedParent, position)))

    updateSelection(lookup.values.toSet)
    addToHistory()
  }
  import GlycanoCanvas.InputState
  import InputState._

  val state = Var[InputState](Default)

  val selection = Var(Set.empty[Residue])
/*
  Obs(state) {
    state() match {
      case Default =>
      case PlaceResidue =>
      case BoxSelect =>
        clearSelection()
      case Drag =>
      case CreateBond =>
    }
  }*/

  def changeResidue(old: Residue, added: Residue): Residue = {
    val select = selection() contains old
    if (select) updateSelection(selection() - old)

    val parent = old.parent
    val children = old.children
    val pos = old.getItem.get.position
    val rot = old.getItem.get.rotation
    removeResidue(old)

    addResidue(added, pos, rot)
    for (link <- parent) addBond(Bond(added, link))
    for {
      map <- children
      (pos, from) <- map
    } addBond(Bond(from, Link(added, pos)))

    if (select) updateSelection(selection() + added)
    added
  }

  def changeResidueAnomer(r: Residue, ano: Anomer): Residue =
    changeResidue(r, Residue.next(r.rt, ano, r.absolute))

  def changeResidueAbsolute(r: Residue, abs: Absolute): Residue =
    changeResidue(r, Residue.next(r.rt, r.anomer, abs))

  val selectedAnnotation = Var[Option[Int]](None)

  Obs(selectedAnnotation) {
    val sel = selectedAnnotation()
    for {
      (id, annot) <- annotations()
      item <- annot.getItem
    } item.selected = sel.contains(id)
  }

  val bondsLabelled = Rx[Set[Bond]] {
    val show = GlycanoWeb.bondLabels()
    if (show) {
      for {
        from <- bonds()
        to <- from.parent
      } yield Bond(from, to)
    } else Set.empty[Bond]
  }

  Obs(bondsLabelled) {
    ctx.bondLabels := bondsLabelled()
    redraw()
  }

  val boxSelect = Var[Option[p.Rectangle]](None)

  Obs(boxSelect) {
    ctx.drawBoxSelection(boxSelect())
  }

  def updateBoxSelect(begin: p.Point, end: p.Point): Unit = {
    boxSelect() = Some(new p.Rectangle(begin, end))
    for(rect <- boxSelect()) {
      val newSelection = residues().filter(ctx.testSelection(rect, _)).toSet
      updateSelection(newSelection)
    }
  }

  def endBoxSelect(): Unit = {
    boxSelect() = None
  }

  def updateSelection(newSelection: Set[Residue]): Unit = {
    selectedAnnotation() = None
    val oldSelection = selection()
    if(newSelection != oldSelection) {
      for (res <- oldSelection diff newSelection) ctx.unhighlightResidue(res)
      for (res <- newSelection diff oldSelection) ctx.highlightResidue(res)
      selection() = newSelection
    }
  }

  var hitHandle: Boolean = false

  val tempBond: DiffMap[TempBond, p.Path] = new DiffMap[TempBond, p.Path] {
    override val updateWhenCreating = false
    override def createItem(s: TempBond): p.Path = {
      val line = p.Path.Line(s.p1, s.p2)
      line.strokeWidth = 10
      line.strokeColor = "#AAAAAA"
      line.dashArray = js.Array(1, 15)
      line.strokeCap = "round"
      line
    }
    override def updateItem(s: TempBond, t: p.Path): Unit = {
      t.segments(0).point = s.p1
      t.segments(1).point = s.p2
    }
    override def removeItem(s: TempBond, t: p.Path): Unit = t.remove()
  }

  val tempLink: DiffMap[Link, p.PointText] = new DiffMap[Link, p.PointText] {
    override val updateWhenCreating: Boolean = true
    override def createItem(source: Link): PointText = {
      val num = new p.PointText(ctx.linkPosition(source))
      num.content = s"${source.position}"
      num.fillColor = new p.Color("white")
      num.strokeColor = new p.Color("black")
      num.strokeWidth = 1
      num.fontSize = 30
      num
    }
    override def updateItem(source: Link, item: PointText): Unit = {
      item.position = ctx.linkPosition(source)
    }
    override def removeItem(source: Link, item: PointText): Unit = item.remove()
  }

  def beginBond(residue: Residue, point: p.Point): Unit = {
    for(link @ Link(p, i) <- residue.parent) {
      graph() -= Link(residue, 1)
      ctx.removeBond(residue)
      if(i == 1) {
        graph() -= link
        ctx.removeBond(p)
        addToHistory()
      }
    }
    tempLink(Link(residue, 1))
    tempBond(TempBond.LinkToPoint(Link(residue, 1), point))
  }

  def repeatTargetValid(link: Link): Boolean = {
    link.residue.rt match {
      case ResidueType.Begin => false
      case ResidueType.End => link.position == 2
      case _ => true
    }
  }

  def updateBondTarget(point: p.Point): Unit = {
    for {
      from <- tempBond.items.keys.headOption collect {
        case TempBond.LinkToPoint(link, _) => link
        case TempBond.LinkToLink(link, _) => link
      }
      item <- from.residue.getItem
    } {
      val closestLink = ctx.getClosestLinkAnyFilter(
        linkFilter = link => link.residue != from.residue && repeatTargetValid(link),
        from = point,
        angle = item.rotation
      )
      closestLink.fold {
        tempLink(from)
        tempBond(TempBond.LinkToPoint(from, point))
      } { to =>
        tempLink(Set(from, to))
        tempBond(TempBond.LinkToLink(from, to))
      }
    }
  }

  def endBond(): Unit = {
    tempBond.items.keys foreach {
      case TempBond.LinkToLink(Link(from, 1), toLink @ Link(to, i)) if from != to =>
        if(i == 1) {
          for (parent <- to.parent) {
            graph() -= toLink
            ctx.removeBond(to)
          }
        }
        for (child <- to.child(i)) {
          graph() -= Link(child, 1)
          ctx.removeBond(child)
        }

        graph() += Bond(from, toLink)
        ctx.addBond(from, to, i)

        addToHistory()
      case _ =>
    }
  }

  val residueTemplate = Rx {
    GlycanoWeb.residueType().map { rt =>
      tempBond := Set.empty[TempBond]
      tempLink := Set.empty[Link]
      ctx.showResidueTemplate(none, null)
      Residue.next(rt, GlycanoWeb.anomeric(), GlycanoWeb.absolute())
    }
  }

  val clearOldTemplate = Obs(residueTemplate) {
    tempBond := Set.empty[TempBond]
    tempLink := Set.empty[Link]
    for (rTemp <- residueTemplate()) {
      canvas.style.cursor = "none"

      state() = PlaceResidue
    }
    scope.view.draw()
  }

  Obs(GlycanoWeb.substituentType) {
    tempBond := Set.empty[TempBond]
    tempLink := Set.empty[Link]
    ctx.showSubstituentTemplate(None, null)
    for (st <- GlycanoWeb.substituentType()) {
      canvas.style.cursor = "default"
      state() = AddSubstituent
    }
    scope.view.draw()
  }

  Obs(GlycanoWeb.showModeSelect) {
    if(!GlycanoWeb.showModeSelect()) {
      canvas.style.cursor = "default"
      state() = Default
    }
    ctx.showResidueTemplate(None, null)
    ctx.showSubstituentTemplate(None, null)
    scope.view.draw()
  }

  def updateSelectionBonds(): Unit = updateResidueSetBonds(selection())

  def updateResidueSetBonds(set: Set[Residue]): Unit = {
    val affectedBonds = for {
      r <- set
      b <- r +: (r.children.fold(Seq[Residue]())(_.values.toSeq)
        ++ r.parent.toSeq.map(_.residue))
    } yield b

    for {
      from <- affectedBonds
      Link(to, i) <- from.parent
    } ctx.updateBond(from, to, i)

    ctx.bondLabels := bondsLabelled()

    if (dc.name == "UCT") for {
      r <- set
      (pos, subs) <- r.substituents
      top = ctx.linkPosition(Link(r, pos))
      (sub, offset) <- subs.zip(stackPositions(subs))
      subItem <- sub.getItem
    } subItem.position = top.add(0, offset)
  }

  def moveSelectionBy(delta: p.Point) {
    val updates = for {
      residue <- selection()
      item <- residue.getItem
    } yield {
      item.position = item.position add delta
//      for {
//        (i, substs) <- residue.substituents
//        subst <- substs
//        substItem <- subst.getItem
//      } substItem.position = substItem.position add delta
      residue -> Placement(item.position.x, item.position.y, item.rotation)
    }
    graph() = updates.foldLeft(graph()) {
      case (g, (r, placement)) =>
        g.updated(r, placement)
    }
    for (annotId <- selectedAnnotation()) {
      val annot = annotations()(annotId)
      annotations() += annotId -> GlyAnnot(annotId, annot.x + delta.x, annot.y + delta.y, annot.rot, annot.text, annot.size)
      for (item <- annotations()(annotId).getItem)
        item.selected = true
    }
  }

  def canvasTopLeft = {
    val left = jQ(canvas).offset().asInstanceOf[js.Dynamic].left.asInstanceOf[Double]
    val top = jQ(canvas).offset().asInstanceOf[js.Dynamic].top.asInstanceOf[Double]
    val scrollLeft = jQ(org.scalajs.dom.window).scrollLeft()
    val scrollTop = jQ(org.scalajs.dom.window).scrollTop()
    p.Point(left - scrollLeft, top - scrollTop)
  }

//  def clientToProject(e: MouseEvent): p.Point = {
//    val pointClient = p.Point(e.clientX, e.clientY)
//    val pointOffset = pointClient subtract canvasTopLeft
//    scope.view.viewToProject(pointOffset)
//  }

  def cancelPlace(): Unit = {
    tempBond(Set.empty[TempBond])
    tempLink(Set.empty[Link])
    GlycanoWeb.setResidueType(None)
  }

  def cancelSubst(): Unit = {
    GlycanoWeb.setSubstituentType(None)
  }

  def cancelAll(): Unit = {
    cancelPlace()
    cancelSubst()

    redraw()
  }

  def linkFilter(link: Link): Boolean = {
    val bondOk = if (link.position == 1) !link.residue.hasParent else true
    link.residue.child(link.position).isEmpty && bondOk && repeatTargetValid(link)
  }

  def showPlaceTempBonds(point: p.Point, angle: Double): Unit = {
    for {
      t <- residueTemplate()
      ti <- ctx.residueTemplateItem()
    } {
      val ris = for {
        r <- residues()
        item <- r.getItem
      } yield r -> item
      val (right, left) = ris.partition {
        case (r, i) => ctx.onRightOf(point, i.position, angle)
      }
      val rightTB = for {
        point <- ctx.residueTemplateLinkPosition(1) if t.rt != ResidueType.End
        link <- ctx.getClosestLinkAnyFilterFrom(right.map(_._1), linkFilter, point, Some(true), angle, 200 * 200)
      } yield TempBond.LinkToTemplate(link, 1)

      val validLefts = for {
        (r, i) <- left.toSeq if !r.hasParent
        if t.rt != ResidueType.Begin
        if r.rt != ResidueType.End
        link = Link(r, 1)
        linkPt = ctx.linkPosition(link)
        dist = linkPt.getDistance(point, squared = true)
        if dist < 200 * 200
      } yield (link, i, dist)
      val leftSorted = validLefts.sortBy(_._3).take(t.rt.linkage - 1)
      val slots = for {
        i: Int <- (1 to t.rt.linkage).toSet
        pt <- ctx.residueTemplateLinkPosition(i)
      } yield i -> pt

      def slotAll(lefts: List[Link], slots: Set[(Int, p.Point)]): List[TempBond.LinkToTemplate] =
        lefts match {
          case Nil => Nil
          case x :: xs =>
            val lp = ctx.linkPosition(x)
            val min = slots.minBy(_._2.getDistance(lp, squared = true))
            TempBond.LinkToTemplate(x, min._1) :: slotAll(xs, slots - min)
        }
      val leftTBs = slotAll(leftSorted.map(_._1).toList, slots)
      tempLink(leftTBs.map(_.link) ++ rightTB.map(_.link))
      tempBond(leftTBs ++ rightTB)
    }
  }

  //Obs(state){println(state())}

  def mouseDown(e: p.ToolEvent): Unit = {
    canvas.focus()
    canvas.blur()
    val point = e.point//clientToProject(e)
    state() match {
      case Default =>
        if(e.event.button == 0) {
          val hitTest = scope.project.hitTest(point)
          Option(hitTest) match {
            case Some(hit) =>
              hit.item.name.getOrElse("") match {
                case "delete" =>
                  deleteSelection()
                case "handle" =>
                  for (r <- hit.item.getResidue) {
                    ctx.handlePress(r.some)
                    beginBond(r, point)
                    ctx.handleHL(None)
                    state() = CreateBond
                  }
                case "rotate" =>
                  for (r <- hit.item.getResidue) {
                    state() = Rotate(hit.item)
                  }
                case _ =>
                  //println("hit nothing")
                  val foundResOpt = for (r <- hit.item.getResidue) yield {
                    if (e.event.shiftKey || e.event.ctrlKey) updateSelection {
                      if (selection() contains r) selection() - r else selection() + r
                    } else {
                      state() = Hit(point, hit.item)
                      if(!selection().contains(r)) {
                          updateSelection(Set(r))
                      }
                    }
                  }
//                  val foundRes = foundResOpt.isDefined
//                  val hitAnnots = for {
//                    (annotId, annot) <- annotations()
//                    item <- annot.getItem
//                    if item.bounds.contains(point)
//                  } yield annot

                  for (annot <- hit.item.getAnnotation /*orElse hitAnnots.headOption*/) {
                    updateSelection(Set.empty)
                    selectedAnnotation() = Some(annot.id)
                    state() = Hit(point, hit.item)
                  }
              }
            case None =>
              updateBoxSelect(point, point)
              state() = BoxSelect(point)
          }
        }
      case PlaceResidue =>
        e.event.button match {
          case 0 =>
            for(t <- residueTemplate()) {
              val added = addResidue(t.anomer, t.absolute, t.rt, point)
              tempBond.items.keys foreach {
                case TempBond.LinkToTemplate(Link(from, 1), i) =>
                  graph() += Bond(from, Link(added, i))
                  ctx.addBond(from, added, i)
                case TempBond.LinkToTemplate(toLink @ Link(to, i), 1) =>
                  graph() += Bond(added, toLink)
                  ctx.addBond(added, to, i)
                case _ =>
              }
              addToHistory()
            }
          case 2 =>
            cancelPlace()
        }
      case AddSubstituent =>
        e.event.button match {
          case 0 =>
            for {
              st <- GlycanoWeb.substituentType()
              link <- ctx.getClosestLinkAny(point, None)
            } {
              addSubstituent(link, st)
              addToHistory()
            }
          case 2 =>
            cancelPlace()
        }
      case BoxSelect(_) =>
      case Drag(_) =>
      case CreateBond =>
        if(e.event.button == 0) {
          endBond()
        }
        tempBond(Set.empty[TempBond])
        tempLink(Set.empty[Link])
        state() = PostCreateBond
      case PostCreateBond =>
      case Hit(_, _) =>
      case Rotate(_) =>
      case AddAnnotation =>
        val added = GlyAnnot.next(point.x, point.y, 0, "Annotation", 20)
        annotations() += (added.id -> added)
        state() = Default
        selectedAnnotation() = Some(added.id)
        addToHistory()
    }
  }

  def mouseUp(e: p.ToolEvent): Unit = {
    state() match {
      case Default =>
      case PlaceResidue =>
      case AddSubstituent =>
      case BoxSelect(_) =>
        endBoxSelect()
        state() = Default
      case Drag(_) =>
        state() = Default
        addToHistory()
      case CreateBond =>
        ctx.handlePress(None)
      case PostCreateBond =>
        state() = Default
      case Hit(_, item) =>
        for(r <- item.getResidue) {
          updateSelection(Set(r))
        }
        state() = Default
      case Rotate(_) =>
        val oldSel = selection()
        updateSelection(Set.empty)
        updateSelection(oldSel)
        state() = Default
      case AddAnnotation =>
    }
  }

  def stackOffset(stack: Vector[Substituent]): Double = {
    stack match {
      case x +: xs =>
        val first = x.getItem.map(_.bounds.height / 2).getOrElse(0.0)
        val rest = xs.flatMap(_.getItem).map(_.bounds.height).sum
        first + rest
      case _ => 0
    }
  }

  def stackPositions(stack: Vector[Substituent]): Vector[Double] = {
    for {
      head <- stack.headOption.toVector
      first = head.getItem.get.bounds.height / 2
      heights = stack.flatMap(_.getItem).map(_.bounds.height)
      offset <- heights.tail.scanLeft(first) {
        case (top, height) => top + height
      }.zip(heights).map {
        case (b, h) => b - (h / 2)
      }
    } yield offset
  }

  def nextSubstPos(link: Link): (p.Point, Boolean) = {
    val stack = link.substituents
    val top = ctx.linkPosition(link)
    val offset = stackOffset(stack)
    (top.add(0, offset), stack.isEmpty)
  }

  def mouseMove(e: p.ToolEvent): Unit = {
    val point = e.point//clientToProject(e)
    state() match {
      case Default =>
        val hitTest = scope.project.hitTest(point)
        val hitHandle = for {
          hit <- Option(hitTest)
          r <- hit.item.getResidue
          if ctx.hitHandle(r, point)
        } yield r
        ctx.handleHL(hitHandle)
      case PlaceResidue =>
        ctx.showResidueTemplate(residueTemplate(), point)
        showPlaceTempBonds(point, angle = 0.0)
      case AddSubstituent =>
        ctx.getClosestLinkAny(point, None).fold {
          ctx.showSubstituentTemplate(GlycanoWeb.substituentType(), point, mid = true)
        } { link =>
          val (pos, mid) = nextSubstPos(link)
          ctx.showSubstituentTemplate(GlycanoWeb.substituentType(), pos, mid)
        }
      case BoxSelect(down) =>
        updateBoxSelect(down, point)
      case Drag(last) =>
        val delta = point subtract last
        moveSelectionBy(delta)
        updateSelectionBonds()
        state() = Drag(point)
      case CreateBond =>
        updateBondTarget(point)
      case PostCreateBond =>
      case Hit(down, _) =>
        state() = Drag(down)
      case Rotate(item) =>
        for (r <- item.getResidue; ri <- r.getItem) {
          val mid = ri.bounds.center
          val dir = point.subtract(mid)
          //ri.asInstanceOf[js.Dynamic].setRotation(dir.angle + 90)
          ri.rotation = dir.angle + 90
          updateResidueSetBonds(Set(r))
        }
      case AddAnnotation =>
    }
  }

  def keyPress(c: Int): Unit = {
    c match {
      case 46 =>
        deleteSelection()
      case 27 | 32 =>
        if(GlycanoWeb.showModeSelect()) {
          cancelPlace()
        }
      case _ =>
    }
  }
}
*/

/*
object GlycanoCanvas {
sealed trait TempBond {
  def p1: p.Point
  def p2: p.Point
}
object TempBond {
  case class LinkToPoint(link: Link, point: p.Point)(implicit c: PaperJSContext) extends TempBond {
    override def p1: Point = c.linkPosition(link)
    override def p2: Point = point
  }
  case class LinkToLink(link1: Link, link2: Link)(implicit c: PaperJSContext) extends TempBond {
    override def p1: Point = c.linkPosition(link1)
    override def p2: Point = c.linkPosition(link2)
  }
  case class PointToPoint(p1: p.Point, p2: p.Point) extends TempBond
  case class LinkToTemplate(link: Link, i: Int)(implicit c: PaperJSContext) extends TempBond  {
    override def p1: Point = c.linkPosition(link)
    override def p2: Point = c.residueTemplateLinkPosition(i).getOrElse(p.Point(0, 0))
  }
}
//  case class TempBond(from: Link, to: p.Point, toPos: Option[Int], toLink: Option[Link])
case class Selected(residue: Residue, offset: p.Point)

}
*/