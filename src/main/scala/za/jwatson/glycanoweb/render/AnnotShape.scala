package za.jwatson.glycanoweb.render

import za.jwatson.glycanoweb.GlyAnnot
import importedjs.{paper => p}

case class AnnotShape(annotation: GlyAnnot) {
  val item = new p.PointText()
  item.content = annotation.text
  item.point = new p.Point(annotation.x, annotation.y)
}
