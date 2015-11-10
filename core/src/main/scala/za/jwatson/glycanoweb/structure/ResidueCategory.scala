package za.jwatson.glycanoweb.structure

import japgolly.scalajs.react.extra.Reusability

sealed trait ResidueCategory {
  def name: String
  override def toString: String = name
}

object ResidueCategory {
  case object Aldose extends ResidueCategory { def name = "Aldose" }
  case object Ketose extends ResidueCategory { def name = "Ketose" }
  case object Alditol extends ResidueCategory { def name = "Alditol" }
  case object Misc extends ResidueCategory { def name = "Misc" }
  case object Repeat extends ResidueCategory { def name = "Repeat" }
  val ResidueCategories = Seq[ResidueCategory](Aldose, Ketose, Alditol, Repeat)
  val ResidueCategoryMap = ResidueCategories.map(rc => rc.name -> rc).toMap

  implicit val reusability: Reusability[ResidueCategory] = Reusability.by_==
}