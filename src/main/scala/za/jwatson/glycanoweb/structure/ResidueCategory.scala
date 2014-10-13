package za.jwatson.glycanoweb.structure

sealed trait ResidueCategory {
  def name: String
  override def toString: String = name
}

object ResidueCategory {
  case object Aldose extends ResidueCategory { def name = "Aldose" }
  case object Ketose extends ResidueCategory { def name = "Ketose" }
  case object Alditol extends ResidueCategory { def name = "Alditol" }
  case object Repeat extends ResidueCategory { def name = "Repeat" }
  case object Annotation extends ResidueCategory { def name = "Annotation" }
  val ResidueCategories = Seq[ResidueCategory](Aldose, Ketose, Alditol, Repeat, Annotation)
  val ResidueCategoryMap = ResidueCategories.map(rc => rc.name -> rc).toMap
}