package za.jwatson.glycanoweb.structure

sealed trait ResidueCategory {
  def name: String
  override def toString: String = name
}

object ResidueCategory {
  case object Aldose extends ResidueCategory { def name = "Aldoses" }
  case object Ketose extends ResidueCategory { def name = "Ketoses" }
  case object Alditol extends ResidueCategory { def name = "Alditols" }
  case object Repeat extends ResidueCategory { def name = "Repeat" }
  val ResidueCategories = Seq[ResidueCategory](Aldose, Ketose, Alditol, Repeat)
  val ResidueCategoryMap = ResidueCategories.map(rc => rc.name -> rc).toMap
}