package za.jwatson.glycanoweb.structure

sealed trait ResidueCategory {
  def name: String
}

object ResidueCategory {
  case object Aldose extends ResidueCategory { def name = "Aldose" }
  case object Ketose extends ResidueCategory { def name = "Ketose" }
  case object Alditol extends ResidueCategory { def name = "Alditol" }
  case object Repeat extends ResidueCategory { def name = "Repeat" }
  val ResidueCategories = Seq(Aldose, Ketose, Alditol, Repeat)
}