package za.jwatson.glycanoweb.structure


case class ResidueType(symbol: String, desc: String, linkage: Int)

object ResidueType {
  val Glycero = ResidueType("Glycero", "Glycero", 3)
  val Erythro = ResidueType("Erythro", "Erythro", 4)
  val Threo = ResidueType("Threo", "Threo", 4)
  val Ara = ResidueType("Ara", "Ara", 5)
  val Lyx = ResidueType("Lyx", "Lyx", 5)
  val Rib = ResidueType("Rib", "Rib", 5)
  val Xyl = ResidueType("Xyl", "Xyl", 5)
  val Ido = ResidueType("Ido", "Ido", 6)
  val All = ResidueType("All", "All", 6)
  val Alt = ResidueType("Alt", "Alt", 6)
  val Gal = ResidueType("Gal", "Gal", 6)
  val Glc = ResidueType("Glc", "Glc", 6)
  val Gul = ResidueType("Gul", "Gul", 6)
  val Man = ResidueType("Man", "Man", 6)
  val Tal = ResidueType("Tal", "Tal", 6)

  val Aldoses = Seq(Glycero, Erythro, Threo, Ara, Lyx, Rib, Xyl, Ido, All, Alt, Gal, Glc, Gul, Man, Tal)
  val ResidueTypes = Aldoses //todo: ++ Ketoses ++ Alditols
}
