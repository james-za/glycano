package za.jwatson.glycanoweb.structure


case class ResidueType(symbol: String, desc: String, linkage: Int)

object ResidueType extends Aldoses with Ketoses with Alditols {
  val ResidueTypes = Aldoses ++ Ketoses ++ Alditols
}

trait Aldoses {
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
}

trait Ketoses {
  val Three = ResidueType("Three", "Three", 3)
  val Four = ResidueType("Four", "Four", 4)
  val Rul = ResidueType("Rul", "Rul", 5)
  val Xul = ResidueType("Xul", "Xul", 5)
  val Fru = ResidueType("Fru", "Fructose", 6)
  val Psi = ResidueType("Psi", "Psi", 6)
  val Sor = ResidueType("Sor", "Sor", 6)
  val Tag = ResidueType("Tag", "Tag", 6)
  val AltHep = ResidueType("AltHep", "AltroHeptulose", 7)
  val ManOct = ResidueType("ManOct", "MannoseOctulose", 8)
  val Ketoses = Seq(Three, Four, Rul, Xul, Fru, Psi, Sor, Tag, AltHep, ManOct)
}

trait Alditols {
  val Glycerol = ResidueType("Glycerol", "Glycerol", 3)
  val Erythritol = ResidueType("Erythritol", "Erythritol", 4)
  val Threitol = ResidueType("Threitol", "Threitol", 4)
  val MesoRibitol = ResidueType("MesoRibitol", "MesoRibitol", 5)
  val Arabinitol = ResidueType("Arabinitol", "Arabinitol", 5)
  val MesoXylitol = ResidueType("MesoXylitol", "MesoXylitol", 5)
  val Glucitol = ResidueType("Glucitol", "Glucitol", 6)
  val Unknown1 = ResidueType("Unknown1", "Unknown1", 6)
  val Unknown2 = ResidueType("Unknown2", "Unknown2", 6)
  val MesoAllitol = ResidueType("MesoAllitol", "MesoAllitol", 6)
  val Alditols = Seq(Glycerol, Erythritol, Threitol, MesoRibitol, Arabinitol, MesoXylitol, Glucitol, Unknown1, Unknown2, MesoAllitol)
}