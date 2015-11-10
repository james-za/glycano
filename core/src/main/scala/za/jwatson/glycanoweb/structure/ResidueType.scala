package za.jwatson.glycanoweb.structure

import za.jwatson.glycanoweb.structure.ResidueCategory._
import ResidueType._

case class ResidueType(symbol: String, desc: String, linkage: Int, category: ResidueCategory)

object ResidueType extends Aldoses with Ketoses with Alditols with Misc with Repeat {
  val ResidueTypes = Aldoses ++ Ketoses ++ Alditols ++ Miscs ++ Repeats
  val ResidueTypeCategories = ResidueTypes.groupBy(_.category)// + (Repeat -> Seq.empty)
  val ResidueTypeMap = ResidueTypes.map(rt => rt.symbol -> rt).toMap

  def aldose(symbol: String, desc: String, linkage: Int) =
    ResidueType(symbol, desc, linkage, Aldose)
  def ketose(symbol: String, desc: String, linkage: Int) =
    ResidueType(symbol, desc, linkage, Ketose)
  def alditol(symbol: String, desc: String, linkage: Int) =
    ResidueType(symbol, desc, linkage, Alditol)
  def repeat(symbol: String, desc: String, linkage: Int) =
    ResidueType(symbol, desc, linkage, Repeat)
  def misc(symbol: String, desc: String, linkage: Int) =
    ResidueType(symbol, desc, linkage, Misc)

  def unapply(str: String): Option[ResidueType] = ResidueTypeMap.get(str)
}

trait Repeat {
  val Begin = repeat("[", "Repeat-Begin", 1)
  val End = repeat("]", "Repeat-End", 2)
  val Repeats = Seq(Begin, End)
}

trait Aldoses {
  val Glycero = aldose("Glycero", "Glycero", 3)
  val Erythro = aldose("Erythro", "Erythro", 4)
  val Threo = aldose("Threo", "Threo", 4)
  val Ara = aldose("Ara", "Ara", 5)
  val Lyx = aldose("Lyx", "Lyx", 5)
  val Rib = aldose("Rib", "Rib", 5)
  val Xyl = aldose("Xyl", "Xyl", 5)
  val Ido = aldose("Ido", "Ido", 6)
  val All = aldose("All", "All", 6)
  val Alt = aldose("Alt", "Alt", 6)
  val Gal = aldose("Gal", "Gal", 6)
  val Glc = aldose("Glc", "Glc", 6)
  val Gul = aldose("Gul", "Gul", 6)
  val Man = aldose("Man", "Man", 6)
  val Tal = aldose("Tal", "Tal", 6)
  val Aldoses = Seq(Glycero, Erythro, Threo, Ara, Lyx, Rib, Xyl, Ido, All, Alt, Gal, Glc, Gul, Man, Tal)
}

trait Ketoses {
  val Three = ketose("Three", "Three", 3)
  val Four = ketose("Four", "Four", 4)
  val Rul = ketose("Rul", "Rul", 5)
  val Xul = ketose("Xul", "Xul", 5)
  val Fru = ketose("Fru", "Fructose", 6)
  val Psi = ketose("Psi", "Psi", 6)
  val Sor = ketose("Sor", "Sor", 6)
  val Tag = ketose("Tag", "Tag", 6)
  val Api = ketose("Api", "Api", 6)
  val AltHep = ketose("AltHep", "AltroHeptulose", 7)
  val ManOct = ketose("ManOct", "MannoseOctulose", 8)
  val Non = ketose("Non", "Nonose", 9)
  val Ketoses = Seq(Three, Four, Rul, Xul, Fru, Psi, Sor, Tag, Api, AltHep, ManOct, Non)
}

trait Alditols {
  val Glycerol = alditol("Glycerol", "Glycerol", 3)
  val Erythritol = alditol("Erythritol", "Erythritol", 4)
  val Threitol = alditol("Threitol", "Threitol", 4)
  val MesoRibitol = alditol("MesoRibitol", "MesoRibitol", 5)
  val Arabinitol = alditol("Arabinitol", "Arabinitol", 5)
  val MesoXylitol = alditol("MesoXylitol", "MesoXylitol", 5)
  val Glucitol = alditol("Glucitol", "Glucitol", 6)
  val UnknownA = alditol("UnknownA", "UnknownA", 6)
  val UnknownB = alditol("UnknownB", "UnknownB", 6)
  val MesoAllitol = alditol("MesoAllitol", "MesoAllitol", 6)
  val Alditols = Seq(Glycerol, Erythritol, Threitol, MesoRibitol, Arabinitol, MesoXylitol, Glucitol, UnknownA, UnknownB, MesoAllitol)
}

trait Misc {
  // "Unknown" in ESN
  val Kdn = misc("Kdn", "Kdn", 6)
  val NeuAc = misc("NeuAc", "NeuAc", 6)
  val NeuGc = misc("NeuGc", "NeuGc", 6)
  val Neu = misc("Neu", "Neu", 6)

  // "Assigned" in ESN
  val Bac = misc("Bac", "Bac", 6)
  val LDManHep = misc("LDManHep", "LDManHep", 6)
  val Kdo = misc("Kdo", "Kdo", 6)
  val Dha = misc("Dha", "Dha", 6)
  val DDManHep = misc("DDManHep", "DDManHep", 6)
  val MurNAc = misc("MurNAc", "MurNAc", 6)
  val MurNGc = misc("MurNGc", "MurNGc", 6)
  val Mur = misc("Mur", "Mur", 6)

  val Miscs = Seq(Kdn, NeuAc, NeuGc, Neu, Bac, LDManHep, Kdo, Dha, DDManHep, MurNAc, MurNGc, Mur)
}