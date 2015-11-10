package za.jwatson.glycanoweb.convention

object ESN {
  //def ESNHexagon = RegularPolygon(n="6", r="30")
  val text =
    """
      |convention "ESN" {
      |   def ESNCircle = Circle(r="30")
      |   def ESNSquare = Rect(width="60", height="60")
      |   def ESNSquareTR = Polygon(points="0,0 60,0 60,60")
      |   def ESNDiamond = Polygon(points="30,0 60,30 30,60 0,30")
      |   def ESNDiamondT = Polygon(points="0,30 30,0 60,30")
      |   def ESNDiamondB = Polygon(points="60,30 30,60 0,30")
      |   def ESNTriangle = Polygon(points="0,60 30,0 60,60")
      |   def ESNTriangleR = Polygon(points="30,60 30,0 60,60")
      |   def ESNStar = Star(n="5", r1="10", r2="30")
      |   def ESNRectangle = Rect(width="70", height="35")
      |   def ESNHexagon = Polygon(points="0,30 15,0 50,0 65,30 50,60 15,60")
      |   def ESNPentagon = RegularPolygon(n="5", r="30")
      |
      |   palette "Pentose" { Rib, Ara, Xyl, Lyx }
      |   palette "Hexose" { All, Alt, Glc, Man, Gul, Ido, Gal, Tal }
      |   palette "NAc" { All2NAc, Alt2NAc, Glc2NAc, Man2NAc, Gul2NAc, Ido2NAc, Gal2NAc, Tal2NAc }
      |   palette "N" { All2N, Alt2N, Glc2N, Man2N, Gul2N, Ido2N, Gal2N, Tal2N }
      |   palette "A" { All2COOH, Alt2COOH, Glc2COOH, Man2COOH, Gul2COOH, Ido2COOH, Gal2COOH, Tal2COOH }
      |   palette "6Deoxy" { Glc6Deoxy, Man6Deoxy, Alt6Deoxy, Tal6Deoxy, Gal6Deoxy }
      |   palette "NAc6Deoxy" { Glc2NAc6Deoxy, Man2NAc6Deoxy, Gal2NAc6Deoxy }
      |   palette "Di-deoxy" {
      |     Glc2Deoxy6Deoxy, All2Deoxy6Deoxy,
      |     Man3Deoxy6Deoxy, Gul3Deoxy6Deoxy, Alt3Deoxy6Deoxy, Tal3Deoxy6Deoxy
      |   }
      |   palette "Nonulosonate" { Kdn, NeuAc, NeuGc, Neu }
      |   palette "Unknown" { Bac, LDManHep, Kdo, Dha, DDManHep, MurNAc, MurNGc, Mur }
      |   palette "Assigned" { Api, Fru, Tag, Sor, Psi }
      |
      |   default
      |   -> #1 [primary] ESNCircle
      |   -> #2 [outline] ESNCircle
      |   -> style [primary] { fill: #FFFFFF }
      |   -> style [outline] { stroke: #000000; stroke-width: 3 }
      |
      |   (All, Alt, Glc, Man, Gul, Ido, Gal, Tal) <>
      |   -> #1 [primary] ESNCircle
      |   -> #2 [outline] ESNCircle
      |
      |   (All, Alt, Glc, Man, Gul, Ido, Gal, Tal) <N, Ac>
      |   -> #1 [primary] ESNSquare
      |   -> #2 [outline] ESNSquare
      |
      |   (All, Alt, Glc, Man, Gul, Ido, Gal, Tal) <N>
      |   -> #1 [secondary] ESNSquare
      |   -> #2 [primary] ESNSquareTR
      |   -> #3 [thin] ESNSquareTR
      |   -> #4 [outline] ESNSquare
      |
      |   (All, Glc, Man, Gul, Gal, Tal) <COOH>
      |   -> #1 [secondary] ESNDiamond
      |   -> #2 [primary] ESNDiamondT
      |   -> #3 [thin] ESNDiamondT
      |   -> #4 [outline] ESNDiamond
      |
      |   (Alt, Ido) <COOH>
      |   -> #1 [secondary] ESNDiamond
      |   -> #2 [primary] ESNDiamondB
      |   -> #3 [thin] ESNDiamondB
      |   -> #4 [outline] ESNDiamond
      |
      |   (Gal, Glc, Man, Alt, Tal) <6Deoxy>
      |   -> #1 [primary] ESNTriangle
      |   -> #2 [outline] ESNTriangle
      |
      |   (Gal, Glc, Man) <6Deoxy, N, Ac>
      |   -> #1 [secondary] ESNTriangle
      |   -> #2 [primary] ESNTriangleR
      |   -> #3 [thin] ESNTriangleR
      |   -> #2 [outline] ESNTriangle
      |
      |   (Glc, All) <2Deoxy, 6Deoxy>
      |   -> #1 [primary] ESNRectangle
      |   -> #2 [outline] ESNRectangle
      |
      |   (Man, Gul, Alt, Tal) <3Deoxy, 6Deoxy>
      |   -> #1 [primary] ESNRectangle
      |   -> #2 [outline] ESNRectangle
      |
      |   (Rib, Ara, Xyl, Lyx) <>
      |   -> #1 [primary] ESNStar
      |   -> #2 [outline] ESNStar
      |
      |   (Kdn, NeuAc, NeuGc, Neu) <>
      |   -> #1 [primary] ESNDiamond
      |   -> #2 [outline] ESNDiamond
      |
      |   (Bac, LDManHep, Kdo, Dha, DDManHep, MurNAc, MurNGc, Mur) <>
      |   -> #1 [primary] ESNHexagon
      |   -> #2 [outline] ESNHexagon
      |
      |   (Api, Fru, Tag, Sor, Psi) <>
      |   -> #1 [primary] ESNPentagon
      |   -> #2 [outline] ESNPentagon
      |
      |   (Glc,             Bac,      Api) -> style [primary] { fill: #0000FA }
      |   (Man, Ara, Kdn,   LDManHep, Fru) -> style [primary] { fill: #00C832 }
      |   (Gal, Lyx,        Kdo,      Tag) -> style [primary] { fill: #FFFF00 }
      |   (Gul, Xyl,        Dha,      Sor) -> style [primary] { fill: #FA6400 }
      |   (Alt, Rib,        DDManHep, Psi) -> style [primary] { fill: #FA78FA }
      |   (All,      NeuAc, MurNAc       ) -> style [primary] { fill: #7D007D }
      |   (Tal,      NeuGc, MurNGc       ) -> style [primary] { fill: #97F1FA }
      |   (Ido,      Neu,   Mur          ) -> style [primary] { fill: #966432 }
      |   (Fuc) -> style [primary] { fill: #FF0000 }
      |
      |   * -> style [outline] { fill: none; stroke: #000000; stroke-width: 3 }
      |   * -> style [thin] { fill: none; stroke: #000000; stroke-width: 1 }
      |   * -> style [secondary] { fill: #FFFFFF }
      |}
    """.stripMargin
}
