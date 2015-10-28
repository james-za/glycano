package za.jwatson.glycanoweb.convention

object ESN {
  val text =
    """
      |convention "ESN" {
      |   def ESNCircle = Circle(r="30")
      |   def ESNSquare = Rect(width="60", height="60")
      |   def ESNSquareTR = Polygon(points="0,0 60,0 60,60")
      |   def ESNDiamond = Polygon(points="30,0 60,30 30,60 0,30")
      |   def ESNDiamondT = Polygon(points="0,30 30,0 60,30")
      |   def ESNDiamondB = Polygon(points="60,30 30,60 0,30")
      |   def ESNDiamondL = Polygon(points="30,60 0,30 30,0")
      |   def ESNDiamondR = Polygon(points="30,0 60,30 30,60")
      |   def ESNTriangle = Polygon(points="0,60 30,0 60,60")
      |   def ESNStar = Star(n="5", r1="10", r2="30")
      |
      |   palette "Pentoses" { Rib, Ara, Xyl, Lyx }
      |   palette "Hexoses" { All, Alt, Glc, Man, Gul, Ido, Gal, Tal }
      |   palette "NAc" { All2NAc, Alt2NAc, Glc2NAc, Man2NAc, Gul2NAc, Ido2NAc, Gal2NAc, Tal2NAc }
      |   palette "N" { All2N, Alt2N, Glc2N, Man2N, Gul2N, Ido2N, Gal2N, Tal2N }
      |   palette "A" { All2COOH, Alt2COOH, Glc2COOH, Man2COOH, Gul2COOH, Ido2COOH, Gal2COOH, Tal2COOH }
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
      |   (All, Alt, Glc, Man, Gul, Ido, Gal, Tal) <COOH>
      |   -> #1 [secondary] ESNDiamond
      |   -> #2 [primary] ESNDiamondB
      |   -> #3 [thin] ESNDiamondB
      |   -> #4 [outline] ESNDiamond
      |
      |   (Fuc) <>
      |   -> #1 [primary] ESNTriangle
      |   -> #2 [outline] ESNTriangle
      |
      |   (Rib, Ara, Xyl, Lyx) <>
      |   -> #1 [primary] ESNStar
      |   -> #2 [outline] ESNStar
      |
      |   (All) -> style [primary] { fill: #7D007D }
      |   (Alt) -> style [primary] { fill: #FA6400 }
      |   (Glc) -> style [primary] { fill: #0000FA }
      |   (Man) -> style [primary] { fill: #00C832 }
      |   (Gul) -> style [primary] { fill: #FA78FA }
      |   (Ido) -> style [primary] { fill: #966432 }
      |   (Gal) -> style [primary] { fill: #FFFF00 }
      |   (Tal) -> style [primary] { fill: #FA0000 }
      |
      |   (Rib) -> style [primary] { fill: #0000FA }
      |   (Ara) -> style [primary] { fill: #00C832 }
      |   (Xyl) -> style [primary] { fill: #FA6400 }
      |   (Lyx) -> style [primary] { fill: #FFFF00 }
      |
      |   * -> style [outline] { fill: none; stroke: #000000; stroke-width: 3 }
      |   * -> style [thin] { fill: none; stroke: #000000; stroke-width: 1 }
      |   * -> style [secondary] { fill: #FFFFFF }
      |}
    """.stripMargin
}
