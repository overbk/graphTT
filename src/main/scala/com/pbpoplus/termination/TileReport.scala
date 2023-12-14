package com.pbpoplus.termination

final case class TileReport[O, A](
  tileInfo: WeightedTileInfo[O, A],
  slideReport: TileSlideReport[O, A],
  validTilingsIsoInR: Set[A],
  aLargestValidTilingOfL: Set[A]
):
  import tileInfo._

  val counting = countingClass match
    case CountingClass.RegularMono => "REGULAR MONOS only"
    case CountingClass.Mono => "MONOS only"
    case CountingClass.Homomorphism => "all HOMOMORPHISMS"
  
  override def toString = 
    s"""
    |~~~ Tile $name with weight $weight, and counting $counting
    |  $stringRepresentation
    |
    |- The tiling of R has size:             ${validTilingsIsoInR.size}
    |- Giving a weight of:                   ${validTilingsIsoInR.size} * $weight = ${validTilingsIsoInR.size * weight}
    |- A largest valid tiling of L has size: ${aLargestValidTilingOfL.size}
    |- Giving a weight of:                   ${aLargestValidTilingOfL.size} * $weight = ${aLargestValidTilingOfL.size * weight}
    |
    |Slide data:
    |${slideReport.toString}""".mkString
