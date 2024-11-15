package com.pbpoplus.termination

final case class WeightedTileInfo[O, A](
    tile: O,
    weight: Int,
    name: String,
    stringRepresentation: String,
    countingClass: CountingClass
)
