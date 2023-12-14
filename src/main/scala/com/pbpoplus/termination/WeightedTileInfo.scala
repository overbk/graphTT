package com.pbpoplus.termination

import com.pbpoplus.termination.CountingClass

final case class WeightedTileInfo[O, A](
  tile: O,
  weight: Int,
  name: String,
  stringRepresentation: String,
  countingClass: CountingClass
)
