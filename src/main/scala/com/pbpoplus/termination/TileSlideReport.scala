package com.pbpoplus.termination

import com.pbpoplus.rewriting.PbpoPlusRule
import com.pbpoplus.labeledgraph.LabeledGraph

final case class TileSlideReport[O, A](
  tile: O,
  rule: PbpoPlusRule[O, A],
  tilingsIntoR1: Set[A], // including invalid ones, i.e., for which pb(tile, t_R) is not in class
  validTilingsIsoInR: Set[A], // pb(tile, t_R) in class and pb(t_R, tile) is iso
  validTilingsNonisoInR: Set[A], // pb(tile, t_R) in class and pb(t_R, tile) is noniso
  slideOptions: Set[Set[A]] // tiles that are slid may have multiple candidates into L'. slideOptions contains all sets of choices
):
  override def toString =
    s"""
    |# morphisms into R':           ${tilingsIntoR1.size}
    |# of which valid:              ${validTilingsIsoInR.size + validTilingsNonisoInR.size}
    |# iso in R:                    ${validTilingsIsoInR.size}
    |# noniso in R:                 ${validTilingsNonisoInR.size}
    |# number of ways to slide:     ${slideOptions.size}""".mkString
