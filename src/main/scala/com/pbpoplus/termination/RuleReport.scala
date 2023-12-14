package com.pbpoplus.termination

import com.pbpoplus.rewriting.PbpoPlusRule

final case class RuleReport[O, A](
  rule: PbpoPlusRule[O, A],
  name: String,
  toTileReport: Map[WeightedTileInfo[O, A], TileReport[O, A]],
  weightOfR: Int,
  weightOfDelta: Int,
  slidingIsSuccessful: Boolean,
  ruleIsProvablyDecreasing: Boolean,
  ruleIsProvablyNonincreasing: Boolean
):
  override def toString = 
    val slideResult = 
      if slidingIsSuccessful then "SUCCESSFUL" else "UNSUCCESSFUL"
    val measureStatus =
      if ruleIsProvablyDecreasing
      then "the rule is PROVABLY DECREASING"
      else if ruleIsProvablyNonincreasing
            then "the rule is PROVABLY NONINCREASING"
            else "the rule is POSSIBLY INCREASING"

    s""">>>>>>>>>>>>>>> rule $name <<<<<<<<<<<<<<<
        |Summary:
        |  - The sliding is $slideResult.
        |  - The weight of Delta is $weightOfDelta.
        |  - The weight of R is $weightOfR.
        |  - Conclusion: $measureStatus.
        |
        |The details per tile for this rule now follow.
        |  ${toTileReport.toList.map(_._2).mkString("\n  |")}""".mkString
  