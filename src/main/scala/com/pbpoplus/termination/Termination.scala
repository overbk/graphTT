package com.pbpoplus.termination

import com.pbpoplus.categorytheory.Cospan
import com.pbpoplus.categorytheory.Span
import com.pbpoplus.categorytheory.TerminationCategory
import com.pbpoplus.rewriting.PbpoPlusRule
import com.pbpoplus.util._

object Termination:
  def slideOptions[O, A](
      tiling: A,
      rho: PbpoPlusRule[O, A],
      countingClass: CountingClass
  )(implicit category: TerminationCategory[O, A]): Set[A] =
    import category.{codomain, pullback, rightInversesFor}
    require(codomain(tiling) == rho.R1.get)

    /*
     * L1 <-l1- K1 -r1-> R1
     *          ^        ^
     *          |        |
     *          |   PB  tiling
     *          |        |
     *          X -----> T
     */

    val `K1 <- X -> T` = pullback(Cospan(rho.r1.get, tiling))
    val `K1 <- X`: A = `K1 <- X -> T`.left
    val `X -> T`: A = `K1 <- X -> T`.right
    val `{K1 <- X <- T}`: Set[A] = rightInversesFor(`X -> T`).map(`K1 <- X` o _)

    val inClass = countingClass.predicate
    val secondFactor = countingClass.secondFactor

    `{K1 <- X <- T}`.view
      .filter(f => inClass(rho.l1 o secondFactor(f)))
      .map(rho.l1 o _)
      .toSet

  def slideOptionsForSet[O, A](
      tilings: Set[A],
      rho: PbpoPlusRule[O, A],
      countingClass: CountingClass
  )(implicit category: TerminationCategory[O, A]): Set[Set[A]] =
    val optionsPerTiling: List[Set[A]] =
      tilings.toList.map(slideOptions(_, rho, countingClass))
    val noncollidingCombinations: Set[Seq[A]] = noncollidingChoices(
      optionsPerTiling
    )
    noncollidingCombinations.map(_.toSet): Set[Set[A]]

  def createSlideReport[O, A](
      tile: O,
      rho: PbpoPlusRule[O, A],
      countingClass: CountingClass
  )(implicit category: TerminationCategory[O, A]): TileSlideReport[O, A] =
    import category.{homSet, pullback, isIso}

    val inClass = countingClass.predicate

    /*  for all tilings t in Hom(T, R1):
     *
     *  tile <--iso?--- X
     *   |              |
     *   t     PB   in class?
     *   |              |
     *   V              V
     *   R1 <---tR----- R
     */

    val `Hom(tile, R1)`: Set[A] = homSet(tile, rho.R1.get)
    val tilingToPb: Map[A, Span[A]] =
      `Hom(tile, R1)`.mapTo(t => pullback(Cospan(t, rho.tR.get)))
    val relevantTilings: Set[A] = tilingToPb.collect {
      case (t, pb) if inClass(pb.right) => t
    }.toSet
    val (isoInR, notIsoInR) =
      relevantTilings.partition(t => isIso(tilingToPb(t).left))
    val slideOptions = slideOptionsForSet(notIsoInR, rho, countingClass)

    TileSlideReport(
      tile = tile,
      rule = rho,
      tilingsIntoR1 = `Hom(tile, R1)`,
      validTilingsIsoInR = isoInR,
      validTilingsNonisoInR = notIsoInR,
      slideOptions = slideOptions
    )

  def createTileReport[O, A](
      tileInfo: WeightedTileInfo[O, A],
      rho: PbpoPlusRule[O, A]
  )(implicit category: TerminationCategory[O, A]): TileReport[O, A] =
    import category.{homSet, isMonic}
    import tileInfo._

    val inClass = countingClass.predicate
    val slideReport = createSlideReport(tile, rho, countingClass)
    val aLargestValidTilingOfL: Set[A] =
      val candidatesIntoL: Set[A] =
        homSet(tile, rho.L).filter(f => inClass(rho.tL o f))
      if isMonic(rho.tK) // a shortcut afforded by Lemma 44 (arXiv)
      then candidatesIntoL
      else
        slideReport.slideOptions
          .map(slidTiles =>
            candidatesIntoL.filter(f => !slidTiles.contains(rho.tL o f))
          )
          .maxBy(_.size)
    TileReport(
      tileInfo,
      slideReport = slideReport,
      validTilingsIsoInR = slideReport.validTilingsIsoInR,
      aLargestValidTilingOfL = aLargestValidTilingOfL
    )

  def createRuleReport[O, A](
      weightedTiles: Set[WeightedTileInfo[O, A]],
      ruleName: String,
      rule: PbpoPlusRule[O, A]
  )(implicit category: TerminationCategory[O, A]): RuleReport[O, A] =
    val toTileReport =
      weightedTiles.mapTo(t => createTileReport(t, rule))
    val weightOfR =
      weightedTiles.toList
        .map(t => toTileReport(t).validTilingsIsoInR.size * t.weight)
        .sum
    val weightOfDelta =
      weightedTiles.toList
        .map(t => toTileReport(t).aLargestValidTilingOfL.size * t.weight)
        .sum
    val slidingIsSuccessful =
      toTileReport.forall((_, report) =>
        report.slideReport.slideOptions.nonEmpty
      )
    val ruleIsProvablyDecreasing =
      slidingIsSuccessful && (weightOfDelta > weightOfR)
    val ruleIsProvablyNonincreasingButNotProvablyDecreasing =
      slidingIsSuccessful && (weightOfDelta == weightOfR)

    RuleReport(
      rule = rule,
      ruleName,
      toTileReport,
      weightOfR,
      weightOfDelta,
      slidingIsSuccessful,
      ruleIsProvablyDecreasing,
      ruleIsProvablyNonincreasingButNotProvablyDecreasing
    )

  def createSystemReport[O, A](
      weightedTiles: Set[WeightedTileInfo[O, A]],
      system: Map[String, PbpoPlusRule[O, A]]
  )(implicit category: TerminationCategory[O, A]): SystemReport[O, A] =
    val toRuleReport =
      system.map { case (name, rule) =>
        name -> createRuleReport(weightedTiles, name, rule)
      }.toMap
    val slidingIsGloballySuccessful =
      toRuleReport.forall((_, report) => report.slidingIsSuccessful)
    val provablyDecreasingRules =
      system.filter((name, rule) => toRuleReport(name).ruleIsProvablyDecreasing)
    val provablyNonincreasingButNotProvablyDecreasingRules =
      system.filter((name, rule) =>
        toRuleReport(name).ruleIsProvablyNonincreasing
      )
    val prunedSystem =
      if slidingIsGloballySuccessful
      then system -- provablyDecreasingRules.keys
      else system

    SystemReport(
      system,
      toRuleReport,
      slidingIsGloballySuccessful,
      provablyDecreasingRules,
      provablyNonincreasingButNotProvablyDecreasingRules,
      prunedSystem
    )

  def isTerminationProof[O, A](
      weightedTiles: Set[WeightedTileInfo[O, A]],
      system: Map[String, PbpoPlusRule[O, A]]
  )(implicit category: TerminationCategory[O, A]): Boolean =
    val report = createSystemReport(weightedTiles, system)
    report.prunedSystem.isEmpty

end Termination
