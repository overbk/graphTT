package com.pbpoplus.termination

import com.pbpoplus.rewriting.PbpoPlusRule

final case class SystemReport[O, A](
    system: Map[String, PbpoPlusRule[O, A]],
    toRuleReport: Map[String, RuleReport[O, A]],
    slidingIsSuccessful: Boolean,
    provablyDecreasingRules: Map[String, PbpoPlusRule[O, A]],
    provablyNonincreasingRules: Map[String, PbpoPlusRule[O, A]],
    prunedSystem: Map[String, PbpoPlusRule[O, A]]
):
  val ruleNames = system.keys.toSet

  override def toString: String =
    val ruleReports =
      toRuleReport.map((rule, report) => report.toString).toList.mkString("\n")

    val possiblyIncreasingRules =
      system -- provablyDecreasingRules.keys -- provablyNonincreasingRules.keys

    val slidingSuccess = if slidingIsSuccessful then "yes" else "no"

    s"""
        |=============== SYSTEM TERMINATION REPORT ===============
        |---------------          SUMMARY          ---------------
        |
        |The system has ${system.size} rules, named: ${ruleNames.mkString(", ")}
        |Was the sliding successful for every rule? $slidingSuccess
        |Provably decreasing rules: ${provablyDecreasingRules
        .map(_._1)
        .mkString(", ")}
        |Provably nonincreasing (but not provably decreasing) rules: ${provablyNonincreasingRules
        .map(_._1)
        .mkString(", ")}
        |Possibly increasing rules: ${possiblyIncreasingRules
        .map(_._1)
        .mkString(", ")}
        |The pruned system contains rules: ${prunedSystem.keys.mkString(", ")}
        |
        |${
        if prunedSystem.isEmpty
        then "The pruned system is empty, so the system is TERMINATING."
        else "More iterations are needed on the pruned system."
      }
        |
        |---------------   DETAILED RULE REPORTS   ---------------
        |
        |${ruleReports}
        |""".stripMargin
  end toString
