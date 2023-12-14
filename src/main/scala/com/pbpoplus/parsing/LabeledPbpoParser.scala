package com.pbpoplus.parsing

import com.pbpoplus.rewriting.{PbpoPlusRule, LabeledGraphPbpoPlusRule}
import com.pbpoplus.labeledgraph.{LabeledGraph, LabeledGraphMorphism}
import parsing.LabeledRuleParser
import com.pbpoplus.labeledgraph.LabeledGraphCategory

class LabeledPbpoParser extends LabeledRuleParser[LabeledGraphPbpoPlusRule[String]]:
  lazy val graphNames: Set[String] = Set("L", "L'", "K", "K'", "R")
  lazy val morphismNameToEndpoints: Map[String, (String, String)] =
    Map(
      "tL" -> ("L", "L'"),
      "tK" -> ("K", "K'"),
      "l" -> ("K", "L"),
      "l'" -> ("K'", "L'"),
      "r" -> ("K", "R")
    )

  def convertMapToRule(
    m: Map[String, LabeledGraphMorphism[String, String, String, String, String, String]]
  ): LabeledGraphPbpoPlusRule[String] =
    import com.pbpoplus.util.FreshSetProducer
    val category: LabeledGraphCategory[String, String, String, String] = LabeledGraphCategory()
    PbpoPlusRule(m("l"), m("r"), m("l'"), m("tL"), m("tK"))(category)
