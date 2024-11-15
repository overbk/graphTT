package com.pbpoplus.rewriting

import com.pbpoplus.categorytheory.Category
import com.pbpoplus.categorytheory.Quasitopos
import com.pbpoplus.categorytheory.Span

final case class DpoRule[O, A](l: A, r: A)(implicit category: Category[O, A])
  extends RewriteRule[O, A]:
  import category._
  val L: O = codomain(l)
  val K: O = domain(l)
  val R: O = codomain(r)

  require(domain(r) == K, "the domain of r must be K")

  lazy val toPbpoPlus: PbpoPlusRule[O, A] = 
    category match
      case qt: Quasitopos[O, A] =>
        require(qt.isRegularMonic(l), "morphism l needs to be regular monic")
        val tK = qt.partialMapClassifierArrow(K)
        val po = qt.pushout(Span(l, tK))
        val (tL, l1) = (po.left, po.right)
        PbpoPlusRule(l, r, l1, tL, tK)(qt)
      case _ => throw Exception("DPO to PBPO+ translation is only defined in quasitoposes")

  def matchInducedStep(m: A): Set[(O, A)] =
    category match 
      case qt: Quasitopos[O, A] =>
        require(qt.isRegularMonic(m), "morphism l needs to be regular monic")
        toPbpoPlus.matchInducedStep(m)
      case _ => throw Exception("DPO for non-quasitoposes is currently unsupported")
    
end DpoRule
