package com.pbpoplus.rewriting

import scala.annotation.targetName
import com.pbpoplus.categorytheory.{Category, Span, Cospan}
import com.pbpoplus.labeledgraph.{LabeledGraph, LabeledGraphMorphism, LabeledGraphCategory}
import com.pbpoplus.util.FreshSetProducer

final case class PbpoPlusRule[O, A](l: A, r: A, l1: A, tL: A, tK: A)
  (implicit category: Category[O, A]) extends RewriteRule[O, A]:
  import category._
  
  require(domain(l) == domain(r), "the domain of morphisms l and r must coincide")
  require(isPullbackFor(Span(l, tK), Cospan(tL, l1)), 
    "the rewrite rule must contain a pullback square")

  val L = codomain(l)
  val K = domain(l)
  val R = codomain(r)
  val L1 = codomain(l1)
  val K1 = domain(l1)

  private lazy val po: Option[Cospan[A]] = maybePushout(Span(tK, r))
  lazy val r1: Option[A] = po.map(_.left)
  lazy val tR: Option[A] = po.map(_.right)
  lazy val R1: Option[O] = r1.map(codomain)

  @targetName("applyToObject")
  def apply(host: O): Set[(O, (A, A))] = 
    for {
      alpha <- homSet(host, L1)
      (gR, m) <- adherenceInducedStep(alpha)
    } yield (gR, (m, alpha))

  /*  === Rewrite step functions
      Use the following rewrite step diagram as a reference:

      L >--m-> G_L <--gL--- G_K <--u---< K0 >--u--> G_K
      |         |            |           |           |
      =   pb0  alpha  pb1   u1    pb2   iso          |
      |         v            v           |           |      
      L >-tL--> L' <--l1--- K' <--tK---< K    po     gR
                                         |           |
                                         r           |
                                         v           v
                                         R >---w--> G_R
  */

  def adherenceInducedStep(alpha: A): Option[(O, A)] =
    require(codomain(alpha) == L1, "the codomain of alpha needs to be L'")
    for {
      pb0 <- maybePullback(Cospan(tL, alpha))
      if isIso(pb0.left)
      m = pb0.right o inverseOf(pb0.left).get
      pb1 <- maybePullback(Cospan(alpha, l1))
      u1 = pb1.right
      pb2 <- maybePullback(Cospan(u1, tK))
      u = pb2.left
      iso = pb2.right
      po <- maybePushout(Span(r o iso, u))
      gR = po.sink
    } yield (gR, m)

  def matchInducedStep(m: A): Set[(O, A)] =
    require(domain(m) == L,  "the domain of m needs to be L")
    val gL = codomain(m)
    for {
      alpha <- homSet(gL, L1)
      pb <- maybePullback(Cospan(tL, alpha))
      if areIsomorphic(pb, Span(identityArrow(L), m))
      (gR, _) <- adherenceInducedStep(alpha)
    } yield (gR, alpha)

end PbpoPlusRule
