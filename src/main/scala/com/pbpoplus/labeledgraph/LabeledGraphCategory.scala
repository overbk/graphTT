package com.pbpoplus.labeledgraph

import com.pbpoplus.labeledgraph.{LabeledGraph, LabeledGraphMorphism}
import com.pbpoplus.categorytheory.{Cospan, Span, Topos}
import com.pbpoplus.util._

final case class LabeledGraphCategory[V, E, LV, LE]()(
  implicit freshV: FreshSetProducer[V], freshE: FreshSetProducer[E]
) extends Topos[LabeledGraph[V, E, LV, LE], LabeledGraphMorphism[V, E, V, E, LV, LE]]:

  private type LGraph = LabeledGraph[V, E, LV, LE]
  private type Morphism = LabeledGraphMorphism[V, E, V, E, LV, LE]

  def after(g: Morphism, f: Morphism): Morphism = g.after(f)
  def codomain(f: Morphism): LGraph = f.codomain
  def domain(f: Morphism): LGraph = f.domain
  def homSet(from: LGraph, to: LGraph): Set[Morphism] = from.homomorphisms(to)
  def identityArrow(graph: LGraph): Morphism = graph.identityArrow
  def epiRegularMonoFactorization(f: Morphism): (Morphism, Morphism) = 
    val image: LabeledGraph[V, E, LV, LE] =
      val vertices = f.mapV.values.toSet
      val edges = f.mapE.values.toSet
      val s = f.codomain.s.restrictDomainTo(edges)
      val t = f.codomain.t.restrictDomainTo(edges)
      val lV = f.codomain.lV.restrictDomainTo(vertices)
      val lE = f.codomain.lE.restrictDomainTo(edges)
      LabeledGraph(vertices, edges, s, t, lV, lE)

    val epi = LabeledGraphMorphism(f.domain, image, f.mapV, f.mapE)

    val regMono = LabeledGraphMorphism(image, f.codomain, image.vertices.identityMap,
      image.edges.identityMap)

    (epi, regMono)
  
  def isMonic(f: Morphism): Boolean = f.mapV.isInjective && f.mapE.isInjective
  
  def pullback(cospan: Cospan[Morphism]): Span[Morphism] =
    val (leftV, rightV) = cospan.left.mapV.pullbackWith(cospan.right.mapV)
    val (leftE, rightE) = cospan.left.mapE.pullbackWith(cospan.right.mapE)

    val vertices = leftV.keySet
    val edges = leftE.keySet
    val s = edges.mapTo((e1, e2) => (cospan.left.domain.s(e1), cospan.right.domain.s(e2)))
    val t = edges.mapTo((e1, e2) => (cospan.left.domain.t(e1), cospan.right.domain.t(e2)))
    val lV = vertices.mapTo((v, _) => cospan.left.domain.lV(v))
    val lE = edges.mapTo((e, _) => cospan.left.domain.lE(e))

    val vertexRenaming = freshV.create(vertices.size).zip(vertices)
    val renameV = vertexRenaming.toMap
    val renameVInv = vertexRenaming.map(_.swap).toMap 
    val renameE = freshE.create(edges.size).zip(edges).toMap
    
    val vertices1 = renameV.keySet
    val edges1 = renameE.keySet
    val s1 = renameVInv.after(s.after(renameE))
    val t1 = renameVInv.after(t.after(renameE))
    val lV1 = lV.after(renameV)
    val lE1 = lE.after(renameE)

    val pbObject = LabeledGraph(vertices1, edges1, s1, t1, lV1, lE1)

    val leftMorphism =
      val mapV = vertices1.mapTo(renameV(_)._1)
      val mapE = edges1.mapTo(renameE(_)._1)
      LabeledGraphMorphism(pbObject, cospan.left.domain, mapV, mapE)
    val rightMorphism =
      val mapV = vertices1.mapTo(renameV(_)._2)
      val mapE = edges1.mapTo(renameE(_)._2)
      LabeledGraphMorphism(pbObject, cospan.right.domain, mapV, mapE)

    Span(leftMorphism, rightMorphism)
  
  def isEpic(f: Morphism): Boolean = f.isSurjective

  lazy val initialObject: LGraph = LabeledGraph(Set(), Set(), Map(), Map(), Map(), Map())
  
  def pushout(span: Span[Morphism]): Cospan[Morphism] =
    val (leftV, rightV) = 
      val l = Fn(span.left.mapV, span.left.codomain.vertices)
      val r = Fn(span.right.mapV, span.right.codomain.vertices)
      l.pushoutWith(r)

    val (leftE, rightE) = 
      val l = Fn(span.left.mapE, span.left.codomain.edges)
      val r = Fn(span.right.mapE, span.right.codomain.edges)
      l.pushoutWith(r)

    val vertices = leftV.codomain
    val edges = leftE.codomain
    val s = edges.mapTo(equivSet => equivSet.head match
      case Left(e) => leftV(span.left.codomain.s(e))
      case Right(e) => rightV(span.right.codomain.s(e))
    )
    val t = edges.mapTo(equivSet => equivSet.head match
      case Left(e) => leftV(span.left.codomain.t(e))
      case Right(e) => rightV(span.right.codomain.t(e))
    )
    val lV = vertices.mapTo(equivSet => equivSet.head match
      case Left(v) => span.left.codomain.lV(v)
      case Right(v) => span.right.codomain.lV(v)
    )
    val lE = edges.mapTo(equivSet => equivSet.head match
      case Left(e) => span.left.codomain.lE(e)
      case Right(e) => span.right.codomain.lE(e)
    )

    val vertexRenaming = vertices.zip(freshV.create(vertices.size))
    val renameV = vertexRenaming.toMap
    val renameVInv = vertexRenaming.map(_.swap).toMap 
    val edgeRenaming = edges.zip(freshE.create(edges.size))
    val renameE = edgeRenaming.toMap
    val renameEInv = edgeRenaming.map(_.swap).toMap

    val vertices1 = renameV.values.toSet
    val edges1 = renameE.values.toSet
    val s1 = renameV.after(s.after(renameEInv))
    val t1 = renameV.after(t.after(renameEInv))
    val lV1 = lV.after(renameVInv)
    val lE1 = lE.after(renameEInv)

    val poObject = LabeledGraph(vertices1, edges1, s1, t1, lV1, lE1)

    val leftMorphism =
      val mapV = renameV.after(leftV.function)
      val mapE = renameE.after(leftE.function)
      LabeledGraphMorphism(span.left.codomain, poObject, mapV, mapE)
    val rightMorphism =
      val mapV = renameV.after(rightV.function)
      val mapE = renameE.after(rightE.function)
      LabeledGraphMorphism(span.right.codomain, poObject, mapV, mapE)

    Cospan(leftMorphism, rightMorphism)
  
  /**
    * In order to implement the two definitions below, types LV and LE need to have finite universes.
    * This adds complexity to the code base, which is not warranted for its current usage.
    */
  lazy val terminalObject: LGraph = ???

  def partialMapClassifierArrow(o: LGraph): Morphism = ???
end LabeledGraphCategory
