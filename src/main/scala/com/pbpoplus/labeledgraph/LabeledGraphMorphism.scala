package com.pbpoplus.labeledgraph

import com.pbpoplus.util.after
import com.pbpoplus.util.isSurjectiveOn

object LabeledGraphMorphism:
  def isValid[V1, E1, V2, E2, LV, LE](
      domain: LabeledGraph[V1, E1, LV, LE],
      codomain: LabeledGraph[V2, E2, LV, LE],
      mapV: Map[V1, V2],
      mapE: Map[E1, E2]
  ): Boolean =
    mapV.keySet == domain.vertices
      && mapE.keySet == domain.edges
      && mapV.values.toSet.subsetOf(codomain.vertices)
      && mapE.values.toSet.subsetOf(codomain.edges)
      && mapV.forall((v, w) => domain.lV(v) == codomain.lV(w))
      && mapE.forall((e, f) =>
        domain.lE(e) == codomain.lE(f)
          && mapV(domain.s(e)) == codomain.s(f)
          && mapV(domain.t(e)) == codomain.t(f)
      )
end LabeledGraphMorphism

final case class LabeledGraphMorphism[V1, E1, V2, E2, LV, LE](
    domain: LabeledGraph[V1, E1, LV, LE],
    codomain: LabeledGraph[V2, E2, LV, LE],
    mapV: Map[V1, V2],
    mapE: Map[E1, E2]
):
  require(
    mapV.keySet == domain.vertices,
    "mapV must be defined exactly on the set of vertices of the domain"
  )
  require(
    mapE.keySet == domain.edges,
    "mapE must be defined exactly on the set of edges of the domain"
  )
  require(
    mapV.values.toSet.subsetOf(codomain.vertices),
    "the image of mapV must be a subset of the set of vertices of the codomain"
  )
  require(
    mapE.values.toSet.subsetOf(codomain.edges),
    "the image of mapV must be a subset of the set of edges of the codomain"
  )
  require(
    mapV.forall((v, w) => domain.lV(v) == codomain.lV(w)),
    "mapV must preserve labels"
  )
  require(
    mapE.forall((e, f) => domain.lE(e) == codomain.lE(f)),
    "mapE must preserve labels"
  )
  require(
    mapE.forall((e, f) => mapV(domain.s(e)) == codomain.s(f)),
    "mapE preserve sources under mapV"
  )
  require(
    mapE.forall((e, f) => mapV(domain.t(e)) == codomain.t(f)),
    "mapE preserve targets under mapV"
  )

  def after[V3, E3](
      g: LabeledGraphMorphism[V3, E3, V1, E1, LV, LE]
  ): LabeledGraphMorphism[V3, E3, V2, E2, LV, LE] =
    LabeledGraphMorphism(
      g.domain,
      codomain,
      mapV.after(g.mapV),
      mapE.after(g.mapE)
    )

  def isSurjective: Boolean =
    mapV.isSurjectiveOn(codomain.vertices) && mapE.isSurjectiveOn(
      codomain.edges
    )
end LabeledGraphMorphism
