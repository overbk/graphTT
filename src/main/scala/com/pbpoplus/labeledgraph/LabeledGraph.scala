package com.pbpoplus.labeledgraph

import cats.syntax.all._
import com.pbpoplus.util._

object LabeledGraph:
  def empty[T]: LabeledGraph[T, T, T, T] =
    LabeledGraph(
      Set.empty,
      Set.empty,
      Map.empty,
      Map.empty,
      Map.empty,
      Map.empty
    )

final case class LabeledGraph[V, E, LV, LE](
    vertices: Set[V],
    edges: Set[E],
    s: Map[E, V],
    t: Map[E, V],
    lV: Map[V, LV],
    lE: Map[E, LE]
):
  require(s.keySet == edges, "the domain of s must be the set of edges")
  require(t.keySet == edges, "the domain of t must be the set of edges")
  require(
    s.values.toSet.subsetOf(vertices),
    "the image of s must be a subset of the set of vertices"
  )
  require(
    t.values.toSet.subsetOf(vertices),
    "the image of t must be a subset of the set of vertices"
  )
  require(lV.keySet == vertices, "the domain of lV must be the set of vertices")
  require(lE.keySet == edges, "the domain of lE must be the set of edges")

  private type LGraph = LabeledGraph[V, E, LV, LE]

  val vertexLabels: Set[LV] = lV.values.toSet
  val edgeLabels: Set[LE] = lE.values.toSet
  val nrOfVertices: Int = vertices.size
  val nrOfEdges: Int = edges.size
  lazy val isEmpty: Boolean = this == LabeledGraph.empty
  lazy val isolatedVertices: Set[V] = vertices -- s.values -- t.values
  lazy val identityArrow: LabeledGraphMorphism[V, E, V, E, LV, LE] =
    LabeledGraphMorphism(this, this, vertices.identityMap, edges.identityMap)

  def union(that: LGraph): LGraph =
    require(
      edges
        .intersect(that.edges)
        .forall(e => s(e) == that.s(e) && t(e) == that.t(e))
    )
    LabeledGraph(
      vertices ++ that.vertices,
      edges ++ that.edges,
      s ++ that.s,
      t ++ that.t,
      lV = lV ++ that.lV,
      lE = lE ++ that.lE
    )

  ///////////////////////////////////////////////////////////////////////////
  // Useful vocabulary and precomputations.
  ///////////////////////////////////////////////////////////////////////////

  private val outgoingEdgeMap: Map[V, Map[V, Set[E]]] =
    val empty: Map[V, Map[V, Set[E]]] = vertices.map(_ -> Map.empty).toMap
    edges.foldLeft(empty) { case (acc, e) =>
      val source = s(e)
      val target = t(e)
      val updatedAtSource: Map[V, Set[E]] = acc(source).updatedWith(target) {
        case None          => Some(Set(e))
        case Some(edgeSet) => Some(edgeSet + e)
      }
      acc.updated(source, updatedAtSource)
    }

  private val incomingEdgeMap: Map[V, Map[V, Set[E]]] =
    val empty: Map[V, Map[V, Set[E]]] = vertices.map(_ -> Map.empty).toMap
    edges.foldLeft(empty) { case (acc, e) =>
      val source = s(e)
      val target = t(e)
      val updatedAtTarget: Map[V, Set[E]] = acc(target).updatedWith(source) {
        case None          => Some(Set(e))
        case Some(edgeSet) => Some(edgeSet + e)
      }
      acc.updated(target, updatedAtTarget)
    }

  private implicit class VertexOps(v: V):
    def label: LV = lV(v)
    def edgesOutgoingTo(w: V): Set[E] =
      outgoingEdgeMap(v).getOrElse(w, Set.empty)
    def labeledEdgesOutgoingTo(w: V, l: LE): Set[E] =
      edgesOutgoingTo(w).filter(lE(_) == l)
    def edgesIncomingFrom(w: V): Set[E] =
      incomingEdgeMap(v).getOrElse(w, Set.empty)
    def labeledEdgesIncomingFrom(w: V, l: LE): Set[E] =
      edgesIncomingFrom(w).filter(lE(_) == l)
    def edgesBetween(w: V): Set[E] = edgesOutgoingTo(w) ++ edgesIncomingFrom(w)
    def outgoingEdges: Set[E] = outgoingEdgeMap(v).flatMap(_._2).toSet
    def outgoingLabels: Set[LE] = outgoingEdges.map(lE)
    def incomingEdges: Set[E] = incomingEdgeMap(v).flatMap(_._2).toSet
    def incomingLabels: Set[LE] = incomingEdges.map(lE)
    def loops: Set[E] = edgesOutgoingTo(v)
    def loopLabels: Set[LE] = edgesOutgoingTo(v).map(lE)
    def outdegree: Int = outgoingEdges.size
    def indegree: Int = incomingEdges.size
    def outNeighbors: Set[V] =
      outgoingEdges.map(t) // A node its own neighbor iff it has a loop.
    def inNeighbors: Set[V] = incomingEdges.map(s)
    def neighbors: Set[V] = inNeighbors ++ outNeighbors
    def isOutNeighborOf(w: V): Boolean = inNeighbors.contains(w)
    def isInNeighborOf(w: V): Boolean = outNeighbors.contains(w)
    def isNeighborOf(w: V): Boolean = neighbors.contains(w)
  end VertexOps

  private def outNeighborsOf(v: V): Set[V] = v.outNeighbors
  private def inNeighborsOf(v: V): Set[V] = v.inNeighbors

  final case class NeighborView(
      neighborLabel: LV,
      edgesTo: Multiset[LE],
      edgesFrom: Multiset[LE]
  )

  final case class VertexShape(
      label: LV,
      neighborViews: Multiset[NeighborView],
      loops: Multiset[LE]
  )

  lazy val shapeOf: Map[V, VertexShape] =
    vertices.mapTo(v =>
      val label: LV = lV(v)
      val neighborViews: Multiset[NeighborView] =
        val neighbors = Multiset(v.neighbors - v)
        neighbors.map(neighbor =>
          val neighborLabel = lV(neighbor)
          val edgesTo: Multiset[LE] =
            Multiset(
              v.edgesOutgoingTo(neighbor)
                .groupBy(lE)
                .view
                .mapValues(_.size)
                .toMap
            )
          val edgesFrom: Multiset[LE] =
            Multiset(
              v.edgesIncomingFrom(neighbor)
                .groupBy(lE)
                .view
                .mapValues(_.size)
                .toMap
            )
          NeighborView(neighborLabel, edgesTo, edgesFrom)
        )
      val loops: Multiset[LE] =
        Multiset(v.loops.groupBy(lE).view.mapValues(_.size).toMap)
      VertexShape(label, neighborViews, loops)
    )

  private def verticesOfShape(shape: VertexShape): Set[V] =
    vertices.filter(shapeOf(_) == shape)

  ///////////////////////////////////////////////////////////////////////////
  // Homomorphism classes.
  ///////////////////////////////////////////////////////////////////////////

  /** Generates all homomorphisms of some class from this graph to targetGraph.
    * Vertices are checked for compatibility according to some compatibility
    * notion (parameter maybeCompatibility). Parameter injectiveOnly is used to
    * optimize the enumeration.
    */

  private def generateHomomorphisms[V2, E2](
      maybeCompatible: V => V2 => Boolean, // may overapproximate (but never underapproximate)
      targetGraph: LabeledGraph[V2, E2, LV, LE],
      injectiveOnly: Boolean
  ): Set[LabeledGraphMorphism[V, E, V2, E2, LV, LE]] =
    def iterate(
        toPair: Map[V, Set[V2]],
        vertexMap: Map[V, V2]
    ): Set[Map[V, V2]] =
      if toPair.isEmpty
      then Set(vertexMap)
      else
        val vertexWithSmallestCompatibilitySet: V =
          toPair.keyWithMinimalValue(_.size).get
        val possibleTargets = toPair(vertexWithSmallestCompatibilitySet)
        possibleTargets.flatMap(target =>
          val newVertexMap =
            vertexMap + (vertexWithSmallestCompatibilitySet -> target)
          val newToPair: Map[V, Set[V2]] =
            toPair
              .removed(vertexWithSmallestCompatibilitySet)
              .transform((w, candidates) =>
                candidates
                  .applyIf(_ - target, injectiveOnly)
                  .applyIf(
                    _.restrictTo(targetGraph.outNeighborsOf(target)),
                    w.isOutNeighborOf(vertexWithSmallestCompatibilitySet)
                  )
                  .applyIf(
                    _.restrictTo(targetGraph.inNeighborsOf(target)),
                    w.isInNeighborOf(vertexWithSmallestCompatibilitySet)
                  )
              )
          iterate(newToPair, newVertexMap)
        )

    val compatibleWith: Map[V, Set[V2]] =
      vertices.mapTo(v => targetGraph.vertices.filter(maybeCompatible(v)))

    val vertexMaps: Set[Map[V, V2]] = iterate(compatibleWith, Map.empty)

    for {
      vertexMap <- vertexMaps
      compatibleEdges: Map[E, Set[E2]] = edges.mapTo(e =>
        targetGraph
          .VertexOps(vertexMap(s(e)))
          .labeledEdgesOutgoingTo(vertexMap(t(e)), lE(e))
      )
      edgeMap: Map[E, E2] <-
        if injectiveOnly
        then allInjectiveChoices(compatibleEdges)
        else allChoices(compatibleEdges)
      if LabeledGraphMorphism.isValid(this, targetGraph, vertexMap, edgeMap)
      // above condition guards again overapproximation by maybeCompatible
    } yield LabeledGraphMorphism(this, targetGraph, vertexMap, edgeMap)
  end generateHomomorphisms

  def homomorphisms[V2, E2](
      targetGraph: LabeledGraph[V2, E2, LV, LE]
  ): Set[LabeledGraphMorphism[V, E, V2, E2, LV, LE]] =
    val that = targetGraph.VertexOps
    val compatible: V => V2 => Boolean = v =>
      w =>
        (v.label == targetGraph.VertexOps(w).label)
          && (v.incomingLabels subsetOf that(w).incomingLabels)
          && (v.outgoingLabels subsetOf that(w).outgoingLabels)
          && (v.loopLabels subsetOf that(w).loopLabels)

    generateHomomorphisms(compatible, targetGraph, false)

  def isomorphisms[V2, E2](
      targetGraph: LabeledGraph[V2, E2, LV, LE]
  ): Set[LabeledGraphMorphism[V, E, V2, E2, LV, LE]] =
    if nrOfVertices != targetGraph.nrOfVertices || nrOfEdges != targetGraph.nrOfEdges
    then Set()
    else
      val compatible: V => V2 => Boolean = v =>
        w => shapeOf(v) == targetGraph.shapeOf(w)

      generateHomomorphisms(compatible, targetGraph, true)

  def injections[V2, E2](
      targetGraph: LabeledGraph[V2, E2, LV, LE]
  ): Set[LabeledGraphMorphism[V, E, V2, E2, LV, LE]] =
    if nrOfVertices > targetGraph.nrOfVertices || nrOfEdges > targetGraph.nrOfEdges
    then Set()
    else
      val that = targetGraph.VertexOps
      def maybeCompatible: V => V2 => Boolean =
        v =>
          w =>
            v.label == that(w).label
              && v.incomingEdges.toMultiset
                .map(lE)
                .subsetOf(that(w).incomingEdges.toMultiset.map(targetGraph.lE))
              && v.outgoingEdges.toMultiset
                .map(lE)
                .subsetOf(that(w).outgoingEdges.toMultiset.map(targetGraph.lE))
              && v.loops.toMultiset
                .map(lE)
                .subsetOf(that(w).loops.toMultiset.map(targetGraph.lE))

      generateHomomorphisms(maybeCompatible, targetGraph, true)

  ///////////////////////////////////////////////////////////////////////////
  // String and DOT representation.
  ///////////////////////////////////////////////////////////////////////////

  override def toString: String =
    val edgeStrings = edges.map(e =>
      val src = s(e)
      val tgt = t(e)
      s"$src:${lV(src)} -$e:${lE(e)}-> $tgt:${lV { tgt }}"
    )
    val vertexStrings = isolatedVertices.map(v => s"$v:${lV(v)}")
    val sep =
      if edgeStrings.isEmpty || vertexStrings.isEmpty then "" else "\n   "
    "{ " + edgeStrings.mkString("\n") + sep + vertexStrings.mkString(" ") + " }"

  def dottify: String =
    val vertexLines = vertices.map(v => s"\"${v}\" [xlabel=\"${lV((v))}\"];")
    val edgeLines =
      edges.map(e => s"\"${s(e)}\" -> \"${t(e)}\" [label=\"${lE(e)}\"];")
    val lines = (vertexLines ++ edgeLines).map("  " + _).mkString("\n")
    s"""digraph G {
    |  forcelabels=true
    |$lines
    |}""".stripMargin

end LabeledGraph
