package com.pbpoplus.parsing

import com.pbpoplus.labeledgraph.LabeledGraph

import scala.util.matching.Regex
import scala.util.parsing.combinator._

private sealed trait LabeledEdge:
  def id: String
  def label: String
private final case class LabeledLeftEdge(id: String, label: String) extends LabeledEdge
private final case class LabeledRightEdge(id: String, label: String) extends LabeledEdge

private final case class LabeledVertex(id: String, label: String)

class LabeledGraphParser extends RegexParsers:
  override val skipWhitespace = true
  override val whiteSpace: Regex = "[ \t\r\f\n]+".r
  val any: Regex = ".".r

  protected def comment : Parser[Unit] = // changed from None
    "/*" ~> rep(not("*/") ~ any) ~> "*/"^^ { _ => () }

  protected def identifier: Parser[String] = """[A-Za-z0-9'.]+""".r
  private def vertexIdentifier: Parser[String] = identifier
  private def edgeIdentifier: Parser[String] = identifier
  private def label: Parser[String] = identifier

  private def labeledVertex: Parser[LabeledVertex] =
    vertexIdentifier ~ (":" ~> label) ^^ { case id ~ label => LabeledVertex(id, label) }

  private def leftEdges: Parser[List[LabeledLeftEdge]] =
    "<-" ~> (rep1(edgeIdentifier) ~ (":" ~> label)) <~ ("-") ^^ { case ids ~ label => 
      ids.map(LabeledLeftEdge(_, label)) 
    }
  def rightEdges: Parser[List[LabeledRightEdge]] = 
    "-" ~> (rep1(edgeIdentifier) ~ (":" ~> label)) <~ "->" ^^ { case ids ~ label => 
      ids.map(LabeledRightEdge(_, label)) 
    }
  private def labeledEdges: Parser[List[LabeledEdge]] =
    rep1(leftEdges | rightEdges) ^^ { _.flatten }

  def edgeString: Parser[LabeledGraph[String, String, String, String]] =
    labeledVertex ~ labeledEdges ~ labeledVertex ^^ { case v ~ labeledEdges ~ w =>
      val vertices = Set(v.id, w.id)
      val flattenedEdges = labeledEdges.toSet
      val edges = flattenedEdges.map(_.id)
      val s: Map[String, String] = flattenedEdges.map(_ match
        case LabeledLeftEdge(id, _) => id -> v.id
        case LabeledRightEdge(id, _) => id -> w.id
      ).toMap
      val t: Map[String, String] = flattenedEdges.map(_ match
        case LabeledLeftEdge(id, _) => id -> w.id
        case LabeledRightEdge(id, _) => id -> v.id
      ).toMap
      val lV: Map[String, String] = Map(v.id -> v.label, w.id -> w.label)
      val lE: Map[String, String] = flattenedEdges.map(e => e.id -> e.label).toMap
      LabeledGraph(vertices, edges, s, t, lV, lE)
    }

  def labeledGraph: Parser[LabeledGraph[String, String, String, String]] =
    rep(edgeString | labeledVertex) ^^ { tokens => 
      val graphParts: List[LabeledGraph[String, String, String, String]] = tokens.map(_ match
        case LabeledVertex(id, l) => LabeledGraph.empty.copy(vertices = Set(id), lV = Map(id -> l))
        case g: LabeledGraph[String, String, String, String] => g
      )
      graphParts.foldLeft(LabeledGraph.empty) { case (acc, g) => acc.union(g) }
    }
end LabeledGraphParser
