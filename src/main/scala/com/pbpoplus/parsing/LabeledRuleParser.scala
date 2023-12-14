package parsing

import com.pbpoplus.labeledgraph.LabeledGraph
import com.pbpoplus.labeledgraph.LabeledGraphMorphism
import com.pbpoplus.rewriting.RewriteRule
import com.pbpoplus.parsing.LabeledGraphParser
import com.pbpoplus.labeledgraph.LabeledGraphMorphism
import com.pbpoplus.util.mapTo

private type LGraph = LabeledGraph[String, String, String, String]
private type LGraphMorphism = LabeledGraphMorphism[String, String, String, String, String, String]

trait LabeledRuleParser[T <: RewriteRule[LGraph, LGraphMorphism]] extends LabeledGraphParser:
  lazy val graphNames: Set[String]
  lazy val morphismNameToEndpoints: Map[String, (String, String)]
  def convertMapToRule(m: Map[String, LGraphMorphism]): T

  private val nrOfGraphs = graphNames.size

  private def validGraphName: Parser[String] = 
    graphNames.foldLeft(failure("did not find a valid graph name"): Parser[String])(_ ||| _)

  private def namedGraph: Parser[String ~ LGraph] = 
    validGraphName ~ ("{" ~> labeledGraph <~ "}")
  
  private def ruleName: Parser[String] = identifier

  def rule: Parser[T] =
    repN(nrOfGraphs, namedGraph) ^^ { case namedGraphs =>
      val nameToGraph = namedGraphs.map{ case name ~ graph => (name, graph) }.toMap
      val graphs = nameToGraph.values

      require(graphs.size == nrOfGraphs, "encountered an unexpected number of graphs")

      val vertexLabels = graphs.map(_.vertexLabels).flatten.toSet
      val edgeLabels = graphs.map(_.edgeLabels).flatten.toSet

      val nameToMorphism =
        def createMorphism[LV, LE](
          from: LabeledGraph[String, String, LV, LE], 
          to: LabeledGraph[String, String, LV, LE]
        ): LabeledGraphMorphism[String, String, String, String, LV, LE] =
          val mapV = from.vertices.mapTo(v => 
            val target = to.vertices.find(w => v.split('.').toSet subsetOf w.split('.').toSet)
            target.get
          )
          val mapE = from.edges.mapTo(e => 
            val target = to.edges.find(f => e.split('.').toSet subsetOf f.split('.').toSet)
            target.get
          )
          LabeledGraphMorphism(from, to, mapV, mapE)
          
        morphismNameToEndpoints.view.mapValues{ (dom, cod) =>
          createMorphism(nameToGraph(dom), nameToGraph(cod))
        }.toMap

      convertMapToRule(nameToMorphism)
    }

  def namedRule: Parser[(String, T)] =
    ("===" ~> ruleName <~ "===") ~ rule ^^ {
      case name ~ rule => (name, rule)
    }

  def systemOfNamedRules: Parser[Map[String, T]] = 
    rep1(comment | namedRule) ^^ { list =>
      list.filter(_ != ()).map(_.asInstanceOf[(String, T)]).toMap
    }

  def ruleStringRep: Parser[(String, String)] =
    ("===" ~> ruleName <~ "===") ~ "[^=]*".r ^^ { case name ~ ruleAsString => (name, ruleAsString)}
  
  def systemStringRep: Parser[Map[String, String]] =
    rep1(comment | ruleStringRep) ^^ { list =>
      list.filter(_ != ()).map(_.asInstanceOf[(String, String)]).toMap
    }
  