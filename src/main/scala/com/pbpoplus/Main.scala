package com.pbpoplus

import com.pbpoplus.termination.CountingClass
import com.pbpoplus.util.FreshSetProducer

import labeledgraph.LabeledGraphCategory
import rewriting.PbpoPlusRule
import categorytheory.TerminationCategory
import termination.Termination.createSystemReport
import termination.WeightedTileInfo
import repl._
import repl.Util._
import parsing.Util.parse
import parsing.LabeledPbpoParser

@main def main: Unit =
  val labeledGraphCategory = LabeledGraphCategory[String, String, String, String]()
  val p = new LabeledPbpoParser()
  startREPL(
    "./src/main/resources/labeled/systems",
    "./src/main/resources/labeled/tiles",
    parse(p, p.labeledGraph, _),
    parse(p, p.systemOfNamedRules, _),
    parse(p, p.systemStringRep, _),
    labeledGraphCategory
  )

def startREPL[O, A, C <: TerminationCategory[O, A]](
  systemsDir: String,
  tilesDir: String,
  stringToObject: String => O,
  stringToSystemOfNamedRules: String => Map[String, PbpoPlusRule[O, A]],
  stringToRuleStringRepresentations: String => Map[String, String],
  category: C
): Unit =
  println("=== GraphTT REPL ===")
  val systemSelectionLoopCommands: Set[Command] = Set(Help, Exit, Systems, Inspect, Prove)
  val terminationLoopCommands: Set[Command] = Set(Help, Exit, Tiles, Inspect, Back, Use)
  val indexToSystem: Map[Int, Resource] = loadSystems(systemsDir) match
    case Left(errorMsg) => 
      println(s"loading systems failed: $errorMsg. exiting")
      System.exit(1)
      Map()
    case Right(systems) => systems.zipWithIndex.map(_.swap).toMap
  
  val indexToTile: Map[Int, Resource] = loadTiles(tilesDir) match
    case Left(errorMsg) => 
      println(s"loading tiles failed: $errorMsg. exiting")
      System.exit(1)
      Map()
    case Right(tiles) => tiles.zipWithIndex.map(_.swap).toMap

  systemSelectionMode()
  def systemSelectionMode(): Unit = 
    println(">> You are in system selection mode.")
    println(">> Type 'help' to view the available commands.")
    systemSelectionPrompt()
    def systemSelectionPrompt(): Unit =
      val (command, parsed) = promptCommand(systemSelectionLoopCommands)
      command match
        case Help => 
          printAvailableCommands(systemSelectionLoopCommands)
          systemSelectionPrompt()
        case Exit =>
          println(">> Exiting. Goodbye.")
          System.exit(0)
        case Systems =>
          printResources(indexToSystem, "system")
          systemSelectionPrompt()
        case Inspect =>
          val i: Int = parsed.asInstanceOf[Inspect.ReturnType]
          indexToSystem.get(i) match
            case None => println(s">> There is no system with index $i.")
            case Some(system) =>
              println(s"SYSTEM: ${system.name}")
              println(system.content)
          systemSelectionPrompt()
        case Prove =>
          val i: Int = parsed.asInstanceOf[Prove.ReturnType]
          indexToSystem.get(i) match
            case None =>
              println(s">> There is no system with index $i.")
              systemSelectionPrompt()
            case Some(resource) =>
              val system: Map[String, PbpoPlusRule[O, A]] = 
                stringToSystemOfNamedRules(resource.content)
              println(s">> Entering proof mode for system ${resource.name}.")
              val ruleStrings = stringToRuleStringRepresentations(resource.content)
              terminationMode(resource.name, system, ruleStrings)
        case _ =>
          println(">> Command is not valid here.")
          systemSelectionPrompt()

  def terminationMode(
    name: String, 
    system: Map[String, PbpoPlusRule[O, A]],
    ruleNameToStringRep: Map[String, String]
  ): Unit =
    println(s">> The system consists of ${system.size} rule${if system.size == 1 then "" else "s"}.")
    println(s">> The system is as follows:\n")
    ruleNameToStringRep.foreach{ case (ruleName, rule) => println(s"=== $ruleName ===\n$rule")}
    println()
    println(">> Type 'help' to view the available commands.")

    terminationPrompt()
    def terminationPrompt(): Unit = 
      val (command, parsed) = promptCommand(terminationLoopCommands)
      command match
        case Help =>
          printAvailableCommands(terminationLoopCommands)
          terminationPrompt()
        case Exit => 
          println(">> Exiting. Goodbye.")
          System.exit(0)
        case Tiles => 
          printResources(indexToTile, "tile")
          terminationPrompt()
        case Inspect =>
          val i: Int = parsed.asInstanceOf[Inspect.ReturnType]
          indexToTile.get(i) match
            case None => 
              println(s">> There is no tile with index $i.")
              terminationPrompt()
            case Some(resource) =>
              println(s"TILE: ${resource.name}")
              println(resource.content)
              terminationPrompt()
        case Prove =>
          println(">> Already in proof mode.")
          terminationPrompt()
        case Back =>
          println(">> Returning to system selection mode.")
          Thread.sleep(1000)
          systemSelectionMode()
        case Use =>
          val choices: Set[(Int, Int, CountingClass)] = parsed.asInstanceOf[Use.ReturnType]
          if !choices.forall{ case (i, _, _) => indexToTile.contains(i)} 
          then 
            println(">> One or more tile indices are invalid.")
            terminationPrompt()
          else
            val weightedTiles: Set[WeightedTileInfo[O, A]] =
              choices.map{ case (index, weight, countingClass) =>
                val resource = indexToTile(index)
                val tile = stringToObject(resource.content)
                WeightedTileInfo(tile, weight, resource.name, resource.content, countingClass)
              }
            val report = createSystemReport(weightedTiles, system)(category)
            println(report)
            val pruned = report.prunedSystem
            if pruned.isEmpty then
              println(">> The pruned system is empty!")
              println(s">> You have proven system $name terminating.")
              println(">> Returning to system selection mode.")
              println()
              systemSelectionMode()
            else if pruned == system then
              println(s">> No rules were eliminated. Try another set of weighted tiles.")
              terminationPrompt()
            else 
              println(s">> You have eliminated some rules in this iteration.")
              println(s">> ${pruned.size} rule(s) remain.")
              println(s">> Try another set of weighted tiles.")
              println()
              val remainingRuleNames = pruned.keySet
              val updatedSourceString = 
                ruleNameToStringRep.view.filterKeys(remainingRuleNames).toMap
              terminationMode(name, pruned, updatedSourceString)
        case _ =>
          println(">> Command is not valid here.")
          terminationPrompt()
