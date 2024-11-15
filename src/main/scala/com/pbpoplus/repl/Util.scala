package com.pbpoplus.repl

import java.io.File

object Util:
  private val inputPrefix = "graphTT>"

  private def loadResources(dir: String): Either[String, Set[Resource]] =
    val d = new File(dir)
    if !(d.exists && d.isDirectory)
    then Left("invalid directory")
    else
      val resourceFiles: List[File] = d.listFiles.filter(_.isFile).toList
      val resources: Set[Resource] = resourceFiles.map { file =>
        val name: String = file.toString.split("/").last.split("\\.").head
        val content: String = io.Source.fromFile(file).getLines.mkString("\n")
        Resource(name, content)
      }.toSet

      Right(resources)

  def printResources(
      indexToResource: Map[Int, Resource],
      resourceType: String
  ): Unit =
    println(s">> The following ${resourceType}s were loaded:")
    indexToResource.toList.sortBy(_._1).foreach {
      case (i, Resource(name, content)) =>
        println(s"  (${i}) $name")
    }

  def loadSystems(dir: String): Either[String, Set[Resource]] =
    loadResources(
      dir
    )
  def loadTiles(dir: String): Either[String, Set[Resource]] = loadResources(dir)

  def printAvailableCommands(availableCommands: Set[Command]): Unit =
    println(">> Available commands:")
    availableCommands.foreach(cmd => println(s"  ${cmd.description}"))

  def promptCommand(availableCommands: Set[Command]): (Command, Any) =
    print(s"$inputPrefix ")
    Thread.sleep(1000)
    val input: Array[String] = scala.io.StdIn.readLine().split(" ")
    val commandInput = input.head.trim()
    if commandInput.isEmpty
    then promptCommand(availableCommands)
    else
      val command: Option[Command] =
        availableCommands.find(_.stringRepresentation == commandInput)
      command match
        case None =>
          println(s"$commandInput is not a valid command.")
          promptCommand(availableCommands)
        case Some(cmd) =>
          val arguments = input.tail.toList
          val result: Either[String, cmd.ReturnType] =
            cmd.validateArguments(arguments)
          result match
            case Right(parsed) => (cmd, parsed)
            case Left(errorMsg) =>
              println(errorMsg)
              promptCommand(availableCommands)
end Util
