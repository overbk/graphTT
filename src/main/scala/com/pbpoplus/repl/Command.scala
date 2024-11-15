package com.pbpoplus.repl

import com.pbpoplus.termination.CountingClass
import com.pbpoplus.termination.CountingClass._

import scala.util.Failure
import scala.util.Success
import scala.util.Try

sealed trait Command:
  val stringRepresentation: String
  val description: String
  type ReturnType
  def validateArguments(args: List[String]): Either[String, ReturnType]

case object Help extends Command:
  val stringRepresentation = "help"
  val description = "help : print all available commands"
  type ReturnType = Unit
  def validateArguments(args: List[String]): Either[String, ReturnType] = Right(())

case object Exit extends Command:
  val stringRepresentation = "exit"
  val description = "exit : exit the program"
  type ReturnType = Unit
  def validateArguments(args: List[String]): Either[String, ReturnType] = Right(())

case object Inspect extends Command:
  val stringRepresentation = "inspect"
  val description = "inspect [n] : inspect option (system/tile) n in detail"
  type ReturnType = Int
  def validateArguments(args: List[String]): Either[String, Int] =
    if args.length != 1 then
      Left(s">> ${Inspect.stringRepresentation} requires exactly one integer argument")
    else
      args.head.toIntOption match
        case None => Left(s">> ${Inspect.stringRepresentation} requires an integer as argument")
        case Some(n) => Right(n)

case object Prove extends Command:
  val stringRepresentation = "select"
  val description: String = s"$stringRepresentation [n] : select system n for termination proving"
  type ReturnType = Int
  def validateArguments(args: List[String]): Either[String, Int] =
    if args.length != 1 then
      Left(s">> ${stringRepresentation} requires exactly one integer argument")
    else
      args.head.toIntOption match
        case None => Left(s">> ${Inspect.stringRepresentation} requires an integer as argument")
        case Some(n) => Right(n)

case object Back extends Command:
  val stringRepresentation = "back"
  val description: String = s"$stringRepresentation : return to system selection mode"
  type ReturnType = Unit
  def validateArguments(args: List[String]): Either[String, Unit] = Right(())

case object Use extends Command:
  type returnType = Set[(Int, Int, CountingClass)]
  val stringRepresentation = "use"
  val description: String = s"""$stringRepresentation [i w c]+ :
    |    use tile i with weight w, and count morphisms of class c, where:
    |      - i and w are integers (and w is positive), and 
    |      - c is a character (r: regular monos, m: monos, h: homomorphisms)
    |    multiple tiles can be specified. for example, 'use 3 4 h 5 9 r' uses:
    |       - tile 3 with weight 4 (counting homomorphisms), and
    |       - tile 5 with weight 9 (counting regular monos)""".stripMargin
  type ReturnType = Set[(Int, Int, CountingClass)]
  def validateArguments(args: List[String]): Either[String, Set[(Int, Int, CountingClass)]] = 
    if args.length < 3 && args.length % 3 != 0 
    then Left(s">> $stringRepresentation requires a positive multiple of 3 arguments")
    else 
      def peel(toProcess: List[String]): Set[(Int, Int, CountingClass)] = 
        if toProcess.isEmpty
        then Set()
        else 
          val tileNr = toProcess(0).trim().toInt
          val weight = toProcess(1).trim().toInt
          val slideClass = CountingClass.fromString(toProcess(2).trim())
          peel(toProcess.drop(3)) + ((tileNr, weight, slideClass))

      Try(peel(args)) match
        case Failure(exception) =>
          Left(s">> $stringRepresentation was given incorrect input: $exception")
        case Success(parsed) =>
          if parsed.exists(tup => tup(1) < 1)
          then Left(s">> weights must be positive")
          else Right(parsed)

case object Systems extends Command:
  val stringRepresentation = "systems"
  val description: String = s"$stringRepresentation : list the available systems"
  type ReturnType = Unit
  def validateArguments(args: List[String]): Either[String, Unit] = Right(())

case object Tiles extends Command:
  val stringRepresentation = "tiles"
  val description: String = s"$stringRepresentation : list the available tiles"
  type ReturnType = Unit
  def validateArguments(args: List[String]): Either[String, Unit] = Right(())

