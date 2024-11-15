package com.pbpoplus.util

import cats.syntax.all._

object Multiset:
  def apply: Multiset[Nothing] = Multiset(Map.empty)
  def apply[T](s: Set[T]): Multiset[T] = Multiset(s.map(_ -> 1).toMap)

case class Multiset[T] (private val m: Map[T, Int]):
  lazy val size: Int = m.toList.foldLeft(0)(_ + _._2)

  val toMap = m

  def map[U](f: T => U): Multiset[U] = 
    val fApplied: List[(U, Int)] = m.toList.map(f *** identity)
    val grouped: Map[U, List[(U, Int)]] = fApplied.groupBy(_._1)
    val flattened: Map[U, Int] = 
      grouped.view.mapValues(_.foldLeft(0){case (acc, (_, n)) => acc + n}).toMap

    Multiset(flattened)

  def subsetOf(that: Multiset[T]): Boolean = 
    m.forall((k, n) => n <= that.multiplicity(k))
    
  def forall = m.keys.forall
  def exists = m.keys.exists
  def contains = m.contains
  
  def multiplicity(x: T): Int = m.getOrElse(x, 0)

  def add(x: T): Multiset[T] = 
    val result = m.updatedWith(x) {
      case None => Some(1)
      case Some(n) => Some(n + 1)
    }

    Multiset(result)

end Multiset
