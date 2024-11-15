package com.pbpoplus.util

import cats.syntax.all._

extension [T](x: T)
  def applyIf(f: T => T, b: =>Boolean): T = if b then f(x) else x

extension [A](s: Set[A])
  def identityMap: Map[A, A] = s.zip(s).toMap
  def cross[B](t: Set[B]): Set[(A, B)] = for {x <- s; y <- t} yield (x, y)
  def mapTo[B](f: A => B): Map[A, B] = s.map(x => x -> f(x)).toMap
  def restrictTo(t: Set[A]): Set[A] = s.intersect(t)
  def toMultiset: Multiset[A] = Multiset(s)
  def functions[B](t: Set[B]): Set[Map[A, B]] =
    if s.isEmpty
    then Set(Map())
    else 
      if (t.isEmpty)
      then Set()
      else
        for {
          v: A <- s
          rec: Map[A, B] <- (s - v).functions(t)
          w: B <- t
        } yield rec + (v -> w)
end extension

extension [A, B](m: Map[A, B])
  def after[C](n: Map[C, A]): Map[C, B] = n.view.mapValues(m).toMap
  def isInjective: Boolean = m.keys.toSet.size == m.values.toSet.size
  def isSurjectiveOn(s: Set[B]) = m.values.toSet == s
  def restrictDomainTo(s: Set[A]) = m.filter((b, _) => s.contains(b))
  def restrictCodomainTo(s: Set[B]) = m.filter((_, c) => s.contains(c))
  def rename[X, Y](renameKey: A => X, renameValue: B => Y): Map[X, Y] = 
    m.map(renameKey *** renameValue)

  def keyWithMinimalValue[D](interpret: B => D)(implicit ord: Ordering[D]): Option[A] =
    val sorted = m.toList.sortBy(d => interpret(d._2))
    sorted.headOption.map(_._1)
  
  def pullbackWith[C](n: Map[C, B]): (Map[(A, C), A], Map[(A, C), C]) =
    val pullbackObject = m.keySet.cross(n.keySet).filter((a, c) => m(a) == n(c))
    val left = pullbackObject.mapTo(_._1)
    val right = pullbackObject.mapTo(_._2)
    (left, right)
end extension

/**
* @param sets: a sequence of sets S1, ..., Sn
* @return a set of sequences (x1 \in S1, ..., xn \in Sn) such that
*         for all i,j, xi =/= xj.
*/
def noncollidingChoices[A](sets: Seq[Set[A]]): Set[Seq[A]] =
  // naive implementation
  if (sets.isEmpty)
    Set(Seq.empty)
  else 
    for {
      x <- sets.head
      tail <- noncollidingChoices(sets.tail)
      if !tail.contains(x)
    } yield (x +: tail)

def allInjectiveChoices[A, B](m: Map[A, Set[B]]): Set[Map[A, B]] =
  if m.isEmpty then 
    Set(Map.empty)
  else 
    val (headKey, headOptions) = m.head
    for {
      headChoice <- headOptions
      modifiedTail = m.tail.view.mapValues(_ - headChoice).toMap
      tailChoice <- allInjectiveChoices(modifiedTail)
    } yield tailChoice + (headKey -> headChoice)

def allChoices[A, B](m: Map[A, Set[B]]): Set[Map[A, B]] =
  if m.isEmpty then
    Set(Map.empty)
  else 
    val (headKey, headValues) = m.head
    for {
      tailChoice <- allChoices(m.tail)
      headChoice <- headValues
    } yield tailChoice + (headKey -> headChoice)
