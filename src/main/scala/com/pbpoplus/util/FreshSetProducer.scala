package com.pbpoplus.util

trait FreshSetProducer[X]:
  def create(amount: Int, existing: Set[X] = Set.empty): Set[X]

object FreshSetProducer:
  given FreshSetProducer[Int] with
    def create(amount: Int, existing: Set[Int]): Set[Int] =
      val start = if existing.isEmpty then 0 else existing.max + 1
      (start until start + amount).toSet

  given FreshSetProducer[String] with
    def create(amount: Int, existing: Set[String]): Set[String] =
      val root =
        if existing.isEmpty then "" else existing.toList.sortBy(_.length).last
      (0 until amount).map(i => root + i.toString).toSet
