package com.pbpoplus.categorytheory

trait CategoryWithEpis[O, A] extends Category[O, A]:
  def isEpic(f: A): Boolean
  def epiSet(from: O, to: O): Set[A] = homSet(from, to).filter(isEpic)
