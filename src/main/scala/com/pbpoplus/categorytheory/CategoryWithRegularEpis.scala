package com.pbpoplus.categorytheory

trait CategoryWithRegularEpis[O, A] extends CategoryWithEpis[O, A]:
  def isRegularEpic(f: A): Boolean
  def regularEpiSet(from: O, to: O): Set[A] =
    epiSet(from, to).filter(isRegularEpic)
