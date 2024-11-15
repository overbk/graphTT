package com.pbpoplus.categorytheory

trait CategoryWithRegularMonos[O, A] extends CategoryWithMonos[O, A]:
  def isRegularMonic(f: A): Boolean
  def regularMonoSet(from: O, to: O): Set[A] =
    monoSet(from, to).filter(isRegularMonic)
