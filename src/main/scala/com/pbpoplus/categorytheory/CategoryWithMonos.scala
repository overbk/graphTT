package com.pbpoplus.categorytheory

trait CategoryWithMonos[O, A] extends Category[O, A]:
  def isMonic(f: A): Boolean
  def monoSet(from: O, to: O): Set[A] = homSet(from, to).filter(isMonic)
  