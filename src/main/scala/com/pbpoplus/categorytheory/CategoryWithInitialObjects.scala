package com.pbpoplus.categorytheory

trait CategoryWithInitialObjects[O, A] extends Category[O, A]:
  lazy val initialObject: O
  def initialArrow(o: O): A = homSet(initialObject, o).head
  def isInitial(o: O): Boolean = isIso(initialArrow(o))
  