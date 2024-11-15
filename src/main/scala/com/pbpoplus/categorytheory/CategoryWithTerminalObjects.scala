package com.pbpoplus.categorytheory

trait CategoryWithTerminalObjects[O, A] extends Category[O, A]:
  lazy val terminalObject: O
  def terminalArrow(o: O): A = homSet(o, terminalObject).head
  def isTerminal(o: O): Boolean = isIso(terminalArrow(o))
