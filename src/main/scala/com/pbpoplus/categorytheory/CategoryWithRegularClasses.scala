package com.pbpoplus.categorytheory

trait CategoryWithRegularClasses[O, A]
    extends CategoryWithRegularMonos[O, A]
    with CategoryWithRegularEpis[O, A]:
  override def isIso(f: A): Boolean = isRegularMonic(f) && isEpic(f)
  override def isoSet(from: O, to: O): Set[A] =
    regularMonoSet(from, to).filter(isEpic)

  override def rightInversesFor(f: A): Set[A] =
    regularMonoSet(codomain(f), domain(f)).filter(isRightInverseFor(_, f))
