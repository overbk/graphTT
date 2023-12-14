package com.pbpoplus.categorytheory

trait Topos[O, A] extends Quasitopos[O, A] with RmAdhesive:
  final def isRegularEpic(f: A): Boolean = isEpic(f)
  final def isRegularMonic(f: A): Boolean = isMonic(f)
  final def regularEpiMonoFactorization(f: A): (A, A) = epiRegularMonoFactorization(f)
