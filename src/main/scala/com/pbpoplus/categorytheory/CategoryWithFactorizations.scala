package com.pbpoplus.categorytheory

trait CategoryWithFactorizations[O, A] extends Category[O, A]:
  def epiRegularMonoFactorization(f: A): (A, A) // (epi, reg mono)
  def image(f: A) = epiRegularMonoFactorization(f)._2

  def regularEpiMonoFactorization(f: A): (A, A) // (reg epi, mono)
