package com.pbpoplus.categorytheory

trait FiniteCategory[O, A] extends Category[O, A]:
  val objects: Set[O]
  val arrows: Set[A]
