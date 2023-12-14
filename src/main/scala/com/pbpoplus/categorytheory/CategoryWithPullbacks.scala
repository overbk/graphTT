package com.pbpoplus.categorytheory

trait CategoryWithPullbacks[O, A] extends Category[O, A]:
  def pullback(cospan: Cospan[A]): Span[A]
  override def maybePullback(cospan: Cospan[A]): Option[Span[A]] = Some(pullback(cospan))
  