package com.pbpoplus.categorytheory

trait CategoryWithPushouts[O, A] extends Category[O, A]:
  def pushout(span: Span[A]): Cospan[A]
  override def maybePushout(span: Span[A]): Option[Cospan[A]] =
    Some(
      pushout(span)
    )
