package com.pbpoplus.categorytheory

trait FunctorCategory[O1, A1, O2, A2](
    source: Category[O1, A1],
    target: Category[O2, A2]
) extends Category[
      Functor[O1, A1, O2, A2],
      NaturalTransformation[O1, A1, O2, A2]
    ]:

  def domain(
      alpha: NaturalTransformation[O1, A1, O2, A2]
  ): Functor[O1, A1, O2, A2] = alpha.from

  def codomain(
      alpha: NaturalTransformation[O1, A1, O2, A2]
  ): Functor[O1, A1, O2, A2] = alpha.to

  def identityArrow(
      f: Functor[O1, A1, O2, A2]
  ): NaturalTransformation[O1, A1, O2, A2] =
    NaturalTransformation(f, f, (o: O1) => target.identityArrow(f(o)))

  def after(
      beta: NaturalTransformation[O1, A1, O2, A2],
      alpha: NaturalTransformation[O1, A1, O2, A2]
  ): NaturalTransformation[O1, A1, O2, A2] =
    NaturalTransformation(
      alpha.from,
      beta.to,
      (o: O1) => target.after(beta(o), alpha(o))
    )
