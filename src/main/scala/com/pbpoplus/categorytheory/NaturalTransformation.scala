package com.pbpoplus.categorytheory

final case class NaturalTransformation[O1, A1, O2, A2](
    from: Functor[O1, A1, O2, A2],
    to: Functor[O1, A1, O2, A2],
    componentMap: O1 => A2
):
  def apply(o: O1): A2 = componentMap(o)
