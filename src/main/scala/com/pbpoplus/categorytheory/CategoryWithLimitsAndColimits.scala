package com.pbpoplus.categorytheory

/* Not all limits and colimits are implemented. */
trait CategoryWithLimitsAndColimits[O, A]
    extends CategoryWithPullbacks[O, A]
    with CategoryWithInitialObjects[O, A]
    with CategoryWithPushouts[O, A]
    with CategoryWithTerminalObjects[O, A]:
  def coproduct(o1: O, o2: O): Cospan[A] =
    pushout(
      Span(initialArrow(o1), initialArrow(o2))
    )
  def product(o1: O, o2: O): Span[A] =
    pullback(
      Cospan(terminalArrow(o1), terminalArrow(o2))
    )
end CategoryWithLimitsAndColimits
