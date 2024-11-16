package com.pbpoplus.rewriting

final case class PbpoPlusSystem[O, A](
    name: String,
    rules: Set[PbpoPlusRule[O, A]]
)
