package rewriting

import com.pbpoplus.rewriting.PbpoPlusRule

final case class PbpoPlusSystem[O, A](
    name: String,
    rules: Set[PbpoPlusRule[O, A]]
)
