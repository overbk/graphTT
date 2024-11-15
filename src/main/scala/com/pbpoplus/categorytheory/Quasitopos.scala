package com.pbpoplus.categorytheory

trait Quasitopos[O, A]
    extends CategoryWithRegularClasses[O, A]
    with CategoryWithLimitsAndColimits[O, A]
    with CategoryWithFactorizations[O, A]
    with CategoryWithClassification[O, A]
    with RmAdhesive
    with RegularMonosAreStableUnderDecomposition
    with RegularMonosAreStableUnderComposition
