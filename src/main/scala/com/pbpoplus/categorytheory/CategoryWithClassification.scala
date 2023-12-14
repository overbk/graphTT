package com.pbpoplus.categorytheory

trait CategoryWithClassification[O, A] extends CategoryWithRegularMonos[O, A]
  with CategoryWithPullbacks[O, A]
  with CategoryWithTerminalObjects[O, A]:

  ///////////////////////////////////////////////////////////////////////////
  // Partial map classification.
  ///////////////////////////////////////////////////////////////////////////

  def partialMapClassifierArrow(o: O): A /* the PMC arrow of O into partialMapClassifier(O) */
  def partialMapClassifier(o: O): O = codomain(partialMapClassifierArrow(o))

  def partialMapClassify(span: Span[A]): A =
    /**
      *       X ------ f -----> A
      *       |                 |
      *    subobject   PB     eta_f = partialMapClassifierArrow(A)
      *       |                 |
      *       V                 V  
      *       B --- ! ---> partialMapClassifier(A)
      */
    val subobject = span.left
    require(isRegularMonic(subobject))

    val f = span.right
    val eta_f = partialMapClassifierArrow(codomain(f))

    homSet(codomain(subobject), codomain(eta_f)).find(g =>
        isPullbackFor(Span(subobject, f), Cospan(g, eta_f))
    ).get
  
  ///////////////////////////////////////////////////////////////////////////
  // Subobject classification.
  ///////////////////////////////////////////////////////////////////////////

  lazy val trueArrow: A = partialMapClassifierArrow(terminalObject)
  lazy val omega: O = codomain(trueArrow)

  def classifySubobject(m: A): A =
    require(isRegularMonic(m))
    partialMapClassify(Span(m, terminalArrow(domain(m))))
