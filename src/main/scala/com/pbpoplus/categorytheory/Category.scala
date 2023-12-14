package com.pbpoplus.categorytheory

trait Category[O, A]:
  def domain(f: A): O
  def codomain(f: A): O
  def identityArrow(o: O): A
  def after(g: A, f: A): A
  def homSet(from: O, to: O): Set[A]
  
  implicit class ArrowOps(g: A):
    infix def o(f: A) = after(g, f)
  end ArrowOps

  def areCompatible(g: A, f: A) = domain(g) == codomain(f)
  def areOpposite(f: A, g: A) = areCompatible(f, g) && areCompatible(g, f)
  def areParallel(f: A, g: A) = domain(f) == domain(g) && codomain(f) == codomain(g)

  def isLeftInverseFor(g: A, f: A) = areOpposite(g, f) && (g o f) == identityArrow(domain(f))
  def leftInversesFor(f: A): Set[A] = homSet(codomain(f), domain(f)).filter(isLeftInverseFor(_, f))
  def isRightInverseFor(f: A, g: A) =  
    areOpposite(f, g) && (g o f) == identityArrow(domain(f))
  def rightInversesFor(f: A): Set[A] = 
    homSet(codomain(f), domain(f)).filter(isRightInverseFor(_, f))
  def areInverses(f: A, g: A) = areOpposite(f, g) && (f o g) == identityArrow(domain(g)) &&
    (g o f) == identityArrow(domain(f))
  def inverseOf(f: A): Option[A] = // if one exists, it exists uniquely
    homSet(codomain(f), domain(f)).find(areInverses(f, _))

  def isIso(f: A): Boolean = inverseOf(f).isDefined

  def isoSet(from: O, to: O): Set[A] = homSet(from, to).filter(isIso)

  def isMonicFor(f: A, arrows: Set[A]): Boolean =
    require(arrows.forall(areCompatible(f, _)))
    arrows.size == arrows.map((f o _)).size

  ///////////////////////////////////////////////////////////////////////////
  // Spans and cospans.
  ///////////////////////////////////////////////////////////////////////////

  extension (span: Span[A])
    def isValid = domain(span.left) == domain(span.right)
    def root = domain(span.left)
  end extension

  extension (cospan: Cospan[A])
    def isValid = codomain(cospan.left) == codomain(cospan.right)
    def sink = codomain(cospan.left)
  end extension

  ///////////////////////////////////////////////////////////////////////////
  // Testing for object isomorphism.
  ///////////////////////////////////////////////////////////////////////////

  def areIsomorphic(o1: O, o2: O): Boolean = isoSet(o1, o2).nonEmpty

  def isoSet(from: Span[A], to: Span[A]): Set[A] =
    require(codomain(from.left) == codomain(to.left))
    require(codomain(from.right) == codomain(to.right))

    val rootIsos: Set[A] = isoSet(from.root: O, to.root: O)

    rootIsos.filter(i => from.left == (to.left o i) && from.right == (to.right o i))
  
  def areIsomorphic(sp1: Span[A], sp2: Span[A]): Boolean = isoSet(sp1, sp2).nonEmpty

  def isoSet(from: Cospan[A], to: Cospan[A]): Set[A] =
    require(codomain(from.left) == codomain(to.left))
    require(codomain(from.right) == codomain(to.right))

    val rootIsos = isoSet(from.sink, to.sink)
    rootIsos.filter(i => (i o from.left) == to.left && (i o from.right) == to.right)
  
  def areIsomorphic(co1: Cospan[A], co2: Cospan[A]): Boolean = isoSet(co1, co2).nonEmpty

  ///////////////////////////////////////////////////////////////////////////
  // Commuting squares.
  ///////////////////////////////////////////////////////////////////////////

  def isCommutingSquare(s: Span[A], c: Cospan[A]): Boolean =
    areCompatible(c.left, s.left) && areCompatible(c.right, s.right)
      && (c.left o s.left) == (c.right o s.right)

  def maybePullback(cospan: Cospan[A]): Option[Span[A]]

  def isPullbackFor(span: Span[A], cospan: Cospan[A]): Boolean = 
    val pb = maybePullback(cospan)
    pb.isDefined && areIsomorphic(span, pb.get)

  def maybePushout(span: Span[A]): Option[Cospan[A]]

  def isPushoutFor(span: Span[A], cospan: Cospan[A]): Boolean =
    val po = maybePushout(span)
    po.isDefined && areIsomorphic(cospan, po.get)

end Category
