package com.pbpoplus.categorytheory

import com.pbpoplus.util._

final case class SetCategory[X]()(implicit freshX: FreshSetProducer[X]) 
  extends Topos[Set[X], Fn[X, X]]:

  def after(g: Fn[X, X], f: Fn[X, X]): Fn[X, X] = g.after(f)
  def domain(f: Fn[X, X]): Set[X] = f.domain
  def codomain(f: Fn[X, X]): Set[X] = f.codomain
  def identityArrow(set: Set[X]): Fn[X, X] = Fn(set.identityMap, set)

  def isMonic(f: Fn[X, X]): Boolean = f.isInjective
  def isEpic(f: Fn[X, X]): Boolean = f.isSurjective

  def pullback(cospan: Cospan[Fn[X, X]]): Span[Fn[X, X]] = 
    val (pbLeft, pbRight) = cospan.left.pullbackWith(cospan.right)
    val pbObject = pbLeft.domain

    val pbObjectInX = freshX.create(pbObject.size)
    val bijection = pbObjectInX.zip(pbObject).toMap

    val pbLeftt = Fn(pbObjectInX.mapTo(x => pbLeft(bijection(x))), cospan.left.domain)
    val pbRightt = Fn(pbObjectInX.mapTo(x => pbRight(bijection(x))), cospan.right.domain)

    Span(pbLeftt, pbRightt)

  def pushout(span: Span[Fn[X, X]]): Cospan[Fn[X, X]] = 
    val (poLeft, poRight) = span.left.pushoutWith(span.right)
    val poObject = poLeft.codomain

    val poObjectInX = freshX.create(poObject.size)
    val bijection = poObject.zip(poObjectInX).toMap

    val poLeftt = Fn(span.left.codomain.mapTo(x => bijection(poLeft(x))), poObjectInX)
    val poRightt = Fn(span.right.codomain.mapTo(x => bijection(poRight(x))), poObjectInX)

    Cospan(poLeftt, poRightt)

  lazy val initialObject: Set[X] = Set.empty

  lazy val terminalObject: Set[X] = freshX.create(1)

  def homSet(from: Set[X], to: Set[X]): Set[Fn[X, X]] = from.functions(to).map(Fn(_, to))

  def partialMapClassifierArrow(set: Set[X]): Fn[X, X] = 
    val singletonSet = freshX.create(1, set)
    Fn(set.identityMap, set + singletonSet.head)

  def epiRegularMonoFactorization(f: Fn[X, X]): (Fn[X, X], Fn[X, X]) =
    (Fn(f.function, f.image), Fn(f.image.identityMap, f.codomain))
