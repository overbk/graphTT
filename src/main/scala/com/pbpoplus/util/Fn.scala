package com.pbpoplus.util



final case class Fn[X, Y](function: Map[X, Y], codomain: Set[Y]):
  lazy val domain = function.keySet
  lazy val image = function.values.toSet

  def apply(x: X): Y = function(x)

  override def toString: String = function.toString

  def after[Z](that: Fn[Z, X]): Fn[Z, Y] =
    require(domain == that.codomain)
    Fn(function.after(that.function), codomain)
  
  lazy val isInjective: Boolean = function.isInjective
  lazy val isSurjective: Boolean = function.isSurjectiveOn(codomain)

  def pullbackWith[Z](g: Fn[Z, Y]): (Fn[(X, Z), X], Fn[(X, Z), Z]) = 
    require(codomain == g.codomain)

    val product: Set[(X, Z)] = domain.cross(g.domain)
    val pullbackObject = product.filter((x, y) => function(x) == g.function(y))

    val left = pullbackObject.mapTo(_._1)
    val right = pullbackObject.mapTo(_._2)

    (Fn(left, domain), Fn(right, g.domain))

  def pushoutWith[Z](g: Fn[X, Z]): (Fn[Y, Set[Either[Y, Z]]], Fn[Z, Set[Either[Y, Z]]]) =
    require(domain == g.domain)

    val pushoutObject: Set[Set[Either[Y, Z]]] =
      def unionWithIntersectingSet[T](pairwiseDisjointSets: Set[Set[T]], set: Set[T]): Set[Set[T]] = 
        if pairwiseDisjointSets.isEmpty
        then Set(set)
        else
          val head = pairwiseDisjointSets.head
          if head.intersect(set).nonEmpty
          then pairwiseDisjointSets.tail + head.union(set)
          else unionWithIntersectingSet(pairwiseDisjointSets.tail, set) + head
 
      val equivalenceClassesSeed: Set[Set[Either[Y, Z]]] = 
        domain.map(x => Set(Left(function(x)), Right(g.function(x))))

      val clusteredInImage = 
        equivalenceClassesSeed.foldLeft(Set.empty: Set[Set[Either[Y, Z]]])(unionWithIntersectingSet)
      val leftOutsideImage: Set[Set[Either[Y, Z]]] = 
        (codomain -- image).map(x => Set(Left(x)))
      val rightOutsideImage: Set[Set[Either[Y, Z]]] =
        (g.codomain -- g.image).map(x => Set(Right(x)))

      clusteredInImage.union(leftOutsideImage).union(rightOutsideImage)

    val left = codomain.mapTo(y => pushoutObject.find(_.contains(Left(y))).get)
    val right = g.codomain.mapTo(z => pushoutObject.find(_.contains(Right(z))).get)

    (Fn(left, pushoutObject), Fn(right, pushoutObject))

  def imageFactorize: (Fn[X, Y], Fn[Y, Y]) =
    (Fn(function, codomain.restrictTo(image)), Fn(image.identityMap, codomain))

end Fn
