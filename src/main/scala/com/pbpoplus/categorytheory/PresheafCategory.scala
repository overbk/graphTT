package com.pbpoplus.categorytheory

import com.pbpoplus.util.Fn
import com.pbpoplus.util.FreshSetProducer
import com.pbpoplus.util.mapTo
import com.pbpoplus.util.restrictDomainTo

abstract class PresheafCategory[O, A, X]
  (indexCategory: FiniteCategory[O, A])(implicit freshX: FreshSetProducer[X]) 
  extends FunctorCategory[O, A, Set[X], Fn[X, X]](indexCategory, SetCategory[X]())
  with Topos[Functor[O, A, Set[X], Fn[X, X]], NaturalTransformation[O, A, Set[X], Fn[X, X]]]:

  val objectIndices: Set[O] = indexCategory.objects
  val arrowIndices: Set[A] = indexCategory.arrows

  ///////////////////////////////////////////////////////////////////////////
  // Morphism properties.
  ///////////////////////////////////////////////////////////////////////////

  def isMonic(eta: NaturalTransformation[O, A, Set[X], Fn[X, X]]): Boolean =
    objectIndices.forall(eta(_).isInjective)

  def isEpic(eta: NaturalTransformation[O, A, Set[X], Fn[X, X]]): Boolean = 
    objectIndices.forall(eta(_).isSurjective)
  
  ///////////////////////////////////////////////////////////////////////////
  // Limits and colimits.
  ///////////////////////////////////////////////////////////////////////////

  override lazy val initialObject: Functor[O, A, Set[X], Fn[X, X]] = 
    val initialSet = SetCategory[X]().initialObject
    val objectMap = objectIndices.mapTo(_ => initialSet)
    val arrowMap = arrowIndices.mapTo(_ => Fn(Map(): Map[X, X], initialSet))
    Functor(objectMap, arrowMap)

  override lazy val terminalObject: Functor[O, A, Set[X], Fn[X, X]] = 
    val terminalSet = SetCategory[X]().terminalObject
    val objectMap = objectIndices.mapTo(_ => terminalSet)
    val arrowMap = 
      arrowIndices.mapTo(_ => Fn(Map(terminalSet.head -> terminalSet.head), terminalSet))
    Functor(objectMap, arrowMap)


  override def pushout(span: Span[NaturalTransformation[O, A, Set[X], Fn[X, X]]])
    : Cospan[NaturalTransformation[O, A, Set[X], Fn[X, X]]] =
    /**
      *       A -- etaRight --> B
      *       |                 |
      *     etaLeft    PO    etaRight1
      *       |                 |
      *       V                 V  
      *       C -- etaLeft1 --> F
      */
    
    val etaLeft = span.left
    val etaRight = span.right
    val C: Functor[O, A, Set[X], Fn[X, X]] = etaLeft.to
    val B: Functor[O, A, Set[X], Fn[X, X]] = etaRight.to

    val componentwisePushouts: Map[O, Cospan[Fn[X, X]]] = objectIndices.mapTo(o =>
      SetCategory[X]().pushout(Span(etaLeft(o), etaRight(o)))
    )

    val etaLeft1ComponentMap: Map[O, Fn[X, X]] = 
      componentwisePushouts.view.mapValues(_.left).toMap
    val etaRight1ComponentMap: Map[O, Fn[X, X]] = 
      componentwisePushouts.view.mapValues(_.right).toMap
    
    val D: Functor[O, A, Set[X], Fn[X, X]] = 
      val DObj: Map[O, Set[X]] = objectIndices.mapTo(etaLeft1ComponentMap(_).codomain)
      val DArr: Map[A, Fn[X, X]] = arrowIndices.mapTo(f => 
        def dom = indexCategory.domain
        def cod = indexCategory.codomain
      
        val `D(dom(f))`: Set[X] = DObj(dom(f))
        val `D(cod(f))`: Set[X] = DObj(cod(f))

        val `D(f)`: Map[X, X] = `D(dom(f))`.mapTo(x => 
          val preimageInC: Option[X] = 
            etaLeft1ComponentMap(dom(f)).function.find((_, y) => y == x).map(_._1)
          if (preimageInC.isDefined)
            val targetInC = C(f)(preimageInC.get)
            etaLeft1ComponentMap(cod(f))(targetInC)
          else 
            val preimageInB: Option[X] = 
              etaRight1ComponentMap(dom(f)).function.find((_, y) => y == x).map(_._1)
            val targetInB = B(f)(preimageInB.get)
            etaRight1ComponentMap(cod(f))(targetInB)
        )

        Fn(`D(f)`, `D(cod(f))`)
      )

      Functor(DObj, DArr)

    val etaLeft1 = NaturalTransformation(C, D, etaLeft1ComponentMap)
    val etaRight1 = NaturalTransformation(B, D, etaRight1ComponentMap)

    Cospan(etaLeft1, etaRight1)
  end pushout

  override def pullback(cospan: Cospan[NaturalTransformation[O, A, Set[X], Fn[X, X]]]):
    Span[NaturalTransformation[O, A, Set[X], Fn[X, X]]] = 
      /**
      *       A  -- etaRight --> B
      *       |                  |
      *     etaLeft    PB   etaRight1
      *       |                  |
      *       V                  V  
      *       C -- etaLeft1 ---> D
      */
    
    val etaLeft1 = cospan.left
    val etaRight1 = cospan.right
    val C: Functor[O, A, Set[X], Fn[X, X]] = etaLeft1.from
    val B: Functor[O, A, Set[X], Fn[X, X]] = etaRight1.from

    val componentwisePullbacks: Map[O, Span[Fn[X, X]]] = objectIndices.mapTo(o =>
      SetCategory[X]().pullback(Cospan(etaLeft1(o), etaRight1(o)))
    )
    val etaLeftComponentMap: Map[O, Fn[X, X]] = 
      componentwisePullbacks.view.mapValues(_.left).toMap
    val etaRightComponentMap: Map[O, Fn[X, X]] = 
      componentwisePullbacks.view.mapValues(_.right).toMap
    
    val A = 
      val AObj: Map[O, Set[X]] = objectIndices.mapTo(etaLeftComponentMap(_).domain)
      val AArr: Map[A, Fn[X, X]] = arrowIndices.mapTo(f => 
        def dom = indexCategory.domain
        def cod = indexCategory.codomain
      
        val `A(dom(f))`: Set[X] = AObj(dom(f))
        val `A(cod(f))`: Set[X] = AObj(cod(f))

        val `A(f)`: Map[X, X] = `A(dom(f))`.mapTo(x =>
          val xInB = etaRightComponentMap(dom(f))(x)
          val fxInB = B(f)(xInB)
          val Bcandidates = `A(cod(f))`.filter(y => etaRightComponentMap(cod(f))(y) == fxInB)

          val xInC = etaLeftComponentMap(dom(f))(x)
          val fxInC = C(f)(xInC)
          val Ccandidates = `A(cod(f))`.filter(y => etaLeftComponentMap(cod(f))(y) == fxInC)

          Bcandidates.intersect(Ccandidates).head
        )

        Fn(`A(f)`, `A(cod(f))`)
      )

      Functor(AObj, AArr)

    val etaLeft = NaturalTransformation(A, C, etaLeftComponentMap)
    val etaRight = NaturalTransformation(A, B, etaRightComponentMap)

    Span(etaLeft, etaRight)
  end pullback

  override def epiRegularMonoFactorization(eta: NaturalTransformation[O, A, Set[X], Fn[X, X]]):
    (NaturalTransformation[O, A, Set[X], Fn[X, X]], NaturalTransformation[O, A, Set[X], Fn[X, X]]) =
    val image =
      val objectMap = objectIndices.mapTo(o => eta.from(o).map(x => eta(o)(x)))
      val arrowMap = arrowIndices.mapTo(f => 
        val fDomain = objectMap(indexCategory.domain(f))
        val fCodomain = objectMap(indexCategory.domain(f))
        
        val toF: Fn[X, X] = eta.to(f)
        val function = toF.function.restrictDomainTo(fDomain)

        Fn(function, fCodomain)
      )

      Functor(objectMap, arrowMap)
    
    val componentMapFactors = 
      objectIndices.mapTo(o => eta(o).imageFactorize)
    val etaEpi = componentMapFactors.view.mapValues(_._1).toMap
    val etaRegMono = componentMapFactors.view.mapValues(_._2).toMap

    val epi = NaturalTransformation(eta.from, image, etaEpi)
    val regularMono = NaturalTransformation(image, eta.to, etaRegMono)
    (epi, regularMono)

  /* If this override is not used, then at some point a Map[A, B] may be compared with
     a A => B, which evaluates to false and gives no warning. */
  override def after(
    beta: NaturalTransformation[O, A, Set[X], Fn[X, X]],
    alpha: NaturalTransformation[O, A, Set[X], Fn[X, X]]
  ): NaturalTransformation[O, A, Set[X], Fn[X, X]] = 
    NaturalTransformation(alpha.from, beta.to, 
      objectIndices.mapTo(o => SetCategory[X]().after(beta(o), alpha(o)))
    )

  /* If this override is not used, then at some point a Map[A, B] may be compared with
     a A => B, which evaluates to false and gives no warning. */
  override def identityArrow(f: Functor[O, A, Set[X], Fn[X, X]])
    : NaturalTransformation[O, A, Set[X], Fn[X, X]] = 
    NaturalTransformation(f, f, objectIndices.mapTo(o => SetCategory[X]().identityArrow(f(o))))

end PresheafCategory
