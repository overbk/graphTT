package com.pbpoplus.termination

import com.pbpoplus.categorytheory.TerminationCategory

object CountingClass:
  def fromString(s: String): CountingClass =
    s.trim.toLowerCase match
      case "r" => RegularMono
      case "rm" => RegularMono
      case "m" => Mono
      case "mono" => Mono
      case "h" => Homomorphism
      case "hom" => Homomorphism
      case _ => throw Exception(s"could not parse ${s.trim}")
      
enum CountingClass:
  def predicate[A](implicit category: TerminationCategory[_, A]): A => Boolean = this match
    case RegularMono => category.isRegularMonic
    case Mono => category.isMonic
    case Homomorphism => { (_: A) => true }
    
  def secondFactor[A](implicit category: TerminationCategory[_, A]): A => A = this match
    case RegularMono => category.image
    case Mono => f => category.regularEpiMonoFactorization(f)._2
    case Homomorphism => identity
  
  case RegularMono, Mono, Homomorphism
