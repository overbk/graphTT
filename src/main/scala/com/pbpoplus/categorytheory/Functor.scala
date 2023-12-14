package com.pbpoplus.categorytheory

import scala.annotation.targetName
import com.pbpoplus.categorytheory.Category

final case class Functor[O1, A1, O2, A2](objectMap: Map[O1, O2], arrowMap: Map[A1, A2]):

  @targetName("applyObject")
  def apply(o: O1): O2 = objectMap(o)

  @targetName("applyArrow")
  def apply(f: A1): A2 = arrowMap(f)

  override def toString: String =
    val objects = objectMap.map((x, y) => s"$x = $y")
    val arrows = arrowMap.map((x, y) => s"$x = $y")
    (objects ++ arrows).mkString("\n")
