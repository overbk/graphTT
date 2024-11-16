package com.pbpoplus.util

import scala.annotation.targetName

extension (a: Boolean)
  @targetName("implies")
  infix def ==>(b: => Boolean): Boolean = !a || b
