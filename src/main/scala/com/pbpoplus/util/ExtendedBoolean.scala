package com.pbpoplus.util

implicit class extendedBoolean(a: Boolean):
  infix def ==>(b: => Boolean) = !a || b
  