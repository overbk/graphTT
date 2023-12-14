package com.pbpoplus.parsing

import scala.util.parsing.combinator.*
import scala.util.parsing.combinator.Parsers
import com.pbpoplus.rewriting.RewriteRule

object Util:
  def parse[X](parsers: RegexParsers, parser: RegexParsers#Parser[X], input: String): X =
    parsers.parseAll(parser.asInstanceOf[parsers.Parser[X]], input) match {
      case parsers.Success(result, _) => result
      case x: parsers.Failure => throw new RuntimeException(x.toString)
      case x: parsers.Error => throw new RuntimeException(x.toString)
    }
    