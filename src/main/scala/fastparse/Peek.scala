package fastparse.yaml

import fastparse.core.Mutable
import fastparse.core.{Precedence, ParseCtx, Parsed, Parser}
/**
 *  Same as fastparse.parsers.Combinators.Lookahead but keeps the value.
 */
case class Peek[T](p: Parser[T]) extends Parser[T]{
  def parseRec(cfg: ParseCtx, index: Int) = {
    p.parseRec(cfg, index) match{
      case s: Mutable.Success[T] =>
        s.cut = false
        success(cfg.success, s.value, index, s.traceParsers, false)
      case f: Mutable.Failure =>
        f.cut = false
        failMore(f, index, cfg.logDepth)
    }
  }
  override def toString = s"Peek($p)"
}

