package com.bigjason.semver.internal

/**
  * === Based on BNF from [[https://github.com/npm/node-semver]] ===
  * {{{
  * range-set  ::= range ( logical-or range ) *
  * logical-or ::= ( ' ' ) * '||' ( ' ' ) *
  * range      ::= hyphen | simple ( ' ' simple ) * | ''
  * hyphen     ::= partial ' - ' partial
  * simple     ::= primitive | partial | tilde | caret
  * primitive  ::= ( '<' | '>' | '>=' | '<=' | '=' | ) partial
  * partial    ::= xr ( '.' xr ( '.' xr qualifier ? )? )?
  * xr         ::= 'x' | 'X' | '*' | nr
  * nr         ::= '0' | ['1'-'9'] ( ['0'-'9'] ) *
  * tilde      ::= '~' partial
  * caret      ::= '^' partial
  * qualifier  ::= ( '-' pre )? ( '+' build )?
  * pre        ::= parts
  * build      ::= parts
  * parts      ::= part ( '.' part ) *
  * part       ::= nr | [-0-9A-Za-z]+
  * }}}
  */
object RangeParser {
  import fastparse.all._

  val isWhiteSpace = NamedFunction(Character.isWhitespace, "isWhiteSpace")
  def whiteSpace(min: Int): P[Unit] = P(fastparse.all.CharsWhile(isWhiteSpace, min = min))
  val whiteSpaceOpt : P[Unit] = whiteSpace(0)
  val whiteSpaceChar: P[Unit] = whiteSpace(1)

  val nr       : P[Int]            = P(CharIn(Seq('0')).map(_ => 0) | (CharIn('1' to '9').! ~/ CharIn('0' to '9').rep.!).map { case (a, b) => a + b }.map(_.toInt))
  val xr       : P[Part]           = P(nr.map(NumericPart) | CharIn(Seq('x', 'X', '*')).map(_ => WildcardPart))
  val operator : P[Operator]       = P(StringIn(Operator.AllOperators: _*).!).map(Operator.All)
  val partial  : P[PartialVersion] = P(xr ~ ("." ~ xr ~ ("." ~ xr).?).?).map {
    case (WildcardPart, _)                                                     => Wild
    case (NumericPart(m), Some((WildcardPart, _)) | None)                      => MajorThenWild(m)
    case (NumericPart(m), Some((NumericPart(min), Some(WildcardPart) | None))) => MinorThenWild(m, min)
    case (NumericPart(m), Some((NumericPart(min), Some(NumericPart(p)))))      => PatchThenWild(m, min, p)
  }
  val primitive: P[Range]          = P(operator ~ partial).map {
    case (`LT`, v)  => LessThan(v)
    case (`LTE`, v) => LessThanOrEqual(v)
    case (`GT`, v)  => GreaterThan(v)
    case (`GTE`, v) => GreaterThanOrEqual(v)
    case (`EQ`, v)  => EqualTo(v)
  }
  val tilde    : P[Tilde]          = P("~" ~ whiteSpaceOpt ~ partial).map(Tilde)
  val caret    : P[Caret]          = P("^" ~ whiteSpaceOpt ~ partial).map(Caret)
  val simple   : P[Range]          = P(primitive | partial | caret | tilde)
  val hyphen   : P[Hyphen]         = P(partial ~ whiteSpace(1) ~ "-" ~ whiteSpace(1) ~ partial).map(Hyphen.tupled)
  val range    : P[Range]          = P {
    hyphen | simple.rep(min = 1, sep = whiteSpace(1)).map(_.toList).map(RangeList(_))
  }
  val logicalOr: P[Unit]           = P(whiteSpaceOpt ~ "||" ~ whiteSpaceOpt)
  val rangeSet : P[RangeList]      = P(range.rep(min = 1, sep = logicalOr)).map { items =>
    items.foldLeft(RangeList.empty) { (results, curr) =>
      curr match {
        case ranges: RangeList => results ++ ranges
        case other             => results :+ other
      }
    }
  }
}
