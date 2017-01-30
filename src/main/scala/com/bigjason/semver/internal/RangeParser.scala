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
  val primitive: P[Comparator]     = P(operator ~ partial).map {
    case (`LT`, v)  => LessThan(v)
    case (`LTE`, v) => LessThanOrEqual(v)
    case (`GT`, v)  => GreaterThan(v)
    case (`GTE`, v) => GreaterThanOrEqual(v)
    case (`EQ`, v)  => EqualTo(v)
    case (`NEQ`, v) => Not(EqualTo(v))
  }
  val tilde    : P[ComparatorSet]  = P("~" ~ whiteSpaceOpt ~ partial).map {
    case ver@MajorThenWild(major)       => ComparatorSet.from(ver, MajorThenWild(major + 1))
    case ver@MinorThenWild(_, minor)    => ComparatorSet.from(ver, ver.copy(minor = minor + 1))
    case ver@PatchThenWild(_, minor, _) => ComparatorSet.from(ver, ver.copy(minor = minor + 1))
    case Wild                           => ComparatorSet.from(MajorThenWild(0), MajorThenWild(1))
  }
  val caret    : P[ComparatorSet]  = P("^" ~ whiteSpaceOpt ~ partial).map {
    case MajorThenWild(0) | Wild          => ComparatorSet.from(MajorThenWild(0), MajorThenWild(1))
    case start@MajorThenWild(major)       => ComparatorSet.from(start, MajorThenWild(major + 1))
    case start@MinorThenWild(0, minor)    => ComparatorSet.from(start, MinorThenWild(0, minor + 1))
    case start@MinorThenWild(major, _)    => ComparatorSet.from(start, start.copy(major = major + 1))
    case start@PatchThenWild(0, 0, patch) => ComparatorSet.from(start, start.copy(patch = patch + 1))
    case start@PatchThenWild(0, minor, _) => ComparatorSet.from(start, MinorThenWild(0, minor + 1))
    case start@PatchThenWild(major, _, _) => ComparatorSet.from(start, MajorThenWild(major + 1))
  }
  val simple   : P[ComparatorSet]  = P {
    primitive.map(ComparatorSet.single) | partial.map(EqualTo).map(ComparatorSet.single) | caret | tilde
  }
  val hyphen   : P[ComparatorSet]  = P(partial ~ whiteSpace(1) ~ "-" ~ whiteSpace(1) ~ partial).map((ComparatorSet.from _).tupled)
  val range    : P[ComparatorSet]  = P {
    hyphen | simple.rep(min = 1, sep = whiteSpace(1)).map(_.toList).map(_.foldLeft(ComparatorSet.empty)(_ ++ _))
  }
  val logicalOr: P[Unit]           = P(whiteSpaceOpt ~ "||" ~ whiteSpaceOpt)
  val rangeSet : P[RangeList]      = P(range.rep(min = 1, sep = logicalOr)).map(_.foldLeft(RangeList.empty)(_ :+ _))
}
