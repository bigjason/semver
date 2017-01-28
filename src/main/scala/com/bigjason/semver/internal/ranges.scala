package com.bigjason.semver.internal

sealed trait Part
case class NumericPart(i: Int) extends Part
case object WildcardPart extends Part

sealed trait Operator {val symbol: List[String]}
object Operator {
  val All: Map[String, Operator] = List(LT, GT, LTE, GTE, EQ).flatMap(o => o.symbol.map(_ -> o)).toMap
  val AllOperators: List[String] = All.keys.toList
}
case object LT extends Operator {override val symbol = List("<")}
case object GT extends Operator {override val symbol = List(">")}
case object LTE extends Operator {override val symbol = "<=".permutations.toList}
case object GTE extends Operator {override val symbol = ">=".permutations.toList}
case object EQ extends Operator {override val symbol = List("=", "==")}

sealed trait Range

sealed trait PartialVersion extends Range
case class MajorThenWild(major: Int) extends PartialVersion
case class MinorThenWild(major: Int, minor: Int) extends PartialVersion
case class PatchThenWild(major: Int, minor: Int, patch: Int) extends PartialVersion
case object Wild extends PartialVersion

case class EqualTo(rhs: PartialVersion) extends Range
case class GreaterThan(rhs: PartialVersion) extends Range
case class GreaterThanOrEqual(rhs: PartialVersion) extends Range
case class LessThan(rhs: PartialVersion) extends Range
case class LessThanOrEqual(rhs: PartialVersion) extends Range
case class Or(lhs: Range, rhs: Range) extends Range
case class And(lhs: Range, rhs: Range) extends Range
case class Hyphen(lhs: Range, rhs: Range) extends Range
case class Tilde(rhs: PartialVersion) extends Range
case class Caret(rhs: PartialVersion) extends Range
case class RangeList(ranges: List[Range]) extends Range {
  def ++(that: RangeList): RangeList = RangeList(ranges ++ that.ranges)
  def :+(that: Range): RangeList = RangeList(ranges :+ that)
}
object RangeList {
  val empty: RangeList = RangeList(Nil)
}