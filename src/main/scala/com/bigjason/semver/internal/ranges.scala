package com.bigjason.semver.internal

sealed trait Part
case class NumericPart(i: Int) extends Part
case object WildcardPart extends Part

sealed trait Operator {val symbol: List[String]}
object Operator {
  val All         : Map[String, Operator] = List(LT, GT, LTE, GTE, EQ, NEQ).flatMap(o => o.symbol.map(_ -> o)).toMap
  val AllOperators: List[String]          = All.keys.toList
}
case object LT extends Operator {override val symbol = List("<")}
case object GT extends Operator {override val symbol = List(">")}
case object LTE extends Operator {override val symbol = "<=".permutations.toList}
case object GTE extends Operator {override val symbol = ">=".permutations.toList}
case object EQ extends Operator {override val symbol = List("=", "==")}
case object NEQ extends Operator {override val symbol = List("!=", "/=")}

sealed trait PartialVersion {
  def tupled: (Int, Int, Int) = this match {
    case MajorThenWild(major)               => (major, 0, 0)
    case MinorThenWild(major, minor)        => (major, minor, 0)
    case PatchThenWild(major, minor, patch) => (major, minor, patch)
    case Wild                               => (0, 0, 0)
  }
}
case class MajorThenWild(major: Int) extends PartialVersion
case class MinorThenWild(major: Int, minor: Int) extends PartialVersion
case class PatchThenWild(major: Int, minor: Int, patch: Int) extends PartialVersion
case object Wild extends PartialVersion

sealed trait Satisfyable

sealed trait Comparator extends Satisfyable
case class EqualTo(rhs: PartialVersion) extends Comparator
case class GreaterThan(rhs: PartialVersion) extends Comparator
case class GreaterThanOrEqual(rhs: PartialVersion) extends Comparator
case class LessThan(rhs: PartialVersion) extends Comparator
case class LessThanOrEqual(rhs: PartialVersion) extends Comparator
case class Not(rhs: Comparator) extends Comparator

/**
  * Must match ALL of the [[Comparator]]'s
  */
case class ComparatorSet(items: List[Comparator]) extends Satisfyable {
  def ++(that: ComparatorSet): ComparatorSet = ComparatorSet(items ++ that.items)
  def :+(that: Comparator): ComparatorSet = ComparatorSet(items :+ that)
}
object ComparatorSet {
  val empty = ComparatorSet(Nil)

  def from(start: PartialVersion, end: PartialVersion): ComparatorSet =
    ComparatorSet(List(GreaterThanOrEqual(start), LessThan(end)))

  def single(item: Comparator): ComparatorSet = ComparatorSet(List(item))
}
/**
  * Match at least one comparator in the list.
  */
case class RangeList(ranges: List[Satisfyable]) extends Satisfyable {
  def ++(that: RangeList): RangeList = RangeList(ranges ++ that.ranges)
  def :+(that: Satisfyable): RangeList = RangeList(ranges :+ that)
}
object RangeList {
  val empty: RangeList = RangeList(Nil)
}