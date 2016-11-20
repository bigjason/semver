package com.bigjason.semver.internal

import com.bigjason.semver.Version

sealed trait Range {
  def satisfiedBy(v: Version): Boolean
}

case class EqualTo(rhs: Version) extends Range {
  override def satisfiedBy(v: Version): Boolean = v == rhs
}

case class GreaterThan(rhs: Version) extends Range {
  override def satisfiedBy(v: Version): Boolean = v > rhs
}

case class GreaterThanOrEqual(rhs: Version) extends Range {
  override def satisfiedBy(v: Version): Boolean = v >= rhs
}

case class LessThan(rhs: Version) extends Range {
  override def satisfiedBy(v: Version): Boolean = v < rhs
}

case class LessThanOrEqual(rhs: Version) extends Range {
  override def satisfiedBy(v: Version): Boolean = v <= rhs
}

case class Or(lhs: Range, rhs: Range) extends Range {
  override def satisfiedBy(v: Version): Boolean = lhs.satisfiedBy(v) || rhs.satisfiedBy(v)
}

case class And(lhs: Range, rhs: Range) extends Range {
  override def satisfiedBy(v: Version): Boolean = lhs.satisfiedBy(v) && rhs.satisfiedBy(v)
}
