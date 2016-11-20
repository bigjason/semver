package com.bigjason.semver

import org.scalatest._
import prop._

class VersionParsingSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val strictVersions = Table(
    ("Version String", "Expected")
    , ("0.0.1", Some(Version(0, 0, 1)))
    , ("0.1.0", Some(Version(0, 1, 0)))
    , ("1.0.0", Some(Version(1, 0, 0)))
    , ("1.0.0-beta", Some(Version(1, 0, 0, List("beta"), Nil)))
    , ("1.0.0-beta.1", Some(Version(1, 0, 0, List("beta", "1"), Nil)))
    , ("1.0.0+ios", Some(Version(1, 0, 0, Nil, List("ios"))))
    , ("1.0.0+ios.10", Some(Version(1, 0, 0, Nil, List("ios", "10"))))
    , ("01.0.0", None)
  )

  property("strict parsing") {
    forAll(strictVersions) { (input, expected) =>
      val actual = Version(input)
      actual should equal(expected)
    }
  }

  val dirtyVersions = Table(
    ("Version String", "Expected")
    , ("0.0.1", Some(Version(0, 0, 1)))
    , ("0.1.0", Some(Version(0, 1, 0)))
    , ("1.0.0", Some(Version(1, 0, 0)))
    , ("v1.0.0", Some(Version(1, 0, 0)))
    , (" v 1.0.0", Some(Version(1, 0, 0)))
    , (" 1.0.0", Some(Version(1, 0, 0)))
    , (" =1.0.0", Some(Version(1, 0, 0)))
    , (" = 1.0.0", Some(Version(1, 0, 0)))
    , (" == 1.0.0  ", Some(Version(1, 0, 0)))
    , ("12", Some(Version(12, 0, 0)))
    , ("12.1", Some(Version(12, 1, 0)))
    , (s"${Int.MaxValue}.1.2", Some(Version(Int.MaxValue, 1, 2)))
    , ("00.0.0", None)
    , ("0.0.0", None)
    , ("0.0", None)
    , ("0", None)
  )

  property("dirty parsing") {
    forAll(dirtyVersions) { (input, expected) =>
      val actual = Version.parseDirty(input).right.toOption
      actual should equal(expected)
    }
  }
}
