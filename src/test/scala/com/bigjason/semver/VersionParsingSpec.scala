package com.bigjason.semver

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop._

class VersionParsingSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  property("strict parsing") {
    val strictVersions = Table(
      ("Version String", "Expected")
      , ("0.0.1", Some(Version(0, 0, 1)))
      , ("0.1.0", Some(Version(0, 1, 0)))
      , ("1.0.0", Some(Version(1, 0, 0)))
      , ("199.200.444", Some(Version(199, 200, 444)))
      , ("1.0.0-beta", Some(Version(1, 0, 0, List("beta"), Nil)))
      , ("1.0.0-beta.1", Some(Version(1, 0, 0, List("beta", "1"), Nil)))
      , ("1.0.0+ios", Some(Version(1, 0, 0, Nil, List("ios"))))
      , ("1.0.0+ios.10", Some(Version(1, 0, 0, Nil, List("ios", "10"))))
      , ("01.0.0", None)
    )

    forAll(strictVersions) { (input, expected) =>
      val actual = Version(input)
      actual should equal(expected)
    }
  }

  property("dirty parsing") {
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
      , (" 199.200.444 ", Some(Version(199, 200, 444)))
      , ("00.0.0", None)
      , ("0.0.0", None)
      , ("0.0", None)
      , ("0", None)
    )

    forAll(dirtyVersions) { (input, expected) =>
      val actual = Version.parseDirty(input).right.toOption
      actual should equal(expected)
    }
  }
}

class VersionGeneratedParsingSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with Inside {
  private val versionStringGen = for {
    major <- Gen.choose(0, 99000)
    minor <- Gen.choose(0, 99000)
    patch <- Gen.choose(0, 99000)
  } yield (s"$major.$minor.$patch", (major, minor, patch))

  property("dirty parsing") {
    forAll((versionStringGen, "versionString")) { case (raw, (maj, min, patch)) =>
      val input = s" $raw "
      val actual = Version.parseDirty(input)
      inside(actual) {
        case Right(Version(`maj`, `min`, `patch`, _, _)) => succeed
      }
    }
  }

  property("clean parsing") {
    forAll((versionStringGen, "versionString")) { case (input, (maj, min, patch)) =>
      val actual = Version.parse(input)
      inside(actual) {
        case Right(Version(`maj`, `min`, `patch`, _, _)) => succeed
      }
    }
  }
}