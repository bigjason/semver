package com.bigjason.semver

import org.scalatest.{FunSpec, Matchers}

class VersionSpec extends FunSpec with Matchers {
  describe("version preconditions") {
    it("should not allow negative major version") {
      assertThrows[IllegalArgumentException] {
        Version(-1, 0, 0)
      }
    }

    it("should not allow negative minor version") {
      assertThrows[IllegalArgumentException] {
        Version(0, -1, 0)
      }
    }

    it("should not allow negative patch version") {
      assertThrows[IllegalArgumentException] {
        Version(0, 0, -1)
      }
    }

    it("should not allow a completely 0 version (aka 0.0.0)") {
      assertThrows[IllegalArgumentException] {
        Version(0, 0, 0)
      }
    }
  }

  describe("toString") {
    it("should contain the entire encoded version") {
      val version = Version(1, 2, 3, List("beta", "1"), List("mac", "19"))
      version.toString should endWith(version.value)
      version.toString should startWith("v")
    }
  }

  describe("incrementing the major version") {
    it("should clear all lower values") {
      Version(1, 1, 1).incMajor should equal(Version(2, 0, 0))
      Version(1, 1, 0).incMajor should equal(Version(2, 0, 0))
      Version(0, 1123, 123).incMajor should equal(Version(1, 0, 0))
    }

    it("should clear the pre-release data") {
      Version(1, 1, 0, List("beta", "1"), Nil).incMajor should equal(Version(2, 0, 0))
      Version(1, 1, 100, List("beta"), Nil).incMajor should equal(Version(2, 0, 0))
    }

    it("should preserve the beta information") {
      val beta = List("ios", "10")
      Version(1, 1, 0, Nil, beta).incMajor.tupled should equal(Version(2, 0, 0, Nil, beta).tupled)
      Version(1, 1, 0, Nil, beta).incMajor.tupled shouldNot equal(Version(2, 0, 0).tupled)
      Version(200, 1, 100, Nil, beta).incMajor.tupled should equal(Version(201, 0, 0, Nil, beta).tupled)
    }
  }

  describe("incrementing the minor version") {
    it("should clear all lower values") {
      Version(1, 1, 1).incMinor should equal(Version(1, 2, 0))
      Version(1, 1, 0).incMinor should equal(Version(1, 2, 0))
      Version(0, 1123, 123).incMinor should equal(Version(0, 1124, 0))
    }

    it("should clear the pre-release data") {
      Version(1, 1, 0, List("beta", "1"), Nil).incMinor should equal(Version(1, 2, 0))
      Version(1, 1, 100, List("beta"), Nil).incMinor should equal(Version(1, 2, 0))
    }

    it("should preserve the beta information") {
      val beta = List("ios", "10")
      Version(1, 1, 0, Nil, beta).incMinor.tupled should equal(Version(1, 2, 0, Nil, beta).tupled)
      Version(1, 1, 0, Nil, beta).incMinor.tupled shouldNot equal(Version(1, 2, 0).tupled)
      Version(200, 1, 100, Nil, beta).incMinor.tupled should equal(Version(200, 2, 0, Nil, beta).tupled)
    }
  }

  describe("incrementing the patch version") {
    it("should clear no other value") {
      Version(1, 1, 1).incPatch should equal(Version(1, 1, 2))
      Version(1, 1, 0).incPatch should equal(Version(1, 1, 1))
      Version(0, 1123, 123).incPatch should equal(Version(0, 1123, 124))
    }

    it("should clear the pre-release data") {
      Version(1, 1, 0, List("beta", "1"), Nil).incPatch should equal(Version(1, 1, 1))
      Version(1, 1, 100, List("beta"), Nil).incPatch should equal(Version(1, 1, 101))
    }

    it("should preserve the beta information") {
      val beta = List("ios", "10")
      Version(1, 1, 0, Nil, beta).incPatch.tupled should equal(Version(1, 1, 1, Nil, beta).tupled)
      Version(1, 1, 0, Nil, beta).incPatch.tupled shouldNot equal(Version(1, 1, 1).tupled)
      Version(200, 1, 100, Nil, beta).incPatch.tupled should equal(Version(200, 1, 101, Nil, beta).tupled)
    }
  }

  describe("incrementing the preRelease") {
    it("should increment the last part") {
      val actual = Version(1, 0, 0).withPreRelease(List("alpha", "1")).incPreRelease
      actual.preReleaseString.endsWith("2")
    }

    it("should add a new part with the number 1 if the prerelease has no trailing digits") {
      val actual = Version(1, 0, 0).withPreRelease(List("alpha")).incPreRelease
      actual.preReleaseString.endsWith("1")
    }
  }

  describe("setting the prerelease") {
    val pr = List("alpha", "8")
    val prString = pr.mkString(".")

    it("should update when passed a collection of parts") {
      val updated = Version(1, 1, 1).withPreRelease(pr)
      updated.preRelease should equal(pr)
      updated.value should endWith(prString)
    }

    it("should update when passed a string") {
      Version(1, 0, 0).withPreRelease(prString) match {
        case None          => fail("did not parse string for prerelease")
        case Some(updated) =>
          updated.preRelease should equal(pr)
          updated.value should endWith("-" + prString)
      }
    }

    it("should not update when passed a malformed string") {
      val str = "@No__+"
      Version(1, 0, 0).withPreRelease(str) match {
        case None          => succeed
        case Some(updated) => fail(s"should not have allowed malformed string: $str")
      }
    }
  }

  describe("setting the build info") {
    val bi = List("ios", "10")
    val biStr = bi.mkString(".")

    it("should change when passed a collection of parts") {
      val updated = Version(1, 0, 0).withBuildInfo(bi)
      updated.buildInfo should equal(bi)
      updated.value should endWith("+" + biStr)
    }

    it("should change when passed a string") {
      Version(1, 0, 0).withBuildInfo(biStr) match {
        case None          => fail("did not parse string for build info")
        case Some(updated) =>
          updated.buildInfo should equal(bi)
          updated.value should endWith("+" + biStr)
      }
    }

    it("should not change when passed a malformed string") {
      val str = "@No__+"
      Version(1, 0, 0).withBuildInfo(str) match {
        case None          => succeed
        case Some(updated) => fail(s"should not have allowed malformed string: $str")
      }
    }
  }

  describe("encoded value") {
    List[(String, String => Unit)](
      ("1.0.0", { s => s should startWith("1.") })
      , ("1.1.0", { s => s should startWith("1.1.") })
      , ("1.1.1", { s => s should startWith("1.1.1") })
      , ("199999.0.0", { s => s should startWith("199999.") })
      , ("1.1.1-alpha.4", { s => s should include("-alpha.4") })
      , ("1.1.1+ios.10", { s => s should include("+ios.10") })
    ).foreach { case (input, predicate) =>
      it(s"should output parsed $input properly") {
        Version(input) match {
          case None          => fail("input string could not be parsed")
          case Some(version) => predicate(version.value)
        }
      }
    }
  }

  describe("equality comparisons") {
    it("should be equal to matching versions") {
      v"1.2.3" should equal(v"1.2.3")
      v"1.2.3-rc.1" should equal(v"1.2.3-rc.1")
      v"1.2.3-rc.1" shouldNot equal(v"1.2.3-rc.2")
    }

    it("should be equal to matching versions regardless of build info") {
      v"1.2.3+ios.9" should equal(v"1.2.3+ios.10")
      v"1.2.3-rc.1+ios.9" should equal(v"1.2.3-rc.1+ios.10")
      v"1.2.3-rc.2+ios.10" shouldNot equal(v"1.2.3-rc.1+ios.10")
    }

    it("should order for major differences") {
      v"1.0.0" should equal(v"1.0.0")
      v"1.2.3" should be < v"2.2.3"
      v"1.2.3" should be <= v"1.2.3"
      v"5.0.0" should be > v"1.2.3"
      v"5-beta.1" should be > v"5-alpha.1"
      v"5" should be >= v"5"
    }

    it("should order for minor differences") {
      v"1.2" should equal(v"1.2")
      v"1.2.3" should be < v"1.5.3"
      v"0.2.3" should be <= v"0.2.3"
      v"5.5.0" should be > v"5.2.3"
      v"5.5.0" should be > v"5.2.2"
      v"5.5.0" should be >= v"5.5.0"
    }

    it("should order for patch differences") {
      v"1.2.1" should equal(v"1.2.1")
      v"1.2.3" should be < v"1.5.4"
      v"0.2.3" should be <= v"0.2.3"
      v"5.5.3" should be > v"5.5.0"
      v"5.5.1" should be >= v"5.5.1"
    }

    it("should order for pre-release differences") {
      v"1.2.1-alpha" should equal(v"1.2.1-alpha")
      v"1.2.1-alpha.0" should equal(v"1.2.1-alpha")
      v"1.2.3-BETA.1" should equal(v"1.2.3-beta.1")
      v"1.2.3-beta.1" should be < v"1.2.3-rc.1"
      v"1.2.3-beta.1" should be < v"1.2.3-beta.4"
      v"1.2.3-beta.1" should be <= v"1.2.3-beta.1"
      v"1.2.3-rc.1" should be > v"1.2.3-beta.1"
      v"1.2.3-beta.4" should be > v"1.2.3-beta.1"
      v"1.2.3-beta.1" should be >= v"1.2.3-beta.1"
    }
  }
}