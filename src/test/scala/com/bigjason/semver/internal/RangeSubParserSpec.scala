package com.bigjason.semver.internal

import fastparse.core.Parsed.{Failure, Success}
import org.scalatest._
import org.scalatest.prop._

class RangeSubParserSpec extends FunSpec with TableDrivenPropertyChecks with Matchers {
  describe("partial part parser") {
    val partialPartExamples = Table[String, PartialVersion](
      ("parts", "expected"),
      ("1.0.0", PatchThenWild(1, 0, 0)),
      ("12.123.1234", PatchThenWild(12, 123, 1234)),
      ("1 ", MajorThenWild(1)),
      ("15.16.256", PatchThenWild(15, 16, 256)),
      ("x", Wild),
      ("1.x", MajorThenWild(1)),
      ("1.x.X", MajorThenWild(1)),
      ("1.0.X", MinorThenWild(1, 0)),
      ("1.0.*", MinorThenWild(1, 0))
    )

    forAll(partialPartExamples) { case (parts, expected) =>
      it(s"should parse '$parts'") {
        RangeParser.partial.parse(parts) match {
          case Success(result, _)       => result should equal(expected)
          case failure@Failure(_, _, _) => fail(s"part did not parse: ${failure.msg}")
        }
      }
    }
  }

  describe("hyphen part parser") {
    val hyphenRangeExamples = Table[String, Hyphen](
      ("input string", "expected"),
      ("1 - 2", Hyphen(MajorThenWild(1), MajorThenWild(2))),
      ("1.0 - 2.11", Hyphen(MinorThenWild(1, 0), MinorThenWild(2, 11))),
      ("1.3.5 - 2.9.2", Hyphen(PatchThenWild(1, 3, 5), PatchThenWild(2, 9, 2))),
      ("1.3 - 2.9", Hyphen(MinorThenWild(1, 3), MinorThenWild(2, 9))),
      ("1.3 - 2", Hyphen(MinorThenWild(1, 3), MajorThenWild(2)))
    )

    forAll(hyphenRangeExamples) { case (input, expected) =>
      it(s"should parse '$input'") {
        RangeParser.hyphen.parse(input) match {
          case Success(result, _)       => result should equal(expected)
          case failure@Failure(_, _, _) => fail(s"part did not parse: ${failure.msg}")
        }
      }
    }

    val hyphenRangeFailExamples = Table[String](
      "bad input string",
      "1.3.5-2.9.2",
      "1.3-2.9",
      "1.3-2",
      "1.3- 2",
      "1.0 -2.11",
      "1.0- 2.11",
      "99-12",
      "",
      "2.2.2"
    )

    forAll(hyphenRangeFailExamples) { input =>
      it(s"should fail with whitespace of '$input'") {
        RangeParser.hyphen.parse(input) match {
          case Success(parsed, _) => fail(s"should have failed to parse: ${parsed.toString}")
          case Failure(_, _, _)   => succeed
        }
      }
    }
  }
}
