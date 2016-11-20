package com.bigjason.semver.internal

import fastparse.all._
import fastparse.core.Parser

object VersionParser {
  type VersionParserImpl = Parser[(Int, Int, Int, Option[List[String]], Option[List[String]]), Char, String]

  val alphaNum = ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z')
  val isWhitespace = NamedFunction(Character.isWhitespace, "isWhiteSpace")
  val validParts = NamedFunction[(Int, Int, Int), Boolean]({
    case (major: Int, minor: Int, patch: Int) => major > 0 || minor > 0 || patch > 0
  }, "validParts")

  val part: P[Int] = P((CharIn('1' to '9') ~/ CharIn('0' to '9').rep).! | "0".!).map(_.toInt)
  val dirtyParts: P[(Int, Int, Int)] = P(part ~ ("." ~ part ~ ("." ~ part).?).?).map {
    case (f, None)               => (f, 0, 0)
    case (f, Some((s, None)))    => (f, s, 0)
    case (f, Some((s, Some(t)))) => (f, s, t)
  }.filter(validParts)
  val strictParts: P[(Int, Int, Int)] = P(part ~/ "." ~ part ~/ "." ~ part).filter(validParts)

  val metaData: P[List[String]] = P(CharIn(alphaNum, Seq('-')).rep(min = 1).!.rep(min = 1, sep = ".").map(v => v.toList))
  def meta(prefix: String): P[List[String]] = P(prefix ~ metaData)
  val pre: P[List[String]] = meta("-")
  val build: P[List[String]] = meta("+")

  val version: VersionParserImpl =
    P(Start ~ ("=" | "v").?  ~/ strictParts ~ pre.? ~ build.? ~ End)

  val versionDirty: VersionParserImpl =
    P(Start ~ ("=" | "v" | CharsWhile(isWhitespace)).rep ~/ dirtyParts ~ pre.? ~ build.? ~ CharsWhile(isWhitespace).rep ~ End)
}
