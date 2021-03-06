package com.bigjason.semver

import java.util.Objects

import scala.util.Try

import com.bigjason.semver.internal.VersionParser
import fastparse.core.Parsed.{Failure, Success}

final case class Version(major: Int, minor: Int, patch: Int, preRelease: List[String], buildInfo: List[String]) extends Ordered[Version] {
  import Version.PreRelease

  // Note: The parser should help avoid at least some of this. These checks are here as a fail safe.
  require(major >= 0, s"major is $major but must be 0 or greater")
  require(minor >= 0, s"minor is $minor but must be 0 or greater")
  require(patch >= 0, s"patch is $patch but must be 0 or greater")
  require(major > 0 || minor > 0 || patch > 0, "at least one part of the version must be greater than 0")

  def incMajor: Version = Version(major + 1, 0, 0, Nil, buildInfo)
  def incMinor: Version = Version(major, minor + 1, 0, Nil, buildInfo)
  def incPatch: Version = Version(major, minor, patch + 1, Nil, buildInfo)
  def incPreRelease: Version = {
    val (pre, ver) = preReleaseCompTuple
    copy(preRelease = pre :+ (ver + 1).toString)
  }

  def withBuildInfo(s: String): Option[Version] = VersionParser.metaData.parse(s) match {
    case Success(build, _) => Some(withBuildInfo(build))
    case Failure(_, _, _)  => None
  }
  def withBuildInfo(build: List[String]): Version = copy(buildInfo = build)

  def withPreRelease(s: String): Option[Version] = VersionParser.metaData.parse(s) match {
    case Success(preRel, _) => Some(withPreRelease(preRel))
    case Failure(_, _, _)   => None
  }
  def withPreRelease(preRel: List[String]): Version = copy(preRelease = preRel)

  @inline def preReleaseString: String = preRelease.mkString(".")
  @inline def preReleaseVersion: Int = preRelease.lastOption.flatMap(s => Try(s.toInt).toOption).getOrElse(0)
  @inline def buildInfoString: String = buildInfo.mkString(".")

  override def toString = s"v$value"

  def value: String = {
    val sb = new StringBuilder(64)
    sb.append(major).append('.')
    sb.append(minor).append('.')
    sb.append(patch)
    if (preRelease.nonEmpty) sb.append('-').append(preReleaseString)
    if (buildInfo.nonEmpty) sb.append('+').append(buildInfoString)
    sb.toString
  }

  private[semver] def preReleaseCompTuple: (List[String], Int) = preRelease.lastOption.flatMap(s => Try(s.toInt).toOption) match {
    case Some(v) => (preRelease.take(preRelease.length - 1), v)
    case None    => (preRelease, 0)
  }

  private[semver] def getPreRelease = {
    val (rest, ver) = preReleaseCompTuple
    val restStr = rest.mkString(".")
    PreRelease(restStr, ver)
  }

  override def compare(that: Version): Int = that match {
    case Version(`major`, `minor`, `patch`, `preRelease`, _) => 0
    case Version(`major`, `minor`, `patch`, _, _)            =>
      getPreRelease compare that.getPreRelease
    case Version(`major`, `minor`, _, _, _)                  =>
      patch compare that.patch
    case Version(`major`, _, _, _, _)                        =>
      minor compare that.minor
    case Version(_, _, _, _, _)                              =>
      major compare that.major
  }

  override def equals(obj: scala.Any) = obj match {
    case that: Version => compare(that) == 0
    case _             => false
  }

  override def hashCode() =
    Objects.hash(Int.box(major), Int.box(minor), Int.box(patch), preRelease)

  def tupled = Version.unapply(this).get
}

object Version {
  private[semver] final case class PreRelease(pre: String, version: Int) extends Ordered[PreRelease] {
    override def compare(that: PreRelease): Int =
      if (pre equalsIgnoreCase that.pre) version compare that.version
      else pre compareToIgnoreCase that.pre
  }

  @inline
  private[semver] def parseWith(s: String, parser: VersionParser.VersionParserImpl): Either[String, Version] = parser.parse(s) match {
    case Success((major, minor, patch, pre, build), _) =>
      Right(Version(major, minor, patch, pre.getOrElse(Nil), build.getOrElse(Nil)))
    case failure@Failure(_, _, _)                      =>
      Left(failure.msg)
  }

  def apply(major: Int, minor: Int, patch: Int): Version = new Version(major, minor, patch, Nil, Nil)
  def apply(s: String): Option[Version] = parse(s).right.toOption
  def parse(s: String): Either[String, Version] = parseWith(s, VersionParser.version)
  def parseDirty(s: String): Either[String, Version] = parseWith(s, VersionParser.versionDirty)
}