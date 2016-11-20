package com.bigjason

package object semver {
  implicit class VersionStringInterpolation(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Version = {
      val strings = sc.parts.iterator
      val values = args.iterator
      val versionBuilder = new StringBuilder(32)
      while (strings.hasNext) {
        versionBuilder.append(strings.next())
        if (values.hasNext)
          versionBuilder.append(values.next())
      }
      Version.parseDirty(versionBuilder.toString()) match {
        case Right(v)  => v
        case Left(msg) => throw new Exception(s"Not a valid version string '${versionBuilder.toString()}': $msg")
      }
    }
  }
}
