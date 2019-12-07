name := "Advent of Code 2019"
cancelable in Global := true
scalaVersion := "2.13.0"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies +=
  "com.storm-enroute" %% "scalameter-core" % "0.19"