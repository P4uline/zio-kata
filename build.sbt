name := "zio-kata"

version := "0.1"

scalaVersion := "3.10.0"

libraryDependencies += "dev.zio" %% "zio" % "2.1.27"
libraryDependencies += "dev.zio" %% "zio-streams" % "2.1.27"
libraryDependencies += "dev.zio" %% "zio-json" % "1.1.2"
libraryDependencies += "io.d11" %% "zhttp" % "2.0.0-RC11"

libraryDependencies += "dev.zio" %% "zio-test"     % "2.1.27" % Test
libraryDependencies += "dev.zio" %% "zio-test-sbt" % "2.1.27" % Test

