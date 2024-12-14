scalaVersion := "3.3.0"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client3" %% "core" % "3.9.0",
  "com.softwaremill.sttp.client3" %% "circe" % "3.9.0",
  "io.circe" %% "circe-parser" % "0.14.5",
  "io.circe" %% "circe-generic" % "0.14.5"
)

