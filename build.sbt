lazy val fpInScala = (project in file(".")).
  settings(
    name    := "fpInScala",
    version := "1.0",
    scalaVersion := "2.11.7",
    sbtVersion   := "0.13.9",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.6" % "test"
    ),
    testOptions := Seq(Tests.Argument("-oS"))
  )
