name := "wanaboat"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  filters,
  "org.postgresql" % "postgresql" % "9.4-1201-jdbc41",
  jdbc,
  ws,
  "com.typesafe.play" %% "anorm" % "2.5.3",
  "org.apache.lucene" % "lucene-core" % "6.3.0",
  "org.mindrot" % "jbcrypt" % "0.3m",
  "org.flywaydb" %% "flyway-play" % "2.2.1",
  "com.typesafe.play" %% "play-mailer" % "3.0.1",
  "commons-io" % "commons-io" % "2.4"
)

play.sbt.PlayScala.projectSettings

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings", "-Xlint")

TwirlKeys.templateImports ++= Seq("models._", "views._")

//initialCommands in console := """play.core.server.ProdServerStart.main(Array())"""

// offline := true

// skip in update := true

routesGenerator := InjectedRoutesGenerator
