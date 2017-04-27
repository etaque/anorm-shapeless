name := "anorm-shapeless"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.postgresql" % "postgresql" % "9.4-1201-jdbc41",
  jdbc,
  "com.typesafe.play" %% "anorm" % "2.5.3",
  "com.chuusai" %% "shapeless" % "2.3.2"
)

play.sbt.PlayScala.projectSettings

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings", "-Xlint")

routesGenerator := InjectedRoutesGenerator
