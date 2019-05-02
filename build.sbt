scalaOrganization in ThisBuild := "org.typelevel"
scalaVersion      in ThisBuild := "2.12.1"

scalacOptions in Global ++= Seq(
    "-deprecation"
  , "-encoding", "UTF-8"
   , "-unchecked"
  , "-feature"
   , "-Xlint"
   , "-Xfatal-warnings"
   , "-Ywarn-dead-code"
  , "-Yliteral-types"
  )

libraryDependencies in Global ++= Seq(
    "com.chuusai"    %% "shapeless"     % "2.3.3"
  , "org.hsqldb"     %  "hsqldb"        % "2.3.4"
  , "org.scalactic"  %% "scalactic"     % "3.0.7" % Test
  , "org.scalatest"  %% "scalatest"     % "3.0.7" % Test
  )

lazy val api =
  project
    .in(file("api"))
    .settings(libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value)

lazy val persistence =
  project.in(file("persistence"))

lazy val root = project.in(file("."))
  .aggregate(
    api
  , persistence
  )
