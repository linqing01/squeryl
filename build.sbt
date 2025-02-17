name := "squeryl"

description := "A Scala ORM and DSL for talking with Databases using minimum verbosity and maximum type safety"

organization := "org.squeryl"

version := "0.9.16"

javacOptions := Seq("-source", "1.8", "-target", "1.8")

//only release *if* -Drelease=true is passed to JVM
version := {
  val v = version.value
  val release = Option(System.getProperty("release")).contains("true")
  if (release)
    v
  else {
    val suffix = Option(System.getProperty("suffix"))
    val i = (v.indexOf('-'), v.length) match {
      case (x, l) if x < 0 => l
      case (x, l) if v substring (x + 1) matches """\d+""" => l //patch level, not RCx
      case (x, _) => x
    }
    v.substring(0, i) + "-" + (suffix getOrElse "SNAPSHOT")
  }
}

parallelExecution := false

publishMavenStyle := true

val Scala213 = "2.13.4"

scalaVersion := Scala213

crossScalaVersions := Seq(Scala213)

scalacOptions in(Compile, doc) ++= {
  val base = (baseDirectory in LocalRootProject).value.getAbsolutePath
  val hash = sys.process.Process("git rev-parse HEAD").lineStream_!.head
  Seq("-sourcepath", base, "-doc-source-url", "https://github.com/squeryl/squeryl/tree/" + hash + "€{FILE_PATH}.scala")
}

scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) if v >= 11 =>
      Seq(
        "-Xsource:3",
      )
    case _ =>
      Nil
  }
}

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:reflectiveCalls",
  "-language:existentials"
)

scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) if v <= 12 =>
      Seq("-Xfuture")
    case _ =>
      Nil
  }
}

val unusedWarnings = Def.setting(
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 10)) =>
      Nil
    case Some((2, 11)) =>
      Seq("-Ywarn-unused-import")
    case _ =>
      Seq("-Ywarn-unused:imports")
  }
)

scalacOptions ++= unusedWarnings.value

Seq(Compile, Test).flatMap(c =>
  scalacOptions in(c, console) --= unusedWarnings.value
)

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage := Some(url("https://squeryl.org"))

pomExtra := (<scm>
  <url>git@github.com:squeryl/squeryl.git</url>
  <connection>scm:git:git@github.com:squeryl/squeryl.git</connection>
</scm>
  <developers>
    <developer>
      <id>max-l</id>
      <name>Maxime Lévesque</name>
      <url>https://github.com/max-l</url>
    </developer>
    <developer>
      <id>davewhittaker</id>
      <name>Dave Whittaker</name>
      <url>https://github.com/davewhittaker</url>
    </developer>
  </developers>)

credentials ~= { c =>
  (Option(System.getenv().get("SONATYPE_USERNAME")), Option(System.getenv().get("SONATYPE_PASSWORD"))) match {
    case (Some(username), Some(password)) =>
      c :+ Credentials(
        "Sonatype Nexus Repository Manager",
        "oss.sonatype.org",
        username,
        password)
    case _ => c
  }
}

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

libraryDependencies ++= Seq(
  "cglib" % "cglib-nodep" % "3.3.0",
  "com.h2database" % "h2" % "1.4.200" % "provided",
  "mysql" % "mysql-connector-java" % "8.0.22" % "provided",
  "org.postgresql" % "postgresql" % "42.2.18" % "provided",
  "net.sourceforge.jtds" % "jtds" % "1.3.1" % "provided",
  "org.apache.derby" % "derby" % "10.11.1.1" % "provided",
  "org.xerial" % "sqlite-jdbc" % "3.34.0" % "test",
  "org.json4s" %% "json4s-scalap" % "3.6.10",
)

libraryDependencies ++= {
  Seq("org.scalatest" %% "scalatest" % "3.2.3" % "test")
}

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      Seq("org.scala-lang.modules" %% "scala-xml" % "1.3.0")
    case _ =>
      Nil
  }
}
