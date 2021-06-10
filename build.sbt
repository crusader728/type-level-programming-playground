name := "Type level programming playground"

version := "1.0"

scalaVersion := "2.13.6"

// https://mvnrepository.com/artifact/org.scala-lang/scala-reflect
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

// https://mvnrepository.com/artifact/com.chuusai/shapeless
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.7"

scalacOptions ++= Seq(
  "-Xlog-implicits"
)
