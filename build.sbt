name := "bananas-lenses-envelopes-and-barbed-wire"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

scalacOptions in ThisBuild ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:higherKinds",
//  "-Xprint:typer",
  "-deprecation"  
)
