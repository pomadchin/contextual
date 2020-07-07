name := "contextual"

version := "0.1"

scalaVersion := "2.13.3"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-Ymacro-annotations",
  "-language:implicitConversions",
  "-language:reflectiveCalls",
  "-language:higherKinds",
  "-language:postfixOps",
  "-language:existentials",
  "-feature",
  "-Ywarn-unused:imports",         // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",          // Warn if a local definition is unused.
  "-Ywarn-unused:patvars",         // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",        // Warn if a private member is unused.
  "-Ywarn-unused:implicits",       // Warn if an import selector is not referenced.
  "-Xlint:adapted-args",           // Warn if an argument list is modified to match the receiver.
  "-Xlint:delayedinit-select",     // Selecting member of DelayedInit.
  "-Xlint:doc-detached",           // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",           // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",              // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",   // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override",       // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit",           // Warn when nullary methods return Unit.
  "-Xlint:option-implicit",        // Option.apply used implicit view.
  "-Xlint:package-object-classes", // Class or object defined in package object.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",         // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",            // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",  // A local type parameter shadows a type already in scope.
  "-Xlint:constant"                // Evaluation of a constant arithmetic expression results in an error.
)

val tofuVersion = "0.7.8"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic"          % "1.2.3",
  "org.typelevel"  %% "cats-core"               % "2.1.1",
  "org.typelevel"  %% "cats-effect"             % "2.1.3",
  "ru.tinkoff"     %% "tofu-logging"            % tofuVersion,
  "ru.tinkoff"     %% "tofu-logging-derivation" % tofuVersion,
  "ru.tinkoff"     %% "tofu-core"               % tofuVersion,
  "ru.tinkoff"     %% "tofu-derivation"         % tofuVersion,
  "ru.tinkoff"     %% "tofu-optics-core"        % tofuVersion,
  "ru.tinkoff"     %% "tofu-optics-macro"       % tofuVersion
)

addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1")
addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.11.0" cross CrossVersion.full)
