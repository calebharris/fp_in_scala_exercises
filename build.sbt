name := "functional_programming_in_scala"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ypartial-unification")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

//initialCommands in console += "import fpinscala.monoids._, Monoid._, MonoidInstances._, FoldableInstances._\n"
//initialCommands in console += "import fpinscala.functors._, Functor._, FunctorSyntax._\n"
//initialCommands in console += "import fpinscala.monads._, Monad._\n"
//initialCommands in console += "import fpinscala.applicatives._, Applicative._, Validation._\n"
//initialCommands in console += "import fpinscala.iomonad._, examples._\n"
//initialCommands in console += "import fpinscala.free._, Free._"
initialCommands in console += "import fpinscala.mutability._"