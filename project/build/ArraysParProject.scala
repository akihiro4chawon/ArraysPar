import sbt._
import de.element34.sbteclipsify._

class ScalaAppletTestProject(info: ProjectInfo)
  extends DefaultProject(info)
  with BoilerplateForSorter
  with Eclipsify {
  
  val specsDependency = "org.scala-tools.testing" % "specs_2.9.0" % "1.6.8" % "test"

  val scalacheckDependency = "org.scala-tools.testing" % "scalacheck_2.9.0" % "1.9"
  
  override def mainSourceRoots = super.mainSourceRoots +++ srcManagedScala##
  override def compileAction = super.compileAction dependsOn(generateSort)
}

