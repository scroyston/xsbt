import sbt._

trait NoUpdate extends ManagedBase with EmptyTask
{
	override final def updateAction = emptyTask
}
trait NoCrossPaths extends Project
{
	override def disableCrossPaths = true
}
trait JavaProject extends BasicScalaProject with NoCrossPaths
{
	// ensure that interfaces are only Java sources and that they cannot reference Scala classes
//	override def mainSources = descendents(mainSourceRoots, "*.java")
	override def compileOrder = CompileOrder.JavaThenScala
}
trait SourceProject extends BasicScalaProject with NoCrossPaths
{
	override def packagePaths = mainResources +++ mainSources // the default artifact is a jar of the main sources and resources
}
trait ManagedBase extends BasicScalaProject
{
	override def deliverScalaDependencies = Nil
	override def managedStyle = ManagedStyle.Ivy
	override def useDefaultConfigurations = false
	val defaultConf = Configurations.Default
	val testConf = Configurations.Test
}
trait Component extends DefaultProject
{
	override def projectID = componentID match { case Some(id) => super.projectID extra("e:component" -> id); case None => super.projectID }
	def componentID: Option[String] = None
}
trait PrecompiledInterface extends BasicScalaProject with ManagedBase
{
	override def fullClasspath(config: Configuration) = super.fullClasspath(config) filter(f => !f.asFile.getName.contains("scala-compiler"))
	def binID = "compiler-interface-bin"
	def bincID = binID + "_" + buildScalaInstance.actualVersion
	override def jarPath = mkJarPath(binID)
	override def defaultMainArtifact = Artifact(binID) extra("e:component" -> bincID)
	def mkJarPath(id: String) = outputPath / (id + "-" + version.toString + ".jar")
	override def ivyXML: scala.xml.NodeSeq = <publications/> // Remove when we build with 0.7.3, which does not unnecessarily add a default artifact
}
trait EmptyTask extends Project {
	def emptyTask = task {None}
}
trait NoRemotePublish extends BasicManagedProject with EmptyTask
{
	override def deliverAction = emptyTask
	override def publishAction = emptyTask
}
trait NoLocalPublish extends BasicManagedProject with EmptyTask
{
	override def publishLocalAction = emptyTask
	override def deliverLocalAction = emptyTask
}
trait NoPublish extends NoLocalPublish with NoRemotePublish
