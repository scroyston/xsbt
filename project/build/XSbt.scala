import sbt._

import java.io.File

class XSbt(info: ProjectInfo) extends ParentProject(info) with NoCrossPaths
{
	/*** Subproject declarations ***/

		// defines the Java interfaces through which the launcher and the launched application communicate
	val launchInterfaceSub = project(launchPath / "interface", "Launcher Interface", new LaunchInterfaceProject(_))

		// the launcher.  Retrieves, loads, and runs applications based on a configuration file.
	val launchSub = project(launchPath, "Launcher", new LaunchProject(_), launchInterfaceSub)

		// defines Java structures used across Scala versions, such as the API structures and relationships extracted by
		//   the analysis compiler phases and passed back to sbt.  The API structures are defined in a simple 
		//   format from which Java sources are generated by the datatype generator subproject
	val interfaceSub = project("interface", "Interface", new InterfaceProject(_))

		// defines operations on the API of a source, including determining whether it has changed and converting it to a string
	val apiSub = baseProject(compilePath / "api", "API", interfaceSub)

	/***** Utilities *****/

	val controlSub = baseProject(utilPath / "control", "Control")
	val collectionSub = testedBase(utilPath / "collection", "Collections")
		// The API for forking, combining, and doing I/O with system processes
	val processSub = project(utilPath / "process", "Process", new Base(_) with TestWithIO)
		// Path, IO (formerly FileUtilities), NameFilter and other I/O utility classes
	val ioSub = testedBase(utilPath / "io", "IO", controlSub)
		// Utilities related to reflection, managing Scala versions, and custom class loaders
	val classpathSub = baseProject(utilPath / "classpath", "Classpath", launchInterfaceSub, ioSub)
		// Command line-related utilities.  Currently, history.
	val completeSub = project(utilPath / "complete", "Completion", new InputProject(_), collectionSub, controlSub, ioSub)
		// logging
	val logSub = project(utilPath / "log", "Logging", new LogProject(_), interfaceSub, processSub)
		// class file reader and analyzer
	val classfileSub = testedBase(utilPath / "classfile", "Classfile", ioSub, interfaceSub, logSub)
		// generates immutable or mutable Java data types according to a simple input format
	val datatypeSub = baseProject(utilPath /"datatype", "Datatype Generator", ioSub)
		// persisted, hierarchical properties
	val envSub= baseProject(utilPath / "env", "Properties", ioSub, logSub, classpathSub)

	/***** Intermediate-level Modules *****/

		// Apache Ivy integration
	val ivySub = project("ivy", "Ivy", new IvyProject(_), interfaceSub, launchInterfaceSub, logSub, ioSub)
		// Runner for uniform test interface
	val testingSub = project("testing", "Testing", new TestingProject(_), ioSub, classpathSub, logSub)
		// Basic task engine
	val taskSub = testedBase(tasksPath, "Tasks", controlSub, collectionSub)
		// Persisted caching based on SBinary
	val cacheSub = project(cachePath, "Cache", new CacheProject(_), ioSub, collectionSub)
		// Builds on cache to provide caching for filesystem-related operations
	val trackingSub = baseProject(cachePath / "tracking", "Tracking", cacheSub, ioSub)
		// Embedded Scala code runner
	val runSub = baseProject("run", "Run", ioSub, logSub, classpathSub, processSub)

	/***** compilation/discovery related modules *****/

		// Compiler-side interface to compiler that is compiled against the compiler being used either in advance or on the fly.
		//   Includes API and Analyzer phases that extract source API and relationships.
	val compileInterfaceSub = project(compilePath / "interface", "Compiler Interface", new CompilerInterfaceProject(_), interfaceSub)
		// Implements the core functionality of detecting and propagating changes incrementally.
		//   Defines the data structures for representing file fingerprints and relationships and the overall source analysis
	val compileIncrementalSub = testedBase(compilePath / "inc", "Incremental Compiler", collectionSub, apiSub, ioSub)
		// Persists the incremental data structures using SBinary
	val compilePersistSub = project(compilePath / "persist", "Persist", new PersistProject(_), compileIncrementalSub, apiSub)
		// sbt-side interface to compiler.  Calls compiler-side interface reflectively
	val compilerSub = project(compilePath, "Compile", new CompileProject(_),
		launchInterfaceSub, interfaceSub, ivySub, ioSub, classpathSub, compileInterfaceSub, logSub)
		// Searches the source API data structures, currently looks for subclasses and annotations
	val discoverySub = project(compilePath / "discover", "Discovery", new DiscoveryProject(_), compileIncrementalSub, apiSub)

	val scriptedBaseSub = project("scripted" / "base", "Scripted Framework", new TestProject(_), ioSub, processSub)
	val scriptedSbtSub = baseProject("scripted" / "sbt", "Scripted sbt", ioSub, logSub, processSub, scriptedBaseSub, launchInterfaceSub /*should really be a 'provided' dependency*/)	

		// Standard task system.  This provides map, flatMap, join, and more on top of the basic task model.
	val stdTaskSub = testedBase(tasksPath / "standard", "Task System", taskSub, collectionSub, logSub, ioSub, processSub)
		// Implementation and support code for defining actions.
	val actionsSub = baseProject(mainPath / "actions", "Actions",
		classfileSub, classpathSub, compileIncrementalSub, compilePersistSub, compilerSub, completeSub, discoverySub,
		interfaceSub, ioSub, ivySub, logSub, processSub, runSub, stdTaskSub, taskSub, trackingSub, testingSub)

		// The main integration project for sbt.  It brings all of the subsystems together, configures them, and provides for overriding conventions.
	val mainSub = project(mainPath, "Main", new Main(_), actionsSub, interfaceSub, ioSub, ivySub, launchInterfaceSub, logSub, processSub, runSub)
		// Strictly for bringing implicits and aliases from subsystems into the top-level sbt namespace through a single package object
		//  technically, we need a dependency on all of mainSub's dependencies, but we don't do that since this is strictly an integration project
		//  with the sole purpose of providing certain identifiers without qualification (with a package object)
	val sbtSub = project(sbtPath, "Simple Build Tool", new Sbt(_), mainSub)
	val scriptedPluginSub = project("scripted" / "plugin", "Scripted Plugin", new Scripted(_), sbtSub, classpathSub)

	/** following modules are not updated for 2.8 or 0.9 */
	/*
	val installerSub = project(sbtPath / "install", "Installer", new InstallerProject(_) {}, sbtSub)
	lazy val dist = task { None } dependsOn(launchSub.proguard, sbtSub.publishLocal, installerSub.publishLocal)*/

	def baseProject(path: Path, name: String, deps: Project*) = project(path, name, new Base(_), deps : _*)
	def testedBase(path: Path, name: String, deps: Project*) = project(path, name, new TestedBase(_), deps : _*)
	
		/* Multi-subproject paths */
	def sbtPath = path("sbt")
	def cachePath = path("cache")
	def tasksPath = path("tasks")
	def launchPath = path("launch")
	def utilPath = path("util")
	def compilePath = path("compile")
	def mainPath = path("main")

	def compilerInterfaceClasspath = compileInterfaceSub.projectClasspath(Configurations.Test)

	//run in parallel
	override def parallelExecution = true

	def jlineDep = "jline" % "jline" % "0.9.94" intransitive()

	override def managedStyle = ManagedStyle.Ivy
	val publishTo = Resolver.url("typesafe-ivy-releases", new java.net.URL("http://typesafe.artifactoryonline.com/typesafe/ivy-releases/"))
	val additional = publishTo
	Credentials(Path.userHome / ".ivy2" / ".typesafe-credentials", log)

		/* Subproject configurations*/
	class LaunchProject(info: ProjectInfo) extends Base(info) with TestWithIO with TestDependencies with ProguardLaunch with NoCrossPaths
	{
		val jline = jlineDep
		val ivy = "org.apache.ivy" % "ivy" % "2.2.0"
		override def deliverProjectDependencies = Nil

		// defines the package that proguard operates on
		def rawJarPath = jarPath
		def rawPackage = `package`
		override def packagePaths = super.packagePaths +++ launchInterfaceSub.packagePaths

		// configure testing
		override def testClasspath = super.testClasspath +++ interfaceSub.compileClasspath +++ interfaceSub.mainResourcesPath
		override def testCompileAction = super.testCompileAction dependsOn(interfaceSub.publishLocal, testSamples.publishLocal)

		// used to test the retrieving and loading of an application: sample app is packaged and published to the local repository
		lazy val testSamples = project("test-sample", "Launch Test", new TestSamples(_), interfaceSub, launchInterfaceSub)
		class TestSamples(info: ProjectInfo) extends Base(info) with NoCrossPaths with NoRemotePublish {
			override def deliverProjectDependencies = Nil
		}
	}
	class InputProject(info: ProjectInfo) extends TestedBase(info)
	{
		val jline = jlineDep
	}
	trait TestDependencies extends Project
	{
		val sc = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test"
		val sp = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.7.2" % "test"
	}
	class LogProject(info: ProjectInfo) extends Base(info) with TestDependencies
	{
		val opt = Configurations.Optional
		val jline = jlineDep % "optional"
	}
	class CacheProject(info: ProjectInfo) extends Base(info) with SBinaryDep
	class PersistProject(info: ProjectInfo) extends Base(info) with SBinaryDep
	{
//		override def compileOptions = super.compileOptions ++ compileOptions("-Xlog-implicits")
	}
	trait SBinaryDep extends BasicManagedProject
	{
		// these compilation options are useful for debugging caches and task composition
		//override def compileOptions = super.compileOptions ++ List(Unchecked,ExplainTypes, CompileOption("-Xlog-implicits"))
		val sbinary = "org.scala-tools.sbinary" %% "sbinary" % "0.4.0"
	}
	class Base(info: ProjectInfo) extends DefaultProject(info) with ManagedBase with Component with Licensed
	{
		override def scratch = true
		override def consoleClasspath = testClasspath
		override def compileOptions = super.compileOptions ++ compileOptions("-Xelide-below", "0")
	}
	class TestedBase(info: ProjectInfo) extends Base(info) with TestDependencies
	trait Licensed extends BasicScalaProject
	{
		def notice = path("NOTICE")
		abstract override def mainResources = super.mainResources +++ notice +++ Path.lazyPathFinder( extractLicenses )
		lazy val seeRegex = """\(see (.*?)\)""".r
		def licensePath(str: String): Path = { val path = Path.fromString(XSbt.this.info.projectPath, str); if(path.exists) path else error("Referenced license '" + str + "' not found at " + path) }
		def seePaths(noticeString: String): List[Path] = seeRegex.findAllIn(noticeString).matchData.map(d => licensePath(d.group(1))).toList
		def extractLicenses = if(!notice.exists) Nil else FileUtilities.readString(notice asFile, log).fold(_ => { log.warn("Could not read NOTICE"); Nil} , seePaths _)
	}
	class TestingProject(info: ProjectInfo) extends Base(info)
	{
		val testInterface = "org.scala-tools.testing" % "test-interface" % "0.5"
	}
	class DiscoveryProject(info: ProjectInfo) extends TestedBase(info) with TestWithCompile
	class CompileProject(info: ProjectInfo) extends Base(info) with TestWithLog with TestWithLaunch with TestWithAPI
	{
		override def testCompileAction = super.testCompileAction dependsOn(compileInterfaceSub.`package`, interfaceSub.`package`)
		override def testClasspath = super.testClasspath +++ compileInterfaceSub.packageSrcJar --- compilerInterfaceClasspath --- interfaceSub.mainCompilePath +++ interfaceSub.jarPath +++ buildCompilerJar
	}
	class IvyProject(info: ProjectInfo) extends Base(info) with TestWithIO with TestWithLog with TestWithLaunch
	{
		val ivy = "org.apache.ivy" % "ivy" % "2.2.0"
		val jsch = "com.jcraft" % "jsch" % "0.1.31" intransitive()
	}
	abstract class BaseInterfaceProject(info: ProjectInfo) extends DefaultProject(info) with ManagedBase with TestWithLog with Component with JavaProject
	class InterfaceProject(info: ProjectInfo) extends BaseInterfaceProject(info)
	{
		override def componentID: Option[String] = Some("xsbti")
		override def packageAction = super.packageAction dependsOn generateVersions
		def versionPropertiesPath = mainResourcesPath / "xsbt.version.properties"
		lazy val generateVersions = task {
			import java.util.{Date, TimeZone}
			val formatter = new java.text.SimpleDateFormat("yyyyMMdd'T'HHmmss")
			formatter.setTimeZone(TimeZone.getTimeZone("GMT"))
			val timestamp = formatter.format(new Date)
			val content = "version=" + version + "\ntimestamp=" + timestamp
			log.info("Writing version information to " + versionPropertiesPath + " :\n" + content)
			FileUtilities.write(versionPropertiesPath.asFile, content, log)
		}

		override def watchPaths = super.watchPaths +++ apiDefinitionPaths --- sources(generatedBasePath)
		override def mainSourceRoots = super.mainSourceRoots +++ (generatedBasePath ##)
		def srcManagedPath = path("src_managed")
		def generatedBasePath = srcManagedPath / "main" / "java"
		/** Files that define the datatypes.*/
		def apiDefinitionPaths: PathFinder = "definition"
		/** Delete up the generated sources*/
		lazy val cleanManagedSrc = cleanTask(srcManagedPath)
		override def cleanAction = super.cleanAction dependsOn(cleanManagedSrc)
		/** Runs the generator compiled by 'compile', putting the classes in src_managed and processing the definitions 'apiDefinitions'. */
/*		lazy val generateSource = generateSourceAction dependsOn(cleanManagedSrc, datatypeSub.compile)
		def generateSourceTask(immutable: Boolean, pkg: String, apiDefinitions: PathFinder): Task =
		{
			val m = if(immutable) "immutable" else "mutable"
			generateSourceTask(m :: pkg :: generatedBasePath.absolutePath :: apiDefinitions.get.toList.map(_.absolutePath))
		}
		def generateSourceTask(args: List[String]): Task =
			runTask(datatypeSub.getMainClass(true), datatypeSub.runClasspath, args)
		def generateSourceAction =
			//generateSourceTask(false, "xsbti.api", "definition" +++ "type") &&
			generateSourceTask(true, "xsbti.api", "other" +++ "definition" +++ "type")
		/** compiles the generated sources */
		override def compileAction = super.compileAction dependsOn(generateSource)*/
	}
	class LaunchInterfaceProject(info: ProjectInfo) extends BaseInterfaceProject(info)
	{
		override def componentID = None
	}
	class Scripted(info: ProjectInfo) extends Base(info)
	{
		override def managedStyle = ManagedStyle.Ivy
		override def scratch = true
	}
	class TestProject(info: ProjectInfo) extends Base(info)
	{
		val process = "org.scala-tools.sbt" % "process" % "0.1"
	}
	class CompilerInterfaceProject(info: ProjectInfo) extends Base(info) with PrecompiledInterface with NoCrossPaths with TestWithIO with TestWithLog
	{ cip => 
		//val jline = jlineDep artifacts(Artifact("jline", Map("e:component" -> srcID)))
		// necessary because jline is not distributed with 2.8 and we will get a compile error 
		// sbt should work with the above inline declaration, but it doesn't, so the inline Ivy version is used for now.
		override def ivyXML =
			( <publications />
			<dependencies>
				<dependency org="jline" name="jline" rev="0.9.94" transitive="false">
					<artifact name="jline" type="jar" e:component={srcID}/>
				</dependency>
			</dependencies> )

		def srcID = "compiler-interface-src"
		lazy val srcArtifact = Artifact(srcID) extra("e:component" -> srcID)
		override def packageSrcJar = mkJarPath(srcID)
		lazy val pkgSrc = packageSrc // call it something else because we don't need dependencies to run package-src
		override def packageAction = super.packageAction dependsOn(pkgSrc)
		
		// sub projects for each version of Scala to precompile against other than the one sbt is built against
		// each sub project here will add ~100k to the download
		lazy val precompiled29 = precompiledSub("2.9.0")
		lazy val precompiled28 = precompiledSub("2.8.0")
		//lazy val precompiled27 = precompiledSub("2.7.7")

		def precompiledSub(v: String) = 
			project(info.projectPath, "Precompiled " + v, new Precompiled(v)(_), cip.info.dependencies.toSeq : _* /*doesn't include subprojects of cip*/ )

		/** A project that compiles the compiler interface against the Scala version 'sv'.
		* This is done for selected Scala versions (generally, popular ones) so that it doesn't need to be done at runtime. */
		class Precompiled(sv: String)(info: ProjectInfo) extends Base(info) with PrecompiledInterface with NoUpdate {
			/** force the Scala version in order to precompile the compiler interface for different Scala versions*/
			override def buildScalaVersion = sv

			/** Get compilation classpath from parent.  Scala dependencies are added on top of this and this
			* subproject does not depend on any Scala subprojects, so mixing versions is not a problem. */
			override def compileClasspath = cip.compileClasspath --- cip.mainUnmanagedClasspath +++ mainUnmanagedClasspath

			override def compileOptions = Nil
			// these ensure that the classes compiled against other versions of Scala are not exported (for compilation/testing/...)
			override def projectClasspath(config: Configuration) = Path.emptyPathFinder
		}
	}
	trait TestWithAPI extends TestWith {
		override def testWithTestClasspath = super.testWithTestClasspath ++ Seq(apiSub)
	}
	trait TestWithCompile extends TestWith {
		override def testWithTestClasspath = super.testWithTestClasspath ++ Seq(compilerSub)
	}
	trait TestWithIO extends TestWith {
		override def testWithTestClasspath = super.testWithTestClasspath ++ Seq(ioSub)
	}
	trait TestWithLaunch extends TestWith {
		override def testWithTestClasspath = super.testWithTestClasspath ++ Seq(launchSub)
	}
	trait TestWithLog extends TestWith {
		override def testWithCompileClasspath = super.testWithCompileClasspath ++ Seq(logSub)
	}
	trait TestWith extends BasicScalaProject
	{
		def testWithCompileClasspath: Seq[BasicScalaProject] = Nil
		def testWithTestClasspath: Seq[BasicScalaProject] = Nil
		override def testCompileAction = super.testCompileAction dependsOn((testWithTestClasspath.map(_.testCompile) ++ testWithCompileClasspath.map(_.compile)) : _*)
		override def testClasspath = (super.testClasspath /: (testWithTestClasspath.map(_.testClasspath) ++  testWithCompileClasspath.map(_.compileClasspath) ))(_ +++ _)
	}
	class Sbt(info: ProjectInfo) extends Base(info) with TestWith
	{
		override def normalizedName = "sbt"
		override def testWithCompileClasspath = super.testWithCompileClasspath ++ Seq(scriptedSbtSub)
		override def testAction = super.testAction dependsOn(publishLocal)
		def scriptedScalaVersions = "2.8.1"
		lazy val scripted = task { args => task {
			val launcher  = launchSub.outputJar.asFile
			val loader = ClasspathUtilities.toLoader(scriptedSbtSub.testClasspath, scriptedSbtSub.buildScalaInstance.loader)
			val m = ModuleUtilities.getObject("sbt.test.ScriptedTests", loader)
			val r = m.getClass.getMethod("run", classOf[File], classOf[Boolean], classOf[String], classOf[String], classOf[String], classOf[Array[String]], classOf[File])
			try { r.invoke(m, sourcePath / "sbt-test" asFile, true: java.lang.Boolean, version.toString, buildScalaVersion, scriptedScalaVersions, args, launcher) }
			catch { case e: java.lang.reflect.InvocationTargetException => throw e.getCause }
			None
		} dependsOn(publishLocal, scriptedSbtSub.compile, testCompile) }
	}
	class Main(info: ProjectInfo) extends Base(info) with Sxr
	{
		def concatPaths[T](s: Seq[T])(f: PartialFunction[T, PathFinder]): PathFinder =
		{
			def finder: T => PathFinder = (f orElse { case _ => Path.emptyPathFinder })
			(Path.emptyPathFinder /: s) { _ +++ finder(_) }
		}
		def deepBaseDirectories = Path.finder { topologicalSort.flatMap { case p: ScalaPaths => p.mainSourceRoots.getFiles } }
		def deepSources = concatPaths(mainSub.topologicalSort){ case p: ScalaPaths => p.mainSources }
//		override def documentOptions = CompoundDocOption("-sourcepath", deepBaseDirectories.absString) :: LinkSource :: super.documentOptions.toList
		lazy val sbtGenDoc = scaladocTask("sbt", deepSources, docPath, docClasspath, documentOptions) dependsOn(compile)
	}
}
