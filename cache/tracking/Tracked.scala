/* sbt -- Simple Build Tool
 * Copyright 2009, 2010 Mark Harrah
 */
package xsbt

import java.io.{File,IOException}
import CacheIO.{fromFile, toFile}
import sbinary.Format
import scala.reflect.Manifest
import Task.{iterableToBuilder, iterableToForkBuilder}

trait Tracked extends NotNull
{
	/** Cleans outputs.  This operation might require information from the cache, so it should be called first if clear is also called.*/
	def clean: Task[Unit]
	/** Clears the cache. If also cleaning, 'clean' should be called first as it might require information from the cache.*/
	def clear: Task[Unit]
}
class Timestamp(val cacheFile: File) extends Tracked
{
	val clean = Clean(cacheFile)
	def clear = Task.empty
	def apply[T](f: Long => Task[T]): Task[T] =
	{
		val getTimestamp = Task { readTimestamp }
		getTimestamp bind f map { result =>
			FileUtilities.write(cacheFile, System.currentTimeMillis.toString)
			result
		}
	}
	def readTimestamp: Long =
		try { FileUtilities.read(cacheFile).toLong }
		catch { case _: NumberFormatException | _: java.io.FileNotFoundException => 0 }
}
object Clean
{
	def apply(src: Task[Set[File]]): Task[Unit] = src map FileUtilities.delete
	def apply(srcs: File*): Task[Unit] = Task(FileUtilities.delete(srcs))
	def apply(srcs: Set[File]): Task[Unit] = Task(FileUtilities.delete(srcs))
}

class Changed[O](val task: Task[O], val cacheFile: File)(implicit input: InputCache[O]) extends Tracked
{
	val clean = Clean(cacheFile)
	def clear = Task.empty
	def apply[O2](ifChanged: O => O2, ifUnchanged: O => O2): Task[O2] =
		task map { value =>
			val cache =
				try { OpenResource.fileInputStream(cacheFile)(input.uptodate(value)) }
				catch { case _: IOException => new ForceResult(input)(value) }
			if(cache.uptodate)
				ifUnchanged(value)
			else
			{
				OpenResource.fileOutputStream(false)(cacheFile)(cache.update)
				ifChanged(value)
			}
		}
}
object Difference
{
	sealed class Constructor private[Difference](defineClean: Boolean, filesAreOutputs: Boolean) extends NotNull
	{
		def apply(filesTask: Task[Set[File]], style: FilesInfo.Style, cache: File): Difference = new Difference(filesTask, style, cache, defineClean, filesAreOutputs)
		def apply(files: Set[File], style: FilesInfo.Style, cache: File): Difference = apply(Task(files), style, cache)
	}
	object outputs extends Constructor(true, true)
	object inputs extends Constructor(false, false)
}
class Difference(val filesTask: Task[Set[File]], val style: FilesInfo.Style, val cache: File, val defineClean: Boolean, val filesAreOutputs: Boolean) extends Tracked
{
	val clean =  if(defineClean) Clean(Task(raw(cachedFilesInfo))) else Task.empty
	val clear = Clean(cache)
	
	private def cachedFilesInfo = fromFile(style.formats, style.empty)(cache)(style.manifest).files
	private def raw(fs: Set[style.F]): Set[File] = fs.map(_.file)
	
	def apply[T](f: ChangeReport[File] => Task[T]): Task[T] =
		filesTask bind { files =>
			val lastFilesInfo = cachedFilesInfo
			val lastFiles = raw(lastFilesInfo)
			val currentFiles = files.map(_.getAbsoluteFile)
			val currentFilesInfo = style(currentFiles)

			val report = new ChangeReport[File]
			{
				lazy val checked = currentFiles
				lazy val removed = lastFiles -- checked // all files that were included previously but not this time.  This is independent of whether the files exist.
				lazy val added = checked -- lastFiles // all files included now but not previously.  This is independent of whether the files exist.
				lazy val modified = raw(lastFilesInfo -- currentFilesInfo.files) ++ added
				lazy val unmodified = checked -- modified
			}

			f(report) map { result =>
				val info = if(filesAreOutputs) style(currentFiles) else currentFilesInfo
				toFile(style.formats)(info)(cache)(style.manifest)
				result
			}
		}
}
class DependencyTracked[T](val cacheDirectory: File, val translateProducts: Boolean, cleanT: T => Unit)(implicit format: Format[T], mf: Manifest[T]) extends Tracked
{
	private val trackFormat = new TrackingFormat[T](cacheDirectory, translateProducts)
	private def cleanAll(fs: Set[T]) = fs.foreach(cleanT)
	
	val clean = Task(cleanAll(trackFormat.read.allProducts))
	val clear = Clean(cacheDirectory)
	
	def apply[R](f: UpdateTracking[T] => Task[R]): Task[R] =
	{
		val tracker = trackFormat.read
		f(tracker) map { result =>
			trackFormat.write(tracker)
			result
		}
	}
}
object InvalidateFiles
{
	def apply(cacheDirectory: File): InvalidateTransitive[File] = apply(cacheDirectory, true)
	def apply(cacheDirectory: File, translateProducts: Boolean): InvalidateTransitive[File] =
	{
		import sbinary.DefaultProtocol.FileFormat
		new InvalidateTransitive[File](cacheDirectory, translateProducts, FileUtilities.delete)
	}
}

object InvalidateTransitive
{
	import scala.collection.Set
	def apply[T](tracker: UpdateTracking[T], files: Set[T]): InvalidationReport[T] =
	{
		val readTracker = tracker.read
		val invalidated = Set() ++ invalidate(readTracker, files)
		val invalidatedProducts = Set() ++ invalidated.filter(readTracker.isProduct)

		new InvalidationReport[T]
		{
			val invalid = invalidated
			val invalidProducts = invalidatedProducts
			val valid = Set() ++ files -- invalid
		}
	}
	def andClean[T](tracker: UpdateTracking[T], cleanImpl: Set[T] => Unit, files: Set[T]): InvalidationReport[T] =
	{
		val report = apply(tracker, files)
		clean(tracker, cleanImpl, report)
		report
	}
	def clear[T](tracker: UpdateTracking[T], report: InvalidationReport[T]): Unit =
		tracker.removeAll(report.invalid)
	def clean[T](tracker: UpdateTracking[T], cleanImpl: Set[T] => Unit, report: InvalidationReport[T])
	{
		clear(tracker, report)
		cleanImpl(report.invalidProducts)
	}
	
	private def invalidate[T](tracker: ReadTracking[T], files: Iterable[T]): Set[T] =
	{
		import scala.collection.mutable.HashSet
		val invalidated = new HashSet[T]
		def invalidate0(files: Iterable[T]): Unit =
			for(file <- files if !invalidated(file))
			{
				invalidated += file
				invalidate0(invalidatedBy(tracker, file))
			}
		invalidate0(files)
		invalidated
	}
	private def invalidatedBy[T](tracker: ReadTracking[T], file: T) =
		tracker.products(file) ++ tracker.sources(file) ++ tracker.usedBy(file) ++ tracker.dependsOn(file)
		
}
class InvalidateTransitive[T](cacheDirectory: File, translateProducts: Boolean, cleanT: T => Unit)
	(implicit format: Format[T], mf: Manifest[T]) extends Tracked
{
	def this(cacheDirectory: File, translateProducts: Boolean)(implicit format: Format[T], mf: Manifest[T]) =
		this(cacheDirectory, translateProducts, (_: T) => ())
		
	private val tracked = new DependencyTracked(cacheDirectory, translateProducts, cleanT)
	def clean = tracked.clean
	def clear = tracked.clear

	def apply[R](changes: ChangeReport[T])(f: (InvalidationReport[T], UpdateTracking[T]) => Task[R]): Task[R] =
		apply(Task(changes))(f)
	def apply[R](changesTask: Task[ChangeReport[T]])(f: (InvalidationReport[T], UpdateTracking[T]) => Task[R]): Task[R] =
	{
		changesTask bind { changes =>
			tracked { tracker =>
				val report = InvalidateTransitive.andClean[T](tracker, _.foreach(cleanT), changes.modified)
				f(report, tracker)
			}
		}
	}
}
class BasicTracked(filesTask: Task[Set[File]], style: FilesInfo.Style, cacheDirectory: File) extends Tracked
{
	private val changed = Difference.inputs(filesTask, style, new File(cacheDirectory, "files"))
	private val invalidation = InvalidateFiles(new File(cacheDirectory, "invalidation"))
	private def onTracked(f: Tracked => Task[Unit]) = Seq(invalidation, changed).forkTasks(f).joinIgnore
	val clear = onTracked(_.clear)
	val clean = onTracked(_.clean)
	
	def apply[R](f: (ChangeReport[File], InvalidationReport[File], UpdateTracking[File]) => Task[R]): Task[R] =
		changed { sourceChanges =>
			invalidation(sourceChanges) { (report, tracking) =>
				f(sourceChanges, report, tracking)
			}
		}
}