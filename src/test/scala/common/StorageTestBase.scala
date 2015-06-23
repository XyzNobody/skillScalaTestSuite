package common

import common.randomHelpers.Distribution
import common.storage._
import java.nio.file.Path
import java.io.File
import scala.util.Random
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.sys.process._
import java.lang.management.ManagementFactory

/**
 * This class contains the common implementation for simple tests to find
 * storage overuse by the generated code. Simple tests create n objects,
 * write them to a SkillState, reread the SkillState, append n more objects
 * and write again.
 */
abstract class StorageTestBase[StateType](val name : String) extends CommonTest {
  // implicitly typed variants of generic storage framework types
  type Action = storage.Action[StateType]
  type TypedAction = storage.TypedAction[StateType]
  type Task = storage.Task[StateType]
  
  protected def classPath =
    s"""-cp "${System.getProperty("java.class.path")}" """

  // runner: protect against OutOfMemoryError and/or jvm effects
  @inline def runThread(test: ⇒ Unit) {
    // @note: protect against OutOfMemoryError
    val t = new Thread(new Runnable {
      def run {
        test
      }
    })
    t.start
    t.join
  }
  @inline def runProcess(param: String, file: Path, count: Int, action: Action) = {
    val execute = s"""javaw $param $classPath $mainObjectName count=$count "file=$file" ${action.name}"""
    class OutputThread(work : (⇒ java.io.InputStream) ⇒ Unit) extends Thread {
      private var stream : java.io.InputStream = null
      override def run = work(stream)
      def start(str : java.io.InputStream) {
        stream = str
        start()
      }
    }
    // @warning: ValueReporter is not portable (works not on all JVMs)!
    var reporter: ValueReporter = null
    var otherInput : java.io.PrintStream = null
    val results = action.results
    @inline def measuring(start: Boolean) {
      reporter.switchResults(None, if (start && results.hasNext) results.next() else None)
      if (otherInput != null) {
        otherInput.println("next")
        otherInput.flush
      }
    }
    var stop = false
    val outThread = new OutputThread(out ⇒ {
          val reader = new java.io.BufferedReader(new java.io.InputStreamReader(out))
          while (!stop) {
            if (reader.ready) {
              val str = reader.readLine
              println(str)
              if (reporter == null)
                reporter = new ValueReporter(str, count, None, None)
              else if (str == "start")
                measuring(true)
              else if (str == "finished")
                measuring(false)
            }
          }
          reader.close
        })
    val errThread = new OutputThread(err ⇒ {
          val reader = new java.io.BufferedReader(new java.io.InputStreamReader(err))
          while (!stop) {
            if (reader.ready)
              Console.err.println(reader.readLine)
          }
          err.close
        })
    val io = new ProcessIO(in ⇒ if (otherInput == null) otherInput = new java.io.PrintStream(in),
        out ⇒ outThread.start(out),
        err ⇒ errThread.start(err))

    val proc = execute.run(io);
    proc.exitValue
    if (otherInput != null) otherInput.close
    stop = true
    outThread.join
    errThread.join
    proc.destroy
  }
  
  // used to require an object that can be started as an external test process
  def getMainObject : StorageTestBase.ExternalTest[StateType]
  @inline final def mainObjectName = {
    @inline def removeTrailingDollar(s : String) = if (s.isEmpty() || s.last != '$') s else s.substring(0, s.length - 1) 
    removeTrailingDollar(getMainObject.getClass.getName)
  }
  
  // called to create a SkillState
  def create: StateType
  // called to create Elements
  def createElements(σ: StateType, n: Int): Unit
  // called to write a SkillState
  def write(σ: StateType, f: Path): Unit
  // called to read a SkillState
  def read(f: Path): StateType
  // called to create additional Elements for a read SkillState
  def createMoreElements(state: StateType, n: Int): Unit
  // called to append to a SkillState
  def append(state: StateType): Unit
  
  // create task
  def createTask(printer: TaskBase.Printer, action: Action) = new Task(printer, action)
  
  // basic actions
  class Create(val createRes: Option[Result]) extends TypedAction {
    override def apply(t: Task, n: Int, f: Path) {
      t.startMeasuring(None, createRes)
      t.σ = Option(create)
      t.σ.map(createElements(_, n))
      t.stopMeasuring()
    }
    
    override def name = "create"
    override def results = Iterator.single(createRes)
    
    override def toString = "create state and elements"
  }
  object Create {
    def apply(res: Option[Result]) = new Create(res)
  }
  class Write(val writeRes: Option[Result]) extends TypedAction {
    override def apply(t: Task, n: Int, f: Path) {
      t.startMeasuring(None, writeRes)
      t.σ.map(write(_, f))
      t.stopMeasuring()
    }
    
    override def name = "write"
    override def results = Iterator.single(writeRes)
    
    override def toString = "write state"
  }
  object Write {
    def apply(res: Option[Result]) = new Write(res)
  }
  class Read(val readRes: Option[Result]) extends TypedAction {
    override def apply(t: Task, n: Int, f: Path) {
      t.startMeasuring(None, readRes)
      t.σ = Option(read(f))
      t.stopMeasuring()
    }
    
    override def name = "read"
    override def results = Iterator.single(readRes)
    
    override def toString = "read state"
  }
  object Read {
    def apply(res: Option[Result]) = new Read(res)
  }
  class CreateMore(val createMoreRes: Option[Result]) extends TypedAction {
    override def apply(t: Task, n: Int, f: Path) {
      t.startMeasuring(None, createMoreRes)
      t.σ.map(createMoreElements(_, n))
      t.stopMeasuring()
    }
    
    override def name = "createmore"
    override def results = Iterator.single(createMoreRes)
    
    override def toString = "create more elements"
  }
  object CreateMore {
    def apply(res: Option[Result]) = new CreateMore(res)
  }
  class Append(val appendRes: Option[Result]) extends TypedAction {
    override def apply(t: Task, n: Int, f: Path) {
      t.startMeasuring(None, appendRes)
      t.σ.map(append(_))
      t.stopMeasuring()
    }
    
    override def name = "append"
    override def results = Iterator.single(appendRes)
    
    override def toString = "append to state"
  }
  object Append {
    def apply(res: Option[Result]) = new Append(res)
  }
  
  // standard combined actions
  def createAndWrite(create: Option[Result], write: Option[Result]) = Create(create) +> Write(write)
  def readAndAppend(read: Option[Result], create: Option[Result], append: Option[Result]) = Read(read) +> CreateMore(create) +> Append(append)

  // standard tests
  
  // repeats the test for each count in counts repetitions times using threads
  def repeatedTest(counts: Array[Int], repetitions: Int, task: Task) {

    for (count ← counts) {
      System.gc
      System.runFinalization
      for (rep ← 0 until repetitions) {
        val f = tmpFile(s"st_${name}_rep_benchmark")
        runThread(task(count, f))
      }
    }
  }
  // repeats the test for each count in counts repetitions times using processes
  def repeatedTest(counts: Array[Int], repetitions: Int, param: String, tasks: Action*) {

    for (count ← counts) {
      println(s"started with $count elements")
      for (rep ← 0 until repetitions) {
        val f = File.createTempFile(s"st_${name}_rep_benchmark", ".sf").toPath
        for (task ← tasks) {
          runProcess(param, f, count, task)
        }
        f.toFile.delete
      }
      println(s"finished $count elements")
    }
  }

  // chooses samples random values with the given distribution and executes the task for them using threads
  def randomizedTest(samples: Int, distribution: Distribution, task: Task)
  {
    for (i ← 0 until samples) {
      val n = distribution.next
      val f = tmpFile(s"st_${name}_rand_benchmark")
      runThread(task(n, f))
    }
  }
  // chooses samples random values with the given distribution and executes the tasks for them using processes
  def randomizedTest(samples: Int, distribution: Distribution, param: String, tasks: Action*)
  {
    for (i ← 0 until samples) {
      val n = distribution.next
      println(s"started with $n elements")
      val f = File.createTempFile(s"st_${name}_rand_benchmark", ".sf").toPath
      for (task ← tasks) {
        runProcess(param, f, n, task)
      }
      f.toFile.delete
      println(s"finished $n elements")
    }
  }
  
  // action from string
  def stringToAction(str: String) : Action = str.toLowerCase match {
    // basic actions
    case "" ⇒ DummyAction // action that is eliminated
    case "pause" ⇒ Pause
    case "gc" ⇒ GC
    case "create" ⇒ Create(None)
    case "write" ⇒ Write(None)
    case "delete" ⇒ Delete
    case "read" ⇒ Read(None)
    case "createmore" ⇒ CreateMore(None)
    case "append" ⇒ Append(None)
    // compound actions
    case "createandwrite" ⇒ createAndWrite(None, None)
    case "readandappend" ⇒ readAndAppend(None, None, None)
    case _ ⇒
      if (str.contains(' '))
        Action.fold(for (s ← str.split(' ')) yield stringToAction(s))
      else
        throw new Exception("invalid action")
  }

  // for execution of subprocesses in tests
  final def externalTestMain(args: Array[String]) {
    @inline def filterArgs(args: Array[String]) = {
      val actions = ArrayBuffer[String]()
      var count: Int = 0
      var file: Path = null
      for (str ← args) {
        if (str.startsWith("count="))
          count = str.substring(6).toInt
        else if (str.startsWith("file="))
          file = new java.io.File(str.substring(5)).toPath
        else
          actions += str
      }
      (actions, count, file)
    }
    
    // from VisualVM source code: get pid (and output to console)
    val selfName = ManagementFactory.getRuntimeMXBean().getName();
    println(selfName.substring(0, selfName.indexOf('@')));

    val t = filterArgs(args)
    val task = createTask(StartStopPrint(), Action.fold(for (str ← t._1)
      yield try
        stringToAction(str)
      catch {
        case e: Exception ⇒ { System.err.println(e.getMessage); DummyAction }
      }))
    task(t._2, t._3)
  }
}
object StorageTestBase {
  trait ExternalTest[StateType] {
    def createTest : StorageTestBase[StateType]
    
    final def main(args : Array[String]) {
      createTest.externalTestMain(args)
    }
  }
}
