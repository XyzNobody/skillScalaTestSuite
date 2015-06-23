package common.storage

import java.nio.file.Path
import java.lang.management.ManagementFactory

sealed trait Action[+StateType] {
  // execute this action with the given parameters
  def apply(t: TaskBase, n: Int, f: Path): Unit
  // sequencing of actions
  def +>(b : UntypedAction) : Action[StateType]
  def +>[StateType2 >: StateType](b : TypedAction[StateType2]) : Action[StateType2]
  
  // name of this action for serialization
  def name : String
  // a sequence of results used by this action
  def results : Iterator[Option[Result]]
}

object Action {
  def fold[StateType](seq: Iterable[Action[StateType]]) = {
    var res: Action[StateType] = DummyAction
    for (a ← seq) a match {
      case a : UntypedAction ⇒ res +>= a
      case a ⇒ res +>= a.asInstanceOf[TypedAction[StateType]]
    }
    res
  }
}

trait TypedAction[StateType] extends Action[StateType] {
  self ⇒
  
  @inline final override def apply(t: TaskBase, n: Int, f: Path): Unit = apply(t.asInstanceOf[Task[StateType]], n, f)
  def apply(t: Task[StateType], n: Int, f: Path): Unit
  
  @inline final override def +>[StateType2 >: StateType](b: TypedAction[StateType2]): TypedAction[StateType2] = {
    new TypedAction[StateType2] {
      override def apply(t: Task[StateType2], n: Int, f: Path) {
        self(t, n, f); b(t, n, f)
      }
      
      override def name = self.name + " " + b.name
      override def results = self.results ++ b.results
      
      override def toString = self + " +> " + b
    }
  }
  
  @inline final override def +>(b: UntypedAction): TypedAction[StateType] =
    if (b == DummyAction)
      this
    else
      new TypedAction[StateType] {
        override def apply(t: Task[StateType], n: Int, f: Path) {
          self(t, n, f); b(t, n, f)
        }
        
        override def name = self.name + " " + b.name
        override def results = self.results ++ b.results
        
        override def toString = self + " +> " + b
      }
}

trait UntypedAction extends Action[Nothing] {
  self ⇒
  
  @inline override def +>[StateType](b: TypedAction[StateType]): TypedAction[StateType] = {
    new TypedAction[StateType] {
      override def apply(t: Task[StateType], n: Int, f: Path) {
        self(t, n, f); b(t, n, f)
      }
      
      override def name = self.name + " " + b.name
      override def results = self.results ++ b.results
      
      override def toString = self + " +> " + b
    }
  }
  
  @inline override def +>(b: UntypedAction): UntypedAction =
    if (b == DummyAction)
      this
    else
      new UntypedAction {
        override def apply(t: TaskBase, n: Int, f: Path) {
          self(t, n, f); b(t, n, f)
        }
        
        override def name = self.name + " " + b.name
        override def results = self.results ++ b.results
        
        override def toString = self + " +> " + b
      }
}

// standard actions, independent of any type
object DummyAction extends UntypedAction {
  override def apply(t: TaskBase, n: Int, f: Path) {}
  
  override def +>[StateType](b: TypedAction[StateType]): TypedAction[StateType] = b
  override def +>(b: UntypedAction): UntypedAction = b
  
  override def name = ""
  override def results = Iterator.empty
  
  override def toString = "do nothing"
}

object Pause extends UntypedAction {
  override def apply(t: TaskBase, n: Int, f: Path) {
    Console.err.println("waiting...")
    Console.in.readLine
  }
  
  override def name = "pause"
  override def results = Iterator.empty
  
  override def toString = "wait for input"
}

object GC extends UntypedAction {
  override def apply(t: TaskBase, n: Int, f: Path) {
    System.gc
    System.runFinalization
  }
  
  override def name = "gc"
  override def results = Iterator.empty
  
  override def toString = "run GC"
}

object Delete extends UntypedAction {
  override def apply(t: TaskBase, n: Int, f: Path) {
    t.reset
    System.gc
    System.runFinalization
  }
  
  override def name = "delete"
  override def results = Iterator.empty
  
  override def toString = "delete state"
}

// printing of results
object ConsolePrint {
  @inline def apply() = new TaskBase.ConsolePrint
}
object ResultPrint {
  @inline def apply() = new TaskBase.ResultPrint
}
object StartStopPrint {
  @inline def apply() = TaskBase.StartStopPrint
}

object TaskBase {
  sealed trait Printer {
    protected[TaskBase] def print(): Unit
    protected[TaskBase] def setCount(n: Int): Unit
    protected[TaskBase] def setResults(cap: Option[Result], used: Option[Result]): Unit
  }
  
  class ConsolePrint extends Printer {
    private val reporter = {
      val selfName = ManagementFactory.getRuntimeMXBean().getName()
      new ValueReporter(selfName.substring(0, selfName.indexOf('@')), 0)
    }
    val cap = Some(CollapsedResult("capacity", Math.max))
    val used = Some(CollapsedResult("used", Math.max))
    
    protected[TaskBase] override def print() {
      println(s"${reporter.n}\t${cap.get.storage(reporter.n)}\t${used.get.storage(reporter.n)}")
      reporter.switchResults(None, None)
      cap.get.reset()
      used.get.reset()
    }
    protected[TaskBase] override def setCount(n: Int) {
      reporter.switchResults(None, None)
      reporter.n = n
    }
    protected[TaskBase] override def setResults(_cap: Option[Result], _used: Option[Result]) {
      // ignore parameters, use internal results instead
      reporter.switchResults(cap, used)
    }
  }
  class ResultPrint extends Printer {
    private val reporter = {
      val selfName = ManagementFactory.getRuntimeMXBean().getName()
      new ValueReporter(selfName.substring(0, selfName.indexOf('@')), 0)
    }
    protected[TaskBase] override def print() {
      // only stop recording; everything else is already done by reporter
      reporter.switchResults(None, None)
    }
    protected[TaskBase] override def setCount(n: Int) {
      reporter.switchResults(None, None)
      reporter.n = n
    }
    protected[TaskBase] override def setResults(cap: Option[Result], used: Option[Result]) {
      reporter.switchResults(cap, used)
    }
  }
  // printer for external tests; memory has to be recorded externally
  object StartStopPrint extends Printer {
    protected[TaskBase] override def print() {
      // signal end of measuring; memory is recorded elsewhere
      println("finished")
      Console.in.readLine // wait for signal to continue
    }
    protected[TaskBase] override def setCount(n: Int) {
      // do nothing, everything is done in another process
    }
    protected[TaskBase] override def setResults(cap: Option[Result], used: Option[Result]) {
      // signal start of measuring; memory is recorded elsewhere
      println("start")
      Console.in.readLine // wait for signal to continue
    }
  }
}

sealed abstract class TaskBase(val printer: TaskBase.Printer) {
  def reset: Unit
  @inline final def startMeasuring(cap: Option[Result], used: Option[Result]) {
    printer.setResults(cap, used)
  }
  @inline final def stopMeasuring() = {
    printer.print()
  }
  @inline protected final def setCount(n: Int) = printer.setCount(n)
}

// task built from actions
final class Task[StateType](printMemory: TaskBase.Printer, private val action: Action[StateType]) extends TaskBase(printMemory) {
  var σ: Option[StateType] = None
  
  override def reset { σ = None }
  
  def apply(n: Int, f: Path) = { setCount(n); action(this, n, f) }
  
  override def toString = action.toString
}
