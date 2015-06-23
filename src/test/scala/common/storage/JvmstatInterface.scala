package common.storage

import sun.jvmstat.monitor.event._
import sun.jvmstat.monitor._
import scala.collection.JavaConverters._
import scala.collection.mutable.Buffer

// @warning: this class is not portable!
// implementation follows VisualVM implementation
// (classes com.sun.tools.visualvm.jvmstat.JvmJvmstatModel_5, com.sun.tools.visualvm.jvmstat.JvmstatModelImpl)
class ValueReporter(pid : String, var n : Int) extends VmListener {
  def this(pid : String, n : Int, cap : Option[Result], used : Option[Result]) {
    this(pid, n)
    capacityResult = cap
    usedResult = used
  }
  
  private var capacityResult: Option[Result] = None
  private var usedResult: Option[Result] = None
  
  private val host = MonitoredHost.getMonitoredHost("localhost")
  private val vm = host.getMonitoredVm(new VmIdentifier("//" + pid), 10)
  vm.addVmListener(this)
  
  private val capacityMonitors = vm.findByPattern("sun.gc.generation.[0-9]+.capacity").asScala
  private val usedMonitors = vm.findByPattern("sun.gc.generation.[0-9]+.space.[0-9]+.used").asScala
  
  def switchResults(capacity: Option[Result], used: Option[Result]) {
    capacityResult = capacity
    usedResult = used
  }
  
  private def updateData {
    capacityResult.foreach { cr ⇒ cr += (n, sumGenerations(capacityMonitors)) }
    usedResult.foreach { ur ⇒ ur += (n, sumGenerations(usedMonitors)) }
  }
  
  private def sumGenerations(gen: Buffer[Monitor]) = {
    var res = 0.0
    for (m ← gen if m != null)
      res += m.getValue.asInstanceOf[java.lang.Long]
    res
  }
  
  // implementation of VmListener
  override def disconnected(event: VmEvent) {
    vm.removeVmListener(this)
    vm.detach
  }
  override def monitorStatusChanged(event: MonitorStatusChangeEvent) {}
  override def monitorsUpdated(event: VmEvent) {
    try {
      // check that the application is still alive
      if (host.activeVms().contains(pid.toInt)) {
        updateData
      } else { // application is not alive
        disconnected(null)
      }
    } catch {
      case ex: MonitorException ⇒ disconnected(null)
    }
  }
}
