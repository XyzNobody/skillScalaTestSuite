package common.storage

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.util.Sorting

sealed trait Result {
  val name: String
  
  def +=(n: Int, s: Double)
}

trait SingleValueResult extends Result {
  val storage : HashMap[Int, Double]
}
object SingleValueResult {
  def saveGraph(file: String, caption: String, axisStyle: String, coordinateStyle: String, data: Seq[SingleValueResult]) = {
    if (!data.isEmpty) {
      val keys = data.head.storage.keySet.toArray
      Sorting.quickSort(keys)
      Console.withOut(new java.io.PrintStream(new java.io.FileOutputStream(file)))(
        benchmarks.BenchmarkTools.printGraph(caption, axisStyle, coordinateStyle, keys, data.map { res ⇒ (res.name, res.storage) }))
    }
  }
}

trait MultiValueResult extends Result {
  val storage : HashMap[Int, ArrayBuffer[Double]]
}
object MultiValueResult {
  def saveGraph(file: String, caption: String, axisStyle: String, coordinateStyle: String, data: Seq[MultiValueResult]) = {
    if (!data.isEmpty) {
      val keys = data.head.storage.keySet.toArray
      Sorting.quickSort(keys)
      Console.withOut(new java.io.PrintStream(new java.io.FileOutputStream(file)))(
        benchmarks.BenchmarkTools.printMultiGraph(caption, axisStyle, coordinateStyle, keys, data.map { res ⇒ (res.name, res.storage) }))
    }
  }
}

case class MultiResult(val name : String) extends MultiValueResult {
  val storage = HashMap[Int, ArrayBuffer[Double]]()
  
  def +=(n: Int, s: Double) {
    if (!storage.contains(n))
      storage.put(n, ArrayBuffer(s))
    else
      storage(n) += s
  }
  
  def reset() = storage.clear()
  
  def toSingleValue(collapse: ArrayBuffer[Double] ⇒ Double) = new SingleValueResult {
    val name = MultiResult.this.name
    def += (n: Int, s: Double) = ???
    val storage = MultiResult.this.storage.map { a ⇒ (a._1, collapse(a._2)) }
  }
  
  def modifyValues(f: Double ⇒ Double) {
    storage.foreach { entry ⇒ for(i ← 0 until entry._2.size) entry._2(i) = f(entry._2(i)) }
  }
}

case class CollapsedResult(val name : String, val collapse: (Double, Double) ⇒ Double) extends SingleValueResult {
  val storage = HashMap[Int, Double]()
  
  def +=(n: Int, s: Double) {
    if (!storage.contains(n))
      storage.put(n, s)
    else
      storage(n) = collapse(storage(n), s)
  }
  
  def reset() = storage.clear()
  
  def modifyValues(f: Double ⇒ Double) {
    storage.foreach(entry ⇒ storage(entry._1) = f(entry._2))
  }
}