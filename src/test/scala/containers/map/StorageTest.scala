package containers.map

import common.CommonTest
import containers.map.api.SkillState
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import scala.collection.mutable.HashMap
import java.nio.file.Path
import java.lang.Runtime;

/**
 * This test is used to find storage overuse by the generated code.
 *
 * run with: -XX:MaxHeapFreeRatio=99 -Xmx8G -Xms4G
 * @author Jonathan Roth
 */
@RunWith(classOf[JUnitRunner])
class StorageTest extends CommonTest {
  val counts = Array(1, 10, 100, 1000, 10000)
  val repetitions = 10
  
  @inline def memoryOut(test: ⇒ Unit) {
    System.gc
    System.runFinalization
    // @note: we use a different thread to break single runs when they exceed memory (OutOfMemoryError is thrown)
    val t = new Thread(new Runnable {
      def run {
        for (count ← 0 until repetitions) {
          System.gc
          System.runFinalization
          test
        }
      }
    })
    t.start
    t.join
  }

  test("StorageBenchmark") {
    val rt = Runtime.getRuntime
    @inline def t(n: Int, s: Int) {
      // yield to the gc
      //Thread.sleep((Math.sqrt(n) * 0.3).toLong);

      val f = tmpFile("st_benchmark");
      
      val empty = rt.totalMemory - rt.freeMemory
      print(empty)
      print("\t")
      
      locally {
        // create
        val σ = SkillState.create;
//        for (i ← 0 until n)
//          σ.MapContainer(HashMap((for (j ← 0 until s) yield (j, j)): _*))
        System.gc
        System.runFinalization
        print(rt.totalMemory - rt.freeMemory - empty)
        print("\t")
        
        // write
        σ.write(f)
        print(rt.totalMemory - rt.freeMemory - empty)
        print("\t")
      }
      
      // destroy old state
      System.gc
      System.runFinalization
      
      locally {
        // read
        val σ = SkillState.read(f);
        print(rt.totalMemory - rt.freeMemory - empty)
        print("\t")
        
        // create more
//        for (i ← 0 until n)
//          σ.MapContainer(HashMap((for (j ← 0 until s) yield (j, j)): _*))
        System.gc
        System.runFinalization
        print(rt.totalMemory - rt.freeMemory - empty)
        print("\t")
        
        // append
        σ.append
        print(rt.totalMemory - rt.freeMemory - empty)
        print("\n")
      }
    }

    // produces nice latex output
    for (objectCount ← counts; elementCount ← counts) {
      print("objects:\t")
      print(objectCount)
      print("\telements:\t")
      println(elementCount)
      print("empty\tcreated\twritten\tread\tmore created\tappended\n")
      memoryOut(t(objectCount, elementCount))
      println()
    }
  }
}
