package number

import common.CommonTest
import number.api.SkillState
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import java.nio.file.Path

/**
 * This test is used to produce results for the WSR'14 paper.
 * 
 * @note These results did not make it into the paper, because the graph example was found to be more interesting
 *
 * run with: -XX:MaxHeapFreeRatio=99 -Xmx4G -Xms4G
 * @author Timm Felden
 */
@RunWith(classOf[JUnitRunner])
class WSR14Test extends CommonTest {

  val counts = Array(1, 100, 10000)

  // set to 500 for wsr results; reduced for tests
  // @note: running all tests with 500 repetitions will cause funny errors:)
  val repetitions = 10;

  @inline def averageOut(test: ⇒ Unit) {
    System.gc
    System.runFinalization
    // @note: we use a different thread to escape from a very wired memory leak produced by scala's parser combinators
    val t = new Thread(new Runnable {
      def run {
        val t = System.nanoTime
        for (count ← 0 until repetitions)
          test

        print((System.nanoTime - t).toDouble * 1e-6 / repetitions)
      }
    })
    t.start
    t.join
  }

  test("linear") {

    @inline def t(n: Int) {
      val f = tmpFile("wsr");
      val σ = SkillState.create;
      for (i ← 0 to n)
        σ.Number(i)
      σ.write(f)
    }

    // produces nice latex output
    // run twice to get rid of JIT effects
    for (jit ← 0 to 1) {
      print("create")
      for (n ← counts) {
        print(" & ")
        averageOut(t(n))
      }
      print("\\\\\n")
    }
  }

  test("randomize") {

    @inline def make(f: Path, n: Int) {
      val σ = SkillState.create;
      for (i ← 0 to n)
        σ.Number(0)
      σ.write(f)
    }

    @inline def t(f: Path, n: Int) {
      val out = tmpFile("wsr");
      val σ = SkillState.read(f);
      for (n ← σ.Number.all)
        n.number = Random.nextLong
      σ.write(out)
    }

    // produces nice latex output
    // run twice to get rid of JIT effects
    for (jit ← 0 to 1) {
      print("randomize")
      for (n ← counts) {
        print(" & ")
        val f = tmpFile("wsr");
        make(f, n)
        averageOut(t(f, n))
      }
      print("\\\\\n")
    }
  }

  test("sort") {
    @inline def make(f: Path, n: Int) {
      val σ = SkillState.create;
      for (i ← 0 to n)
        σ.Number(Random.nextLong)
      σ.write(f)
    }

    @inline def t(f: Path, n: Int) {
      val out = tmpFile("wsr_sorted");
      val σ = SkillState.read(f);
      σ.Number.all.toBuffer.sortBy[Long](_.number)
      σ.write(out)
    }

    // produces nice latex output
    // run twice to get rid of JIT effects
    for (jit ← 0 to 1) {
      print("sort")
      for (n ← counts) {
        print(" & ")
        val f = tmpFile("wsr_random");
        make(f, n)
        averageOut(t(f, n))
      }
      print("\\\\\n")
    }
  }
}