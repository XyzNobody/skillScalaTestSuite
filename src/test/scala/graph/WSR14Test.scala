package graph

import java.io.File
import java.nio.file.Files
import scala.Array.canBuildFrom
import scala.util.Random
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import graph.api.SkillState
import scala.collection.mutable.HashSet

/**
 * This test is used to produce results for the WSR'14 paper.
 *
 * run with: -XX:MaxHeapFreeRatio=99 -Xmx4G -Xms4G
 * @author Timm Felden
 */
@RunWith(classOf[JUnitRunner])
class WSR14Test extends FunSuite {

  @inline final def tmpFile(s: String) = {
    val r = File.createTempFile(s, ".sf")
    //    r.deleteOnExit
    r.toPath
  }

  // we use a deterministic random number generator, in order to get reproducible results over several runs
  val Random = new Random

  // set upper bound to 9 for wsr results; reduced to 6 for test-suite
  val counts = (0 to 6).map(1000 * 1 << _).toArray

  // set to 10 for wsr results; reduced for tests
  val repetitions = 2;

  @inline def averageTime(test: Int ⇒ Long) = counts.map { n ⇒
    Random.setSeed(31337)
    var total = 0.0
    for (count ← 0 until repetitions) {
      System.gc
      System.runFinalization
      // yield to the gc
      Thread.sleep(1);

      val t = System.nanoTime

      test(n)

      total += (System.nanoTime - t)
    }

    total *= 1e-9 / repetitions.toDouble
    println(f"$total%4.4f")
    total
  }

  @inline def averageSize(implicit test: Int ⇒ Long) = counts.map { n ⇒
    Random.setSeed(31337)

    val size = test(n)

    println(size+" Bytes")
    size
  }

  /**
   * produces nice latex output; first size, then speed to get rid of JIT effects
   */
  @inline def eval(name: String, test: Int ⇒ Long) = {
    println(name)
    (averageSize(test), averageTime(test))
  }

  test("make wsr14 results") {
    val cr = create
    System.gc
    System.runFinalization
    val wr = write
    System.gc
    System.runFinalization
    val re = read
    System.gc
    System.runFinalization
    val ap = append

    // first plot: absolute time spend for each phase
    locally {
      println("""
\begin{figure}
 \begin{tikzpicture}
  \begin{loglogaxis}""")

      val results = Array(cr._2, wr._2, re._2, ap._2)

      for (r ← results)
        println(
          (
            for (i ← 0 until r.length)
              yield s"(${counts(i)},${r(i)})"
          ).mkString("""
    \addplot+[smooth] coordinates
     {""", " ", "};")
        )

      println("""
  \end{loglogaxis}
 \end{tikzpicture}
 \caption{time per node}
\end{figure}""")
    }

    // second plot: time spend for each phase divided by base count
    locally {
      println("""
\begin{figure}
 \begin{tikzpicture}
  \begin{semilogxaxis}""")

      val results = Array(wr, re, ap)

      for (r ← results)
        println(
          (
            for (i ← 0 until r._1.length)
              yield s"(${counts(i)},${r._2(i) / r._1(i).toDouble})"
          ).mkString("""
    \addplot+[smooth] coordinates
     {""", " ", "};")
        )

      println("""
  \end{semilogxaxis}
 \end{tikzpicture}
 \caption{time per size}
\end{figure}""")
    }
  }

  def create = {
    val f = tmpFile("wsr.create");

    @inline def t(n: Int): Long = {
      val σ = SkillState.create;
      for (i ← 0 until n)
        σ.Node("black", σ.makeRefSet(σ.Node))

      // create random colors
      for (n ← σ.Node.all)
        n.color = Random.nextInt(4) match {
          case 0 ⇒ "black"
          case 1 ⇒ "red"
          case 2 ⇒ "blue"
          case 3 ⇒ "green"
        }

      // add edges 100 tries
      val nodes = σ.Node.all.toArray
      for (node ← σ.Node.all; j ← 0 until 100)
        node.edges.add(nodes(Random.nextInt(nodes.length)));

      0L
    }

    eval("create", t)
  }

  def write = {
    val f = tmpFile("wsr.write");

    @inline def t(n: Int): Long = {
      val σ = SkillState.create;
      for (i ← 0 until n)
        σ.Node("black", σ.makeRefSet(σ.Node))

      // create random colors
      for (n ← σ.Node.all)
        n.color = Random.nextInt(4) match {
          case 0 ⇒ "black"
          case 1 ⇒ "red"
          case 2 ⇒ "blue"
          case 3 ⇒ "green"
        }

      // add edges 100 tries
      val nodes = σ.Node.all.toArray
      for (node ← σ.Node.all; j ← 0 until 100)
        node.edges.add(nodes(Random.nextInt(nodes.length)));

      σ.write(f)
      Files.size(f)
    }

    eval("\\ +write", t)
  }

  def read = {

    @inline def t(n: Int): Long = {
      val f = tmpFile("wsr.read");
      locally {
        val σ = SkillState.create;
        for (i ← 0 until n)
          σ.Node("black", σ.makeRefSet(σ.Node))

        // create random colors
        for (n ← σ.Node.all)
          n.color = Random.nextInt(4) match {
            case 0 ⇒ "black"
            case 1 ⇒ "red"
            case 2 ⇒ "blue"
            case 3 ⇒ "green"
          }

        // add edges 100 tries
        val nodes = σ.Node.all.toArray
        for (node ← σ.Node.all; j ← 0 until 100)
          node.edges.add(nodes(Random.nextInt(nodes.length)));

        σ.write(f)
      }

      locally {
        val σ = SkillState.read(f);
        Files.size(f)
      }
    }

    eval("\\ +read", t)
  }

  def append = {

    @inline def t(n: Int): Long = {
      val f = tmpFile("wsr.append");
      locally {
        val σ = SkillState.create;
        for (i ← 0 until n)
          σ.Node("black", σ.makeRefSet(σ.Node))

        // create random colors
        for (n ← σ.Node.all)
          n.color = Random.nextInt(4) match {
            case 0 ⇒ "black"
            case 1 ⇒ "red"
            case 2 ⇒ "blue"
            case 3 ⇒ "green"
          }

        // add edges 100 tries
        val nodes = σ.Node.all.toArray
        for (node ← σ.Node.all; j ← 0 until 100)
          node.edges.add(nodes(Random.nextInt(nodes.length)));

        σ.write(f)
      }

      locally {
        val σ = SkillState.read(f);
        // add 100% orange nodes
        // fix, because pool access is not yet an indexed seq or something like that
        val nodes = σ.Node.all.toArray
        for (i ← 0 until n) {
          val n = σ.Node("orange", σ.makeRefSet(σ.Node))
          for (j ← 0 until 100)
            n.edges.add(nodes(Random.nextInt(nodes.length)));
        }

        σ.append

        Files.size(f)
      }
    }

    eval("\\ +append", t)
  }
}