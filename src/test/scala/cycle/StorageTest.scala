package cycle

import common.StorageTestBase
import common.randomHelpers.UniformDistribution
import common.storage._
import cycle.api.SkillState
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import scala.util.Sorting
import java.nio.file.Path
import java.lang.Runtime;

/**
 * This test is used to find storage overuse by the generated code.
 *
 * run with: -XX:MaxHeapFreeRatio=99 -Xmx8G -Xms4G
 * @author Jonathan Roth
 */
@RunWith(classOf[JUnitRunner])
class StorageTest extends StorageTestBase[SkillState]("cycle") {
  override def getMainObject = StorageTest
  
  override def create = SkillState.create
  override def createElements(σ: SkillState, n: Int) {
    var c = σ.Cycle(null)
    for (i ← 1 until n) { c = σ.Cycle(c) }
    σ.Cycle.head.prev = c
  }
  override def write(σ: SkillState, f: Path) = σ.write(f)
  override def read(f: Path) = SkillState.read(f)
  override def createMoreElements(σ: SkillState, n: Int) {
    var c = σ.Cycle.last
    for (i ← 0 until n) { c = σ.Cycle(c) }
    σ.Cycle.head.prev = c
  }
  override def append(σ: SkillState) = σ.append
  
  test("Randomized storage test") {
    val createRes = CollapsedResult("create", Math.max)
    val writeRes = CollapsedResult("write", Math.max)
    val readRes = CollapsedResult("read", Math.max)
    val createMoreRes = CollapsedResult("create more", Math.max)
    val appendRes = CollapsedResult("append", Math.max)
    val random = new Random
    random.setSeed(31948)
    randomizedTest(100, new UniformDistribution(random, 1, 30000000), "-Xmx8G",
        createAndWrite(Some(createRes), Some(writeRes)), readAndAppend(Some(readRes), Some(createMoreRes), Some(appendRes)))
    val results = Seq(createRes, writeRes, readRes, createMoreRes, appendRes)
    SingleValueResult.saveGraph("results/storageTest/cycle.tex", "Cycle test", "axis", "only marks", results)
  }
}

object StorageTest extends StorageTestBase.ExternalTest[SkillState] {
  def createTest = new StorageTest
}
