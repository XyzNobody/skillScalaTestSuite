package containers.array.fixed

import common.StorageTestBase
import common.storage._
import common.randomHelpers.UniformDistribution
import containers.array.fixed.api.SkillState
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import java.nio.file.Path
import java.lang.Runtime;

/**
 * This test is used to find storage overuse by the generated code.
 *
 * run with: -XX:MaxHeapFreeRatio=99 -Xmx8G -Xms4G
 * @author Jonathan Roth
 */
@RunWith(classOf[JUnitRunner])
class StorageTest extends StorageTestBase[SkillState]("array.fixed") {
  override def getMainObject = StorageTest
  
  override def create = SkillState.create
  override def createElements(σ: SkillState, n: Int) =
    for (i ← 0 until n) {
      val data = σ.makeAnnotationArray(10)
      σ.Fixed(data)
    }
  override def write(σ: SkillState, f: Path) = σ.write(f)
  override def read(f: Path) = SkillState.read(f)
  override def createMoreElements(σ: SkillState, n: Int) = createElements(σ, n)
  override def append(σ: SkillState) = σ.append
  
  test("Randomized storage test") {
    val createRes = CollapsedResult("create", Math.max)
    val writeRes = CollapsedResult("write", Math.max)
    val readRes = CollapsedResult("read", Math.max)
    val createMoreRes = CollapsedResult("create more", Math.max)
    val appendRes = CollapsedResult("append", Math.max)
    val random = new Random
    random.setSeed(31948)
    randomizedTest(100, new UniformDistribution(random, 1, 3000000), "-Xmx8G",
        createAndWrite(Some(createRes), Some(writeRes)), readAndAppend(Some(readRes), Some(createMoreRes), Some(appendRes)))
    val results = Seq(createRes, writeRes, readRes, createMoreRes, appendRes)
    SingleValueResult.saveGraph("results/storageTest/fixed.tex", "Fixed size array test", "axis", "only marks", results)
  }
}
object StorageTest extends StorageTestBase.ExternalTest[SkillState] {
  def createTest = new StorageTest
}
