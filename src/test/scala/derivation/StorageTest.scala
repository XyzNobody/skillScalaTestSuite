package derivation

import common.StorageTestBase
import common.randomHelpers.UniformDistribution
import common.storage._
import derivation.api.SkillState
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
class StorageTest extends StorageTestBase[SkillState]("derivation") {
  override def getMainObject = StorageTest
  
  override def create = SkillState.create
  override def createElements(σ: SkillState, n: Int) = for (i ← 0 until n) σ.Derived(i, i)
  override def write(σ: SkillState, f: Path) = σ.write(f)
  override def read(f: Path) = SkillState.read(f)
  override def createMoreElements(σ: SkillState, n: Int) = createElements(σ, n)
  override def append(σ: SkillState) = σ.append
  
  def createEmptyElements(σ: SkillState, n: Int) = for (i ← 0 until n) σ.Empty(i, i)
  
  override def stringToAction(str: String) = str.toLowerCase match {
    case "createempty" ⇒ CreateEmpty(None)
    case "createmoreempty" ⇒ CreateMoreEmpty(None)
    case _ ⇒ super.stringToAction(str)
  }
  
  class CreateEmpty(val createRes: Option[Result]) extends TypedAction {
    override def apply(t: Task, n: Int, f: Path) {
      t.startMeasuring(None, createRes)
      t.σ = Option(create)
      t.σ.map(createEmptyElements(_, n))
      t.stopMeasuring()
    }
    
    override def name = "createempty"
    override def results = Iterator.single(createRes)
    
    override def toString = "create state and elements"
  }
  object CreateEmpty {
    def apply(createRes : Option[Result]) = new CreateEmpty(createRes)
  }
   
  class CreateMoreEmpty(val createRes: Option[Result]) extends TypedAction {
    override def apply(t: Task, n: Int, f: Path) {
      t.startMeasuring(None, createRes)
      t.σ.map(createEmptyElements(_, n))
      t.stopMeasuring()
    }
    
    override def name = "createmoreempty"
    override def results = Iterator.single(createRes)
    
    override def toString = "create more elements"
  }
  object CreateMoreEmpty {
    def apply(createRes : Option[Result]) = new CreateMoreEmpty(createRes)
  }
 
  test("Randomized storage test 1")
  {
    val createRes = CollapsedResult("create", Math.max)
    val writeRes = CollapsedResult("write", Math.max)
    val readRes = CollapsedResult("read", Math.max)
    val createMoreRes = CollapsedResult("create more", Math.max)
    val appendRes = CollapsedResult("append", Math.max)
    val random = new Random
    random.setSeed(31948)
    randomizedTest(100, new UniformDistribution(random, 1, 30000000), "-Xmx8G",
        createAndWrite(Some(createRes), Some(writeRes)),
        readAndAppend(Some(readRes), Some(createMoreRes), Some(appendRes)))
    val results = Seq(createRes, writeRes, readRes, createMoreRes, appendRes)
    val keys = createRes.storage.keySet.toArray
    Sorting.quickSort(keys)
    SingleValueResult.saveGraph("results/storageTest/derivation1.tex", "Derived test", "axis", "only marks",
        Seq(createRes, writeRes, readRes, createMoreRes, appendRes))
  }
 
  test("Randomized storage test 2")
  {
    val createRes = CollapsedResult("create", Math.max)
    val writeRes = CollapsedResult("write", Math.max)
    val readRes = CollapsedResult("read", Math.max)
    val createMoreRes = CollapsedResult("create more", Math.max)
    val appendRes = CollapsedResult("append", Math.max)
    val random = new Random
    random.setSeed(31948)
    randomizedTest(100, new UniformDistribution(random, 1, 30000000), "-Xmx8G",
        CreateEmpty(Some(createRes)) +> Write(Some(writeRes)),
        Read(Some(readRes)) +> CreateMoreEmpty(Some(createMoreRes)) +> Append(Some(appendRes)))
    val results = Seq(createRes, writeRes, readRes, createMoreRes, appendRes)
    val keys = createRes.storage.keySet.toArray
    Sorting.quickSort(keys)
    SingleValueResult.saveGraph("results/storageTest/derivation2.tex", "Empty test", "axis", "only marks",
        Seq(createRes, writeRes, readRes, createMoreRes, appendRes))
  }
}

object StorageTest extends StorageTestBase.ExternalTest[SkillState] {
  def createTest = new StorageTest
}
