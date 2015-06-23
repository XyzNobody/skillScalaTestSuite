package containers.set

import common.StorageTestBase
import common.randomHelpers.UniformDistribution
import common.storage._
import containers.set.api.SkillState
import scala.collection.mutable.HashSet
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
class StorageTest extends StorageTestBase[SkillState]("set") {
  override def getMainObject = StorageTest
  
  override def create = SkillState.create
  override def createElements(σ: SkillState, n: Int) = for (i ← 0 until n) σ.ASet(HashSet(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  override def write(σ: SkillState, f: Path) = σ.write(f)
  override def read(f: Path) = SkillState.read(f)
  override def createMoreElements(σ: SkillState, n: Int) = createElements(σ, n)
  override def append(σ: SkillState) = σ.append
  
  def createVarElements(σ: SkillState, n: Int) =
    for (i ← 0 until 10) {
      val data = new HashSet[Long]()
      for (j ← 0 until n)
        data += j
      σ.ASet(data)
    }
  def createMoreVarElements(σ: SkillState, n: Int) =
    for (obj ← σ.ASet.all) {
      val data = obj.aSet
      for (j ← 0 until n)
        data += j + n
    }
  
  override def stringToAction(str: String) = str.toLowerCase match {
    case "createvar" ⇒ CreateVar(None)
    case "createmorevar" ⇒ CreateMoreVar(None)
    case _ ⇒ super.stringToAction(str)
  }
  
  class CreateVar(val createRes: Option[Result]) extends TypedAction {
    override def apply(t: Task, n: Int, f: Path) {
      t.startMeasuring(None, createRes)
      t.σ = Option(create)
      t.σ.map(createVarElements(_, n))
      t.stopMeasuring()
    }
    
    override def name = "createvar"
    override def results = Iterator.single(createRes)
    
    override def toString = "create state and elements"
  }
  object CreateVar {
    def apply(createRes : Option[Result]) = new CreateVar(createRes)
  }
   
  class CreateMoreVar(val createRes: Option[Result]) extends TypedAction {
    override def apply(t: Task, n: Int, f: Path) {
      t.startMeasuring(None, createRes)
      t.σ.map(createVarElements(_, n))
      t.stopMeasuring()
    }
    
    override def name = "createmorevar"
    override def results = Iterator.single(createRes)
    
    override def toString = "create more elements"
  }
  object CreateMoreVar {
    def apply(createRes : Option[Result]) = new CreateMoreVar(createRes)
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
    randomizedTest(100, new UniformDistribution(random, 1, 3000000), "-Xmx8G",
        createAndWrite(Some(createRes), Some(writeRes)),
        readAndAppend(Some(readRes), Some(createMoreRes), Some(appendRes)))
    val results = Seq(createRes, writeRes, readRes, createMoreRes, appendRes)
    val keys = createRes.storage.keySet.toArray
    Sorting.quickSort(keys)
    SingleValueResult.saveGraph("results/storageTest/set1.tex", "Set test", "axis", "only marks",
        Seq(createRes, writeRes, readRes, createMoreRes, appendRes))
  }
 
  test("Randomized storage test 2")
  {
    val createRes = CollapsedResult("create", Math.max)
    val writeRes = CollapsedResult("write", Math.max)
    val readRes = CollapsedResult("read", Math.max)
    val createMoreRes = CollapsedResult("create more", Math.max)
    val appendRes = CollapsedResult("write 2", Math.max)
    val random = new Random
    random.setSeed(31948)
    randomizedTest(100, new UniformDistribution(random, 1, 3000000), "-Xmx8G",
        CreateVar(Some(createRes)) +> Write(Some(writeRes)),
        Read(Some(readRes)) +> CreateMoreVar(Some(createMoreRes)) +> Write(Some(appendRes)))
    val results = Seq(createRes, writeRes, readRes, createMoreRes, appendRes)
    val keys = createRes.storage.keySet.toArray
    Sorting.quickSort(keys)
    SingleValueResult.saveGraph("results/storageTest/set2.tex", "Set test", "axis", "only marks",
        Seq(createRes, writeRes, readRes, createMoreRes, appendRes))
  }
}

object StorageTest extends StorageTestBase.ExternalTest[SkillState] {
  def createTest = new StorageTest
}
