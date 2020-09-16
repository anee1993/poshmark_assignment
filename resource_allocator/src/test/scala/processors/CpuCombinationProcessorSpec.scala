package processors

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Constants.us_east

class CpuCombinationProcessorSpec extends AnyFlatSpec
  with Matchers
  with CpuResourceInitializer {

  behavior of "CpuCombinationProcessor"

  "CpuCombinationProcessor" should " be able to return valid server combinations" in {
    CpuCombinationProcessor.getValidCpuCombos(100, List(1, 2, 4, 8, 16, 32)) shouldBe List(
      Map(32 -> 3, 4 -> 1),
      Map(16 -> 6, 4 -> 1),
      Map(8 -> 12, 4 -> 1),
      Map(4 -> 25),
      Map(2 -> 50),
      Map(1 -> 100)
    )
  }

  it should "return the optimal price to server configuration" in {
    val possibleCPUCombos = CpuCombinationProcessor.getValidCpuCombos(100, List(1, 2, 4, 8, 16, 32))
    val regionalMap = regionCostModelMap(us_east)
    CpuCombinationProcessor.getBestPossibleHourToCostPackage(possibleCPUCombos, regionalMap) shouldBe (8.849999999999998,Map(16 -> 6, 4 -> 1))
  }
}
