package processors

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import utils.TypeSimplifier.MapOfMaps

import scala.io.Source

class ProcessorSpec extends AnyFlatSpec with Matchers {

  private val cpuResources = Source.fromResource("cpu_resource.json").getLines().mkString
  private val regionCostModelMap = Json.parse(cpuResources).as[MapOfMaps]

  val processor = new Processor(regionCostModelMap)
  "Processor" should "be able to take number of cpus and max hours and compute allocation" in {
    processor.get_costs(24, 115) shouldBe "[{\"region\":\"us-east\",\"total_cost\":\"245.04\",\"servers\":[[\"10xlarge\",3],[\"8xlarge\",1],[\"xlarge\",1],[\"large\",1]]},{\"region\":\"us-west\",\"total_cost\":\"255.12\",\"servers\":[[\"10xlarge\",3],[\"8xlarge\",1],[\"large\",3]]},{\"region\":\"asia\",\"total_cost\":\"205.68\",\"servers\":[[\"8xlarge\",7],[\"xlarge\",1],[\"large\",1]]}]"
  }

  it should "be able to take maximum price and number of hours and compute allocation" in {
    processor.get_costs(24,120.87f) shouldBe "[{\"region\":\"us-east\",\"total_cost\":\"119.86\",\"servers\":[[\"10xlarge\",1],[\"8xlarge\",1],[\"4xlarge\",1]]},{\"region\":\"us-west\",\"total_cost\":\"119.11\",\"servers\":[[\"10xlarge\",1],[\"8xlarge\",1],[\"2xlarge\",1],[\"large\",2]]},{\"region\":\"asia\",\"total_cost\":\"120.72\",\"servers\":[[\"8xlarge\",4],[\"xlarge\",1],[\"large\",1]]}]"
  }

  it should "be able to take maximum price, number of hours and min number of cpus to compute allocation" in {
    processor.get_costs(7,214,95) shouldBe "[{\"region\":\"us-east\",\"total_cost\":\"94.40\",\"servers\":[[\"8xlarge\",13],[\"2xlarge\",1],[\"xlarge\",1]]},{\"region\":\"us-west\",\"total_cost\":\"87.97\",\"servers\":[[\"8xlarge\",13],[\"2xlarge\",1],[\"large\",2]]},{\"region\":\"asia\",\"total_cost\":\"79.70\",\"servers\":[[\"8xlarge\",13],[\"xlarge\",3]]}]"
  }

  it should "throw a Runtime exception when cpu count passed is 0" in {
    intercept[RuntimeException] {
      processor.get_costs(12, 0)
    }
  }

  it should "throw a Runtime exception when hours passed is 0" in {
    intercept[RuntimeException] {
      processor.get_costs(0, 4)
    }
  }

  it should "throw a Runtime exception when any of the three params - cpu, hours or max price passed is 0" in {
    intercept[RuntimeException] {
      processor.get_costs(0, 0, 0)
    }
  }
}
