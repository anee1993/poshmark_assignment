package utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ResultFormatterSpec extends AnyFlatSpec with Matchers {

  behavior of "Result Formatter"

  "ResultFormatter" should " be able to format the json obtained into suitable result format" in {
    val json = "[{\"region\":\"us-east\",\"total_cost\":\"94.40\",\"servers\":[[\"8xlarge\",13],[\"2xlarge\",1],[\"xlarge\",1]]},{\"region\":\"us-west\",\"total_cost\":\"87.97\",\"servers\":[[\"8xlarge\",13],[\"2xlarge\",1],[\"large\",2]]},{\"region\":\"asia\",\"total_cost\":\"79.70\",\"servers\":[[\"8xlarge\",13],[\"xlarge\",3]]}]"
    new ResultFormatter().formatResult(json) shouldBe "[{\"region\":\"us-east\",\"total_cost\":\"94.40\",\"servers\":[(\"8xlarge\",13),(\"2xlarge\",1),(\"xlarge\",1)]},{\"region\":\"us-west\",\"total_cost\":\"87.97\",\"servers\":[(\"8xlarge\",13),(\"2xlarge\",1),(\"large\",2)]},{\"region\":\"asia\",\"total_cost\":\"79.70\",\"servers\":[(\"8xlarge\",13),(\"xlarge\",3)]}]"
  }
}
