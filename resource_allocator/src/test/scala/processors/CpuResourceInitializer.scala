package processors

import play.api.libs.json.Json
import utils.TypeSimplifier.MapOfMaps

import scala.io.Source

trait CpuResourceInitializer {
  val cpuResources: String = Source.fromResource("cpu_resource.json").getLines().mkString
  val regionCostModelMap: MapOfMaps = Json.parse(cpuResources).as[MapOfMaps]
}
