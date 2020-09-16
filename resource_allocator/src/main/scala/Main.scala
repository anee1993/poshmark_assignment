import com.typesafe.scalalogging.LazyLogging
import play.api.libs.json.Json
import processors.Processor
import utils.TypeSimplifier.MapOfMaps

import scala.io.Source

object Main extends App with LazyLogging {
  logger.info("Starting main app!!")

  val cpuResources = Source.fromResource("cpu_resource.json").getLines().mkString
  logger.info(s"Obtained the resource configuration from classpath - $cpuResources")

  val regionCostModelMap = Json.parse(cpuResources).as[MapOfMaps]

  val processor = new Processor(regionCostModelMap)

  println(processor.get_costs(24,115))
  println(processor.get_costs(24,120.87f))
  println(processor.get_costs(7,214,95))
}
