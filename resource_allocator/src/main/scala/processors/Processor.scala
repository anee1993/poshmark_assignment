package processors

import com.typesafe.scalalogging.LazyLogging
import model.Allocation
import play.api.libs.json.Json
import utils.Constants.{asia_region, us_east, us_west}
import utils.TypeSimplifier.MapOfMaps

class Processor(map: MapOfMaps) extends LazyLogging {

  private val usEast = map.get(us_east).fold(throw new RuntimeException("Expected region not available"))(region => region)
  private val usWest = map.get(us_west).fold(throw new RuntimeException("Expected region not available"))(region => region)
  private val asia = map.get(asia_region).fold(throw new RuntimeException("Expected region not available"))(region => region)

  def get_costs(hours: Int, minCpus: Int, maxPrice: Float): String = {
    logger.info("Computing resources based on hours and cpu count and max affordable price!")
    val usEastServerConfig = RegionBasedComputation.compute(usEast, hours, minCpus, maxPrice)
    val usWestServerConfig = RegionBasedComputation.compute(usWest, hours, minCpus, maxPrice)
    val asiaServerConfig = RegionBasedComputation.compute(asia, hours, minCpus, maxPrice)
    val regionalConfigurationList = List(
      wrapDataIntoAllocationModel((usEastServerConfig._1,usEastServerConfig._2), us_east),
      wrapDataIntoAllocationModel((usWestServerConfig._1, usWestServerConfig._2), us_west),
      wrapDataIntoAllocationModel((asiaServerConfig._1, asiaServerConfig._2), asia_region))

    Json.toJson(regionalConfigurationList).toString()
  }

  def get_costs(hours: Int, minCpus: Int): String = {
    logger.info("Computing resources based on hours and cpu count!")
    val usEastServerConfig = RegionBasedComputation.compute(usEast, hours, minCpus)
    val usWestServerConfig = RegionBasedComputation.compute(usWest, hours, minCpus)
    val asiaServerConfig = RegionBasedComputation.compute(asia, hours, minCpus)

    val regionalConfigurationList = List(
      wrapDataIntoAllocationModel((usEastServerConfig._1,usEastServerConfig._2), us_east),
      wrapDataIntoAllocationModel((usWestServerConfig._1, usWestServerConfig._2), us_west),
      wrapDataIntoAllocationModel((asiaServerConfig._1, asiaServerConfig._2), asia_region))

    Json.toJson(regionalConfigurationList).toString()
  }

  def get_costs(hours: Int, maxPrice: Float): String = {
    logger.info("Computing resources based on hours and max affordable price!")
    val usEastServerConfig = RegionBasedComputation.compute(usEast, hours, maxPrice)
    val usWestServerConfig = RegionBasedComputation.compute(usWest, hours, maxPrice)
    val asiaServerConfig = RegionBasedComputation.compute(asia, hours, maxPrice)
    val regionalConfigurationList = List(
      wrapDataIntoAllocationModel((usEastServerConfig._1,usEastServerConfig._2), us_east),
      wrapDataIntoAllocationModel((usWestServerConfig._1, usWestServerConfig._2), us_west),
      wrapDataIntoAllocationModel((asiaServerConfig._1, asiaServerConfig._2), asia_region))

    Json.toJson(regionalConfigurationList).toString()
  }

  private def wrapDataIntoAllocationModel(data: (Double, Map[String, Int]), region: String): Allocation = {
    Allocation(region, "%.2f".format(data._1), data._2.toSeq)
  }
}
