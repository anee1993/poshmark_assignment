package processors

import com.typesafe.scalalogging.LazyLogging
import model.Allocation
import play.api.libs.json.Json
import utils.Constants.{asia_region, us_east, us_west}
import utils.TypeSimplifier.{MapOfMaps, Tuple}

class AllocationProcessor(map: MapOfMaps) extends LazyLogging {

  private val usEast = map.get(us_east).fold(throw new RuntimeException("Expected region not available"))(region => region)
  private val usWest = map.get(us_west).fold(throw new RuntimeException("Expected region not available"))(region => region)
  private val asia = map.get(asia_region).fold(throw new RuntimeException("Expected region not available"))(region => region)

  /*
    This method takes three params
    @param: hours  - total number of hours for which the servers are requested
    @param: minCpus - minimum number of CPUs requested
    @param: maxPrice - maximum price the user is willing to bear

    @return: A Json String that captures the Average cost incurred for allocating resources based on region
   */
  def get_costs(hours: Int, minCpus: Int, maxPrice: Float): String = {
    logger.info("Computing resources based on hours and cpu count and max affordable price!")

    val usEastServerConfig = ConfigurationProcessor.compute(usEast, hours, minCpus, maxPrice)
    val usWestServerConfig = ConfigurationProcessor.compute(usWest, hours, minCpus, maxPrice)
    val asiaServerConfig = ConfigurationProcessor.compute(asia, hours, minCpus, maxPrice)

    getAJsonFromServerConfigs(usEastServerConfig, usWestServerConfig, asiaServerConfig)
  }

  /*
    This method takes two params
    @param: hours  - total number of hours for which the servers are requested
    @param: minCpus - minimum number of CPUs requested

    @return: A Json String that captures the Average cost incurred for allocating resources based on region
   */
  def get_costs(hours: Int, minCpus: Int): String = {
    logger.info("Computing resources based on hours and cpu count!")

    val usEastServerConfig = ConfigurationProcessor.compute(usEast, hours, minCpus)
    val usWestServerConfig = ConfigurationProcessor.compute(usWest, hours, minCpus)
    val asiaServerConfig = ConfigurationProcessor.compute(asia, hours, minCpus)

    getAJsonFromServerConfigs(usEastServerConfig, usWestServerConfig, asiaServerConfig)
  }

  /*
    This method takes three params
    @param: hours  - total number of hours for which the servers are requested
    @param: maxPrice - maximum price the user is willing to bear

    @return: A Json String that captures the Average cost incurred for allocating resources based on region
   */
  def get_costs(hours: Int, maxPrice: Float): String = {
    logger.info("Computing resources based on hours and max affordable price!")

    val usEastServerConfig = ConfigurationProcessor.compute(usEast, hours, maxPrice)
    val usWestServerConfig = ConfigurationProcessor.compute(usWest, hours, maxPrice)
    val asiaServerConfig = ConfigurationProcessor.compute(asia, hours, maxPrice)

    getAJsonFromServerConfigs(usEastServerConfig, usWestServerConfig, asiaServerConfig)
  }

  /*
    This method takes the (average price, Server config Map) tuple along with region and binds this data to Allocation Model
    @return - Allocation instance
   */
  private def wrapDataIntoAllocationModel(data: (Double, Map[String, Int]), region: String): Allocation = {
    Allocation(region, "%.2f".format(data._1), data._2.toSeq)
  }

  /*
    This is a helper method that takes the server configs, puts them into a list
    and writes them as Json String.

    @return: Json String
   */
  private def getAJsonFromServerConfigs(usEastServerConfig: Tuple,
                                        usWestServerConfig: Tuple,
                                        asiaServerConfig:   Tuple): String = {
    val regionalConfigurationList = List(
      wrapDataIntoAllocationModel((usEastServerConfig._1, usEastServerConfig._2), us_east),
      wrapDataIntoAllocationModel((usWestServerConfig._1, usWestServerConfig._2), us_west),
      wrapDataIntoAllocationModel((asiaServerConfig._1, asiaServerConfig._2), asia_region))

    Json.toJson(regionalConfigurationList).toString()
  }
}
