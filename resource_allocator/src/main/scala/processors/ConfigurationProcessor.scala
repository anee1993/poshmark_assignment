package processors

import com.typesafe.scalalogging.LazyLogging
import utils.Constants.sizeCpuMap
import utils.TypeSimplifier.{Quadruple, Tuple}
import utils.{Constants, MapKeyReplacer}

import scala.annotation.tailrec

object ConfigurationProcessor extends LazyLogging {

  /*
    This method takes three parameters -

    @param: regionalParameters - A map of server name with per hour usage charges for a given region, eg. us-east
    @param: hours  - total number of hours for which the servers are requested
    @param: cpuCount - minimum number of CPUs requested

    and returns

    @return: (Average price for the configuration, Server config Map --- Map(ServerName -> number of servers) tuple
   */
  def compute(regionalParameters: Map[String, Double], hours: Int, cpuCount: Int): Tuple = {
    val cpuCountHourlyChargePairs = MapKeyReplacer.replaceMap(regionalParameters).toSeq.sortBy(x => x._1)

    if (cpuCount <= 0 || hours <= 0) {
      throw new RuntimeException(s"Invalid Request! CPU count or hours should be greater than 0")
    }
    else {
      val (price, configuration) = cpuCountBasedRecursiveComputation(cpuCountHourlyChargePairs, cpuCount, hours)
      logger.info(s"The configuration requested with $cpuCount CPUs is granted for operational use of $hours hours")
      (price, configuration)
    }
  }

  /*
   This method takes three parameters -

   @param: regionalParameters - A map of server name with per hour usage charges for a given region, eg. us-east
   @param: hours  - total number of hours for which the servers are requested
   @param: maxPrice - maximum price the user is willing to bear

   and returns

   @return: (Average price for the configuration, Server config Map --- Map(ServerName -> number of servers) tuple
  */
  def compute(regionalParameters: Map[String, Double], hours: Int, maxPrice: Float): Tuple = {
    if (maxPrice <= 0 || hours <= 0) {
      throw new RuntimeException(s"Invalid Request! Requested Price or hours should be greater than 0")
    }
    val cpuCountHourlyChargePairs = MapKeyReplacer.replaceMap(regionalParameters).toSeq.sortBy(x => x._1)
    val (price, configuration) = maxPriceBasedRecursiveComputation(cpuCountHourlyChargePairs, maxPrice, hours)
    logger.info(s"The configuration requested with $maxPrice USD is granted for operational use of $hours hours")
    (price, configuration)
  }

  /*
   This method takes three parameters -

   @param: regionalParameters - A map of server name with per hour usage charges for a given region, eg. us-east
   @param: hours  - total number of hours for which the servers are requested
   @param: cpuCount - minimum number of CPUs requested
   @param: maxPrice - maximum price the user is willing to bear

   and returns

   @return: (Average price for the configuration, Server config Map --- Map(ServerName -> number of servers) tuple
  */
  def compute(regionalParameters: Map[String, Double], hours: Int, minCpus: Int, maxPrice: Float): Tuple = {
    if (minCpus <= 0 || hours <= 0 || maxPrice <= 0) {
      throw new RuntimeException(s"Invalid Request! CPU count or hours or Requested price should be greater than 0")
    }
    val (weOfferPrice, configuration, extraAmountToComplyToUserMentionedHours, hoursOfferedForCurrentRequest) = maxPriceAndMinCpuRecursiveComputation(regionalParameters, maxPrice, hours, minCpus)
    if (extraAmountToComplyToUserMentionedHours != 0.0) {
      logger.info(s"For your request, we can offer this configuration of $minCpus CPUs for $hoursOfferedForCurrentRequest hours only." +
        s"You can get to utilize full $hours hours by paying an additional $extraAmountToComplyToUserMentionedHours USD tariff.")
    }
    logger.info(s"The current configuration requested with $minCpus CPUs is granted for operational use of $hoursOfferedForCurrentRequest hours")

    (weOfferPrice, configuration)
  }

  /*
   This method takes 3 parameters (two default which will hold the result) -
   @param: listOfTuples - A Sequence of tuple - (cpu cores, cost per hour of usage)
   @param: cpuCount - Minimum CPUs the user is requesting for
   @param: hours - The total hours for which the servers are requested by user

   and returns

   @return: (Price incurred for resource usage for said hours, Map of Server Name to count of servers)
  */

  @tailrec
  private def cpuCountBasedRecursiveComputation(listOfTuples: Seq[(Int, Double)],
                                                cpuCount: Int,
                                                hours: Int,
                                                cost: Double = 0,
                                                serverMap: Map[String, Int] = Map()): Tuple = {
    if (cpuCount <= 0 || listOfTuples.isEmpty) {
      ("%.2f".format(cost).toDouble, serverMap)
    }
    else {
      val highestCount = listOfTuples.last._1
      val costPerHour = listOfTuples.last._2

      if (cpuCount >= highestCount) {
        val cpuName = sizeCpuMap(highestCount)
        if (serverMap.contains(cpuName)) {
          val count = serverMap(cpuName)
          cpuCountBasedRecursiveComputation(listOfTuples, cpuCount - highestCount, hours, cost + (hours * costPerHour), serverMap.updated(cpuName, count + 1))
        }
        else {
          cpuCountBasedRecursiveComputation(listOfTuples, cpuCount - highestCount, hours, cost + (hours * costPerHour), serverMap ++ Map(cpuName -> 1))
        }
      }
      else {
        cpuCountBasedRecursiveComputation(listOfTuples.dropRight(1), cpuCount, hours, cost, serverMap)
      }
    }
  }

  /*
    This method takes 3 parameters (two default which will hold the result) -
    @param: listOfTuples - A Sequence of tuple - (cpu cores, cost per hour of usage)
    @param: maxPrice - Maximum price the user is willing to pay
    @param: hours - The total hours for which the servers are requested by user

    and returns

    @return: (Price incurred within range of maxPrice, Map of Server Name to count of servers)
   */
  @tailrec
  private def maxPriceBasedRecursiveComputation(listOfTuples: Seq[(Int, Double)],
                                                maxPrice: Float,
                                                hours: Int,
                                                cost: Double = 0,
                                                serverMap: Map[String, Int] = Map()): Tuple = {
    if (listOfTuples.isEmpty) {
      ("%.2f".format(cost).toDouble, serverMap)
    }
    else {
      val highestCount = listOfTuples.last._1
      val costPerHour = listOfTuples.last._2

      val costForGivenHours = costPerHour * hours
      val price = (maxPrice - costForGivenHours).toFloat
      if (price > 0) {
        val cpuName = sizeCpuMap(highestCount)
        if (serverMap.contains(cpuName)) {
          val count = serverMap(cpuName)
          maxPriceBasedRecursiveComputation(listOfTuples, price, hours, cost + costForGivenHours, serverMap.updated(cpuName, count + 1))
        }
        else {
          maxPriceBasedRecursiveComputation(listOfTuples, price, hours, cost + costForGivenHours, serverMap ++ Map(cpuName -> 1))
        }
      }
      else {
        maxPriceBasedRecursiveComputation(listOfTuples.dropRight(1), maxPrice, hours, cost, serverMap)
      }
    }
  }

  /*
    This method takes 4 paramters -
    @param: regionalCostMap - The Map of server name to cost per hour
    @param: affordablePrice - The maximum price user is willing to pay
    @param: hours - The maximum hours for which user wants to use the servers
    @param: cpuCount - the number of CPUs user has requested

    and returns

    @return: (the best price,
    Map of server config,
    Additional price that user might have to pay to use the config for requested hours,
    Maximum hours the user will get to use this configuration for his "quoted" price) - quadruple?
   */
  private def maxPriceAndMinCpuRecursiveComputation(regionalCostMap: Map[String, Double],
                                                    affordablePrice: Float,
                                                    hours: Int,
                                                    cpuCount: Int): Quadruple = {


    val listOfTuples = MapKeyReplacer.replaceMap(regionalCostMap).toSeq.sortBy(x => x._1)
    val listOfCpuCores = listOfTuples.map(x => x._1).toList
    val resultant = CpuCombinationProcessor.getValidCpuCombos(cpuCount, listOfCpuCores)
    val optimalCombo = CpuCombinationProcessor.getBestPossibleHourToCostPackage(resultant, regionalCostMap)
    val perHourCharge = optimalCombo._1
    val maxPrice = perHourCharge * hours
    val priceDifference = maxPrice - affordablePrice
    if (priceDifference < 0) {
      (maxPrice.floor, optimalCombo._2.flatMap(x => Map(Constants.sizeCpuMap(x._1) -> x._2)), 0.0, hours)
    }
    else {
      val possibleValue = getEffectiveHoursAndPrice(affordablePrice, hours, perHourCharge)
      ("%.2f".format(possibleValue._2).toDouble, optimalCombo._2.flatMap(x => Map(Constants.sizeCpuMap(x._1) -> x._2)), "%.2f".format(priceDifference).toDouble, possibleValue._1)
    }
  }

  /*
     This method identifies what is the maximum hours that a given configuration can be run based on user request.

     @return: (max hours for which a given configuration can be comfortably run within user budget, the price it incurs for the said hours) tuple
   */
  @tailrec
  private def getEffectiveHoursAndPrice(maxPrice: Float, hours: Int, perHourCharge: Double): (Int, Double) = {
    val totalPrice = hours * perHourCharge
    if (totalPrice < maxPrice)
      (hours, totalPrice)
    else
      getEffectiveHoursAndPrice(maxPrice, hours - 1, perHourCharge)
  }
}
