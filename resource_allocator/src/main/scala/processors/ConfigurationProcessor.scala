package processors

import com.typesafe.scalalogging.LazyLogging
import utils.Constants.sizeCpuMap
import utils.{Constants, MapKeyReplacer}

import scala.annotation.tailrec

object ConfigurationProcessor extends LazyLogging {

  def compute(regionalParameters: Map[String, Double], hours: Int, cpuCount: Int): (Double, Map[String, Int]) = {
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

  def compute(regionalParameters: Map[String, Double], hours: Int, maxPrice: Float): (Double, Map[String, Int]) = {
    if (maxPrice <= 0 || hours <= 0) {
      throw new RuntimeException(s"Invalid Request! Requested Price or hours should be greater than 0")
    }
    val cpuCountHourlyChargePairs = MapKeyReplacer.replaceMap(regionalParameters).toSeq.sortBy(x => x._1)
    val (price, configuration) = maxPriceBasedRecursiveComputation(cpuCountHourlyChargePairs, maxPrice, hours)
    logger.info(s"The configuration requested with $maxPrice USD is granted for operational use of $hours hours")
    (price, configuration)
  }

  def compute(regionalParameters: Map[String, Double], hours: Int, minCpus: Int, maxPrice: Float): (Double, Map[String, Int]) = {
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

  @tailrec
  private def cpuCountBasedRecursiveComputation(listOfTuples: Seq[(Int, Double)],
                                                cpuCount: Int,
                                                hours: Int,
                                                cost: Double = 0,
                                                serverMap: Map[String, Int] = Map()): (Double, Map[String, Int]) = {
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

  @tailrec
  private def maxPriceBasedRecursiveComputation(listOfTuples: Seq[(Int, Double)],
                                                maxPrice: Float,
                                                hours: Int,
                                                cost: Double = 0,
                                                serverMap: Map[String, Int] = Map()): (Double, Map[String, Int]) = {
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

  private def maxPriceAndMinCpuRecursiveComputation(regionalCostMap: Map[String, Double],
                                                    affordablePrice: Float,
                                                    hours: Int,
                                                    cpuCount: Int): (Double, Map[String, Int], Double, Int) = {


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
      val possibleValue = computePriceLoop(affordablePrice, hours, perHourCharge)
      ("%.2f".format(possibleValue._2).toDouble, optimalCombo._2.flatMap(x => Map(Constants.sizeCpuMap(x._1) -> x._2)), "%.2f".format(priceDifference).toDouble, possibleValue._1)
    }
  }

  @tailrec
  private def computePriceLoop(maxPrice: Float, hours: Int, perHourCharge: Double): (Int, Double) = {
    val totalPrice = hours * perHourCharge
    if (totalPrice < maxPrice)
      (hours, totalPrice)
    else
      computePriceLoop(maxPrice, hours - 1, perHourCharge)
  }
}
