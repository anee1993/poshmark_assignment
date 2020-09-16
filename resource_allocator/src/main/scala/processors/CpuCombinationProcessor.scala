package processors

import utils.Constants

import scala.annotation.tailrec

object CpuCombinationProcessor {

  /*
    This method takes two paramters -
    @param: cpuCount - the number of CPUs requested
    @param: cpuList - the list of available servers (in terms of CPU core count)

    and returns

    @return: A list of Maps (Map of CPU core count to number of such CPUs needed)
   */
  @tailrec
  def getValidCpuCombos(cpuCount: Int, cpuList: List[Int], resultantList: List[Map[Int, Int]] = List()): List[Map[Int, Int]] = {
    if (cpuList.isEmpty)
      resultantList
    else
      getValidCpuCombos(cpuCount, cpuList.dropRight(1), resultantList :+ matchCount(cpuCount, cpuList))
  }

  /*
    This method is a helper method for getValidCpuCombos. It returns a Map of CPU core count to number of such CPUs needed,
    eg: (32 -> 2) -- 32 is the cores and 2 is the number of 32 core CPUs.
   */
  @tailrec
  private def matchCount(cpuCount: Int, cpuList: List[Int], result: Map[Int, Int] = Map()): Map[Int, Int] = {
    if (cpuCount <= 0) {
      result
    }
    else {
      val last = cpuList.last
      val remaining = cpuCount - last
      if (remaining >= 0) {
        if (result.contains(last)) {
          val value = result(last)
          matchCount(remaining, cpuList, result.updated(last, value + 1))
        }
        else {
          matchCount(remaining, cpuList, result ++ Map(last -> 1))
        }
      }
      else {
        matchCount(cpuCount, cpuList.dropRight(1), result)
      }
    }
  }

  def getBestPossibleHourToCostPackage(cpuInfo: List[Map[Int, Int]], regionalMap: Map[String, Double]): (Double, Map[Int, Int]) = {
    val priceToProcessorMap = cpuInfo.map(x => x.flatMap(eachMap => Map(Constants.sizeCpuMap(eachMap._1) -> eachMap._2)))
    val priceListPerHour = priceToProcessorMap.map(x => x.map(pair => regionalMap(pair._1) * pair._2)).map(_.sum)
    val costEffectiveComboPrice = priceListPerHour zip cpuInfo
    costEffectiveComboPrice.minBy(x => x._1)
  }
}
