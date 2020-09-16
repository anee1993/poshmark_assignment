package processors

import utils.Constants

import scala.annotation.tailrec

object OptimalCpuCountMatcher {

  @tailrec
  def getAllPossibleCpuCombos(cpuCount: Int, cpuList: List[Int], resultantList: List[Map[Int,Int]] = List()): List[Map[Int, Int]] = {
    if(cpuList.isEmpty)
      resultantList
    else
      getAllPossibleCpuCombos(cpuCount, cpuList.dropRight(1), resultantList :+ matchCount(cpuCount, cpuList))
  }

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

  def getBestPossibleHourToCostPackage(cpuInfo: List[Map[Int, Int]], regionalMap: Map[String, Double], quotedPrice: Float, hours: Int): (Double, Map[Int, Int]) = {
    val priceToProcessorMap = cpuInfo.map(x => x.flatMap(eachMap => Map(Constants.sizeCpuMap(eachMap._1) -> eachMap._2)))
    val priceListPerHour = priceToProcessorMap.map(x => x.map(pair => regionalMap(pair._1) * pair._2)).map(_.sum)
    val costEffectiveComboPrice = priceListPerHour zip cpuInfo
    costEffectiveComboPrice.minBy(x => x._1)
  }
}
