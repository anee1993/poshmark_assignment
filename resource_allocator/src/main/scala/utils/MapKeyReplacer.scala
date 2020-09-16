package utils

object MapKeyReplacer {
  def replaceMap(map: Map[String, Double]): Map[Int, Double] = {
    map.map(x => Constants.cpuSizeMap(x._1) -> x._2)
  }
}
