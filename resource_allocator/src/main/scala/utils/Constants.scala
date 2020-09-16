package utils

object Constants {
  val us_east = "us-east"
  val us_west = "us-west"
  val asia_region = "asia"

  val cpuSizeMap = Map(
    "large" -> 1,
    "xlarge" -> 2,
    "2xlarge" -> 4,
    "4xlarge" -> 8,
    "8xlarge" -> 16,
    "10xlarge" -> 32
  )

  val sizeCpuMap = cpuSizeMap.flatMap(x => Map(x._2 -> x._1))
}
