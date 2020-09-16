package model

import play.api.libs.json.{Json, OFormat}

case class Allocation(region: String, total_cost: String, servers: Seq[(String, Int)])

object Allocation {
  implicit val allocationFormatter: OFormat[Allocation] = Json.format[Allocation]
}
