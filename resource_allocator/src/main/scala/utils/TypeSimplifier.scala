package utils

object TypeSimplifier {
 //Always good to specify custom types for something as long as this.
 type MapOfMaps = Map[String,Map[String, Double]]
 type Tuple = (Double, Map[String, Int])
 type Quadruple = (Double, Map[String, Int], Double, Int)
}
