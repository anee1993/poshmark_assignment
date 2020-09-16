package utils

import scala.collection.mutable

class ResultFormatter {

  val aStack = new mutable.Stack[Char]
  val stringBuffer = new StringBuffer()

  def formatResult(jsonString: String): String = {
    val charArr = jsonString.toCharArray

    charArr.foreach(
      c =>
        if(c == '[' && aStack.nonEmpty && (aStack.top == '\"' || aStack.top == '[' || aStack.top == ',')){
          stringBuffer.append('(')
          aStack.push(c)
        }
        else if(c == ']' && aStack.nonEmpty && (aStack.top != ']' && aStack.top != '}')) {
          stringBuffer.append(')')
          aStack.push(c)
        }
        else {
          aStack.push(c)
          stringBuffer.append(c)
        }
    )

    stringBuffer.toString
  }
}
