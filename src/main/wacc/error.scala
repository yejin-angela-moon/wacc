package wacc
import scala.collection.mutable.ListBuffer


trait error {
  val errorType: String
  val log: ListBuffer[String] = ListBuffer.empty[String]

  def addError(message: String) = {
    log += message
  }

  def printError() = {
    println(s"The number of error is ${log.size.toString()}.")
    for (msg <- log) {
      println(s"$errorType Error: $msg")
    }
  }

  def hasError: Boolean = log.nonEmpty
    
  def resetError() = {
    log.clear()
  }
}

object semanticError extends error {
  override val errorType = "Semantic"
}

object syntaxError extends error {
  override val errorType = "Syntax"
}
