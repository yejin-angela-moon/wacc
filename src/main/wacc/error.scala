package wacc
import scala.collection.mutable.ListBuffer

trait error {
  val errorType : String
  val log: ListBuffer[String] = ListBuffer.empty[String]

  def addError(msg : String) = {
    log += msg
  }

  def printError() = {
    println(s"The number of error is ${log.size.toString()}.")
    for (msg <- log) {
      println(s"$errorType Error: $msg")
    }
  }

  def exitStatus() : Int

  def hasError: Boolean = log.nonEmpty
    
  def resetError() = {
    log.clear()
  }
}

object syntaxError extends error {
  override val errorType = "Syntax"
  override def exitStatus(): Unit = 100
}

object semanticError extends error {
  override val errorType = "Semantic"
  override def exitStatus(): Unit = 200
}

