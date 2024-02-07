package wacc
import scala.collection.mutable.ListBuffer

trait Error {
  val errorType : String
  //val log: ListBuffer[String] = ListBuffer.empty[String]

  //def addError(msg : String) = {
  //  log += msg
  //}

  val log: String

  //def printError() = {
  //  println(s"The number of error is ${log.size.toString()}.")
  //  for (msg <- log) {
  //    println(s"$errorType Error: $msg")
  //  }
  //}

  def exitStatus() : Int

  def hasError: Boolean = log != ""
    
  def resetError(): Unit = ???

}

object SyntaxError extends Error {
  def apply(msg: String): Error = new Error {
    override val errorType: String = "Syntax"
    override val log: String = msg

    override def exitStatus(): Int = 100
  }
}

object SemanticError extends Error {
  def apply(msg: String): Error = new Error {
    override val errorType: String = "Semantic"
    override val log: String = msg
    override def exitStatus(): Int = 200
  }
}


