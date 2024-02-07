package wacc
import scala.collection.mutable.ListBuffer

object Error {
//   val errorType : String
//   val log: ListBuffer[String] = ListBuffer.empty[String]

//   def addError(msg : String) = {
//     log += msg
//   }

//   def printError() = {
//     println(s"The number of error is ${log.size.toString()}.")
//     for (msg <- log) {
//       println(s"$errorType Error: $msg")
//     }
//   }

//   def exitStatus() : Int

//   def hasError: Boolean = log.nonEmpty

//   def resetError() = {
//     log.clear()
//   }

  case class SemanticError(msg: String)
}

object syntaxError extends Error {
  override val errorType = "Syntax"
  override def exitStatus() : Int = 100
}

// object SemanticError extends Error {

//     case class SemanticError()
//   override val errorType = "Semantic"
//   override def exitStatus() : Int = 200
// }


