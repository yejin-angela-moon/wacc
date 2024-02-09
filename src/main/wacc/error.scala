package wacc
import scala.collection.mutable.ListBuffer
import parsley.errors.ErrorBuilder
import ast.Ident
import ast.Type
import java.io.File


//NEED get the file, find the corresponding code, get the line, get the index of the line

object Errors {
  var file: File = _
  var filename: String = _

  def setFilename(f: File): Unit = {
    file = f
    filename = f.getName
  }

trait Error {   
  val errorType : String

  val log: String

  val exitStatus : Int

  val line: Int

  val column: Int

 // val errorDescription: String

  def hasError: Boolean = log != ""
    
  def resetError(): Unit = ???

  def printErrorMessage() = {
     // println("Errors detected during compilation! Exit code " + exitStatus + " returned.")
      println(s"$errorType in $filename ($line, $column)")
      println("  " + log)
    
  }
 
}

case class SyntaxError(msg: String) extends Error {
    override val errorType: String = "Syntax Error"
    override val log = msg

    override val line: Int = 0
    override val column: Int = 0
   /// override val line: Int = 0
   /// override val column: Int = 1

    override val exitStatus: Int = 100
  
}

class SemanticError() extends Error {
    override val errorType = "Semantic Error"
    override val exitStatus: Int = 200
    override val line: Int = 0
    override val column: Int = 0

    override val log: String = "Semantic Error found"
    
  }

  case class TypeError(description: String, expected: Set[Type], found: Set[Type], pos: (Int, Int)) extends SemanticError {
    override val errorType = "Type Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    val expectedTypes = expected.mkString(" | ") 
    val foundTypes = found.mkString(" , ") 
    override val log: String = s"$description type mismatch\n  Expected: $expectedTypes\n  Found: $foundTypes"
  }

  case class TypeDifferentError(description: String, types: Set[Type], pos: (Int, Int)) extends SemanticError {
    override val errorType = "Type Difference Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    val getType = types.mkString(" , ") 
    override val log: String = s"$description type different\n  Expected the same type\n  Found: $getType"
  }

  case class UndefinedFunctionError(ident: Ident, pos: (Int, Int)) extends SemanticError {
    override val errorType = "Undefined Function Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    override val log = s"The function ${ident.x} is undefined"
  }

  case class UndeclaredIdentifierError(ident: String, pos: (Int, Int)) extends SemanticError {
    override val errorType = "Undeclared Identifier Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    override val log = s"The identifier $ident is undeclared"
  }

  case class RedefinedFunctionError(func: String, pos: (Int, Int)) extends SemanticError {
    override val errorType = "Redefined Function Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    override val log = s"Illegal redefinition of function $func"
  }

  case class RedeclaredVariableError(variable: String, pos: (Int, Int))  extends SemanticError {

    override val errorType = "Redeclared Variable Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    override val log = s"Illegal redefinition of variable $variable"
  }

  case class IllegalUsedFunctionOnNonPairTypeError(func: String, pos: (Int, Int)) extends SemanticError {
    override val errorType = "Illegal Use of Function On Non-PairType Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
   // override val log = s"Illgal use of $func on $applyOn, can only be used on PairType"
    override val log = s"Illgal use of $func, can only be used on PairType"
  }

 
  case class NumOfArgumentsError(ident: Ident, expected: Int, found: Int, pos: (Int, Int)) extends SemanticError {
    override val errorType = "Number Of Arguments Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    override val log =  s"Incorrect number of arguments in call to ${ident.x}\n  Expected: $expected\n  Found: $found"
  
  }

  case class TypeInferenceError(ident: Ident, pos: (Int, Int)) extends SemanticError {
    override val errorType = "Type Inference Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    override val log: String = s"Unable to determine the correct type of ${ident.x}"
  }

  case class ScopeError(place: String, pos: (Int, Int)) extends SemanticError {
    override val errorType = "Scope Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    override val log = s"Return from $place is not allowed"
  }

  case class ArrayOutOfBoundsError(ident: Ident, max: Int, found: Int, pos: (Int, Int)) extends SemanticError {
    override val errorType = "Array Out Of Bounds Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    override val log =  s"Array ${ident.x} out of bounds\n  Maximum: $max\n  Found: $found"
  }

  case class MultipleTypesInArrayError(pos: (Int, Int)) extends SemanticError {
    override val errorType = "Multiple Types In Array Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    override val log = "Array elements must be of the same type"
  }

  case class ArrayDimensionalError(length: Int, pos: (Int, Int)) extends SemanticError {
    override val errorType = "Array Dimensional Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    override val log = s"Unexpected at least $length-dimensional array"
  }

  //case class ArrayIndiceError() extends SemanticError {
   // override val errorType = "Array Indices Error"
  //  override val log = "Array indices must be integers"
  //}

  case class ArrayTypeError(ident: String, pos: (Int, Int)) extends SemanticError {
    override val errorType = "Array Type Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    override val log = s"$ident is not an array type"
  }

  case class UndefinedError(pos: (Int, Int)) extends SemanticError {
    override val errorType = "Undefined Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    override val log = "UNDEFINED"
  }

  case class CastingError(strong: Type, weak: Type, pos: (Int, Int)) extends SemanticError {
    override val errorType = "Casting Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    override val log = s"Tried assigning stronger $strong value to weaker $weak"
  }

  case class FreeingError(pos: (Int, Int)) extends SemanticError {
    override val errorType = "Freeing Error"
    override val line: Int = pos._1
    override val column: Int = pos._2
    override val log = "Attempt to free non-dynamically allocated memory"
  }

}






