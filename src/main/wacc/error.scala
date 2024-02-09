package wacc
import scala.collection.mutable.ListBuffer
import parsley.errors.ErrorBuilder
import ast.Ident
import ast.Type


trait Error {
  val errorType : String

  val log: String

  val exitStatus : Int

  //val startLine: Int

  //val endLine: Int

 // val errorDescription: String

  def hasError: Boolean = log != ""
    
  def resetError(): Unit = ???

  def printErrorMessage() = {
     // println("Errors detected during compilation! Exit code " + exitStatus + " returned.")
      println(errorType + " Error: ")// in (" + startLine + ", " + endLine + ")")
      println(log)
    
  }

}

case class SyntaxError(msg: String) extends Error {
    override val errorType: String = "Syntax"
    override val log = msg
   /// override val startLine: Int = 0
   /// override val endLine: Int = 1

    override val exitStatus: Int = 100
  
}

class SemanticError() extends Error {
    override val errorType = "Semantic"
    override val exitStatus: Int = 200

    override val log: String = "Semantic Error found"
    
  }

  case class TypeError(description: String, expected: Set[Type], found: Type) extends SemanticError {
    val expectedTypes = expected.mkString(" | ") 
    override val log: String = s"$description type mismatch\nExpected: $expectedTypes, found: $found"
  }

  case class TypeDifferentError(description: String, types: Set[Type]) extends SemanticError {
    val getType = types.mkString(" , ") 
    override val log: String = s"$description type different\nExpected the same type, found: $getType"
  }

  case class UndefinedFunctionError(ident: Ident) extends SemanticError {
    override val log = s"Undefined function ${ident.x}"
  }

  case class UndeclaredIdentifierError(ident: String)  extends SemanticError {
    override val log = s"Undeclared identifier $ident"
  }

  case class RedefinedFunctionError(func: String) extends SemanticError {
    override val log = s"Illegal redefinition of function $func"
  }

  case class RedeclaredVariableError(variable: String)  extends SemanticError {
    override val log = s"Illegal redefinition of variable $variable"
  }

  case class IllegalUsedFunctionOnNonPairTypeError(func: String) extends SemanticError {
   // override val log = s"Illgal use of $func on $applyOn, can only be used on PairType"
     override val log = s"Illgal use of $func, can only be used on PairType"
  }

 
  case class NumOfArgumentsError(ident: Ident, expected: Int, found: Int) extends SemanticError {
    override val log =  s"Incorrect number of arguments in call to ${ident.x}\nExpected: $expected, found: $found"
  
  }

  case class TypeInferenceError(ident: Ident) extends SemanticError {
    override val log: String = s"Unable to determine the correct type of ${ident.x}"
  }

  case class ScopeError(place: String) extends SemanticError {
    override val log = s"Return from $place is not allowed"
  }

  case class ArrayOutOfBoundsError(ident: Ident, max: Int, found: Int) extends SemanticError {
    override val log =  s"Array ${ident.x} out of bounds\nMaximum: $max, found: $found"
  }

  case class MultipleTypesInArrayError() extends SemanticError {
    override val log = "Array elements must be of the same type"
  }

  case class ArrayDimensionalError(length: Int) extends SemanticError {
    override val log = s"Unexpected at least $length-dimensional array"
  }

  case class ArrayIndiceError() extends SemanticError {
    override val log = "Array indices must be integers"
  }

  case class ArrayTypeError(ident: String) extends SemanticError {
    override val log = s"$ident is not an array type"
  }

  case class UndefinedError() extends SemanticError {
    override val log = "UNDEFINED"
  }

  case class CastingError(strong: Type, weak: Type) extends SemanticError {
    override val log = s"Tried assigning stronger $strong value to weaker $weak"
  }

  case class FreeingError() extends SemanticError {
    override val log = "Attempt to free non-dynamically allocated memory"
  }








