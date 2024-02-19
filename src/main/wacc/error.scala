package wacc
import scala.collection.mutable.ListBuffer
import parsley.errors.ErrorBuilder
import ast.Ident
import ast.Type
import java.io.File
import scala.util._
import parsley.Failure
import parsley.Success
import scala.io._
import scala.util._
import Errors._

// Define the Errors object 
object Errors {
    var file: File = _
    var filename: String = _
   
    // Function to set the filename
    def setFilename(f: File): Unit = {
      file = f
      filename = f.getName
    }
    
    // Define the common structure for all errors
    trait Error {
        val errorType: String // Type of the error
        val log: String // Log message describing the error
        val exitStatus: Int // Exit status associated with the error
        val line: Int // Line number where the error occurred
        val column: Int // Column number where the error occurred
    
        // Function to print the error message with the lines that the error occurred
        def printErrorMessage() = {
            println("  " + log)
            printSurroundingLines(line)
        }
    
        // Function to print the surrounding lines of the error occurred
        def printSurroundingLines(lineNum: Int): Unit = {
            val source = Source.fromFile(file)
            try {
                val lines = source.getLines().toSeq
                val startLine = Math.max(1, lineNum - 1)
                val endLine = Math.min(lines.length, lineNum + 1)
                for (i <- startLine to endLine) {
                    println(s"|${lines(i - 1)}")
                    if (i == lineNum) {
                        var space = ""
                        var j = 0
                        while (j <= column) {
                           space += " "
                           j += 1
                        }
                        println(s"|$space^")
                    }
                }
            } finally {
                source.close()
            }
        }
    }
        
    
    // Syntax errors
    case class SyntaxError(msg: String) extends Error{
        override val errorType: String = "Syntax Error"
        override val log: String = msg
        val line: Int = 0
        val column: Int = 0
        override val exitStatus: Int = 100
    }

    // Semantic errors
    trait SemanticError extends Error {
        override val errorType: String = "Semantic Error"
        override val log: String = "semantic"
        override val exitStatus: Int = 200
    }

    // Define type error 
    case class TypeError(description: String, expected: Set[Type], found: Set[Type], pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Type Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        val expectedTypes = expected.mkString(" | ") 
        val foundTypes = found.mkString(" , ") 
        override val log: String = s"$description type mismatch\n  Expected: $expectedTypes\n  Found: $foundTypes"
    }
    
    // Define type different error 
    case class TypeDifferentError(description: String, types: Set[Type], pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Type Difference Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        val getType = types.mkString(" , ") 
        override val log: String = s"$description type different\n  Expected the same type\n  Found: $getType"
    }
    
    // Define undefined function error 
    case class UndefinedFunctionError(ident: String, pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Undefined Function Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        override val log = s"The function $ident is undefined"
    }
    
    // Define undeclared identifier error 
    case class UndeclaredIdentifierError(ident: String, pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Undeclared Identifier Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        override val log = s"The identifier $ident is undeclared"
    }
    
    // Define redefined function error 
    case class RedefinedFunctionError(func: String, pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Redefined Function Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        override val log = s"Illegal redefinition of function $func"
    }
    
    // Define redeclared variable error 
    case class RedeclaredVariableError(variable: String, pos: (Int, Int))  extends SemanticError {
        override val errorType = "[Semantic] Redeclared Variable Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        override val log = s"Illegal redefinition of variable $variable"
    }
    
    // Define illegal used function on non pair type error 
    case class IllegalUsedFunctionOnNonPairTypeError(func: String, pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Illegal Use of Function On Non-PairType Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        override val log = s"Illgal use of $func, can only be used on PairType"
    }
    
    // Define num of arguments error 
    case class NumOfArgumentsError(ident: Ident, expected: Int, found: Int, pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Number Of Arguments Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        override val log =  s"Incorrect number of arguments in call to ${ident.x}\n  Expected: $expected\n  Found: $found"
      
    }
    
    // Define type inference error 
    case class TypeInferenceError(ident: Ident, pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Type Inference Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        override val log: String = s"Unable to determine the correct type of ${ident.x}"
    }

    // Define scope error 
    case class ScopeError(place: String, pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Scope Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        override val log = s"Return from $place is not allowed"
    }
    
    // Define array out of bounds error 
    case class ArrayOutOfBoundsError(ident: Ident, max: Int, found: Int, pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Array Out Of Bounds Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        override val log =  s"Array ${ident.x} out of bounds\n  Maximum: $max\n  Found: $found"
    }
    
    // Define multiple types in array error 
    case class MultipleTypesInArrayError(pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Multiple Types In Array Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        override val log = "Array elements must be of the same type"
    }
    
    // Define array dimensional error 
    case class ArrayDimensionalError(length: Int, pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Array Dimensional Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        override val log = s"Unexpected at least $length-dimensional array"
    }

    // Define array type error 
    case class ArrayTypeError(ident: String, pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Array Type Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        override val log = s"$ident is not an array type"
    }
    
    // Define undefined error 
    case class UndefinedError(pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Undefined Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        override val log = "UNDEFINED"
    }
    
    // Define casting error 
    case class CastingError(strong: Type, weak: Type, pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Casting Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        override val log = s"Tried assigning stronger $strong value to weaker $weak"
    }
    
    // Define freeing error 
    case class FreeingError(pos: (Int, Int)) extends SemanticError {
        override val errorType = "[Semantic] Freeing Error"
        override val line: Int = pos._1
        override val column: Int = pos._2
        override val log = "Attempt to free non-dynamically allocated memory"
    }
}
