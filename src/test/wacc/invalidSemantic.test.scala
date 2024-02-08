package wacc

import org.scalatest.funsuite.AnyFunSuite
import parsley.Failure
import parsley.Success
import java.io._
import scala.util._
import GetOutput._

class InvalidSemantic extends AnyFunSuite {
  import TestSemantics.testSemantics
  val validDir = new File("invalid/semanticErr/")

  assert(validDir.isDirectory)

  val subDirs = validDir.listFiles()

  for(subDir <- subDirs) {
    println ("Cheking invalid files in \"" + subDir.getPath().substring("invalid/semanticErr/".length()) + "\"") 
    for(validFile <- subDir.listFiles()) {
      
     
      if (validFile.isFile()) {
         // Files

        test ("Testing: "  + validFile.getPath().substring("invalid/semanticErr/".length())){
          assert(validFile.isFile())
          assert(testSemantics(validFile) == "failure")
        }
      } else {
        // Sub-directories

        for(subValidFile <- validFile.listFiles())
          test ("Testing: "  + subValidFile.getPath().substring("invalid/semanticErr/".length())) {
            assert(subValidFile.isFile())
            assert(testSemantics(subValidFile) == "failure")
          }
      }
    }

  }
}

object TestSemantics {
  def testSemantics(f: File) : String = {
    parser.parse(f.getPath()) match {
      case Success(prog) => Semantic.semanticAnalysis(prog) match {
        case Right(_) => "success"
        case Left(value) => {
          for(SemanticError(msg) <- value) 
            println(msg)
          return "failure"
        }
      }
      case Failure(msg) => "PARSER FAIL" 

    }
  }
}