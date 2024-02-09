package wacc

import org.scalatest.funsuite.AnyFunSuite
import parsley.Failure
import parsley.Success
import java.io._
import scala.util._
import Errors._

class InvalidSemantic extends AnyFunSuite {
  import TestSemantics.testSemantics
  val validDir = new File("invalid/semanticErr/")

  assert(validDir.isDirectory)

  val subDirs = validDir.listFiles()

  for(subDir <- subDirs) {
    println ("Checking invalid files in \"" + subDir.getPath().substring("invalid/semanticErr/".length()) + "\"") 
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
    Errors.setFilename(f)
    parser.parse(f.getPath()) match {
      case Success(prog) => Semantic.semanticAnalysis(prog) match {
        case Right(_) => "success"
        case Left(value) => {
          println("Error detected during compilation! Exit code 200 returned.")
          for(semanticError <- value) 
            semanticError.printErrorMessage()
          return "failure"
        }
      }
      case Failure(msg) => "PARSER FAIL"

    }
  }
}