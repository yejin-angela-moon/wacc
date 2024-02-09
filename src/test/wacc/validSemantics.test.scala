package wacc

import org.scalatest.funsuite.AnyFunSuite
import parsley.Failure
import parsley.Success
import java.io._
import scala.util._

class ValidSemantic extends AnyFunSuite {
    import TestSemantics.testSemantics
    val validDir = new File("valid/")

    assert(validDir.isDirectory)

    val subDirs = validDir.listFiles()

  for(subDir <- subDirs) {
    println ("Cheking valid files in \"" + subDir.getPath().substring("valid/".length()) + "\"") 
    for(validFile <- subDir.listFiles()) {
      
     
      if (validFile.isFile()) {
         // Files
            test ("Testing: "  + validFile.getPath().substring("valid/".length())){
            assert(validFile.isFile())
            assert(testSemantics(validFile) == "success")
            }

      } else {
        // Sub-directories

        for(subValidFile <- validFile.listFiles()) {

          test ("Testing: "  + subValidFile.getPath().substring("valid/".length())) {
            assert(subValidFile.isFile())
            assert(testSemantics(subValidFile) == "success")
          
      }

            }
        }
    }


  }
}