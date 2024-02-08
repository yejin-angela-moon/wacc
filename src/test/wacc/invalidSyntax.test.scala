// package wacc

// import org.scalatest.funsuite.AnyFunSuite
// import parsley.Failure
// import parsley.Success
// import java.io._
// import scala.util._
// import GetOutput._

// class InvalidSyntax extends AnyFunSuite {
//   val validDir = new File("invalid/syntaxErr/")

//   assert(validDir.isDirectory)

//   val subDirs = validDir.listFiles()

//   for(subDir <- subDirs) {
//     println ("Cheking invalid files in \"" + subDir.getPath().substring("invalid/syntaxErr/".length()) + "\"") 
//     for(validFile <- subDir.listFiles()) {
      
     
//       if (validFile.isFile()) {
//          // Files

//         test ("Testing: "  + validFile.getPath().substring("invalid/syntaxErr/".length())){
//           assert(validFile.isFile())
//           assert(getOutput(validFile) == "failure")
//         }
//       } else {
//         // Sub-directories

//         for(subValidFile <- validFile.listFiles())
//           test ("Testing: "  + subValidFile.getPath().substring("invalid/syntaxErr/".length())) {
//             assert(subValidFile.isFile())
//             assert(getOutput(subValidFile) == "failure")
//           }
//       }
//     }

//   }

// }