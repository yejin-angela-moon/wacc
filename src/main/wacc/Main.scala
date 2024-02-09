package wacc

import parsley.{Success, Failure}
import scala.io.StdIn
import scala.io.Source
import lexer._
import parser._
import Semantic._
import Errors._

object Main {
    /* The main program for WACC compiler. Takes in a file written in WACC language and performs
       lexical and semantic analysis on it. If any errors are found, print the errors and exit with
       code 100 for syntax error and 200 for semantic error. */
    def main(args: Array[String]): Unit = {
        println("hello WACC!")

        args.headOption match {
            case Some(prog) => {
                /* First parse the program into a statement. */
                parser.parse(prog) match {
                case Success(x) => 
                    println(s"$prog = $x")
                    /* Run the semantic checker on the statement, if parsed successfully without syntax error. */
                    Semantic.semanticAnalysis(x) match {
                        case Right(_) => 
                            println(s"$prog is semantically valid")
                        case Left(list) => {
                            for(semanticError <- list)
                                semanticError.printErrorMessage()
                            System.exit(200)
                        }   
                    }
                /* Exit with code 100 if parsing failed due to syntax error. */
                case Failure(msg) =>
                    println(msg)
                    println("Exiting with code 100 due to syntax error")
                    System.exit(100)
                }
            }
            case None => 
                println("No file found")
                System.exit(0)
        }
    }
}
