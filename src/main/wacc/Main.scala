package wacc

import parsley.{Success, Failure}
import scala.io.StdIn
import scala.io.Source
import lexer._
import parser._
// import semantic._

object Main {
    def main(args: Array[String]): Unit = {
        println("hello WACC!")

        args.headOption match {
            case Some(prog) =>

                parser.parse(prog) match {
                case Success(x) => 
                    println(s"$prog = $x")
                    // semantic.checkSemanticStmt(x, error = semanticError) match {
                    //     case Success(x) => 
                    //         println(s"$prog = $x")
                    //     case Failure(x) =>
                    //         // println(msg)
                    //         println("Exiting with code 200 due to semantic error")
                    //         System.exit(200)
                    // }
                case Failure(msg) =>
                    println(msg)
                    println("Exiting with code 100 due to syntax error")
                    System.exit(100)
            }

            case None => println("please enter a program")
                val input = StdIn.readLine()
                parser.parse(input) match {
                    case Success(result) =>
                        println(s"$input = $result")
                    case Failure(msg) =>
                        println("Exiting with code 100 due to syntax error")
                        System.exit(100)
                }

        }

    }
}
