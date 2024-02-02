package wacc

import parsley.{Success, Failure}
import scala.io.StdIn
import lexer._
import parser._

object Main {
    def main(args: Array[String]): Unit = {
        println("hello WACC!")

        args.headOption match {
            case Some(expr) => parser.parse(expr) match {
                case Success(x) => println(s"$expr = $x")
                case Failure(msg) => println(msg)
            }
            case None => println("please enter an expression")
                val input = StdIn.readLine()
                parser.parse(input) match {
                    case Success(result) => println(s"$input = $result")
                    case Failure(msg) => println(msg)
                }
                
        }

    }
}
