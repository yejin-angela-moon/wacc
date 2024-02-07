package wacc

import scala.collection.mutable.{HashMap}
import parsley.{Result, Success, Failure}
import ast._
import java.lang.foreign.MemorySegment.Scope
import scala.collection.immutable.ListMap
import Error.SemanticError

object Semantic {
    var symbolTable : ListMap[String, Type] = ListMap.empty

    def stmtCheck(prog: Stmt, scopeLevel: String) :
        Either[List[SemanticError], Unit] = { prog match {
            case Func(t, ident, list, body) =>
                var updateScopeLevel = s"$ident.x-$scopeLevel"
                symbolTable + (updateScopeLevel -> t)
                funcEval(Func(t, ident, list, body), symbolTable, updateScopeLevel)
            case Declare(t, ident, rvalue) =>
                var scopeVarible = s"$ident.x-$scopeLevel"

                symbolTable.get(scopeVarible) match {
                    case Some(_) => Left(List(SemanticError(s"Variable $ident already declared.")))
                    case _ => symbolTable += (scopeVarible -> t)
                }

                //check the type of the rvalue is the same as t
                //check if the rvalue is in the same scope
                Right(())
            case Assign(lvalue, rvalue) =>
                val res1 = checkLvalue(lvalue, scopeLevel)
                val res2 = checkRvalue(rvalue, scopeLevel)

                (res1, res2) match {
                    case (Right(t1), Right(t2)) =>
                        if (t1 == t2) {
                            Right(())
                        } else {
                            Left(List(SemanticError("Type mismatch")))
                        }
                    case (Left(errors1), Right(_)) => Left(errors1)
                    case (Right(_), Left(errors2)) => Left(errors2)
                    case (Left(errors1), Left(errors2)) => Left(errors1 ++ errors2)
                }


            case IfThenElse(x, s1, s2) =>
                //check if x is boolean
                //would return errros if not bool

                val res1 = stmtCheck(s1, scopeLevel + "-i")
                val res2 = stmtCheck(s2, scopeLevel + "-e")

                (res1, res2) match {
                    case (Right(_), Right(_)) => Right(())
                    case (Left(errors1), Right(_)) => Left(errors1)
                    case (Right(_), Left(errors2)) => Left(errors2)
                    case (Left(errors1), Left(errors2)) => Left(errors1 ++ errors2)
                }

            case WhileDo(x, s) =>
                //check if x in boolean
                stmtCheck(s, scopeLevel + "-w") match {
                    case Left(errors) => Left(errors)
                    case Right(_) => Right(())
                }

            case BeginEnd(s) =>
                stmtCheck(s, scopeLevel + "-p") match {
                    case Left(errors) => Left(errors)
                    case Right(_) => Right(())
                }
            case StmtList(s1, s2) =>
                //check if the

                val res1 = stmtCheck(s1, scopeLevel)
                val res2 = stmtCheck(s2, scopeLevel)

                (res1, res2) match {
                    case (Right(_), Right(_)) => Right(())
                    case (Left(errors1), Right(_)) => Left(errors1)
                    case (Right(_), Left(errors2)) => Left(errors2)
                    case (Left(errors1), Left(errors2)) => Left(errors1 ++ errors2)
                }

            case Skip => Right(())
            case Read(lvalue) =>Right(())
                //check the type of the lvalue is ...
            case Free(x) =>Right(())
                //check the type of x
            case Return(x) =>Right(())
                //check the type of x
            case Exit(x) =>Right(())
                //check the type of x
            case Print(x) => Right(())//checl the type of x
            case Println(x) => Right(())//check the type of x
        }
    }
5
    def funcEval(func: Func, symbolTable: Map[String, Type], scopeLevel: String) = {
        val paramList = func.list
        val updatedSymbolTable = symbolTable ++ paramList.params.map(param => param.ident -> param.t).toMap
        stmtCheck(func.body, scopeLevel + func.ident.x)
    }

    def checkLvalue(l : Lvalue, scopeLevel : String) : Either[List[SemanticError], Type] = {
        l match{
            case Ident(x) =>
                var scopeVar = x + "-" + scopeLevel
                while (scopeVar.contains("")) {
                    if (symbolTable.contains(scopeVar)) {
                        Right(symbolTable.get(scopeVar))
                    }
                    scopeVar = scopeVar.substring(0, scopeVar.lastIndexOf("-"))
                }
                Left(List(SemanticError("Variable out of scope")))
            case ArrayElem(ident, x) =>
                //check the if x is in range and type is int

                var scopeVar = ident.x + "-" + scopeLevel
                while (scopeVar.contains("")) {
                    if (symbolTable.contains(scopeVar)) {
                        Right(symbolTable.get(scopeVar))
                    }
                    scopeVar = scopeVar.substring(0, scopeVar.lastIndexOf("-"))
                }
                Left(List(SemanticError("Variable out of scope")))
    }}

    def checkRvalue(r : Rvalue, scopeLevel: String) : Either[List[SemanticError], Type] = {
        r match{
            case ArrayLit(x) => //find in the table
            case NewPair(x1, x2) => //check the 
            case Call(i, x) => //check with table again
        }
    }
}
