package wacc

import scala.collection.mutable.{HashMap}
import parsley.{Result, Success, Failure}
import ast._
// import java.lang.foreign.MemorySegment.Scope
import SemanticError._
import Semantic._
import TypeCheck._

object Semantic {
    var symbolTable = new HashMap[String, Type]

    def getValueFromTable(key: String): Either[List[SemanticError], Type] = {
        var scopeVar = key
        while (scopeVar.contains("-")) {
            symbolTable.get(scopeVar) match {
                case Some(value) => {
                    println("Found " + value + " with table name " + scopeVar)
                    return Right(value)
                }
                case None => scopeVar = scopeVar.substring(0, scopeVar.lastIndexOf("-"))
            }
        }
        Left(List(SemanticError(s"unexpected identifier $key")))
    }


    def stmtCheck(prog: Stmt, scopeLevel: String) :
        Either[List[SemanticError], Unit] = { 
            println("----------------------------")
            println("Checking statement:\n" + prog)
        prog match {
            case Declare(t, ident, rvalue) =>
                var scopeVarible = s"${ident.x}-$scopeLevel"

                symbolTable.get(scopeVarible) match {
                    case Some(_) => Left(List(SemanticError(s"Variable $ident already declared.")))

                    case None =>
                        checkRvalue(rvalue, scopeLevel) match {
                            case Right(value) if value == t =>
                                symbolTable += (scopeVarible -> t)
                                Right(())

                            case Right(badType) => Left(List(SemanticError("Type mismatch\nExpected: " + t + " Actual: " + badType)))

                            case Left(value) => Left(value)
                        }
                }

            case Assign(lvalue, rvalue) =>
                val lResult = checkLvalue(lvalue, scopeLevel)
                val rResult = checkRvalue(rvalue, scopeLevel)

                (lResult, rResult) match {
                    case (Right(t1), Right(t2)) =>
                    if (t1 == t2) Right(())
                    else Left(List(SemanticError("Type mismatch 22 ")))

                    case (Left(errors1), Left(errors2)) =>
                    Left(errors1 ++ errors2)

                    case (Left(errors), Right(_)) =>
                    Left(errors)

                    case (Right(_), Left(errors)) =>
                    Left(errors)
                }

            case IfThenElse(x, s1, s2) =>
                findType(x, scopeLevel) match {
                    case Left(error) =>
                        Left(error)

                    case Right(t) if t != BoolType =>
                        Left(List(SemanticError("Condition in IfThenElse must be of type Boolean.")))

                    case Right(_) =>

                        val res1 = stmtCheck(s1, scopeLevel + "-i")
                        val res2 = stmtCheck(s2, scopeLevel + "-e")

                        (res1, res2) match {
                            case (Right(_), Right(_)) => Right(())
                            case (Left(errors1), Right(_)) => Left(errors1)
                            case (Right(_), Left(errors2)) => Left(errors2)
                            case (Left(errors1), Left(errors2)) => Left(errors1 ++ errors2)
                    }
                }

            case WhileDo(x, s) =>

                findType(x, scopeLevel) match {
                    case Left(error) =>
                        Left(error)

                    case Right(t) if t != BoolType =>
                        Left((List(SemanticError("Type mismatch 33"))))

                    case Right(_) =>
                        stmtCheck(s, scopeLevel + "-w")
                }

            case BeginEnd(s) =>
                stmtCheck(s, scopeLevel + "-b")
            case StmtList(s1, s2) =>
                val res1 = stmtCheck(s1, scopeLevel)
                val res2 = stmtCheck(s2, scopeLevel)

                (res1, res2) match {
                    case (Right(_), Right(_)) => Right(())
                    case (Left(errors1), Left(errors2)) => Left(errors1 ++ errors2)
                    case (Left(errors1), Right(_)) => Left(errors1)
                    case (Right(_), Left(errors2)) => Left(errors2)
                }

            case Skip => Right(())
            case Read(lvalue) => checkLvalue(lvalue, scopeLevel) match {
                case Right(t) if t == IntType || t == CharType => Right(())
                case Right(_) => Left(List(SemanticError("Read expects an Int or Char type")))
                case Left(value) => Left(value)
            }
            case Return(x) => (findType(x, scopeLevel), getCurrentFunctionReturnType(scopeLevel)) match {
                case (Right(t1), Right(t2)) if typeMatch(t1, t2) => Right(())
                case (Right(t1), Right(t2)) => Left(List(SemanticError("Return type mismatch")))
                case (Left(e1), Left(e2)) => Left(e1 ++ e2)
                case (_, Left(e)) => Left(e)
                case (Left(e), _) => Left(e)
            }
            case Exit(x) => findType(x, scopeLevel) match {
                case Right(IntType) => Right(())
                case Right(_) => Left(List(SemanticError("Exit expects an Int type")))
                case Left(errors) => Left(errors)
            }
            case Print(x) => findType(x, scopeLevel) match {
                case Right(_) => Right(())
                case Left(errors) => Left(errors)
            }
            case Println(x) => findType(x, scopeLevel) match {
                case Right(_) => Right(())
                case Left(errors) => Left(errors)
            }
            case Free(x) => findType(x, scopeLevel) match {
                case Right(_) => Right(())
                case Left(errors) => Left(errors)
            }
        }
    }

    def checkLvalue(l: Lvalue, scopeLevel: String): Either[List[SemanticError], Type] = {
        l match {
            case Ident(x) =>
                findType(Ident(x), scopeLevel)
            case ArrayElem(ident, exprList) =>
                findType(ArrayElem(ident, exprList), scopeLevel)
            case Fst(lvalue) => checkLvalue(lvalue, scopeLevel)
            case Snd(lvalue) => checkLvalue(lvalue, scopeLevel)
            }
        }


    def checkRvalue(r : Rvalue, scopeLevel: String) : Either[List[SemanticError], Type] = {
        r match{
            case ArrayLit(exprList) =>
                checkArrayList(exprList.getOrElse(Nil), scopeLevel)
            case NewPair(x1, x2) =>

                for {
                    type1 <- findType(x1, scopeLevel) // Find type of x1
                    type2 <- findType(x2, scopeLevel) // Find type of x2
                    pairElemType1 = toPairElemType(type1) // Convert to PairElemType if necessary
                    pairElemType2 = toPairElemType(type2) // Convert to PairElemType if necessary
                } yield PairType(pairElemType1, pairElemType2)

            case Call(ident, x) => getValueFromTable(ident.x + "-" + scopeLevel)
            case Fst(lvalue) => checkLvalue(lvalue, scopeLevel)
            case Snd(lvalue) => checkLvalue(lvalue, scopeLevel)
            case e => findType(e.asInstanceOf[Expr], scopeLevel)
        }
    }

    def toPairElemType(t: Type): PairElemType = t match {
        case pt: PairElemType => pt 
        case _ => PairElemType2(t)
    }

    def checkFuncsList(funcs: List[Func]): Either[List[SemanticError], Unit] = {
        funcs.foldLeft[Either[List[SemanticError], Unit]](Right(())) { (acc, func) =>
            acc.flatMap { _ =>
                getValueFromTable(func.ident.x) match {
                    case Right(_) =>
                        Left(List(SemanticError(s"Function ${func.ident.x} is already defined.")))
                    case Left(_) =>
                        symbolTable.addOne(func.ident.x -> func.t)
                        func.list.params.foreach {
                            param =>
                                val paramNameWithScope = s"${param.ident.x}-${func.ident.x}"
                                symbolTable.addOne(paramNameWithScope -> param.t)
                        }
                        stmtCheck(func.body, func.ident.x)
                }
            }
        }
    }

    def getCurrentFunctionReturnType(scopeLevel: String): Either[List[SemanticError], Type] = {
        symbolTable.get(scopeLevel) match {
            case Some(returnType) => Right(returnType)
            case None => Left(List(SemanticError("return outside of function is not allowed")))
        }
    }


    def semanticAnalysis(prog: Stmt) : Either[List[SemanticError], Unit] = {
        prog match {
            case Program(funcs, body) =>
                (checkFuncsList(funcs), stmtCheck(body, "")) match {
                    case (Right(_), Right(_)) => Right(())
                    case (Left(err1), Left(err2)) => Left(err1 ++ err2)
                    case (Left(errs), _) => Left(errs)
                    case (_, Left(errs)) => Left(errs)
                }
            case _ => Left(List(SemanticError("Program expected")))
        }
    }
}

object TypeCheck {
    private def checkBinaryOp(x: Expr, y: Expr, expectedType: Set[Type],
                            expectedOutputType: Type, scopeLevel: String) :
                                Either[List[SemanticError], Type] = {
        (findType(x, scopeLevel), findType(y, scopeLevel)) match {
            case (Right(t1), Right(t2)) if expectedType(t1) && expectedType(t2) =>
                Right(expectedOutputType)
            case ((Right(t1), Right(t2)) ) => Left(List(SemanticError(s"Type mismatch for binary operation $t1 and $t2")))
            case _ => Left(List(SemanticError("Type mismatch for binary operation")))
        }
    }

    private def checkUnaryOp(x: Expr, expectedType: Set[Type],
                            expectedOutputType: Type, scopeLevel: String) :
                                Either[List[SemanticError], Type] = {
        (findType(x, scopeLevel)) match {
            case Right(t1) => t1 match {
                case ArrayType(t) => Right(ArrayType(t))
                case PairType(p1, p2) => Right(PairType(p1, p2))
                case _ if expectedType(t1) =>
                    Right(expectedOutputType)
                case _ => Left(List(SemanticError("Type mismatch for unary operation")))
            }
            case Left(err) => Left(err)
        }
    }

    def checkArrayList(arrayList: List[Expr], scopeLevel: String): Either[List[SemanticError], Type] = {
        arrayList match {
            case Nil => Left(List(SemanticError("Array cannot be empty")))
            case (head :: tail) =>  
                findType(head, scopeLevel) match {
                    case Right(value) => {
                        for(elem <- tail) {
                            findType(elem, scopeLevel) match {
                                case Right(value1) => if (value1 != value) 
                                    return Left(List(SemanticError("Array elements must be of the same type.")))
                                case Left(value1) => Left(value)
                            }
                        }
                        return Right(ArrayType(value))
                    }
                    case Left(value) => Left(value)
                }
        }
    }

    val everyType: Set[Type] = Set(PairType(AnyType, AnyType), ArrayType(AnyType), IntType, BoolType, CharType, StringType)
    // val everyArrayType: Set[Type] = Set()

    def findType(expr : Expr, scopeLevel : String) : Either[List[SemanticError], Type] = {
        expr match {
            case Add(x, y) => checkBinaryOp(x, y, Set(IntType), IntType, scopeLevel)
            case Sub(x, y) => checkBinaryOp(x, y, Set(IntType), IntType, scopeLevel)
            case Mul(x, y) => checkBinaryOp(x, y, Set(IntType), IntType, scopeLevel)
            case Div(x, y) => checkBinaryOp(x, y, Set(IntType), IntType, scopeLevel)
            case Mod(x, y) => checkBinaryOp(x, y, Set(IntType), IntType, scopeLevel)
            case And(x, y) => checkBinaryOp(x, y, Set(BoolType), BoolType, scopeLevel)
            case Or(x, y) => checkBinaryOp(x, y, Set(BoolType), BoolType, scopeLevel)
            case LT(x, y) => checkBinaryOp(x, y, Set(IntType), BoolType, scopeLevel)
            case LTE(x, y) => checkBinaryOp(x, y, Set(IntType), BoolType, scopeLevel)
            case GT(x, y) => checkBinaryOp(x, y, Set(IntType), BoolType, scopeLevel)
            case GTE(x, y) => checkBinaryOp(x, y, Set(IntType), BoolType, scopeLevel)
            case E(x, y) => checkBinaryOp(x, y, everyType, BoolType, scopeLevel)
            case NE(x, y) => checkBinaryOp(x, y, everyType, BoolType, scopeLevel)
            case Not(x) => checkUnaryOp(x, Set(BoolType), BoolType, scopeLevel)
            case Neg(x) => checkUnaryOp(x, Set(IntType), IntType, scopeLevel)
            case Len(x) => checkUnaryOp(x, Set(ArrayType(AnyType), StringType), IntType, scopeLevel)
            case Ord(x) => checkUnaryOp(x, Set(CharType), IntType, scopeLevel)
            case Chr(x) => checkUnaryOp(x, Set(IntType), CharType, scopeLevel)
            case IntLit(x) => Right(IntType)
            case BoolLit(x) => Right(BoolType)
            case CharLit(x) => Right(CharType)
            case StrLit(x) => Right(StringType)
            case Ident(x) => getValueFromTable(x + "-" + scopeLevel)
            case ArrayElem(ident, exprList) =>
                getValueFromTable(ident.x + "-" + scopeLevel).flatMap {
                    case ArrayType(t) =>
                        val indexTypeChecks = exprList.map(findType(_, scopeLevel))
                        val indexErrors = indexTypeChecks.collect { case Left(errs) => errs }.flatten

                        if (indexErrors.nonEmpty) {
                            Left(indexErrors)
                        } else {
                            // Ensure all index types are IntType
                            val allIndicesAreInt = indexTypeChecks.forall {
                                case Right(IntType) => true
                                case _ => false
                            }

                            if (allIndicesAreInt) {
                                Right(t) // Return the type of the array element
                            } else {
                                Left(List(SemanticError("Array indices must be integers")))
                            }
                    }
                case _ => Left(List(SemanticError(s"${ident.x} is not an array type")))
                }
            case Paran(x) => findType(x, scopeLevel)
            case PairLit => Right(PairType(AnyType, AnyType))
        }
    }

    def checkTypes(e1: Expr, e2: Expr, scopeLevel: String) : Boolean = {
        (findType(e1, scopeLevel), findType(e2, scopeLevel)) match {
            case (Right(t1), Right(t2)) => typeMatch(t1, t2)
            case _ => false
        }
    }

    def typeMatch(t1: Type, t2: Type) : Boolean = {
        t1 == AnyType || t2 == AnyType || t1 == t2
    }
}
