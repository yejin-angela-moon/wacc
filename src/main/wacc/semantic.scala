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
    var functionTable = new HashMap[String, FuncInfo]

    case class FuncInfo(t: Type, params: ParamList)

    def getValueFromTable(key: String): Either[List[SemanticError], Type] = {
        @annotation.tailrec
        def search(scopeVar: String): Either[List[SemanticError], Type] = {
            // println(symbolTable)
            // println("adfd - " + scopeVar)
            symbolTable.get(scopeVar) match {
            case Some(value) => Right(value)
            case None =>
                val nextScopeVar = scopeVar.lastIndexOf("-") match {
                case -1 => "" // No more scopes to backtrack
                case index => scopeVar.substring(0, index)
                }
                if (nextScopeVar.isEmpty) Left(List(SemanticError(s"Unexpected identifier $key")))
                else search(nextScopeVar)
            }
        }


        search(key)
    }

    def getValueFromFuncTable(key: String, args: Option[ArgList]) : Either[List[SemanticError], Type] = {
        def search(key: String): Either[List[SemanticError], Type] = {
            functionTable.get(key) match {
                case Some(FuncInfo(t, ParamList(params))) => 
                    compareParamsArgs(params, args) match {
                        case Left(errors) => Left(errors)
                        case Right(_) => Right(t)
                    }
                case None => Left(List(SemanticError("Call to undefined function")))
            }
        }
        // val getFunctionName = key.split("-")
        // (getFunctionName.lengthCompare(1) < 0) match {
        //     case true => Left(List(SemanticError("return from main is not allowed")))
        //     case false => symbolTable.get(getFunctionName(1)) match {
        //         case Some(returnType) => search(getFunctionName(1))
        //         case None => Left(List(SemanticError("return outside of function is not allowed")))
        //     }
        // }
        search(key)
    }

    def compareParamsArgs(params: List[Param], maybeArgs: Option[ArgList]) : Either[List[SemanticError], Unit] = {
        def compareLengthAndType(params: List[Param], args: List[Expr]) : Either[List[SemanticError], Unit] = {
            if (params.length != args.length) {
                return Left(List(SemanticError("parameter number mismatch")))
            }
            println(s"Comparing $params and $args")
            params.zip(args).find { 
                case (param, arg) => 
                    val Right(argType) = findType(arg, "-g")
                    println(s"comparing ${param.t} and $argType")
                    param.t != argType
            } match {
                case Some(res) => 
                    println(res)
                    Left(List(SemanticError("parameter type mismatch")))
                case None => Right(()) 
            }
        }
        maybeArgs match {
            case Some(ArgList(args)) => compareLengthAndType(params, args)
            case None if params.isEmpty => Right(())
        }
    }


    def stmtCheck(prog: Stmt, scopeLevel: String) :
        Either[List[SemanticError], Unit] = {
            // println("----------------------------")
            // println("Checking statement:\n" + prog)
        prog match {
            case Declare(t, ident, rvalue) =>
                var scopeVar = s"${ident.x}$scopeLevel"

                symbolTable.get(scopeVar) match {
                    case Some(_) => 
                        Left(List(SemanticError(s"Variable ${ident.x} already declared.")))

                    case None =>
                        checkRvalue(rvalue, scopeLevel) match {
                            case Right(value) => {
                                if(t :> value){
                                    symbolTable.addOne(scopeVar -> t)
                                    // println("the hashtable = " + symbolTable)
                                    println(ident.x)
                                    Right(())}
                                else if (value :> t)
                                    // Left(List(SemanticError(s"Tried assigning stronger $value value to weaker $t")))
                                    Left(List(SemanticError("Tried assigning stronger to weaker")))
                                else
                                    // Left(List(SemanticError(s"Declare type mismatch\nExpected: $t Actual: $value")))
                                    Left(List(SemanticError("Declare type mismatch")))
                            }


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
                        Left((List(SemanticError("Type mismatch\nExpected: Boolean Actual: " + t))))

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
                case Right(ArrayType(_)) => Right(())
                case Right(PairType(_,_)) => Right(())
                case Left(errors) => Left(errors)
                case Right(_) => Left(List(SemanticError("Attempt to free non-dynamically allocated memory")))
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

            case Call(ident, list) => getValueFromFuncTable(ident.x, list)
            case Fst(lvalue) => checkLvalue(lvalue, scopeLevel) match {
                case Right(PairType(a, b)) => Right(fromPairElemType(a))
                case _ => Left(List(SemanticError("Can only use fst on pair type"))) 
            }
            case Snd(lvalue) => checkLvalue(lvalue, scopeLevel) match {
                case Right(PairType(a, b)) => Right(fromPairElemType(b))
                case _ => Left(List(SemanticError("Can only use snd on pair type"))) 
            }
            case e => findType(e.asInstanceOf[Expr], scopeLevel)
        }
    }

    def toPairElemType(t: Type): PairElemType = t match {
        case PairElemType1 => PairElemType1
        case _ => PairElemType2(t)
    }

    def fromPairElemType(t: PairElemType) : Type = t match {
        case PairElemType2(a) => a 
        case PairElemType1    => PairElemType1

    }

    def checkFuncsList(funcs: List[Func]): Either[List[SemanticError], Unit] = {

        val errors = funcs.flatMap { f =>
        functionTable.get(f.ident.x) match {
            case Some(_) => 
                List(SemanticError(s"Function redefinition error of function ${f.ident.x}"))
            case None => 
                println(s"Adding function ${f.ident.x} to the function table")
                functionTable.addOne(f.ident.x -> FuncInfo(f.t, f.list))
                None
        }
    }
        if (errors.isEmpty) Right(())
        else (Left(errors))
    }

        // funcs.foreach(f =>
        //     functionTable.get(f.ident.x) match {
        //         case Some(_) => 
        //             Left(List(SemanticError(s"Function redefinition error: illegal redefinition of function ${f.ident.x}")))
        //         case None =>                 
        //             functionTable.addOne(f.ident.x -> FuncInfo(f.t, f.list))
        //             println(s"function ${f.ident.x} added")
        //             Right(())
        //     }
        // )

        // funcs.foldLeft[Either[List[SemanticError], Unit]](Right(())) { (acc, func) =>
        //     acc.flatMap { _ =>

        //         func.list.params.foreach {
        //             param =>
        //                 val paramNameWithScope = s"${func.ident.x}-${param.ident.x}"
        //                 println(paramNameWithScope)
        //                 symbolTable.addOne(paramNameWithScope -> param.t)
        //         }

        //         stmtCheck(func.body, "-" + func.ident.x + "-g")
        //     }
        // }

    def getCurrentFunctionReturnType(scopeLevel: String): Either[List[SemanticError], Type] = {
        val getFunctionName = scopeLevel.split("-")
        (getFunctionName.lengthCompare(1) < 0) match {
            case true => Left(List(SemanticError("return from main is not allowed")))
            case false => functionTable.get(getFunctionName(1)) match {
                case Some(FuncInfo(t, params)) => Right(t)
                case None => Left(List(SemanticError("return outside of function is not allowed")))
            }
        }

    }


    def semanticAnalysis(prog: Stmt) : Either[List[SemanticError], Unit] = {
        symbolTable.clear()
        functionTable.clear()

        prog match {
            case Program(funcs, body) =>
                (checkFuncsList(funcs), stmtCheck(body, "-g")) match {
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
            case (Right(t1), Right(t2)) => {
                    if(t1 :> t2 && t2 :> t1) {
                        if(expectedType.filter((x) => x :> t1).size >= 1)
                            return Right(expectedOutputType)

                        return Left(List(SemanticError("BinOp type mismatch\nExpected: " + expectedType + " Actual: " + t1)))
                    }
                    return Left(List(SemanticError("BinOp differnt types: " + t1 + " and " + t2)))
                }
            case (Left(a), Left(b)) => Left(a ++ b)
            case _ => Left(List(SemanticError("UNDEFINED")))
            }
        }

    // private def checkComparisonOp(x: Expr, y: Expr, scopeLevel: String) : Either[List[SemanticError], Type] = {
    //     (findType(x, scopeLevel), findType(y, scopeLevel)) match {
    //         case (Right(t1), Right(t2)) => (t1, t2) match {
    //             case (CharType, CharType) => Right(CharType)
    //             case (IntType, IntType) => Right(IntType)
    //             case _ => Left(List(SemanticError("Type mismatch\nExpected: Char or Int Actual: " + t1)))
    //         }
    //         case _ => Left(List(SemanticError("Type mismatch for binary operation")))
    //     }
    // }

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
            case Nil => Right(AnyType) // Empty arrays default to AnyType
            case head :: tail =>
                val headTypeResult = findType(head, scopeLevel)
                headTypeResult.flatMap { headType =>
                    // Check each element in the tail has the same type as the head
                    val allMatch = tail.forall { expr =>
                        findType(expr, scopeLevel) match {
                            case Right(t) => typeMatch(t, headType)
                            case _ => false
                        }
                    }
                    // If all match, return ArrayType of the head's type, else return an error
                    if (allMatch) Right(ArrayType(headType))
                    else Left(List(SemanticError("Array elements must be of the same type.")))
                }
        }
    }


    val everyType: Set[Type] = Set(PairType(AnyType, AnyType), ArrayType(AnyType), IntType, BoolType, CharType, StringType)
    val comparisonOpType: Set[Type] = Set(IntType, CharType)
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
            case LT(x, y) => checkBinaryOp(x, y, comparisonOpType, BoolType, scopeLevel)
            case LTE(x, y) => checkBinaryOp(x, y, comparisonOpType, BoolType, scopeLevel)
            case GT(x, y) => checkBinaryOp(x, y, comparisonOpType, BoolType, scopeLevel)
            case GTE(x, y) => checkBinaryOp(x, y, comparisonOpType, BoolType, scopeLevel)
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
            case Ident(x) => getValueFromTable(x + scopeLevel)
            case ArrayElem(ident, exprList) =>
                getValueFromTable(ident.x + scopeLevel).flatMap {
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
        (t1, t2) match {
            case (ArrayType(a), ArrayType(b)) => typeMatch(a, b)
            case (StringType, ArrayType(CharType)) => true
            case (ArrayType(CharType), StringType) => true
            case _ => t1 == AnyType || t2 == AnyType || t1 == t2
        }
    }
}
