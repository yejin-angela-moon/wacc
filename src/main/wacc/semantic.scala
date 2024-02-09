package wacc

import scala.collection.mutable.{HashMap}
import parsley.{Result, Success, Failure}
import ast._
// import java.lang.foreign.MemorySegment.Scope
import Errors._
import Semantic._
import TypeCheck._

object Semantic {
    var symbolTable = new HashMap[String, Type]

    def getValueFromTable(key: String): Either[List[SemanticError], Type] = {
        @annotation.tailrec
        def search(scopeVar: String): Either[List[SemanticError], Type] = {
            symbolTable.get(scopeVar) match {
            case Some(value) => Right(value)
            case None =>
                val nextScopeVar = scopeVar.lastIndexOf("-") match {
                case -1 => "" // No more scopes to backtrack
                case index => scopeVar.substring(0, index)
                }
                if (nextScopeVar.isEmpty) Left(List(UndeclaredIdentifierError(key)))
                else search(nextScopeVar)
            }
        }

        search(key)
    }


    def stmtCheck(prog: Stmt, scopeLevel: String) :
        Either[List[SemanticError], Unit] = {
            // println("----------------------------")
            // println("Checking statement:\n" + prog)
        prog match {
            case Declare(t, ident, rvalue) =>
                var scopeVarible = s"${ident.x}$scopeLevel"

                symbolTable.get(scopeVarible) match {
                    case Some(_) => 
                        Left(List(RedeclaredVariableError(ident.x)))

                    case None =>
                        checkRvalue(rvalue, scopeLevel) match {
                            case Right(value) => {
                                if(t :> value){
                                    if(NullType :> value)
                                        symbolTable.addOne(scopeVarible -> value)
                                    else
                                        symbolTable.addOne(scopeVarible -> t)
                                    Right(())}
                                else if (value :> t)
                                    Left(List(CastingError(value,t)))
                                else
                                    Left(List(TypeError("Declare", Set(t), Set(value))))
                            }


                            case Left(value) => Left(value)
                        }
                }

            case Assign(lvalue, rvalue) =>
                val lResult = checkLvalue(lvalue, scopeLevel)
                val rResult = checkRvalue(rvalue, scopeLevel)

                (lResult, rResult) match {
                    case (Right(t1), Right(t2)) =>
                    if (t1 :> t2) Right(())
                    else Left(List(TypeError("Assign", Set(t1), Set(t2))))

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
                        Left(List(TypeError("Condition", Set(BoolType), Set(t))))

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
                        Left((List(TypeError("Condition", Set(BoolType), Set(t)))))

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
                case Right(t) if t == IntType || t == CharType || t == NullType => Right(())
                case Right(badType) => Left(List(TypeError("Read", Set(IntType, CharType), Set(badType))))
                case Left(value) => Left(value)
            }
            case Return(x) => (findType(x, scopeLevel), getCurrentFunctionReturnType(scopeLevel)) match {
                case (Right(t1), Right(t2)) if t2 :> t1 => Right(())
                case (Right(t1), Right(t2)) => Left(List(TypeError("Return", Set(t2), Set(t1))))
                case (Left(e1), Left(e2)) => Left(e1 ++ e2)
                case (_, Left(e)) => Left(e)
                case (Left(e), _) => Left(e)
            }
            case Exit(x) => findType(x, scopeLevel) match {
                case Right(IntType) => Right(())
                case Right(badType) => Left(List(TypeError("Exit", Set(IntType), Set(badType))))
                case Left(errors) => Left(errors)
            }
            case Print(x) => findType(x, scopeLevel) match {
                case Right(_) => Right(())
                case Left(errors) => Left(errors)
            }
            case Println(x) => 
                findType(x, scopeLevel) match {
                case Right(_) => Right(())
                case Left(errors) => Left(errors)
            }
            case Free(x) => findType(x, scopeLevel) match {
                case Right(ArrayType(_)) | Right(PairType(_,_)) | Right(NullType) => Right(()) 
                case Left(errors) => Left(errors)
                case Right(_) => Left(List(FreeingError()))
            }

        }
    }

    def checkLvalue(l: Lvalue, scopeLevel: String): Either[List[SemanticError], Type] = {
        l match {
            case Ident(x) =>
                findType(Ident(x), scopeLevel)
            case ArrayElem(ident, exprList) =>
                findType(ArrayElem(ident, exprList), scopeLevel)
            case Fst(lvalue) => checkLvalue(lvalue, scopeLevel) match {
                case Right(PairType(a, b)) => Right(fromPairElemType(a))
                case Right(NullType) => Right(NullType)
                case _ => Left(List(IllegalUsedFunctionOnNonPairTypeError("fst")))
              //  case error => error 
            }
            case Snd(lvalue) => checkLvalue(lvalue, scopeLevel) match {
                case Right(PairType(a, b)) => 
                    print("the value of b is " + b)
                    Right(fromPairElemType(b))
                case Right(NullType) => Right(NullType)
                case _ => Left(List(IllegalUsedFunctionOnNonPairTypeError("snd")))
               // case error => error 
            }
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

            case Call(ident, x) => getValueFromTable(ident.x + scopeLevel)
            case Fst(lvalue) => 
                println("the lvalue is " + lvalue)
                checkLvalue(lvalue, scopeLevel) match {
                case Right(PairType(a, b)) => Right(fromPairElemType(a))
                case Right(NullType) => Right(NullType)
                case a => 
                    println(a)
                    println(symbolTable)
                    Left(List(IllegalUsedFunctionOnNonPairTypeError("fst"))) 
            }
            case Snd(lvalue) => checkLvalue(lvalue, scopeLevel) match {
                case Right(PairType(a, b)) => Right(fromPairElemType(b))
                case Right(NullType) => Right(NullType)
                case _ => Left(List(IllegalUsedFunctionOnNonPairTypeError("snd"))) 
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
        case _ => NullType
    }

    def checkFuncsList(funcs: List[Func]): Either[List[SemanticError], Unit] = {
        funcs.foreach(f =>
            symbolTable.get(f.ident.x) match {
                case Some(value) => Left(List(RedefinedFunctionError(f.ident.x)))
                case None => symbolTable.addOne(f.ident.x -> f.t)
            }
        )

        funcs.foldLeft[Either[List[SemanticError], Unit]](Right(())) { (acc, func) =>
            acc.flatMap { _ =>

                func.list.params.foreach {
                    param =>
                        val paramNameWithScope = s"${param.ident.x}-${func.ident.x}"
                        // println(paramNameWithScope)
                        symbolTable.addOne(paramNameWithScope -> param.t)
                }

                stmtCheck(func.body, "-" + func.ident.x + "-b")
            }
        }
    }

    def getCurrentFunctionReturnType(scopeLevel: String): Either[List[SemanticError], Type] = {
        val getFunctionName = scopeLevel.split("-")
        (getFunctionName.lengthCompare(1) < 0) match {
            case true => Left(List(ScopeError("main")))
            case false => symbolTable.get(getFunctionName(1)) match {
                case Some(returnType) => Right(returnType)
                case None => Left(List(ScopeError("outside of function")))
            }
        }

    }


    def semanticAnalysis(prog: Stmt) : Either[List[Error], Unit] = {
        symbolTable.clear()
        val lineNumber = 0

        prog match {
            case Program(funcs, body) =>
                (checkFuncsList(funcs), stmtCheck(body, "-g")) match {
                    case (Right(_), Right(_)) => Right(())
                    case (Left(err1), Left(err2)) => Left(err1 ++ err2)
                    case (Left(errs), _) => Left(errs)
                    case (_, Left(errs)) => Left(errs)
                } 
            case _ => Left(List(SyntaxError("Program expected"))) //TOCHECK: which kind of error
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

                        return Left(List(TypeError("BinOp", expectedType, Set(t1))))
                    }
                    return Left(List(TypeDifferentError("BinOp", Set(t1, t2))))
                }
            case (Left(a), Left(b)) => Left(a ++ b)
            case (Left(err), _) => Left(err)
            case (_, Left(err)) => Left(err) 
            case _ => Left(List(UndefinedError()))
            }
        }

    private def checkUnaryOp(x: Expr, expectedType: Set[Type],
                            expectedOutputType: Type, scopeLevel: String) :
                                Either[List[SemanticError], Type] = {
        (findType(x, scopeLevel)) match {
            case Right(t1) => t1 match {
                case ArrayType(t) => Right(ArrayType(t))
                case PairType(p1, p2) => Right(PairType(p1, p2))
                case _ if expectedType.filter((x) => x :> t1).size >= 1 =>
                    Right(expectedOutputType)
                case badType => Left(List(TypeError("Unary operation", expectedType, Set(badType))))
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
                    else Left(List(MultipleTypesInArrayError()))
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
                        val arrayDepth = getArraydepth(1, t)
                        val indexTypeChecks = exprList.map(findType(_, scopeLevel))
                        val indexErrors = indexTypeChecks.collect { case Left(errs) => errs }.flatten

                        if (indexErrors.nonEmpty) {
                            Left(indexErrors)
                        } else {
                            // Ensure all index types are IntType
                            if (arrayDepth < indexTypeChecks.length) {
                                return Left(List(ArrayDimensionalError(indexTypeChecks.length)))
                            }

                            val otherTypes = Set[Type]() 
                            val allIndicesAreInt = indexTypeChecks.forall {
                                case Right(IntType) => true
                                case Right(otherType) =>
                                    otherTypes + otherType
                                    false
                            }

                            if (allIndicesAreInt) {
                                Right(t) // Return the type of the array element
                            } else {
                                Left(List(TypeError("Array indices", Set(IntType), otherTypes)))
                            }
                    }
                case _ => Left(List(ArrayTypeError(ident.x)))
                }
            case Paran(x) => findType(x, scopeLevel)
            case PairLit => Right(NullType)
        }
    }

    def getArraydepth(depth: Int, array: Type): Int = {
        array match {
            case ArrayType(t) => getArraydepth(depth + 1, t)
            case _ => depth
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
