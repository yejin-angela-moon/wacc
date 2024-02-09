package wacc

import scala.collection.mutable.{HashMap}
import parsley.{Result, Success, Failure}
import ast._
// import java.lang.foreign.MemorySegment.Scope
import Errors._
import Semantic._
import TypeCheck._

object Semantic {

    /* Table for storing Ident -> Type*/
    var symbolTable = new HashMap[String, Type]

    /* Table for storing Function Ident -> FuncInfo(Type, ParamList) */
    var functionTable = new HashMap[String, FuncInfo]

    case class FuncInfo(t: Type, params: ParamList)

    /* Get the Type from the table */
    def getValueFromTable(key: String, pos: (Int, Int)): Either[List[SemanticError], Type] = {
        @annotation.tailrec
        def search(scopeVar: String): Either[List[SemanticError], Type] = {
            symbolTable.get(scopeVar) match {
            case Some(value) => Right(value)
            case None =>
                val nextScopeVar = scopeVar.lastIndexOf("-") match {
                case -1 => ""
                case index => scopeVar.substring(0, index)
                }
                if (nextScopeVar.isEmpty) Left(List(UndeclaredIdentifierError(key, pos)))
                else search(nextScopeVar)
            }
        }


        search(key)
    }

    def stmtCheck(prog: Stmt, scopeLevel: String) :
        Either[List[SemanticError], Unit] = {
        prog match {

            /* Add the mapping Ident -> Type into the symbol table */
            case Declare(t, ident, rvalue) =>
                var scopeVar = s"${ident.x}$scopeLevel"

                /* Check if the Ident are already in the table or not */
                symbolTable.get(scopeVar) match {
                    case Some(_) =>
                        Left(List(RedeclaredVariableError(ident.x, prog.pos)))

                    case None =>
                        /* Check if the rvalue has the same type as declared */
                        checkRvalue(rvalue, scopeLevel, prog.pos) match {
                            case Right(value) => {
                                if(t :> value){
                                    if(NullType :> value)
                                        symbolTable.addOne(scopeVar -> value)
                                    else
                                        symbolTable.addOne(scopeVar -> t)
                                    Right(())}
                                else if (value :> t)
                                    Left(List(CastingError(value, t, prog.pos)))
                                else
                                    Left(List(TypeError("Declare", Set(t), Set(value), prog.pos)))
                            }


                            case Left(value) => Left(value)
                        }
                }

            /* Check if variable can be reassign */
            case Assign(lvalue, rvalue) =>
                val lResult = checkLvalue(lvalue, scopeLevel, prog.pos)
                val rResult = checkRvalue(rvalue, scopeLevel, prog.pos)

                /* Check if the type of the lvalue and the rvalue match */
                (lResult, rResult) match {
                    case (Right(t1), Right(t2)) =>
                        if (t1 :> t2) Right(())
                        else Left(List(TypeError("Assign", Set(t1), Set(t2), prog.pos)))

                    case (Left(errors1), Left(errors2)) =>
                    Left(errors1 ++ errors2)

                    case (Left(errors), Right(_)) =>
                    Left(errors)

                    case (Right(_), Left(errors)) =>
                    Left(errors)
                }

            /* Check if the type of the lvalue and rvalue match */
            case IfThenElse(x, s1, s2) =>
                findType(x, scopeLevel, prog.pos) match {
                    case Left(error) =>
                        Left(error)

                    case Right(t) if t != BoolType =>
                        Left(List(TypeError("Condition", Set(BoolType), Set(t), prog.pos)))

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

            /* Check if the type of the cond is a bool, then check the statement body */
            case WhileDo(x, s) =>

                findType(x, scopeLevel, prog.pos) match {
                    case Left(error) =>
                        Left(error)

                    case Right(t) if t != BoolType =>
                        Left((List(TypeError("Condition", Set(BoolType), Set(t), prog.pos))))

                    case Right(_) =>
                        stmtCheck(s, scopeLevel + "-w")
                }

            /* Check the statement of the body */
            case BeginEnd(s) =>
                stmtCheck(s, scopeLevel + "-b")

            /* Check the statement of the body on the left and right */
            case StmtList(s1, s2) =>
                val res1 = stmtCheck(s1, scopeLevel)
                val res2 = stmtCheck(s2, scopeLevel)

                (res1, res2) match {
                    case (Right(_), Right(_)) => Right(())
                    case (Left(errors1), Left(errors2)) => Left(errors1 ++ errors2)
                    case (Left(errors1), Right(_)) => Left(errors1)
                    case (Right(_), Left(errors2)) => Left(errors2)
                }

            /* Skip return nothing */
            case Skip => Right(())

            /* Check the lvalue is either a int, char or a null type */
            case Read(lvalue) =>
                println(symbolTable)
                checkLvalue(lvalue, scopeLevel, prog.pos) match {
                case Right(t) if t == IntType || t == CharType => Right(())
                case Right(t) => Left(List(TypeError("Read", Set(IntType, CharType), Set(t), prog.pos)))
                case Left(value) => Left(value)
            }

            /* Check that the return is inside of a function body and has the same type as what the function defined */
            case Return(x) => (findType(x, scopeLevel, prog.pos), getCurrentFunctionReturnType(scopeLevel, prog.pos)) match {
                case (Right(t1), Right(t2)) if t2 :> t1 => Right(())
                case (Right(t1), Right(t2)) => Left(List(TypeError("Return", Set(t2), Set(t1), prog.pos)))
                case (Left(e1), Left(e2)) => Left(e1 ++ e2)
                case (_, Left(e)) => Left(e)
                case (Left(e), _) => Left(e)
            }
            case Exit(x) => findType(x, scopeLevel, prog.pos) match {
                case Right(IntType) => Right(())
                case Right(badType) => Left(List(TypeError("Exit", Set(IntType), Set(badType), prog.pos)))
                case Left(errors) => Left(errors)
            }

            /* Check that the expression can be compute */
            case Print(x) => findType(x, scopeLevel, prog.pos) match {
                case Right(_) => Right(())
                case Left(errors) => Left(errors)
            }

            /* Check the statement of the body */
            case Println(x) =>
                findType(x, scopeLevel, prog.pos) match {
                case Right(_) => Right(())
                case Left(errors) => Left(errors)
            }

            /* Check if the expresssion is an Array, a Pair or Null */
            case Free(x) => findType(x, scopeLevel, prog.pos) match {
                case Right(ArrayType(_)) | Right(PairType(_,_)) | Right(NullType) => Right(()) 
                case Left(errors) => Left(errors)
                case Right(_) => Left(List(FreeingError(prog.pos)))
            }

        }
    }

    def checkLvalue(l: Lvalue, scopeLevel: String, pos: (Int, Int)): Either[List[SemanticError], Type] = {
        l match {
            case Ident(x) =>
                findType(Ident(x), scopeLevel, pos)
            case ArrayElem(ident, exprList) =>
                findType(ArrayElem(ident, exprList), scopeLevel, pos)
            case Fst(lvalue) => checkLvalue(lvalue, scopeLevel, pos) match {
                case Right(PairType(a, b)) => Right(fromPairElemType(a))
                case Right(NullType) => Right(NullType)
                case _ =>Left(List(IllegalUsedFunctionOnNonPairTypeError("fst", pos)))
            }
            case Snd(lvalue) => checkLvalue(lvalue, scopeLevel, pos) match {
                case Right(PairType(a, b)) =>
                    print("the value of b is " + b)
                    Right(fromPairElemType(b))
                case Right(NullType) => Right(NullType)
                case _ =>Left(List(IllegalUsedFunctionOnNonPairTypeError("snd", pos)))
            }
            }
        }


    def checkRvalue(r : Rvalue, scopeLevel: String, pos: (Int, Int)) : Either[List[SemanticError], Type] = {
        r match{
            /* Check that every element have the same type and return that type */
            case ArrayLit(exprList) =>
                checkArrayList(exprList.getOrElse(Nil), scopeLevel, pos)
            case NewPair(x1, x2) =>

                for {
                    type1 <- findType(x1, scopeLevel, pos) // Find type of x1
                    type2 <- findType(x2, scopeLevel, pos) // Find type of x2
                    pairElemType1 = toPairElemType(type1) // Convert to PairElemType if necessary
                    pairElemType2 = toPairElemType(type2) // Convert to PairElemType if necessary
                } yield PairType(pairElemType1, pairElemType2)

            case Call(ident, x) => 
                functionTable.get(ident.x) match {
                    case Some(FuncInfo(t, ps)) =>
                        matchArgListWithParamList(x, ps, scopeLevel, pos) match {
                            case Right(_) => Right(t)
                            case Left(value) => Left(value)
                        }
                    case None => Left(List(UndeclaredIdentifierError(ident.x, pos)))

                }
            case Fst(lvalue) => 
                
                checkLvalue(lvalue, scopeLevel, pos) match {
                case Right(PairElemType1) => Left(List(IllegalUsedFunctionOnNonPairTypeError("fst", pos))) 
                case Right(PairType(a, b)) => Right(fromPairElemType(a))
                case Right(NullType) => Right(NullType)
                case a => 
                    Left(List(IllegalUsedFunctionOnNonPairTypeError("fst", pos))) 
            }
            case Snd(lvalue) => checkLvalue(lvalue, scopeLevel, pos) match {
                case Right(PairType(a, b)) => Right(fromPairElemType(b))
                case Right(NullType) => Right(NullType)
                case _ => Left(List(IllegalUsedFunctionOnNonPairTypeError("snd", pos))) 
            }
            case e => findType(e.asInstanceOf[Expr], scopeLevel, pos)
        }
    }

    def matchArgListWithParamList(args: Option[ArgList], paramList: ParamList, scopeLevel: String, pos: (Int, Int)):
        Either[List[SemanticError], Unit] = {
        (args, paramList.params) match {
            case (Some(ArgList(argList)), params) if argList.length == params.length =>
                val typeChecks = argList.zip(params).map { case (arg, param) =>
                    findType(arg, scopeLevel, pos).flatMap { argType =>
                        if (argType == param.t) Right(())
                        else Left(List(TypeError("Assign", Set(argType), Set(param.t), pos)))
                    }
                }

                // Collect any errors
                val errors = typeChecks.collect { case Left(error) => error }.flatten
                if (errors.isEmpty) Right(())
                else return Left(errors)

            case (None, Nil) => Right(())
            case _ => Left(List(TypeError("Assign", Set(), Set(), pos)))
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

    def checkFuncsList(funcs: List[Func], pos: (Int, Int)): Either[List[SemanticError], Unit] = {

        funcs.foreach(f =>
            functionTable.get(f.ident.x) match {
                case Some(_) =>
                    return Left(List(RedefinedFunctionError(f.ident.x, pos)))
                case None =>
                    // symbolTable.addOne(f.ident.x -> f.t)
                    functionTable.addOne(f.ident.x -> FuncInfo(f.t, f.list))
                    println(s"function ${f.ident.x} added")
                    Right(())
            }
        )

        funcs.foldLeft[Either[List[SemanticError], Unit]](Right(())) { (acc, func) =>
            acc.flatMap { _ =>

                func.list.params.foreach {
                    param =>
                        val paramNameWithScope = s"${param.ident.x}-${func.ident.x}"
                        symbolTable.get(paramNameWithScope) match {
                            case None => symbolTable.addOne(paramNameWithScope -> param.t)
                            case Some(value) =>
                                Left(List(RedeclaredVariableError(param.ident.x, pos)))
                        }
                }

                stmtCheck(func.body, "-" + func.ident.x + "-g")
            }
        }
    }



    def getCurrentFunctionReturnType(scopeLevel: String, pos: (Int, Int)): Either[List[SemanticError], Type] = {
        val getFunctionName = scopeLevel.split("-")
        (getFunctionName.lengthCompare(1) < 0) match {
            case true => Left(List(ScopeError("main", pos)))
            case false => functionTable.get(getFunctionName(1)) match {
                case Some(FuncInfo(t, params)) => Right(t)
                case None => Left(List(ScopeError("outside of function", pos)))
            }
        }

    }


    def semanticAnalysis(prog: Stmt) : Either[List[Error], Unit] = {
        symbolTable.clear()
        functionTable.clear()
        val lineNumber = 0

        prog match {
            case Program(funcs, body) =>
                (checkFuncsList(funcs, prog.pos), stmtCheck(body, "-g")) match {
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
                            expectedOutputType: Type, scopeLevel: String, pos: (Int, Int)) :
                                Either[List[SemanticError], Type] = {
        (findType(x, scopeLevel, pos), findType(y, scopeLevel, pos)) match {
            case (Right(t1), Right(t2)) => {
                    if(t1 :> t2 && t2 :> t1) {
                        if(expectedType.filter((x) => x :> t1).size >= 1)
                            return Right(expectedOutputType)

                        return Left(List(TypeError("BinOp", expectedType, Set(t1), pos)))
                    }
                    return Left(List(TypeDifferentError("BinOp", Set(t1, t2), pos)))
                }
            case (Left(a), Left(b)) => Left(a ++ b)
            case (Left(err), _) => Left(err)
            case (_, Left(err)) => Left(err) 
            case _ => Left(List(UndefinedError(pos)))
            }
        }

    private def checkUnaryOp(x: Expr, expectedType: Set[Type],
                            expectedOutputType: Type, scopeLevel: String, pos: (Int, Int)) :
                                Either[List[SemanticError], Type] = {
        (findType(x, scopeLevel, pos)) match {
            case Right(t1) => t1 match {
                case ArrayType(t) => Right(ArrayType(t))
                case PairType(p1, p2) => Right(PairType(p1, p2))
                case _ if expectedType.filter((x) => x :> t1).size >= 1 =>
                    Right(expectedOutputType)
                case badType => Left(List(TypeError("Unary operation", expectedType, Set(badType), pos)))
            }
            case Left(err) => Left(err)
        }
    }

    def checkArrayList(arrayList: List[Expr], scopeLevel: String, pos: (Int, Int)): Either[List[SemanticError], Type] = {
        arrayList match {
            case Nil => Right(AnyType) // Empty arrays default to AnyType
            case head :: tail =>
                val headTypeResult = findType(head, scopeLevel, pos)
                headTypeResult.flatMap { headType =>
                    // Check each element in the tail has the same type as the head
                    val allMatch = tail.forall { expr =>
                        findType(expr, scopeLevel, pos) match {
                            case Right(t) => typeMatch(t, headType)
                            case _ => false
                        }
                    }
                    // If all match, return ArrayType of the head's type, else return an error
                    if (allMatch) Right(ArrayType(headType))
                    else Left(List(MultipleTypesInArrayError(pos)))
                }
        }
    }


    val everyType: Set[Type] = Set(PairType(AnyType, AnyType), ArrayType(AnyType), IntType, BoolType, CharType, StringType)
    val comparisonOpType: Set[Type] = Set(IntType, CharType)
    // val everyArrayType: Set[Type] = Set()

    def findType(expr : Expr, scopeLevel : String, pos: (Int, Int)) : Either[List[SemanticError], Type] = {
        expr match {
            case Add(x, y) => checkBinaryOp(x, y, Set(IntType), IntType, scopeLevel, pos)
            case Sub(x, y) => checkBinaryOp(x, y, Set(IntType), IntType, scopeLevel, pos)
            case Mul(x, y) => checkBinaryOp(x, y, Set(IntType), IntType, scopeLevel, pos)
            case Div(x, y) => checkBinaryOp(x, y, Set(IntType), IntType, scopeLevel, pos)
            case Mod(x, y) => checkBinaryOp(x, y, Set(IntType), IntType, scopeLevel, pos)
            case And(x, y) => checkBinaryOp(x, y, Set(BoolType), BoolType, scopeLevel, pos)
            case Or(x, y) => checkBinaryOp(x, y, Set(BoolType), BoolType, scopeLevel, pos)
            case LT(x, y) => checkBinaryOp(x, y, comparisonOpType, BoolType, scopeLevel, pos)
            case LTE(x, y) => checkBinaryOp(x, y, comparisonOpType, BoolType, scopeLevel, pos)
            case GT(x, y) => checkBinaryOp(x, y, comparisonOpType, BoolType, scopeLevel, pos)
            case GTE(x, y) => checkBinaryOp(x, y, comparisonOpType, BoolType, scopeLevel, pos)
            case E(x, y) => checkBinaryOp(x, y, everyType, BoolType, scopeLevel, pos)
            case NE(x, y) => checkBinaryOp(x, y, everyType, BoolType, scopeLevel, pos)
            case Not(x) => checkUnaryOp(x, Set(BoolType), BoolType, scopeLevel, pos)
            case Neg(x) => checkUnaryOp(x, Set(IntType), IntType, scopeLevel, pos)
            case Len(x) => checkUnaryOp(x, Set(ArrayType(AnyType), StringType), IntType, scopeLevel, pos)
            case Ord(x) => checkUnaryOp(x, Set(CharType), IntType, scopeLevel, pos)
            case Chr(x) => checkUnaryOp(x, Set(IntType), CharType, scopeLevel, pos)
            case IntLit(x) => Right(IntType)
            case BoolLit(x) => Right(BoolType)
            case CharLit(x) => Right(CharType)
            case StrLit(x) => Right(StringType)
            case Ident(x) => getValueFromTable(x + scopeLevel, pos)
            case ArrayElem(ident, exprList) =>
                getValueFromTable(ident.x + scopeLevel, pos).flatMap {
                    case ArrayType(t) =>
                        val arrayDepth = getArraydepth(1, t)
                        val indexTypeChecks = exprList.map(findType(_, scopeLevel, pos))
                        val indexErrors = indexTypeChecks.collect { case Left(errs) => errs }.flatten

                        if (indexErrors.nonEmpty) {
                            Left(indexErrors)
                        } else {
                            // Ensure all index types are IntType
                            if (arrayDepth < indexTypeChecks.length) {
                                return Left(List(ArrayDimensionalError(indexTypeChecks.length, pos)))
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
                                Left(List(TypeError("Array indices", Set(IntType), otherTypes, pos)))
                            }
                    }
                case _ => Left(List(ArrayTypeError(ident.x, pos)))
                }
            case Paran(x) => findType(x, scopeLevel, pos)
            case PairLit => Right(NullType)
        }
    }

    def getArraydepth(depth: Int, array: Type): Int = {
        array match {
            case ArrayType(t) => getArraydepth(depth + 1, t)
            case _ => depth
        }
    }

    def checkTypes(e1: Expr, e2: Expr, scopeLevel: String, pos: (Int, Int)) : Boolean = {
        (findType(e1, scopeLevel, pos), findType(e2, scopeLevel, pos)) match {
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
