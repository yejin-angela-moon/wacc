package wacc

import scala.collection.mutable.{HashMap}
import parsley.{Result, Success, Failure}
import ast._

object Semantic {
  //Array

    val symTable: scala.collection.mutable.Map[String, Type] = HashMap[String, Type]()

    def lookUp (name: String): Type = {
        symTable.get(name).get
    }

    def assign (name: String, newType: Type) = {
        if (symTable.contains(name)) {
            val oldType:Type = lookUp (name)
            if (oldType != newType) {    
                //error
            } else {
                symTable.update(name, newType)
            }
        } else {
            symTable.addOne(name, newType)
        }
    }

    val funcTable : Map[Ident, List[Type]] = new HashMap[Ident, List[Type]]

    def checkSemantic(prog: Stmt) : Unit = {
        prog match {
            case Program (funcs, body) =>
                checkStatementSemantic(body)
                funcs.foreach(f => checkStatmentSemantic(f))
            case default => System.exit(0)}
    }

    def checkExprSemantic(expr: Expr, t: Type) : Either[List[SemanticError], Unit] = {
        expr match {
            case Add(e1, e2) => {
                checkExprSemantic(e1, IntType)
                checkExprSemantic(e2, Inttype)
            }
            case Sub(e1, e2) => {
                checkExprSemantic(e1, IntType)
                checkExprSemantic(e2, IntType)
            }
            case Mul(e1, e2) => {
                checkExprSemantic(e1, IntType)
                checkExprSemantic(e2, IntType)
            }
            case Div(e1, e2) => {
                checkExprSemantic(e1, IntType)
                checkExprSemantic(e2, IntType)
            }
            case Mod(e1, e2) => {
                checkExprSemantic(e1, IntType)
                checkExprSemantic(e2, IntType)
            }
            case And(e1, e2) => {
                checkExprSemantic(e1, BoolType)
                checkExprSemantic(e2, BoolType)
            }
            case Or(e1, e2) => {
                checkExprSemantic(e1, BoolType)
                checkExprSemantic(e2, BoolType)
            }
            case LT(e1, e2) => {
                checkExprSemantic(e1, IntType)
                checkExprSemantic(e2, IntType)
            }
            case LTE(e1, e2) => {
                checkExprSemantic(e1, IntType)
                checkExprSemantic(e2, IntType)
            }
            case GT(e1, e2) => {
                checkExprSemantic(e1, IntType)
                checkExprSemantic(e2, IntType)
            }
            case GTE(e1, e2) => {
                checkExprSemantic(e1, IntType)
                checkExprSemantic(e2, IntType)
            }
            case E(e1, e2) => {
                checkExprSemantic(e1, IntType)
                checkExprSemantic(e2, IntType)
            }
            case NE(e1, e2) => {
                checkExprSemantic(e1, IntType)
                checkExprSemantic(e2, IntType)
            }
            case Not(e1) => {
                checkExprSemantic(e1, BoolType)
            }
            case Neg(e1) => {
                checkExprSemantic(e1, IntType)
            }
            case Len(e1) => {
                checkExprSemantic(e1, ArrayType)
            }
            case Ord(e1) => {
                checkExprSemantic(e1, CharType)
            }
            case Chr(e1) => {
                checkExprSemantic(e1, CharType)
            }
            case default => checkLit(expr)
        }}

    def checkLit (lit: Expr, t: Type) : Either[List[SemanticError], Unit] = {
        lit match {
            case IntLit(_) if expectedType == IntType => Right(())
            case BoolLit(_) if expectedType == BoolType => Right(())
            case CharLit(_) if expectedType == CharType => Right(())
            case StrLit(_) if expectedType == StringType => Right(())
            case Ident(_) => Right(())
            case ArrayElem(_, _) if expectedType == ArrayType => Right(())
            case Paran(_) if expectedType == IntType => Right(())
            case PairLit if expectedType == IntType => Right(())
            case default => Left(SemanticError("Type Mismatch"))
        }
    }

    def checkStatementSemantic(stmt: Stmt, t: Type) : Either[List[SemanticError], Unit] = {
        stmt match {
            case Program(funcs, body) => {}
            case Function(t, ident, body) => {}
            case Skip => {}
            case Declare(t, ident, rvalue) => {
                symbolTable.get(ident.x) match {
                    case Some(_) => Left((SemanticError("Variable already declared: ${ident.x}")))
                    case None =>
                        symbolTable(ident.x) = t
                        checkRvalue(rvalue, t)
                }
            }
            case Assign(lvalue, rvalue) => {
                val t = checkLvalue(lvalue)
                checkRvalue(rvalue, t)
            }
            case Read(lvalue) => {
                checkLvalue(lvalue, IntType ) //int or char
                checkLvalue(lvalue, CharType)
            }
            case Free => {
                //array or pair
            }
            case Return => {}
            case Exit => {}
            case Print => {}
            case Println => {}
            case IfThenElse => {}
            case WhileDo => {}
            case BeginEnd => {}
            case StmtList => {}
        }
    }

    def checkLvalue(v: Lvalue) : Either[List[SemanticError], Type] = {
        v match {
            case ArrayLit => {}
            case NewPair => {}
            case Call => {}
        }
    }

    def checkRvalue(v: Rvalue) : Type = {
        v match {
            case ArrayLit(x) => {}
            case NewPair(x1, x2) => {}
            case Call(i, x) => {}
        }
    }

    def checkTypeSemantic(stmt: Stmt) : Unit = {

    }
}

object CheckType {

    import scala.language.implicitConversions


}
