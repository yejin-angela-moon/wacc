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

    val funcTable: scala.collection.mutable.Map[Ident, List[Type]] = new HashMap[Ident, List[Type]]()

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
            case IntLit(_) if t == IntType => Right(())
            case BoolLit(_) if t == BoolType => Right(())
            case CharLit(_) if t == CharType => Right(())
            case StrLit(_) if t == StringType => Right(())
            case Ident(_) => Right(())
            case ArrayElem(_, _) if t == ArrayType => Right(())
            case Paran(_) if t == IntType => Right(())
            case PairLit if t == IntType => Right(())
            case default => Left(SemanticError("Type Mismatch"))
        }
    }

    def checkStatementSemantic(stmt: Stmt, t: Type) : Either[List[Error], Unit] = {
        stmt match {
            case Program(funcs, body) => Right(()) 
            case Function(t, ident, body) => Right(()) 
            case Skip => Right(()) 
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
            case Free(_) => {
                //array or pair
            }
            case Return(_) => Right(()) 
            case Exit(_) => Right(()) 
            case Print(_) => Right(()) 
            case Println(_) => Right(()) 
            case IfThenElse(_, _, _) => Right(()) 
            case WhileDo(_, _) => Right(()) 
            case BeginEnd(_) => Right(()) 
            case StmtList(_, _) => Right(()) 
        }
    }

    def checkLvalue(v: Lvalue) : Either[List[SemanticError], Type] = {
     //   v match {
     //       case ArrayLit => {}
     //       case Call => {}
     //   }
    }

    def checkRvalue(v: Rvalue, t: Type) : Either[List[SemanticError], Unit]  = {
        v match {
            case ArrayLit(x) => {
                x match { 
                    case Some(list) => 
                        for (e <- list) {
                            checkExprSemantic(e, t)
                        }
                    case None => 
                        Right(()) 
                }
            }
            case NewPair(x1, x2) => {
                checkExprSemantic(x1, t)
                checkExprSemantic(x2, t)
            }
            case Call(i, x) => {}
        }
    }

    def checkTypeSemantic(stmt: Stmt) : Unit = {

    }
}

object CheckType {

    import scala.language.implicitConversions


}

case class ArrayElem(ident: Ident, x: List[Expr]) extends Expr with Lvalue {
        def typeCheck(symTable: Map[String, Type]): Either[Error, Type] = {
            val semanticError = SemanticError
            symTable.get(ident.name) match {  //TODO 
            case Some(ArrayType(elementType)) => 
            if (elementType == PairType) {
                semanticError.addError("Array element type cannot be an erased pair")
                Left(semanticError)
            } else {
                val t = x.head.getClass()
                if (x.forall(_.getClass == t)) {
                   Right(ArrayType(t))
                
                } else {
                    val distinctType = x.map(_.getClass).distinct

                    if (distinctType.length == 1) {

                        if (elementType.isArray && elementType != ArrayType) {
                            semanticError.addError("Arrays are invariant in their type parameter")
                            Left(semanticError)
                        } else if (distinctType == CharLit && ident.getType() == String) {
                            Right(ArrayType(CharType))
                        } else {
                            if (elementType != StringType) {
                               Right(ArrayType(elementType))
                            } else {
                                semanticError.addError("A string cannot take place of char[]")
                                Left(semanticError)
                            }
                        }
                    } else {
                        val  lca = lowestCommonAncestor(distinctType)
                        Right(ArrayType(lca))
                    }
                }
            }
            case Some(_) =>
                semanticError.addError(s"${ident.name} is not an array")
                Left(semanticError)
            case None =>
                semanticError.addError(s"${ident.name} is not declared")
                Left(semanticError)
            }
        }

        def lowestCommonAncestor() = {} //TODO
        
    }
