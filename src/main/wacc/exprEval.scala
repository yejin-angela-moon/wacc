package wacc

import parsley._
import Error._

import ast._
import parser._
import Semantic._

object ExprEval {
    private var expressions: List[A] = Nil

    // private def binOpEval(a1 : A, a2 : A, op: (Expr, Expr) => Expr) : Either[SemanticError, Expr] = {
    //     // check overflow if ints
    //     // return Right(op(a1, a2))
    //     (a1, a2) match {
    //         case (IntLit(x), IntLit(y)) =>
    //             Right(IntLit(op(x, y)))
    //         case (BoolLit(x), BoolLit(y)) =>
    //             Right(BoolLit(op(x, y)))
    //         case _ =>
    //             Left(SemanticError("Type mismatch or operation not supported on given types"))
    //     }
    // }

    def exprEval(expr: Either[SemanticError, Expr], scopeLevel: String) : Either[SemanticError, Expr] = 
        expr match {
            // ERROR CHECKING
            case Left(error) => Left(error)        

            // IDENTIFIER
            case Right(Ident(name)) => getValueFromSymbolTable(s"$name-$scopelevel") match {
                case Some(exprType) => Right(exprType) // returning expression or type??
                case None => Left(SemanticError(s"Undefined identifier $name at scope $scopeLevel"))
            }

            // LITERALS
            case Right(literal @ (_: IntLit | _: BoolLit | _: CharLit | _: StrLit | _: ArrayElem)) => Right(literal.getType())
                
            // BIOP OPERATIONS
            case Right(Add(e1, e2))  => 
                (exprEval(Right(e1), scopeLevel), exprEval(Right(e2), scopeLevel)) match {
                    case (Right(IntLit(_)), Right(IntLit(_))) => e1 + e2
                }

            case Right(expression : Expr) => expression match {
                case Add(IntLit(x), IntLit(y)) => Right(x + y)
                case Add(_, _) => throw 
                // case Sub(_,_) => -
                // case Mul() => *
                // case Div() => /
                // case Mod() => %
             }


            // BOOLEAN OPERATIONS
            case And(e1, e2) | Or(e1, e2) | LT(e1, e2) | LTE(e1, e2) | GT(e1, e2) | GTE(e1, e2) | E(e1, e2) | NE(e1, e2) =>
                (exprEval(e1, s), exprEval(e2, s)) match {
                    case (BoolLit(x), BoolLit(y)) => Right(IntLit(binOpEval(x, y, expr match {
                        case And() => &&
                        case Or()  => ||
                        case LT()  => <
                        case LTE() => <=
                        case GT()  => >
                        case GTE() => >=
                        case E()   => ==
                        case NE()  => !=
                    })))
                    case (err1 : List[A], err2 : List[A]) => Left(err1 + err2)
                    case (err : List[A], _) | (_, err : List[A])  => Left(err)
                    case _ => Left(SemanticError("Not Int type"))
                }

            case Not(e) => exprEval(e) match {
                case BoolLit(x) =>  Right(BoolLit(!x))
                case _ => Left()
            }
            case Neg(e) => exprEval(e) match {
                case IntLit(x) =>   Right(IntLit(neg(x)))
                case _ => Left()
            }
            case Len(e) => exprEval(e) match {
                // Here we might need to parse through the array and chek the types of all members
                case ArrayElem(ident, x) =>  Right(IntLit(len(x)))
                case _ => Left()
            }
            case Ord(e) => exprEval(e) match {
                case CharLit(x) =>  Right(IntLit(x.toInt))
                case _ => Left()
            }
            case Chr(e) => exprEval(e) match {
                case IntLit(x) =>  Right(CharLit(x.toChar))
                case _ => Left()
            }
            case _ => Left(SemanticError("Undefined"))
        }
}