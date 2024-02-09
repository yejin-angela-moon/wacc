package wacc

import parsley.{Parsley, Result}
import parsley.Success
import java.security.Identity
import scala.collection.View.Empty
import java.security.cert.TrustAnchor

object SemanticManager {
    import ast._ 
    def resolve(x: Result[List[Error], Expr], y: Result[List[Error], Expr]) : Result[List[Error], Expr] =
        (x, y) match {
            // /* BOTH lower evaluations failed */
            // case (Failure(a), Failure(b)) => Failure(a ++ b) 

            // /* ONE lower evaluations failed */
            // case (_, Failure(a)) => Failure(a)              
            // case (Failure(a), _) => Failure(a)

            // /* Different Types from Wrapper Condiion (i.e. not BoolLit or IntLit) */
            // case (Success(a), Success(a)) => ??? 

            /* Non-conforming types */                            
            case (Success(a), _) => ???                                     
            case (_, Success(a)) => ???

            //TODO (optional): extract types (using .getType() from semantic.scala) for cleaner error message                                                   
        }
    def overflow(x: BigInt, y: BigInt, op: (BigInt, BigInt) => BigInt) : Result[List[Error], Expr] = {
        // if(true /* overflow detected*/)
        //     return Failure(new RuntimeException("Overflow detected"))
        
        return Success(IntLit(op(x,y)))
    }
}

object ast {
    import parsley.generic._
    import SemanticManager._
    import Semantic.getValueFromTable
    import ast._

    sealed trait Expr extends Rvalue with Lvalue {
        def eval(scope: String) : Result[List[Error], Expr]
    }

    /*
        Binary Operator
        ⟨binary-oper⟩ ::= ‘*’ | ‘/’ | ‘%’ | ‘+’ | ‘-’ | ‘>’ | ‘>=’ | ‘<’ | ‘<=’
                        | ‘==’ | ‘!=’ | ‘&&’ | ‘||’
    */
    case class Add(x: Expr, y: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            (x.eval(scope), y.eval(scope)) match {
                case (Success(IntLit(a)), Success(IntLit(b))) => overflow(a, b, (x, y) => (x + y))
                case _ => resolve(x.eval(scope), y.eval(scope))
            }
    }
    case class Sub(x: Expr, y: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            (x.eval(scope), y.eval(scope)) match {
                case (Success(IntLit(a)), Success(IntLit(b))) => overflow(a, b, (x, y) => (x - y))
                case _ => resolve(x.eval(scope), y.eval(scope))
            }
    }
    case class Mul(x: Expr, y: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            (x.eval(scope), y.eval(scope)) match {
                case (Success(IntLit(a)), Success(IntLit(b))) => overflow(a, b, (x, y) => (x * y))
                case _ => resolve(x.eval(scope), y.eval(scope))
            }
    }
    case class Div(x: Expr, y: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            (x.eval(scope), y.eval(scope)) match {
                case (Success(IntLit(a)), Success(IntLit(b))) => overflow(a, b, (x, y) => (x / y))
                case _ => resolve(x.eval(scope), y.eval(scope))
            }
    }
    case class Mod(x: Expr, y: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            (x.eval(scope), y.eval(scope)) match {
                case (Success(IntLit(a)), Success(IntLit(b))) => overflow(a, b, (x, y) => (x + y))
                case _ => resolve(x.eval(scope), y.eval(scope))
            }
    }
    case class And(x: Expr, y: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            (x.eval(scope), y.eval(scope)) match {
                case (Success(BoolLit(a)), Success(BoolLit(b))) => Success(BoolLit(a && b))
                case _ => resolve(x.eval(scope), y.eval(scope))
            }
    }
    case class Or(x: Expr, y: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            (x.eval(scope), y.eval(scope)) match {
                case (Success(BoolLit(a)), Success(BoolLit(b))) => Success(BoolLit(a || b))
                case _ => resolve(x.eval(scope), y.eval(scope))
            }
    }
    case class LT(x: Expr, y: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            (x.eval(scope), y.eval(scope)) match {
                case (Success(BoolLit(a)), Success(BoolLit(b))) => Success(BoolLit(a < b))
                case _ => resolve(x.eval(scope), y.eval(scope))
            }
    }
    case class LTE(x: Expr, y: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            (x.eval(scope), y.eval(scope)) match {
                case (Success(BoolLit(a)), Success(BoolLit(b))) => Success(BoolLit(a <= b))
                case _ => resolve(x.eval(scope), y.eval(scope))
            }
    }
    case class GT(x: Expr, y: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            (x.eval(scope), y.eval(scope)) match {
                case (Success(BoolLit(a)), Success(BoolLit(b))) => Success(BoolLit(a > b))
                case _ => resolve(x.eval(scope), y.eval(scope))
            }
    }
    case class GTE(x: Expr, y: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            (x.eval(scope), y.eval(scope)) match {
                case (Success(BoolLit(a)), Success(BoolLit(b))) => Success(BoolLit(a >= b))
                case _ => resolve(x.eval(scope), y.eval(scope))
            }
    }
    case class E(x: Expr, y: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            (x.eval(scope), y.eval(scope)) match {
                case (Success(BoolLit(a)), Success(BoolLit(b))) => Success(BoolLit(a == b))
                case _ => resolve(x.eval(scope), y.eval(scope))
            }
    }
    case class NE(x: Expr, y: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            (x.eval(scope), y.eval(scope)) match {
                case (Success(BoolLit(a)), Success(BoolLit(b))) => Success(BoolLit(a != b))
                case _ => resolve(x.eval(scope), y.eval(scope))
            }
    }

    /*
        Unary Operator
        ⟨unary-oper⟩ ::= ‘!’ | ‘-’ | ‘len’ | ‘ord’ | ‘chr’
    */
    case class Not(x: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            x.eval(scope) match {
                case Success(BoolLit(x)) => Success(BoolLit(!x))
                case default => x.eval(scope)
            }
    }
    case class Neg(x: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            x.eval(scope) match {
                case Success(IntLit(x)) => Success(IntLit(-x)) // CHECK OVERFLOW
                case default => x.eval(scope)
                
            }
    }
    case class Len(x: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            x.eval(scope) match {
                case Success(ArrayElem(ident, x)) => Success(IntLit(x.length))
                case default => x.eval(scope)
            }
    }
    case class Ord(x: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            x.eval(scope) match {
                case Success(CharLit(x)) => Success(IntLit(x.toInt))
                case default => x.eval(scope)
            }
    }
    case class Chr(x: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = 
            x.eval(scope) match {
                case Success(IntLit(x)) => Success(CharLit(x.toChar))
                case default => x.eval(scope)
            }
    }

    /*
        Atom
    */
    case class IntLit(x: BigInt) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = Success(this)
    }
    case class BoolLit(x: Boolean) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = Success(this)
    }
    case class CharLit(x: Char) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = Success(this)
    }
    case class StrLit(x: String) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = Success(this)
    }
    case class Ident(x: String) extends Expr with Lvalue {
        def eval(scope: String): Result[List[Error],Expr] = getValueFromTable(x + scope) match {
            case Right(PairType(_,_)) => Success(PairLit)
            case Right(ArrayType(_)) => Success(ArrayElem(this, List(this)))
            case Right(IntType) => Success(IntLit(0))
            case Right(BoolType) => Success(BoolLit(true))
            case Right(CharType) => Success(CharLit('n'))
            case Right(StringType) => Success(StrLit(" "))
        }
    }
    case class ArrayElem(ident: Ident, x: List[Expr]) extends Expr with Lvalue {
        def eval(scope: String): Result[List[Error],Expr] = Success(this)
    }
    case class Paran(x: Expr) extends Expr {
        def eval(scope: String): Result[List[Error],Expr] = x.eval(scope)
    }
    case object PairLit extends Expr with ParserBridge0[Expr] {
        def eval(scope: String): Result[List[Error],Expr] = Success(this)
    }

    /* Program */
    case class Program(funcs: List[Func], body: Stmt) extends Stmt

    /* Function */
    case class Func(t: Type, ident: Ident, list: ParamList, body: Stmt) extends Stmt

    /* Parameter List */
    case class ParamList(params: List[Param])

    /* Parameter */
    case class Param(t: Type, ident: Ident)

    /* Statement */
    sealed trait Stmt
    case object Skip extends Stmt with ParserBridge0[Stmt]
    case class Declare(t: Type, ident: Ident, rvalue: Rvalue) extends Stmt
    case class Assign(lvalue: Lvalue, rvalue: Rvalue) extends Stmt
    case class Read(lvalue: Lvalue) extends Stmt
    case class Free(x: Expr) extends Stmt
    case class Return(x: Expr) extends Stmt
    case class Exit(x: Expr) extends Stmt
    case class Print(x: Expr) extends Stmt
    case class Println(x: Expr) extends Stmt
    case class IfThenElse(x: Expr, s1: Stmt, s2: Stmt) extends Stmt
    case class WhileDo(x: Expr, s:Stmt) extends Stmt
    case class BeginEnd(s: Stmt) extends Stmt
    case class StmtList(s1: Stmt, s2: Stmt) extends Stmt

    /* Lvalue */
    sealed trait Lvalue

    /* Rvalue */
    sealed trait Rvalue
    case class ArrayLit(x: Option[List[Expr]]) extends Rvalue
    case class NewPair(x1: Expr, x2: Expr) extends Rvalue
    case class Call(i: Ident, x: Option[ArgList]) extends Rvalue

    /* Pair Elem */
    sealed trait PairElem extends Lvalue with Rvalue
    case class Fst(x: Lvalue) extends PairElem
    case class Snd(x: Lvalue) extends PairElem

    /* Arguement List */
    case class ArgList(x: List[Expr])

    /* Type */
    sealed trait Type

    case object NullType extends Type

    /* Pair Type */
    case class PairType(p1: PairElemType, p2: PairElemType) extends Type

    /* Pair Elem Type */
    sealed trait PairElemType extends Type
    case object PairElemType1 extends PairElemType with ParserBridge0[PairElemType]
    case class  PairElemType2(t: Type) extends PairElemType
    case class ArrayType(t: Type) extends Type with PairElemType

    /* Base Type */
    sealed trait BaseType extends Type with PairElemType
    case object IntType extends BaseType with ParserBridge0[BaseType]
    case object BoolType extends BaseType with ParserBridge0[BaseType]
    case object CharType extends BaseType with ParserBridge0[BaseType]
    case object StringType extends BaseType with ParserBridge0[BaseType]

    /* Any-Type */
    case object AnyType extends Type with PairElemType

    implicit class TypeBinOp(t: Type) {
        def :>(that: Type) : Boolean = (t, that) match {
            case (AnyType, _) => true
            case (NullType, NullType) => false
            case (NullType, AnyType) => false
            case (_, AnyType | NullType) => true

            case (NullType, x : PairType) => true
            case (NullType, x : PairElemType) => true
            case (StringType, ArrayType(CharType)) => true

            case (PairElemType1, PairType(_, _)) | (PairType(_, _), PairElemType1) => true
            case (PairElemType1, PairElemType2(_)) | (PairElemType2(_), PairElemType1) => true

            case (PairElemType2(a), PairElemType2(b)) => a:>b 
            case (ArrayType(StringType), ArrayType(ArrayType(CharType))) => false
            case (ArrayType(t1), ArrayType(t2)) => t1:>t2
            case (PairType(p1, p2),PairType(t1, t2)) => p1:>t1 && p2:>t2 && t1:>p1 && t2:>p2
            case (a, b) => a == b
        }
    }

    object Div extends ParserBridge2[Expr, Expr, Expr]
    object Mod extends ParserBridge2[Expr, Expr, Expr]
    object Add extends ParserBridge2[Expr, Expr, Expr]
    object Mul extends ParserBridge2[Expr, Expr, Expr]
    object Sub extends ParserBridge2[Expr, Expr, Expr]
    object GT extends ParserBridge2[Expr, Expr, Expr]
    object GTE extends ParserBridge2[Expr, Expr, Expr]
    object LT extends ParserBridge2[Expr, Expr, Expr]
    object LTE extends ParserBridge2[Expr, Expr, Expr]
    object E extends ParserBridge2[Expr, Expr, Expr]
    object NE extends ParserBridge2[Expr, Expr, Expr]
    object And extends ParserBridge2[Expr, Expr, Expr]
    object Or extends ParserBridge2[Expr, Expr, Expr]

    object Not extends ParserBridge1[Expr, Expr]
    object Neg extends ParserBridge1[Expr, Expr]
    object Len extends ParserBridge1[Expr, Expr]
    object Ord extends ParserBridge1[Expr, Expr]
    object Chr extends ParserBridge1[Expr, Expr]
    object CharLit extends ParserBridge1[Char, CharLit]
    object IntLit extends ParserBridge1[BigInt, IntLit]
    object BoolLit extends ParserBridge1[Boolean, BoolLit]
    object StrLit extends ParserBridge1[String, StrLit]
    object Ident extends ParserBridge1[String, Ident]
    object ArrayElem extends ParserBridge2[Ident, List[Expr], ArrayElem]
    object PairType extends ParserBridge2[PairElemType, PairElemType, PairType]

    object Program extends ParserBridge2[List[Func], Stmt, Program]
    object Func extends ParserBridge4[Type, Ident, ParamList, Stmt, Func]
    object ParamList extends ParserBridge1[List[Param], ParamList]
    object Param extends ParserBridge2[Type, Ident, Param]
    object Declare extends ParserBridge3[Type, Ident, Rvalue, Declare]
    object Assign extends ParserBridge2[Lvalue, Rvalue, Assign]
    object Read extends ParserBridge1[Lvalue, Read]
    object Free extends ParserBridge1[Expr, Free]
    object Return extends ParserBridge1[Expr, Return]
    object Exit extends ParserBridge1[Expr, Exit]
    object Print extends ParserBridge1[Expr, Print]
    object Println extends ParserBridge1[Expr, Println]
    object IfThenElse extends ParserBridge3[Expr, Stmt, Stmt, IfThenElse]
    object WhileDo extends ParserBridge2[Expr, Stmt, WhileDo]
    object BeginEnd extends ParserBridge1[Stmt, BeginEnd]
    object StmtList extends ParserBridge2[Stmt, Stmt, StmtList]

    object Fst extends ParserBridge1[Lvalue, Fst]
    object Snd extends ParserBridge1[Lvalue, Snd]

    object ArrayLit extends ParserBridge1[Option[List[Expr]], ArrayLit]
    object NewPair extends ParserBridge2[Expr, Expr, NewPair]
    object Call extends ParserBridge2[Ident, Option[ArgList], Call]

    object ArgList extends ParserBridge1[List[Expr], ArgList]

    object ArrayType extends ParserBridge1[Type, ArrayType]
    object PairElemType2 extends PairElemType with ParserBridge1[Type, PairElemType]

    /* Ident Or Array Elem */
    object IdentOrArrayElem extends ParserBridge2[Ident, List[Expr], Expr]
    {
        def apply(ident: Ident, args: List[Expr]) = args match {
            case Nil => ident
            case args => ArrayElem(ident, args)
        }
    }
}