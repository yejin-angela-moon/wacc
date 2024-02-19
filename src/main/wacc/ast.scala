package wacc

import parsley.{Parsley, Result}
import parsley.Success
import java.security.Identity
import scala.collection.View.Empty
import java.security.cert.TrustAnchor

object ast {
    import parsley.generic._
    import Semantic.getValueFromTable
    import ast._

    sealed trait Expr extends Rvalue with Lvalue 

    /*
        Binary Operator
        ⟨binary-oper⟩ ::= ‘*’ | ‘/’ | ‘%’ | ‘+’ | ‘-’ | ‘>’ | ‘>=’ | ‘<’ | ‘<=’
                        | ‘==’ | ‘!=’ | ‘&&’ | ‘||’
    */
    case class Add(x: Expr, y: Expr) extends Expr
    case class Sub(x: Expr, y: Expr) extends Expr
    case class Mul(x: Expr, y: Expr) extends Expr
    case class Div(x: Expr, y: Expr) extends Expr
    case class Mod(x: Expr, y: Expr) extends Expr
    case class And(x: Expr, y: Expr) extends Expr
    case class Or(x: Expr, y: Expr) extends Expr
    case class LT(x: Expr, y: Expr) extends Expr
    case class LTE(x: Expr, y: Expr) extends Expr
    case class GT(x: Expr, y: Expr) extends Expr
    case class GTE(x: Expr, y: Expr) extends Expr
    case class E(x: Expr, y: Expr) extends Expr
    case class NE(x: Expr, y: Expr) extends Expr

    /*
        Unary Operator
        ⟨unary-oper⟩ ::= ‘!’ | ‘-’ | ‘len’ | ‘ord’ | ‘chr’
    */
    case class Not(x: Expr) extends Expr
    case class Neg(x: Expr) extends Expr
    case class Len(x: Expr) extends Expr
    case class Ord(x: Expr) extends Expr
    case class Chr(x: Expr) extends Expr

    /*
        Atom
    */
    case class IntLit(x: BigInt) extends Expr
    case class BoolLit(x: Boolean) extends Expr
    case class CharLit(x: Char) extends Expr
    case class StrLit(x: String) extends Expr
    case class Ident(x: String) extends Expr with Lvalue
    case class ArrayElem(ident: Ident, x: List[Expr]) extends Expr with Lvalue
    case class Paran(x: Expr) extends Expr
    case object PairLit extends Expr with ParserBridge0[Expr]

    /* Program */
    case class Program(funcs: List[Func], body: List[Stmt]) extends Stmt {
      override var pos: (Int, Int) = (-1, -1)
    }

    /* Function */
    case class Func(t: Type, ident: Ident, list: ParamList, body: List[Stmt]) extends Stmt {
      override var pos: (Int, Int) = (-1, -1)
    }

    /* Parameter List */
    case class ParamList(params: List[Param])

    /* Parameter */
    case class Param(t: Type, ident: Ident)

    /* Statement */
    sealed trait Stmt {
        var pos: (Int, Int)
        def setPosition(line: Int, column: Int): Unit = {
            pos = (line, column)
        }
    }

    case object Skip extends Stmt with ParserBridge0[Stmt] {
        var pos: (Int, Int) = (-1, -1)
    }

    case class Declare(t: Type, ident: Ident, rvalue: Rvalue) extends Stmt {
        override var pos: (Int, Int) = (-1, -1)
    }

    case class Assign(lvalue: Lvalue, rvalue: Rvalue) extends Stmt {
        override var pos: (Int, Int) = (-1, -1)
    }

    case class Read(lvalue: Lvalue) extends Stmt {
        override var pos: (Int, Int) = (-1, -1)
    }

    case class Free(x: Expr) extends Stmt {
        override var pos: (Int, Int) = (-1, -1)
    }

    case class Return(x: Expr) extends Stmt {
        override var pos: (Int, Int) = (-1, -1)
    }

    case class Exit(x: Expr) extends Stmt {
        override var pos: (Int, Int) = (-1, -1)
    }
    
    case class Print(x: Expr) extends Stmt {
        override var pos: (Int, Int) = (-1, -1)
    }

    case class Println(x: Expr) extends Stmt {
        override var pos: (Int, Int) = (-1, -1)
    }

    case class IfThenElse(x: Expr, s1: List[Stmt], s2: List[Stmt]) extends Stmt {
        override var pos: (Int, Int) = (-1, -1)
    }

    case class WhileDo(x: Expr, s:List[Stmt]) extends Stmt {
        override var pos: (Int, Int) = (-1, -1)
    }

    case class BeginEnd(s: List[Stmt]) extends Stmt {
        override var pos: (Int, Int) = (-1, -1)
    }

    // case class StmtList(s1: Stmt, s2: Stmt) extends Stmt {
    //     override var pos: (Int, Int) = (-1, -1)
    // }

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
            case (x, AnyType | NullType) => true

            case (NullType, x : PairType) => true
            case (NullType, x : PairElemType) => true
            case (StringType, ArrayType(CharType)) => true

            case (PairElemType1, PairType(_, _)) | (PairType(_, _), PairElemType1) => true
            case (PairElemType1, PairElemType2(_)) | (PairElemType2(_), PairElemType1) => true

            case (PairElemType2(a), PairElemType2(b)) => a:>b 
            case (ArrayType(StringType), ArrayType(ArrayType(CharType))) => false
            case (ArrayType(t1), ArrayType(t2)) => t1:>t2 && t2 :>t1
            case (PairType(p1, p2),PairType(t1, t2)) => p1:>t1 && p2:>t2  && t1:>p1 && t2:>p2
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

    object Program extends ParserBridge2[List[Func], List[Stmt], Program]
    object Func extends ParserBridge4[Type, Ident, ParamList, List[Stmt], Func]
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
    object IfThenElse extends ParserBridge3[Expr, List[Stmt], List[Stmt], IfThenElse]
    object WhileDo extends ParserBridge2[Expr, List[Stmt], WhileDo]
    object BeginEnd extends ParserBridge1[List[Stmt], BeginEnd]
    //object StmtList extends ParserBridge2[Stmt, Stmt, StmtList]

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