package wacc

object ast {
    import parsley.generic._

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
    case class Program(funcs: List[Func], body: Stmt) extends Stmt

    /* Function */
    case class Func(t: Type, ident: Ident, list: ParamList, body: Stmt)

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

    case object AnyType extends Type with PairElemType

    /* Pair Type */
    case class PairType(p1: PairElemType, p2: PairElemType) extends Type

    /* Pair Elem Type */
    sealed trait PairElemType
    case object PairElemType1 extends PairElemType with ParserBridge0[PairElemType]
    case class  PairElemType2(t: Type) extends PairElemType
    case class ArrayType(t: Type) extends Type with PairElemType

    /* Base Type */
    sealed trait BaseType extends Type with PairElemType
    case object IntType extends BaseType with ParserBridge0[BaseType]
    case object BoolType extends BaseType with ParserBridge0[BaseType]
    case object CharType extends BaseType with ParserBridge0[BaseType]
    case object StringType extends BaseType with ParserBridge0[BaseType]

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