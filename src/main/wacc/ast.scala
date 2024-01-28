object ast {
    import parsley.generic._

    sealed trait Expr

    //Binary Operator
    case class Div(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
    case class Mod(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
    case class Add(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
    case class Mul(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
    case class Sub(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
    case class GT(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
    case class GTE(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
    case class LT(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
    case class LTE(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
    case class E(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
    case class NE(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
    case class And(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr
    case class Or(x: Expr, y: Expr)(val pos: (Int, Int)) extends Expr

    //Unary Operator
    case class Not(x: Expr)(val pos: (Int, Int)) extends Expr
    case class Neg(x: Expr)(val pos: (Int, Int)) extends Expr
    case class Len(x: Expr)(val pos: (Int, Int)) extends Expr
    case class Ord(x: Expr)(val pos: (Int, Int)) extends Expr
    case class CharLit(x: Expr)(val pos: (Int, Int)) extends Expr

    //Atom
    case class IntLit(x: BigInt) extends Expr
    case class BoolLit(x: Bool) extends Expr
    case class CharLit(x: Char) extends Expr
    case class StrLit(x: String) extends Expr
    case class PairLit(x: BigInt) extends Expr
    case class Ident(x: String) extends Expr
    case class ArrayElem(x: Expr)(val pos: (Int, Int)) extends Expr

    sealed trait Stmt
    case class Program(func: Func, body: Stmt) extends Stmt
    case class Func(t: Type, ident: Ident, list: ParamList, body: Stmt) extends Stmt
    case class ParamList(params: List[Param]) extends Stmt
    case class Param(t: Type, ident: Ident) extends Stmt
    case class Skip() extends Stmt
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
    case class SemiCol(s1: Stmt, s2: Stmt) extends Stmt

    case class Lvalue(x: Expr) extends Stmt
    case class Rvalue(x: Expr) extends Stmt

    sealed trait Type

    sealed trait

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
    object CharLit extends ParserBridge1[Expr, Expr]
    object IntLit extends ParserBridge1[Expr, Expr]
    object BoolLit extends ParserBridge1[Expr, Expr]
    object Char extends ParserBridge1[Expr, Expr]
    object StrLit extends ParserBridge1[Expr, Expr]
    object PairLit extends ParserBridge1[Expr, Expr]
    object Ident extends ParserBridge1[Expr, Expr]
    object ArrayElem extends ParserBridge1[Expr, Expr]
}