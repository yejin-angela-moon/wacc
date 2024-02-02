package wacc

import parsley.{Parsley, Result}
import parsley.expr.chain
import parsley.Parsley.{atomic, many, some}
import parsley.combinator.{sepBy, sepBy1, countSome, manyN}
import parsley.expr.{precedence, SOps, InfixL, InfixR, InfixN, Prefix, Atoms}
import parsley.syntax.character.charLift
import parsley.character.digit
import parsley.combinator.option
import lexer.implicits.implicitSymbol
import lexer.{INTEGER, BOOL, STRING, CHAR, IDENT, fully}
import ast._
import ExpressionParser._
import TypeParser._
import StatementParser._
import TypeParser._
import FuncParser._
import LvalueParser._
import RvalueParser._

object ExpressionParser {
    lazy val `<int-liter>`  = IntLit(INTEGER)
    lazy val `<bool-liter>` = BoolLit(BOOL)
    lazy val `<char-liter>` = CharLit(CHAR)
    lazy val `<str-liter>`  = StrLit(STRING)
    lazy val `<ident>`      = Ident(IDENT)
    lazy val `<array-elem>` = ArrayElem(`<ident>`, some("(" ~> `<expr>` <~ ")"))

    lazy val `<atom>`: Parsley[Expr] =
        (`<int-liter>`  |
        `<bool-liter>`  |
        `<char-liter>`  |
        `<str-liter>`   |
        PairLit <# "null" |
        `<ident>` | `<array-elem>`)


    lazy val `<expr>`: Parsley[Expr] = precedence {
        SOps(InfixR)(Or     from "||") +:
        SOps(InfixR)(And    from "&&") +:
        SOps(InfixN)(LT     from "<",   LTE     from "<=",
                     GT     from ">",   GTE     from ">=",
                     E      from "==",  NE      from "!=") +:
        SOps(InfixL)(Mod    from "%") +:
        SOps(InfixL)(Add    from "+",   Sub     from "-") +:
        SOps(Prefix)(Neg    from "-") +:
        SOps(Prefix)(Not    from "!") +:
        SOps(InfixL)(Mul    from "*",   Div     from "/") +:
        SOps(Prefix)(Len    from "len", Ord     from "ord",
                     Chr    from "chr") +:
        Atoms(`<int-liter>`)
    }
}

object TypeParser {
    lazy val `<type>` =
        (`<base-type>` <|>
        `<array-type>`
        )


    lazy val `<base-type>` = (IntType <# "int" |
                             BoolType <# "bool" |
                             CharType <# "char" |
                             StringType <# "string")

    lazy val `<array-type>` = ArrayType <# "[]"

    lazy val `<pair-type>` = PairType("pair" ~> "(" ~> `<pair-elem-type>`, "," ~>
        `<pair-elem-type>` <~ ")")

    lazy val `<pair-elem-type>` = (`<base-type>` | `<array-type>` | `<pair-type>`)
}

object LvalueParser {
    lazy val `<lvalue>` = (
        `<ident>` <|> `<array-elem>` <|> `<pair-elem>`
    )
}

object RvalueParser {
    lazy val `<rvalue>` = (
        `<expr>` | `<array-liter>` |
        NewPair("newpair" ~> "(" ~> `<expr>` <~ ",", `<expr>` <~ ")") |
        `<pair-elem>` |
        Call("call" ~> `<ident>`, "(" ~> option(`<arg-list>`) <~ ")")
    )
}

object StatementParser {

    lazy val `<stmt>`: Parsley[Stmt] = (
        Skip from "skip" |
        Declare(`<type>`, `<ident>`, "=" ~> `<rvalue>`) |
        Assign(`<lvalue>`, "=" ~> `<rvalue>`) |
        Read("read" ~> `<lvalue>`) |
        Free("free" ~> `<expr>`) |
        Return("return" ~> `<expr>` ) |
        Exit("exit" ~> `<expr>`) |
        Print("print" ~> `<expr>`) |
        Println("println" ~> `<expr>`) |
        IfThenElse("if" ~> `<expr>`, "then" ~> `<stmt>`, "else" ~> `<stmt>` <~ "fi") |
        WhileDo("while" ~> `<expr>`, "do" ~> `<stmt>` <~ "done") |
        BeginEnd("begin" ~> `<stmt>` <~ "end") |
        StmtList(`<stmt>`, ";" ~> `<stmt>`)
    )

}

object ProgramParser {
    lazy val `<prog>` = fully(Program("begin" ~> many(`<func>`), `<stmt>` <~ "end"))
}

object FuncParser {
    lazy val `<func>` = Func(`<type>`, `<ident>`,
                        "(" ~> `<param-list>` <~ ")",
                        "is" ~> `<stmt>` <~ "end")

    lazy val `<param-list>` = ParamList(sepBy(`<param>`, ","))
    lazy val `<param>` = Param(`<type>`, `<ident>`)
}

object parser {

    // def parse(expr: String): Result[+Err, +A] =  ???

    val parser = fully(expr)

    val add = (x: BigInt, y: BigInt) => x + y
    val sub = (x: BigInt, y: BigInt) => x - y

    lazy val expr: Parsley[BigInt] =
        chain.left1(INTEGER | "(" ~> expr <~ ")")(
            ("+" as add) | ("-" as sub)
        )
}
