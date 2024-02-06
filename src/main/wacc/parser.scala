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
import scala.util.chaining
import java.io._

object ExpressionParser {

    /*
        ⟨atom⟩ ::= ⟨int-liter⟩ | ⟨bool-liter⟩ | ⟨char-liter⟩ | ⟨str-liter⟩ | ⟨pair-liter⟩
                | ⟨ident⟩ | ⟨array-elem⟩ | ‘(’ ⟨expr⟩ ‘)’
    */

    lazy val `<int-liter>` = IntLit(INTEGER)
    lazy val `<bool-liter>` = BoolLit(BOOL)
    lazy val `<char-liter>` = CharLit(CHAR)
    lazy val `<str-liter>` = StrLit(STRING)
    lazy val `<ident>` = Ident(IDENT)

    lazy val `<atom>`: Parsley[Expr] =
        atomic(`<int-liter>`  |
        `<bool-liter>`  |
        `<char-liter>`  |
        `<str-liter>`   |
        PairLit <# "null" |
        IdentOrArrayElem(`<ident>`,  many("[" ~> `<expr>` <~ "]")) |
        "(" ~> `<expr>` <~ ")"
        )

    /* ⟨expr⟩ ::= ⟨unary-oper⟩ ⟨expr⟩ | ⟨expr⟩ ⟨binary-oper⟩ ⟨expr⟩ | ⟨atom⟩ */
     lazy val `<expr>`: Parsley[Expr] = atomic(precedence (
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
        Atoms(`<atom>`)
    ))
}

object TypeParser {

    /* ⟨type⟩ ::= ⟨base-type⟩ | ⟨array-type⟩ | ⟨pair-type⟩ */
    lazy val `<type>` : Parsley[Type] = atomic(
        `<array-type>` <|>
        `<base-type>` <|>
        `<pair-type>`
    )

    /* ⟨base-type⟩ ::= ‘int’ | ‘bool’ | ‘char’ | ‘string’ */
    lazy val `<base-type>` : Parsley[BaseType] =
        atomic(IntType <# "int" |
        BoolType <# "bool"       |
        CharType <# "char"       |
        StringType <# "string"   )

    /* ⟨array-type⟩ ::= ⟨type⟩ ‘[’ ‘]’ */
    lazy val `<array-type>` : Parsley[Type] = chain.postfix(
        `<base-type>` | `<pair-type>`)(ArrayType from "[]")

    /* ⟨pair-type⟩ ::= ‘pair’ ‘(’ ⟨pair-elem-type⟩ ‘,’ ⟨pair-elem-type⟩ ‘)’ */
    lazy val `<pair-type>` : Parsley[PairType] = atomic(
        PairType("pair" ~> "(" ~> `<pair-elem-type>`, "," ~> `<pair-elem-type>` <~ ")"))

    /* ⟨pair-elem-type⟩ ::= ⟨base-type⟩ | ⟨array-type⟩ | ‘pair’ */
    lazy val `<pair-elem-type>` : Parsley[PairElemType] = atomic(
        PairElemType2(`<array-type>` | `<base-type>`) |
        PairElemType1 <# "pair")
}


object StatementParser {

    /* ⟨program⟩ ::= ‘begin’ ⟨func⟩* ⟨stmt⟩ ‘end’ */
    lazy val `<prog>` : Parsley[Program] = atomic(
        Program("begin" ~> many(`<func>`), `<stmt>` <~ "end"))

    /* ⟨func⟩ ::= ⟨type⟩ ⟨ident⟩ ‘(’ ⟨param-list⟩? ‘)’ ‘is’ ⟨stmt⟩ ‘end’ */
    lazy val `<func>` : Parsley[Func] = atomic(
        Func(`<type>`, `<ident>`, "(" ~> `<param-list>` <~ ")",
                "is" ~> `<stmt>` <~ "end"))

    /* <param-list> ::= <param> (',' <param>)* */
    lazy val `<param-list>` : Parsley[ParamList] = atomic(ParamList(sepBy(`<param>`, ",")))

    /* <param> ::= <type> <ident> */
    lazy val `<param>` : Parsley[Param] = atomic(Param(`<type>`, `<ident>`))

    /*
        ⟨stmt⟩ ::= ‘skip’ | ⟨type⟩ ⟨ident⟩ ‘=’ ⟨rvalue⟩ | ⟨lvalue⟩ ‘=’ ⟨rvalue⟩
            | ‘read’ ⟨lvalue⟩ | ‘free’ ⟨expr ⟩ | ‘return’ ⟨expr ⟩ | ‘exit’ ⟨expr ⟩
            | ‘print’ ⟨expr ⟩ | ‘println’ ⟨expr ⟩ | ‘if’ ⟨expr ⟩ ‘then’ ⟨stmt ⟩ ‘else’ ⟨stmt ⟩ ‘fi’
            | ‘while’ ⟨expr ⟩ ‘do’ ⟨stmt ⟩ ‘done’ | ‘begin’ ⟨stmt ⟩ ‘end’ | ⟨stmt ⟩ ‘;’ ⟨stmt ⟩
    */
    lazy val `<stmt>`: Parsley[Stmt] = atomic(chain.left1(
        (Skip from "skip") |
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
        BeginEnd("begin" ~> `<stmt>` <~ "end")
    )(StmtList from ";"))

    /* <lvalue> ::= ⟨ident⟩ | ⟨array-elem⟩ | ⟨pair-elem⟩ */
    lazy val `<lvalue>` : Parsley[Lvalue] = atomic(
        IdentOrArrayElem(`<ident>`,  many("[" ~> `<expr>` <~ "]")) <|> `<pair-elem>`)

    /* <rvalue> ::= ⟨expr⟩ | ⟨array-liter ⟩ | ‘newpair’ ‘(’ ⟨expr ⟩ ‘,’ ⟨expr ⟩ ‘)’ | ⟨pair-elem⟩
                | ‘call’ ⟨ident⟩ ‘(’ ⟨arg-list⟩? ‘)’ */
    lazy val `<rvalue>` : Parsley[Rvalue] = atomic(
        `<array-liter>` |
        `<expr>` |
        `<pair-elem>` |
        NewPair("newpair" ~> "(" ~> `<expr>` <~ ",", `<expr>` <~ ")") |
        Call("call" ~> `<ident>`, "(" ~> option(`<arg-list>`) <~ ")")
    )

    /* <pair-elem> ::= ‘fst’ ⟨lvalue⟩ | ‘snd’ ⟨lvalue⟩ */
    lazy val `<pair-elem>` : Parsley[PairElem] = atomic(
       Fst("fst" ~> `<lvalue>`) <|>
       Snd("snd" ~> `<lvalue>`))

    /* <array-liter> ::= ‘[’ ( ⟨expr⟩ (‘,’ ⟨expr⟩)* )? ‘]’ */
    lazy val `<array-liter>` : Parsley[ArrayLit] = atomic(
        ArrayLit( "[" ~> option(sepBy1(`<expr>`, ",")) <~ "]")
        )

    /* <arg-list> ::=  ⟨expr⟩ (‘,’ ⟨expr⟩ )* */
    lazy val `<arg-list>` : Parsley[ArgList] = atomic(ArgList(sepBy1(`<expr>`, ",")))

}

object parser {
    import parsley.Parsley
    import parsley.{Result, Success, Failure}
    import scala.util.{Try, Success, Failure}
    import lexer._

    /*
        Parse the content in the file. If success return the parsed statement,
        otherwise return the failure message.
        Exit 0 if the the filename is invalid
    */
    def parse(prog: String): Result[String, Stmt] = {
        fully(`<prog>`).parseFile(new File(prog)) match {
            case scala.util.Success(ret) => ret match {
                case parsley.Success(stmt) => parsley.Success(stmt)
                case parsley.Failure(msg) => parsley.Failure(msg)
            }
            case scala.util.Failure(msg) => {
                System.exit(0)
                return parsley.Failure(msg.getMessage)
            }
        }
    }

}