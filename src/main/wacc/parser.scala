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

object ExpressionParser {
    lazy val `<int-liter>` = IntLit(INTEGER)
    lazy val `<bool-liter>` = BoolLit(BOOL)
    lazy val `<char-liter>` = CharLit(CHAR)
    lazy val `<str-liter>` = StrLit(STRING)
    lazy val `<ident>` = Ident(IDENT)
    lazy val `<array-elem>` = atomic(ArrayElem(`<ident>`, some("[" ~> `<expr>` <~ "]")))

    lazy val `<atom>`: Parsley[Expr] =
        atomic(`<int-liter>`  |
        `<bool-liter>`  |
        `<char-liter>`  |
        `<str-liter>`   |
        PairLit <# "null" |
        `<ident>` | `<array-elem>` |
        "(" ~> `<expr>` <~ ")"
        )

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

    lazy val `<type>` : Parsley[Type] = atomic(
        `<array-type>` <|>
        `<base-type>` <|>
        `<pair-type>` 
    )
        
    lazy val `<base-type>` : Parsley[BaseType] = 
        atomic(IntType <# "int" |
        BoolType <# "bool"       |
        CharType <# "char"       |
        StringType <# "string"   )

    // TO BE MODIFIED 
    lazy val `<array-type>` : Parsley[Type] = chain.postfix(`<base-type>` | `<pair-type>`)(ArrayType from "[]")
    // lazy val `<array-type>` : Parsley[ArrayType[Type]] ?

    lazy val `<pair-type>` : Parsley[PairType] = atomic(PairType("pair" ~> "(" ~> `<pair-elem-type>`, "," ~> `<pair-elem-type>` <~ ")"))

    lazy val `<pair-elem-type>` : Parsley[PairElemType] = atomic(         
        PairElemType2(`<array-type>`) | 
        PairElemType2(`<base-type>`)  |
         PairElemType1 <# "pair")
}


object StatementParser {

    lazy val `<prog>` : Parsley[Program] = atomic(Program("begin" ~> many(`<func>`), `<stmt>` <~ "end"))

    lazy val `<func>` : Parsley[Func] = atomic(Func(`<type>`, `<ident>`,
                        "(" ~> `<param-list>` <~ ")",
                        "is" ~> `<stmt>` <~ "end"))

    lazy val `<param-list>` : Parsley[ParamList] = atomic(ParamList(sepBy(`<param>`, ",")))
    lazy val `<param>` : Parsley[Param] = atomic(Param(`<type>`, `<ident>`))

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

    lazy val `<lvalue>` : Parsley[Lvalue] = atomic(        
        `<ident>`      <|> 
        `<array-elem>` <|>
        `<pair-elem>`)

    lazy val `<rvalue>` : Parsley[Rvalue] = atomic(
        `<array-liter>` |
        `<expr>` | 
        `<pair-elem>` |
        NewPair("newpair" ~> "(" ~> `<expr>` <~ ",", `<expr>` <~ ")") |
        Call("call" ~> `<ident>`, "(" ~> option(`<arg-list>`) <~ ")")
    )


    lazy val `<pair-elem>` : Parsley[PairElem] = atomic(
       Fst("fst" ~> `<lvalue>`) <|>
       Snd("snd" ~> `<lvalue>`))

    lazy val `<array-liter>` : Parsley[ArrayLit] = atomic(
        ArrayLit( "[" ~> option(sepBy1(`<expr>`, ",")) <~ "]")
        )

    lazy val `<arg-list>` : Parsley[ArgList] = atomic(ArgList(sepBy1(`<expr>`, ",")))

}

object parser {
    import parsley.Parsley
    import parsley.{Result, Success, Failure}
    import lexer._

    // def parse(expr: String): Result[String, Expr] = {
    //     fully(`<expr>`).parse(expr) match {
    //         case Success(result) => Success(result)
    //         case Failure(error) => Failure(error.toString)
    //     }
    // }

    def parse(prog: String): Result[String, Stmt] = {
        fully(`<prog>`).parse(prog) match {
            case Success(result) => Success(result)
            case Failure(error) => Failure(error)
        }
    }

    // def parse(typ: String): Result[String, Type] = {
    //     fully(`<type>`).parse(typ) match {
    //         case Success(result) => Success(result)
    //         case Failure(error) => Failure(error.toString)
    //     }
    // }
}
