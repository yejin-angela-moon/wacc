package wacc

import parsley.{Parsley, Result}
import parsley.expr.chain
import parsley.Parsley.atomic
import parsley.combinator.{sepBy, sepBy1, countSome}
import parsley.expr.{precedence, SOps, InfixL, InfixR, InfixN, Prefix, Atoms}
import parsley.syntax.character.charLift
import parsley.character.digit
import lexer.implicits.implicitSymbol
import lexer.{INTEGER, BOOL, STRING, CHAR, IDENT, fully}
import ast._

object Expression {
    private lazy val `<int-liter>` : Parsley[IntLit]    = IntLit(INTEGER)
    private lazy val `<bool-liter>`: Parsley[BoolLit]   = BoolLit(BOOL)
    private lazy val `<char-liter>`: Parsley[CharLit]   = CharLit(CHAR)
    private lazy val `<str-liter>` : Parsley[StrLit]    = StrLit(STRING)
    // private lazy val `<pair-liter>`: Parsley[Null]      = Null
    // private lazy val `<ident>`     : Parsley[Ident]     = ???
    // private lazy val `<array-elem>`: Parsley[ArrayElem] = ???

    // ⟨atom⟩ ::= ⟨int-liter⟩ | ⟨bool-liter⟩
    // | ⟨char-liter⟩ | ⟨str-liter⟩
    // | ⟨pair-liter⟩ | ⟨ident⟩
    // | ⟨array-elem⟩ | ‘(’ ⟨expr⟩ ‘)’

    private lazy val `<atom>`: Parsley[Expr] =
        `<int-liter>`   <|>
        `<bool-liter>`  <|>
        `<char-liter>`  <|>
        `<str-liter>`
        // <|>
        // `<pair-liter>`  <|>
        // `<ident>`
        // <|>  `<array-alem>`


    private lazy val `<expr>`: Parsley[Expr] = precedence {
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

object parser {

    def parse(expr: String): Result[String, BigInt] = parser.parse(expr)

    private val parser = fully(expr)

    private val add = (x: BigInt, y: BigInt) => x + y
    private val sub = (x: BigInt, y: BigInt) => x - y

    private lazy val expr: Parsley[BigInt] =
        chain.left1(INTEGER | "(" ~> expr <~ ")")(
            ("+" as add) | ("-" as sub)
        )
}
