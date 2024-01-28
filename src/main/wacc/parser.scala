package wacc

import parsley.{Parsley, Result}
import parsley.expr.chain
import parsley.Parsley.atomic
import parsley.combinator.{sepBy, sepBy1, countSome}
import parsley.expr.{precedence, SOps, InfixL, InfixR, InfixN, Prefix, Atoms}

import lexer.implicits.implicitSymbol
import lexer.{integer, fully}
import ast._

object parser {

    private lazy val `<int-liter>`: Parsley[IntLit]     = atomic(Int(INTEGER))
    private lazy val `<bool-iter>`: Parseley[BoolLit]   = atomic(BoolLiter(BOOL))
    private lazy val `<char-liter>`: Parsley[CharLit]   = atomic(Char(CHAR))
    private lazy val `<str-liter>`: Parsley[IntLit]     = atomic(StrLit(St))
    private lazy val `<pair-liter>`: Parsley[Pair]      = ?

    // ⟨atom⟩ ::= ⟨int-liter⟩ | ⟨bool-liter⟩
    // | ⟨char-liter⟩ | ⟨str-liter⟩
    // | ⟨pair-liter⟩ | ⟨ident⟩
    // | ⟨array-elem⟩ | ‘(’ ⟨expr⟩ ‘)’

    private lazy val `<atom>`: Parsley[Expr] =
        `<int-liter>`   <|>
        `<bool-liter>`  <|>
        `<char-liter>`  <|>
        `<str-liter>`   <|>
        `<pair-liter>`  <|>
        `<ident>`       <|>
        `<array-alem>`  <|>

    private lazy val `<expr>`: Parseley[Expr] = precedence {
        SOps(InfixR)(Or     from "||") +:
        SOps(InfixR)(And    from "&&") +:
        SOps(InfixN)(LT     from "<",   LTE     from "<=",
                     GT     from ">",   GTE     from ">=",
                     E      from "==",  NE      from "!=") +:
        SOps(InfixL)(Mod    from "%") +:
        SOps(InfixL)(Add    from "+",   Sub     from "-") +:
        SOps(Prefix)(Neg    from "-") +:
        Sops(Prefix)(Not    from "!") +:
        SOps(InfixL)(Mul    from "*",   Div     from "/") +:
        SOps(Prefix)(Len    from "len", Ord     from "ord",
                     Chr    from "chr") +:
        `<atom>`
    }

    private val parser = fully(expr)

    private val add = (x: BigInt, y: BigInt) => x + y
    private val sub = (x: BigInt, y: BigInt) => x - y

    private lazy val expr: Parsley[BigInt] =
        chain.left1(integer | "(" ~> expr <~ ")")(
            ("+" as add) | ("-" as sub)
        )
}
