package wacc

import parsley.{Parsley, Result}
import parsley.expr.chain
import parsley.Parsley.atomic
import parsley.combinator.{sepBy, sepBy1, countSome}
import parsley.expr.{precedence, Ops, SOps, InfixL, InfixR, InfixN, Prefix, Atoms}
import parsley.syntax.character.charLift
import parsley.character.digit
import lexer.implicits.implicitSymbol
import lexer.{INTEGER, BOOL, STRING, CHAR, IDENT, fully}
import ast._

object parser {

    private lazy val `<int-liter>` : Parsley[IntLit]    = INTEGER.map(x => IntLit(x))
    private lazy val `<bool-liter>`: Parsley[BoolLit]   = BOOL.map(x => BoolLit(x))
    private lazy val `<char-liter>`: Parsley[CharLit]   = CHAR.map(x => CharLit(x))
    private lazy val `<str-liter>` : Parsley[StrLit]    = STRING.map(x => StrLit(x))
    private lazy val `<pair-liter>`: Parsley[Pair]      = ???
    private lazy val `<ident>`     : Parsley[Ident]     = IDENT.map(x => Ident(x))
    private lazy val `<array-elem>`: Parsley[ArrayElem] = ???

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
        `<array-alem>` 


    private lazy val `<expr>`: Parsley[Expr] = precedence {
        SOps(InfixR)(Or     from "||") :+
        SOps(InfixR)(And    from "&&") :+
        SOps(InfixN)(LT     from "<",   LTE     from "<=",
                     GT     from ">",   GTE     from ">=",
                     E      from "==",  NE      from "!=") :+
        SOps(InfixL)(Mod    from "%") :+
        SOps(InfixL)(Add    from "+",   Sub     from "-") :+
        SOps(Prefix)(Neg    from "-") :+
        SOps(Prefix)(Not    from "!") :+
        SOps(InfixL)(Mul    from "*",   Div     from "/") :+
        SOps(Prefix)(Len    from "len", Ord     from "ord",
                     Chr    from "chr") :+
        Atoms(`<int-liter>`)
    }
    //def parse(expr: String): Result[+Err, +A] =  ???

    private val parser = fully(expr)

    private val add = (x: BigInt, y: BigInt) => x + y
    private val sub = (x: BigInt, y: BigInt) => x - y

    private lazy val expr: Parsley[BigInt] =
        chain.left1(INTEGER | "(" ~> expr <~ ")")(
            ("+" as add) | ("-" as sub)
        )
}
