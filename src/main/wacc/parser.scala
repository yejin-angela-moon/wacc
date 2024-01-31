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
    lazy val `<int-liter>` : Parsley[IntLit]    = IntLit(INTEGER)
    lazy val `<bool-liter>`: Parsley[BoolLit]   = BoolLit(BOOL)
    lazy val `<char-liter>`: Parsley[CharLit]   = CharLit(CHAR)
    lazy val `<str-liter>` : Parsley[StrLit]    = StrLit(STRING)
    // lazy val `<pair-liter>`: Parsley[PairLit]   = PairLit()
    lazy val `<ident>`     : Parsley[Ident]     = Ident(IDENT)
    // lazy val `<array-elem>`: Parsley[ArrayElem] = ???

    // ⟨atom⟩ ::= ⟨int-liter⟩ | ⟨bool-liter⟩
    // | ⟨char-liter⟩ | ⟨str-liter⟩
    // | ⟨pair-liter⟩ | ⟨ident⟩
    // | ⟨array-elem⟩ | ‘(’ ⟨expr⟩ ‘)’

    lazy val `<atom>`: Parsley[Expr] =
        `<int-liter>`   <|> 
        `<bool-liter>`  <|>
        `<char-liter>`  <|>
        `<str-liter>`   <|>
        // <|>
        // `<pair-liter>`  <|>
        // `<ident>`
        // <|>  `<array-alem>`


     lazy val `<expr>`: Parsley[Expr] = precedence (
        SOps(InfixL)(Add    from "+",   Sub     from "-") +:
        SOps(InfixL)(Mul    from "*",   Div     from "/") +:
        SOps(InfixL)(Mod    from "%") +:
        SOps(InfixR)(And    from "&&") +:
        SOps(InfixR)(Or     from "||") +:
        SOps(InfixN)(LT     from "<",   LTE     from "<=",
                     GT     from ">",   GTE     from ">=",
                     E      from "==",  NE      from "!=") +:
        SOps(Prefix)(Not    from "!") +:
        SOps(Prefix)(Neg    from "-") +:
        SOps(Prefix)(Len    from "len", Ord     from "ord",
                     Chr    from "chr") +:
        Atoms(`<atom>`)
    )
}

object parser {
    import parsley.Parsley
    import parsley.{Result, Success, Failure}
    import Expression._
    import lexer._

    def parse(expr: String): Result[String, Expr] = {
        fully(`<expr>`).parse(expr) match {
            case Success(result) => Success(result)
            case Failure(error) => Failure(error.toString)
        }
    }

    // private val parser = fully(expr)

    // private val add = (x: BigInt, y: BigInt) => x + y
    // private val sub = (x: BigInt, y: BigInt) => x - y

    // private lazy val expr: Parsley[BigInt] =
    //     chain.left1(INTEGER | "(" ~> expr <~ ")")(
    //         ("+" as add) | ("-" as sub) 
    //     )
}
