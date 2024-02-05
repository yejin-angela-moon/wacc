package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions._
import parsley.token.predicate.{Unicode, Basic}

import parsley.character.newline

object lexer{

    private val desc = LexicalDesc.plain.copy(
        NameDesc.plain.copy(
            identifierStart = Basic(c => Character.isLetter(c) || c == '_'),
            identifierLetter = Basic(c => Character.isLetterOrDigit(c) || c == '_'),
        ),
        SymbolDesc.plain.copy(
            hardKeywords = Set("if", "then", "else","fi", "skip", "read", "free",
                            "return", "exit", "print", "println", "while", "do", "done", "begin",
                            "end", "is", "true", "false", "int", "bool", "char", "string","newpair",
                            "fst", "snd", "call", "null"),
            hardOperators = Set("$", "||", "&&", "<", "<=", ">", ">=", "==", "!=", "/",
                            "+", "-", "*", "%", "=", "!"),
        ),
        numeric.NumericDesc.plain,
        text.TextDesc.plain.copy(
            escapeSequences = text.EscapeDesc.plain.copy(
                literals = Set('\'', '\"','\\'),
                mapping = Map("0" -> 0x00,
                            "b" -> 0x08,
                            "t" -> 0x09,
                            "n" -> 0x0a,
                            "f" -> 0x0c,
                            "r" -> 0x0d)
            ),
            graphicCharacter = Basic(c => {
                !Set('\'', "\\", '\"').contains(c) && c >= ' '}
            )
        ),
        SpaceDesc.plain.copy(
            lineCommentStart = "#",
            space = Basic(Character.isWhitespace),
        )
    )
    private val lexer = new Lexer(desc)

    val INTEGER = lexer.lexeme.integer.decimal32[BigInt]
    val IDENT = lexer.lexeme.names.identifier
    val STRING = lexer.lexeme.string.ascii
    val CHAR = lexer.lexeme.character.ascii
    val BOOL = ((lexer.lexeme.symbol.apply("true", "true") #> true) <|>
    (lexer.lexeme.symbol.apply("false", "false") #> false))
    val NEWLINE = lexer.lexeme(newline).void

    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

    val implicits = lexer.lexeme.symbol.implicits
}
