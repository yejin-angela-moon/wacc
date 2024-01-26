package wacc

import parsley.Parsley


object lexer {
    import parsley.token.Lexer
    import parsley.token.descriptions._
    import parsley.token.predicate.{Unicode, Basic}
    import parsley.character.newline

    private val desc = LexicalDesc.plain.copy(
        NameDesc.plain.copy(
            identifierStart = Unicode(c => Character.isLetter(c) || c == '-'),
            identifierLetter = Unicode(c => Character.isLetterOrDigit(c) || c == '_' || c == '\''),
        ),
        SymbolDesc.plain.copy(
            hardKeywords = Set("if", "then", "else","fi", "skip", "read", "free",
                            "return", "exit", "print", "println", "while", "do", "done", "begin",
                            "end", "is", "true", "false", "int", "bool", "char", "string","newpair",
                            "fst", "snd", "call", "null"),
            hardOperators = Set("$", "||", "&&", "<", "<=", ">", ">=", "==", "!=", "/",
                            "+", "-", "*", "%"),
        ),
        NumbericDesc = NumbericDesc.plain
        TextDesc.plain.copy(
            escapeSequences = EscapeDesc.copy(
                escBegin = '\\'
                mappings = Map("0'" -> 0x00,
                            "b" -> 0x08,
                            "t" -> 0x09,
                            "n" -> 0x0a,
                            "f" -> 0x0c,
                            "r" -> 0x0d,
                            '"' -> 0x22,
                            "'" -> 0x27,
                            "\'" -> 0x5c,)
            )
            graphicCharacter = predicate.Basic(c => {
                !Set('\'', "'", '"') && c >= ' '
                }
            )
        ),
        SpaceDesc.plain.copy(
            lineCommentStart = "#",
            space = Basic(c => c == ' '),
        )
    )
    private val lexer = new Lexer(desc)

    val INTEGER = lexer.lexeme.integer.decimal32.label("Integer lit")
    val IDENT = lexer.lexeme.names.identifier
    val STRING = lexer.lexeme.string.ascii
    val CHAR = lexer.lexeme.character.ascii
    val NEWLINE = lexer.lexeme(newline).void

    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

    val implicits = lexer.lexeme.symbol.implicits
}
