val binopPrecedence = mapOf(
    '<' to 10,
    '>' to 10,
    '+' to 20,
    '-' to 20,
    '*' to 40
)

@ExperimentalUnsignedTypes
class Parser(input: String) {
    private val lexer = Lexer(input)
    private val currPrecedence: Int
        get() = binopPrecedence[lexer.currentToken] ?: -1

    private fun parseNumber(): NumberExpr = NumberExpr(lexer.tokenNumber).also { lexer.next() }

    private fun parseParenExpr(): ASTBase {
        Logger.debug("Parse paren expression")
        if (lexer.currentToken != '(') error("'(' is expected")
        lexer.next()
        val res = parseExpression()
        if (lexer.currentToken != ')') error("')' is expected")
        lexer.next()
        return res
    }

    private fun parseIdentifier(): ASTBase {
        Logger.debug("Parse identifier")
        val idName = lexer.tokenIdnt
        lexer.next()
        if (lexer.currentToken != '(') return VarExpr(idName)

        lexer.next()
        val args = mutableListOf<ASTBase>()
        if (lexer.currentToken != ')') {
            while (true) {
                val arg = parseExpression()
                args += arg

                if (lexer.currentToken == ')') break

                if (lexer.currentToken != ',') error("Expected ')'")
                lexer.next()
            }
        }

        lexer.next()

        return CallExpr(idName, args)
    }

    private fun parseBinOpRight(prec: Int, left: ASTBase): ASTBase {
        Logger.debug("Parse bin op right")
        var leftPart = left
        while (true) {
            val currentPrec = currPrecedence
            if (currentPrec < prec) {
                Logger.debug("$currentPrec < $prec. Return left part of expression")
                return leftPart
            }

            val binop = lexer.currentToken
            lexer.next()

            var right = parsePrimary()

            val nextPrec = currPrecedence
            if (currentPrec < nextPrec) {
                right = parseBinOpRight(currentPrec + 1, right)
            }

            leftPart = BinaryExpr(binop, leftPart, right)
        }
    }

    private fun parseExpression(): ASTBase {
        Logger.debug("Parse expression")
        val left = parsePrimary()
        return parseBinOpRight(0, left)
    }

    private fun parsePrototype(): FunctionProto {
        Logger.debug("Parsing prototype")
        if (lexer.currentToken != Lexer.identifier) error("Identifier expected")

        val fnName = lexer.tokenIdnt
        lexer.next()

        if (lexer.currentToken != '(') error("( expected")

        val args = mutableListOf<String>()

        lexer.next()
        while (lexer.currentToken == Lexer.identifier) {
            args += lexer.tokenIdnt
            lexer.next()
        }

        if (lexer.currentToken != ')') error(") expected")

        lexer.next()
        return FunctionProto(fnName, args)
    }

    private fun parseDefinition(): Function {
        Logger.debug("Parsing definition")
        lexer.next()
        val proto = parsePrototype()

        val expression = parseExpression()
        return Function(proto, expression)
    }

    private fun parseExtern(): FunctionProto {
        Logger.debug("Parsing extern")
        lexer.next()
        return parsePrototype()
    }

    private fun parseTopLevel(): Function {
        Logger.debug("Parsing top level expression")
        val expression = parseExpression()
        val proto = FunctionProto("__anon_expr", listOf())
        return Function(proto, expression)
    }

    private fun parsePrimary(): ASTBase {
        Logger.debug("Parsing primary expression")
        return when (lexer.currentToken) {
            Lexer.identifier -> parseIdentifier()
            Lexer.number -> parseNumber()
            '(' -> parseParenExpr()
            else -> error("Unexpected token ${if (lexer.currentToken.toInt() <= 0) lexer.currentToken.toInt() else lexer.currentToken}")
        }
    }

    fun parseSequence() = sequence {
        lexer.next()
        infLoop@ while (true) {
            when (lexer.currentToken) {
                Lexer.eof -> break@infLoop
                ';' -> lexer.next()
                Lexer.def -> yield(parseDefinition() to Type.DEFINITION)
                Lexer.extern -> yield(parseExtern() to Type.EXTERN)
                else -> yield(parseTopLevel() to Type.TOP_LEVEL)
            }
        }
    }

    enum class Type {
        DEFINITION,
        EXTERN,
        TOP_LEVEL
    }
}
