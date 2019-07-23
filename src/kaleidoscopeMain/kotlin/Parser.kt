val binopPrecedence = mutableMapOf(
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

    private fun parseUnary(): ASTBase {
        val currentToken = lexer.currentToken
        Logger.debug("Parse unary - $currentToken")
        if (currentToken.isTok() || currentToken == '(' || currentToken == ',') {
            return parsePrimary()
        }

        lexer.next()
        val operand = parseUnary()
        return UnaryExpr(currentToken, operand)
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

            var right = parseUnary()

            val nextPrec = currPrecedence
            if (currentPrec < nextPrec) {
                right = parseBinOpRight(currentPrec + 1, right)
            }

            leftPart = BinaryExpr(binop, leftPart, right)
        }
    }

    private fun parseExpression(): ASTBase {
        Logger.debug("Parse expression")
        val left = parseUnary()
        return parseBinOpRight(0, left)
    }

    private fun parsePrototype(): FunctionProto {
        Logger.debug("Parsing prototype")

        var precedence = 30u
        val kind: FunctionProto.Type
        val fnName: String

        when (lexer.currentToken) {
            Lexer.identifier -> {
                Logger.debug("Prototype identifier")
                fnName = lexer.tokenIdnt
                kind = FunctionProto.Type.IDENTIFIER
                lexer.next()
            }
            Lexer.unary -> {
                Logger.debug("Prototype unary")
                lexer.next()
                if (lexer.currentToken.isTok()) error("Expected unary operator")
                fnName = "unary${lexer.currentToken}"
                kind = FunctionProto.Type.UNARY
                lexer.next()
            }
            Lexer.binary -> {
                Logger.debug("Prototype binary")
                lexer.next()
                if (lexer.currentToken.isTok()) error("Expected binary operator")
                fnName = "binary${lexer.currentToken}"
                kind = FunctionProto.Type.BINARY
                lexer.next()
                if (lexer.currentToken == Lexer.number) {
                    if (lexer.tokenNumber !in 1..100) error("Precedence must be 1..100")
                    precedence = lexer.tokenNumber.toUInt()
                    lexer.next()
                }
            }
            else -> error("Expected function name in prototype")
        }


        if (lexer.currentToken != '(') error("( expected")

        val args = mutableListOf<String>()

        lexer.next()
        while (lexer.currentToken == Lexer.identifier) {
            args += lexer.tokenIdnt
            Logger.debug("Add argument: ${lexer.tokenIdnt}")
            lexer.next()
        }

        if (lexer.currentToken != ')') error(") expected")

        lexer.next()

        if (kind != FunctionProto.Type.IDENTIFIER && kind.argNum != args.size) error("Wrong number of arguments")
        return FunctionProto(fnName, args, kind != FunctionProto.Type.IDENTIFIER, precedence)
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
        val proto = FunctionProto("__anon_expr", listOf(), false, 30u)
        return Function(proto, expression)
    }

    private fun parseIf(): ASTBase {
        lexer.next()
        val condition = parseExpression()

        if (lexer.currentToken != Lexer.then_token) error("Then expected")

        lexer.next()
        val then = parseExpression()

        if (lexer.currentToken != Lexer.else_token) error("Else expected")

        lexer.next()
        val elseCode = parseExpression()
        return IfExprAst(condition, then, elseCode)
    }

    private fun parseFor(): ASTBase {
        lexer.next()

        if (lexer.currentToken != Lexer.identifier) error("Identifier expected. Got ${lexer.currentToken}")

        val identifier = lexer.tokenIdnt
        lexer.next()

        if (lexer.currentToken != '=') error("= expected. Got ${lexer.currentToken}")
        lexer.next()

        val start = parseExpression()
        if (lexer.currentToken != ',') error(", expected. Got ${lexer.currentToken}")
        lexer.next()

        val end = parseExpression()

        val step = if (lexer.currentToken == ',') {
            lexer.next()
            parseExpression()
        } else null

        if (lexer.currentToken != Lexer.in_token) error("in token expected. Got ${lexer.currentToken}")
        lexer.next()

        val body = parseExpression()

        return ForExprAst(identifier, start, end, step, body)
    }

    private fun parsePrimary(): ASTBase {
        Logger.debug("Parsing primary expression")
        return when (lexer.currentToken) {
            Lexer.identifier -> parseIdentifier()
            Lexer.number -> parseNumber()
            Lexer.if_token -> parseIf()
            Lexer.for_token -> parseFor()
            '(' -> parseParenExpr()
            else -> error("Unexpected token ${lexer.currentToken.toLog()}")
        }
    }

    fun parseSequence() = generateSequence {
        lexer.next()
        while (lexer.currentToken == ';') lexer.next()
        when (lexer.currentToken) {
            Lexer.eof -> null
            Lexer.def -> parseDefinition() to Type.DEFINITION
            Lexer.extern -> parseExtern() to Type.EXTERN
            else -> parseTopLevel() to Type.TOP_LEVEL
        }
    }

    enum class Type {
        DEFINITION,
        EXTERN,
        TOP_LEVEL
    }
}
