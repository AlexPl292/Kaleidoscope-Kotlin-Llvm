/**
 * @author Alex Plate
 */

class Lexer(private val input: String) {
    companion object {
        val eof = (-1).toChar()

        // Commands
        val def = (-2).toChar()
        val extern = (-3).toChar()

        // Primary
        val identifier = (-4).toChar()
        val number = (-5).toChar()

        // Control flow
        val if_token = (-6).toChar()
        val then_token = (-7).toChar()
        val else_token = (-8).toChar()
        val for_token = (-9).toChar()
        val in_token = (-10).toChar()

        // Operators
        val unary = (-11).toChar()
        val binary = (-12).toChar()
    }

    private var pointer = -1
    var currentToken: Char = 0.toChar()
        private set
    var tokenNumber: Int = 0  // Filled in if tok_identifier
        private set
    var tokenIdnt: String = ""  // Filled in if tok_number
        private set

    fun next() {
        pointer++
        if (pointer >= input.length) {
            if (pointer - input.length > 10) error("A lot ahead of file")
            currentToken = eof
            return
        }
        currentToken = getToken()
        Logger.debug("Current token: ${currentToken.toLog()}")
    }


    private fun getToken(): Char {
        val len = input.length
        // Skip any whitespace.
        while (pointer < len && input[pointer].isWhitespace()) pointer++

        if (input[pointer].isLetter()) {
            // Identifier
            val identifierBuilder = StringBuilder(input[pointer].toString())
            while (pointer + 1 < len && input[pointer + 1].isLetterOrDigit()) identifierBuilder.append(input[++pointer])

            return when (val identifierStr = identifierBuilder.toString()) {
                "def" -> def
                "extern" -> extern
                "if" -> if_token
                "then" -> then_token
                "else" -> else_token
                "for" -> for_token
                "in" -> in_token
                "binary" -> binary
                "unary" -> unary
                else -> {
                    tokenIdnt = identifierStr
                    identifier
                }
            }
        }

        if (input[pointer].isDigit()) {
            // Number
            val numberBuilder = StringBuilder(input[pointer].toString())
            while (pointer + 1 < len && input[pointer + 1].isDigit()) numberBuilder.append(input[++pointer])

            tokenNumber = numberBuilder.toString().toInt()
            return number
        }

        if (input[pointer] == '#') {
            // Comment until end of line.
            while (pointer + 1 < len && input[pointer + 1] != '\n') pointer++
        }

        return input[pointer]
    }
}

fun Char.toLog(): String = if (isTok()) (this.toInt() - Char.MAX_VALUE.toInt()).toString() else this.toString()

fun Char.isTok(): Boolean = this.toInt() - Char.MAX_VALUE.toInt() / 2 > 0
