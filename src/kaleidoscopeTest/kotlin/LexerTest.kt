import Lexer.Companion.def
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * @author Alex Plate
 */
class LexerTest {
    @Test
    internal fun `simple test`() {
        val expected = tokens("a")
        val res = parse("a")
        assertEquals(expected, res)
    }

    @Test
    internal fun expression() {
        val expected = tokens("a", "+", "b")
        val res = parse("a + b")
        assertEquals(expected, res)
    }

    @Test
    internal fun brackets() {
        val expected = tokens("(", "a", ")")
        val res = parse("(a)")
        assertEquals(expected, res)
    }

    @Test
    internal fun complicatedExpression() {
        val expected = tokens("a", "+", "b", "*", "3", "+", "(", "abs", "-", "2", ")")
        val res = parse("a + b * 3 + (abs - 2)")
        assertEquals(expected, res)
    }

    @Test
    internal fun function() {
        val expected = tokens(def, "myFun", "(", ")")
        val res = parse("def myFun()")
        assertEquals(expected, res)
    }

    @Test
    internal fun functionWithArgs() {
        val expected = tokens(def, "myFun", "(", "x", ")")
        val res = parse("def myFun(x)")
        assertEquals(expected, res)
    }

    @Test
    internal fun functionWithBody() {
        val expected = tokens(def, "myFun", "(", "x", ")", "x", "+", "1", "+", "3")
        val res = parse("def myFun(x) x + 1 + 3")
        assertEquals(expected, res)
    }

    private fun tokens(vararg vals: Any): List<Pair<Char, String>> {
        return vals.map {
            when (it) {
                is Char -> it to ""
                is String -> when {
                    it.all { c -> c.isDigit() } -> Lexer.number to it
                    it[0].isLetter() -> Lexer.identifier to it
                    else -> it[0] to ""
                }
                else -> error("Cannot parse")
            }
        }
    }

    private fun parse(input: String): List<Pair<Char, String>> {
        val lexer = Lexer(input)
        val res = mutableListOf<Pair<Char, String>>()
        while (true) {
            lexer.next()
            if (lexer.currentToken == Lexer.eof) break

            res += when (lexer.currentToken) {
                Lexer.identifier -> Lexer.identifier to lexer.tokenIdnt
                Lexer.number -> Lexer.number to lexer.tokenNumber.toString()
                else -> lexer.currentToken to ""
            }
        }
        return res
    }
}