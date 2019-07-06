import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * @author Alex Plate
 */
@ExperimentalUnsignedTypes
class ParserTest {
    @Test
    internal fun `simple test`() {
        val res = Parser("1").parse()
        val expected = Program(
            listOf(
                Function(
                    proto = FunctionProto(
                        name = "",
                        args = emptyList()
                    ),
                    body = NumberExpr(1)
                )
            )
        )
        assertEquals(expected, res)
    }

    @Test
    internal fun `simple test with optimization`() {
        val res = Parser("1 + 5").parse()
        val expected = Program(
            listOf(
                Function(
                    proto = FunctionProto(
                        name = "",
                        args = emptyList()
                    ),
                    body = BinaryExpr(
                        operator = '+',
                        left = NumberExpr(1),
                        right = NumberExpr(5)
                    )
                )
            )
        )
        assertEquals(expected, res)
    }

    @Test
    internal fun `expression with parentheses`() {
        val res = Parser("4 * (2 + 3)").parse()
        val expected = Program(
            listOf(
                Function(
                    proto = FunctionProto(
                        name = "",
                        args = emptyList()
                    ),
                    body = BinaryExpr(
                        operator = '*',
                        left = NumberExpr(4),
                        right = BinaryExpr(
                            operator = '+',
                            left = NumberExpr(2),
                            right = NumberExpr(3)
                        )
                    )
                )
            )
        )
        assertEquals(expected, res)
    }


    @Test
    internal fun `named function`() {
        val res = Parser("def myFun(x) x + 15").parse()
        val expected = Program(
            listOf(
                Function(
                    proto = FunctionProto(
                        name = "myFun",
                        args = listOf("x")
                    ),
                    body = BinaryExpr(
                        operator = '+',
                        left = VarExpr("x"),
                        right = NumberExpr(15)
                    )
                )
            )
        )
        assertEquals(expected, res)
    }
}