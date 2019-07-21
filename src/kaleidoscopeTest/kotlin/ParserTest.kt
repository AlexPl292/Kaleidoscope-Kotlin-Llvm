import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * @author Alex Plate
 */
@ExperimentalUnsignedTypes
class ParserTest {
    @Test
    internal fun `simple test`() {
        val res = Parser("1").parseSequence().toList()
        val expected = listOf(
            Function(
                proto = FunctionProto(
                    name = "__anon_expr",
                    args = emptyList(),
                    isOperator = false, precedence = 30u
                ),
                body = NumberExpr(1)
            ) to Parser.Type.TOP_LEVEL
        )
        assertEquals(expected, res)
    }

    @Test
    internal fun `simple test with optimization`() {
        val res = Parser("1 + 5").parseSequence().toList()
        val expected =
            listOf(
                Function(
                    proto = FunctionProto(
                        name = "__anon_expr",
                        args = emptyList(),
                        isOperator = false, precedence = 30u
                    ),
                    body = BinaryExpr(
                        operator = '+',
                        left = NumberExpr(1),
                        right = NumberExpr(5)
                    )
                ) to Parser.Type.TOP_LEVEL
            )
        assertEquals(expected, res)
    }

    @Test
    internal fun `expression with parentheses`() {
        val res = Parser("4 * (2 + 3)").parseSequence().toList()
        val expected =
            listOf(
                Function(
                    proto = FunctionProto(
                        name = "__anon_expr",
                        args = emptyList(),
                        isOperator = false, precedence = 30u
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
                ) to Parser.Type.TOP_LEVEL
            )
        assertEquals(expected, res)
    }


    @Test
    internal fun `named function`() {
        val res = Parser("def myFun(x) x + 15").parseSequence().toList()
        val expected =
            listOf(
                Function(
                    proto = FunctionProto(
                        name = "myFun",
                        args = listOf("x"),
                        isOperator = false, precedence = 30u
                    ),
                    body = BinaryExpr(
                        operator = '+',
                        left = VarExpr("x"),
                        right = NumberExpr(15)
                    )
                ) to Parser.Type.DEFINITION
            )
        assertEquals(expected, res)
    }

    @Test
    internal fun `if condition`() {
        val res = Parser("def myFun(x y) if x > y then x else y").parseSequence().toList()
        val expected = listOf(
            Function(
                proto = FunctionProto(
                    name = "myFun",
                    args = listOf("x", "y"),
                    isOperator = false, precedence = 30u
                ),
                body = IfExprAst(
                    cond = BinaryExpr(
                        operator = '>',
                        left = VarExpr("x"),
                        right = VarExpr("y")
                    ),
                    then = VarExpr("x"),
                    elseCode = VarExpr("y")
                )
            ) to Parser.Type.DEFINITION
        )
        assertEquals(expected, res)
    }

    @Test
    internal fun `for loop`() {
        val res = Parser("def myFun(x y) for i = 1, i < x, 2 in y + y").parseSequence().toList()
        val expected =
            listOf(
                Function(
                    proto = FunctionProto(
                        name = "myFun",
                        args = listOf("x", "y"),
                        isOperator = false, precedence = 30u
                    ),
                    body = ForExprAst(
                        varName = "i",
                        start = NumberExpr(1),
                        end = BinaryExpr(
                            operator = '<',
                            left = VarExpr("i"),
                            right = VarExpr("x")
                        ),
                        step = NumberExpr(2),
                        body = BinaryExpr(
                            operator = '+',
                            left = VarExpr("y"),
                            right = VarExpr("y")
                        )
                    )
                ) to Parser.Type.DEFINITION
            )
        assertEquals(expected, res)
    }

    @Test
    internal fun `binary operator`() {
        val res = Parser("def binary| 5 (x y) if x then 1 else if y then 1 else 0").parseSequence().toList()
        val expected =
            listOf(
                Function(
                    proto = FunctionProto(
                        name = "binary|",
                        args = listOf("x", "y"),
                        isOperator = true, precedence = 5u
                    ),
                    body = IfExprAst(
                        cond = VarExpr("x"),
                        then = NumberExpr(1),
                        elseCode = IfExprAst(
                            cond = VarExpr("y"),
                            then = NumberExpr(1),
                            elseCode = NumberExpr(0)
                        )
                    )
                ) to Parser.Type.DEFINITION
            )
        assertEquals(expected, res)
    }

    @Test
    internal fun `unary operator`() {
        val res = Parser("def unary! (x) if x then 1 else 0").parseSequence().toList()
        val expected =
            listOf(
                Function(
                    proto = FunctionProto(
                        name = "unary!",
                        args = listOf("x"),
                        isOperator = true, precedence = 30u
                    ),
                    body = IfExprAst(
                        cond = VarExpr("x"),
                        then = NumberExpr(1),
                        elseCode = NumberExpr(0)
                    )
                ) to Parser.Type.DEFINITION
            )
        assertEquals(expected, res)
    }
}