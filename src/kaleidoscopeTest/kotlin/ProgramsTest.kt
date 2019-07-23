import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * @author Alex Plate
 */

@ExperimentalUnsignedTypes
class ProgramsTest {
    @Test
    fun `bad fibonacci`() {
        val program = """
            def fib(x)
              if (x < 3) then
                1
              else
                fib(x-1)+fib(x-2);
                
            fib(6)
        """.trimIndent()
        val data = LlvmData(runTopLevel = true)
        CodeGenerator(data).generate(program)
        println("Assert $lastExecutionResult")
        assertEquals(8, lastExecutionResult)
    }
}