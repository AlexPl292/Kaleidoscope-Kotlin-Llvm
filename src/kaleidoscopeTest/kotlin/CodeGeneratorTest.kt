import kotlinx.cinterop.toKString
import llvm.LLVMPrintModuleToString
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * @author Alex Plate
 */

@ExperimentalUnsignedTypes
class CodeGeneratorTest {
    @Test
    internal fun `simple expression`() {
        val data = LlvmData(false)
        CodeGenerator(data).generate("1 + 2")
        val expected = """
           ; ModuleID = 'Kaleidoscope-kotlin'
           source_filename = "Kaleidoscope-kotlin"

           define i64 @__anon_expr() {
           entry:
             ret i64 3
           }
           
        """.trimIndent()
        assertEquals(expected, LLVMPrintModuleToString(data.module)?.toKString())
    }

    @Test
    internal fun `expression with parentheses`() {
        val data = LlvmData(false)
        CodeGenerator(data).generate("4 * (2 + 3)")
        val expected = """
           ; ModuleID = 'Kaleidoscope-kotlin'
           source_filename = "Kaleidoscope-kotlin"

           define i64 @__anon_expr() {
           entry:
             ret i64 20
           }
           
        """.trimIndent()
        assertEquals(expected, LLVMPrintModuleToString(data.module)?.toKString())
    }

    @Test
    internal fun function() {
        val data = LlvmData(false)
        CodeGenerator(data).generate("def myFunction(x) x + 1")
        val expected = """
           ; ModuleID = 'Kaleidoscope-kotlin'
           source_filename = "Kaleidoscope-kotlin"

           define i64 @myFunction(i64 %x) {
           entry:
             %addtmp = add i64 %x, 1
             ret i64 %addtmp
           }
           
        """.trimIndent()
        assertEquals(expected, LLVMPrintModuleToString(data.module)?.toKString())
    }

    @Test
    internal fun `function without optimizations`() {
        val data = LlvmData(false)
        CodeGenerator(data).generate("def test(x) (1+2+x)*(x+(1+2))")
        val expected = """
           ; ModuleID = 'Kaleidoscope-kotlin'
           source_filename = "Kaleidoscope-kotlin"

           define i64 @test(i64 %x) {
           entry:
             %addtmp = add i64 3, %x
             %addtmp1 = add i64 %x, 3
             %multmp = mul i64 %addtmp, %addtmp1
             ret i64 %multmp
           }
           
        """.trimIndent()
        assertEquals(expected, LLVMPrintModuleToString(data.module)?.toKString())
    }
}
