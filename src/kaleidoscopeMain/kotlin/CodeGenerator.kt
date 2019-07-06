import kotlinx.cinterop.toKString
import llvm.*

/**
 * @author Alex Plate
 */

class LlvmData(val optimization: Boolean = true) {
    val context = LLVMContextCreate()
    val module = LLVMModuleCreateWithNameInContext("Kaleidoscope-kotlin", context)
    val builder = LLVMCreateBuilder()
    val namedValues = mutableMapOf<String, LLVMValueRef?>()
}

@ExperimentalUnsignedTypes
class CodeGenerator(private val data: LlvmData) {
    fun generate(code: String) {
        Parser(code).parseSequence().forEach {
            val handler = when (it.second) {
                Parser.Type.DEFINITION -> this::handleDefinition
                Parser.Type.EXTERN -> this::handleExtern
                Parser.Type.TOP_LEVEL -> this::handleTopLevel
            }
            handler(it.first)
        }
    }

    private fun handleTopLevel(topLevel: Llvm) {
        val code = topLevel.codegen(data)
        println("Handle top level:")
        println(LLVMPrintValueToString(code)?.toKString())
    }

    private fun handleExtern(extern: Llvm) {
        val code = extern.codegen(data)
        println("Handle extern:")
        println(LLVMPrintValueToString(code)?.toKString())
    }

    private fun handleDefinition(definition: Llvm) {
        val code = definition.codegen(data)
        println("Handle definition:")
        println(LLVMPrintValueToString(code)?.toKString())
    }

    companion object {
        fun repl() {
            val generator = CodeGenerator(LlvmData())
            while (true) {
                print("ready> ")
                val input = readLine() ?: continue
                if (input == "exit") return
                generator.generate(input)
            }
        }
    }
}

@ExperimentalUnsignedTypes
fun main() {
    CodeGenerator.repl()
}
