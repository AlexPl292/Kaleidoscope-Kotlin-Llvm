import kotlinx.cinterop.*
import llvm.*

/**
 * @author Alex Plate
 */

var initialized = false

fun linkJit() {
    if (!initialized) {
        initialized = true
        LLVMLinkInMCJIT()
        LLVMInitializeNativeTarget()
        LLVMInitializeNativeAsmPrinter()
    }
}

val context = LLVMContextCreate()
val functionProtos = mutableMapOf<String, FunctionProto>()

@ExperimentalUnsignedTypes
fun getFunction(name: String, data: LlvmData): LLVMValueRef? {
    Logger.debug("Search for function $name")
    val foundFunction = LLVMGetNamedFunction(data.module, name)
    if (foundFunction != null) {
        Logger.debug("Found function in current module")
        return foundFunction
    }

    Logger.debug("Generate function proto from saved functions")
    return functionProtos[name]?.codegen(data)
}

class LlvmData(val optimization: Boolean = true, val runTopLevel: Boolean = false) {
    val module = LLVMModuleCreateWithNameInContext("Kaleidoscope-kotlin", context)
    val builder = LLVMCreateBuilder()
    val namedValues = mutableMapOf<String, LLVMValueRef?>()
    val fpm: LLVMPassManagerRef?

    init {
        fpm = LLVMCreateFunctionPassManagerForModule(module)
        // Do simple "peephole" optimizations and bit-twiddling optzns.
        LLVMAddInstructionCombiningPass(fpm)
        // Reassociate expressions.
        LLVMAddReassociatePass(fpm)
        // Eliminate Common SubExpressions.
        LLVMAddGVNPass(fpm)
        // Simplify the control flow graph (deleting unreachable blocks, etc).
        LLVMAddCFGSimplificationPass(fpm)
        LLVMInitializeFunctionPassManager(fpm)
    }
}

@ExperimentalUnsignedTypes
class CodeGenerator(private var data: LlvmData) {

    private val jit: KaleidoscopeJIT

    init {
        linkJit()
        jit = KaleidoscopeJIT()
    }

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

        if (data.runTopLevel) {
            jit.addModule(data.module)
            val moduleInJit = data.module
            data = LlvmData(data.optimization, data.runTopLevel)

            memScoped {
                val name = "__anon_expr"
                val myFunction = alloc<LLVMValueRefVar>()
                val args = alloc<LLVMGenericValueRefVar>()
                LLVMFindFunction(jit.executionEngine, name, myFunction.ptr)

                val res = LLVMRunFunction(jit.executionEngine, myFunction.value, 0u, args.ptr)
                println("Result: " + LLVMGenericValueToInt(res, 0))
            }
            jit.removeModule(moduleInJit)
        }
    }

    private fun handleExtern(extern: Llvm) {
        val proto = extern as FunctionProto
        extern.codegen(data)
        println("Handle extern:")
        functionProtos[proto.name] = proto
    }

    private fun handleDefinition(definition: Llvm) {
        val code = definition.codegen(data)
        println("Handle definition:")
        println(LLVMPrintValueToString(code)?.toKString())
        jit.addModule(data.module)
        data = LlvmData(data.optimization, data.runTopLevel)
    }

    companion object {
        fun repl() {
            val generator = CodeGenerator(LlvmData(optimization = true, runTopLevel = true))
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
