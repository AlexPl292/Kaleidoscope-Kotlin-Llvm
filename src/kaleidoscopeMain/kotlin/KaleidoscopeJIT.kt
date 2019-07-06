import kotlinx.cinterop.*
import llvm.*

/**
 * @author Alex Plate
 */
@ExperimentalUnsignedTypes
class KaleidoscopeJIT {
    var executionEngine: LLVMExecutionEngineRef?
    private val jitModule = LLVMModuleCreateWithNameInContext("jit", context)

    init {
        executionEngine = memScoped {
            val ee = alloc<LLVMExecutionEngineRefVar>()
            val errorPtr = alloc<CPointerVar<ByteVar>>()
            val res = LLVMCreateJITCompilerForModule(ee.ptr, jitModule, 0u, errorPtr.ptr)
            if (res != 0) error("Error in execution engine initialization: ${errorPtr.value?.toKString()}")
            ee.value
        }
    }


    fun addModule(module: LLVMModuleRef?) {
        LLVMAddModule(executionEngine, module)
    }

    fun removeModule(module: LLVMModuleRef?) {
        memScoped {
            val outModule = alloc<LLVMModuleRefVar>()
            val errorPtr = alloc<CPointerVar<ByteVar>>()
            val res = LLVMRemoveModule(executionEngine, module, outModule.ptr, errorPtr.ptr)
            if (res != 0) error("Error in removing module from jit ${errorPtr.value?.toKString()}")
        }
    }
}