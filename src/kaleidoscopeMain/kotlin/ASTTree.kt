import kotlinx.cinterop.*
import llvm.*

/**
 * @author Alex Plate
 */

interface Llvm {
    fun codegen(data: LlvmData): LLVMValueRef?
}

sealed class ASTBase : Llvm

data class NumberExpr(val value: Int) : ASTBase() {
    @ExperimentalUnsignedTypes
    override fun codegen(data: LlvmData): LLVMValueRef? {
        Logger.debug("Generate int - $value")
        return LLVMConstInt(LLVMInt64TypeInContext(context), value.toULong(), 1)
    }
}

data class VarExpr(val name: String) : ASTBase() {
    override fun codegen(data: LlvmData): LLVMValueRef? {
        return data.namedValues[name] ?: error("Unknown variable name")
    }
}

data class BinaryExpr(val operator: Char, val left: ASTBase, val right: ASTBase) : ASTBase() {
    override fun codegen(data: LlvmData): LLVMValueRef? {
        Logger.debug("Generate binary expression")
        val leftCode = left.codegen(data)
        val rightCode = right.codegen(data)

        return when (operator) {
            '+' -> LLVMBuildAdd(data.builder, leftCode, rightCode, "addtmp")
            '-' -> LLVMBuildSub(data.builder, leftCode, rightCode, "subtmp")
            '*' -> LLVMBuildMul(data.builder, leftCode, rightCode, "multmp")
            '>' -> LLVMBuildICmp(data.builder, LLVMIntSGT, leftCode, rightCode, "gtcode")
            '<' -> LLVMBuildICmp(data.builder, LLVMIntSLT, leftCode, rightCode, "ltcode")
            else -> error("Invalid binary operator")
        }
    }
}

data class CallExpr(val name: String, val args: List<ASTBase>) : ASTBase() {
    @ExperimentalUnsignedTypes
    override fun codegen(data: LlvmData): LLVMValueRef? {
        Logger.debug("Generate call to function")
        val function = getFunction(name, data) ?: error("Function is not found")

        if (llvm.LLVMCountParams(function).toInt() != args.size) error("Wrong # of params")

        val codeArgs: CValuesRef<LLVMValueRefVar>? = args.map { it.codegen(data) }.toCValues()
        return LLVMBuildCall(data.builder, function, codeArgs, args.size.toUInt(), "calltmp")
    }
}

data class FunctionProto(val name: String, val args: List<String>) : Llvm {
    @ExperimentalUnsignedTypes
    override fun codegen(data: LlvmData): LLVMValueRef? {
        Logger.debug("Generate function proto")
        val arguments = List(args.size) { LLVMInt64TypeInContext(context) }.toCValues()
        val functionType = LLVMFunctionType(LLVMInt64TypeInContext(context), arguments, args.size.toUInt(), 0)
        val function = LLVMAddFunction(data.module, name, functionType)

        memScoped {
            val array: CArrayPointer<LLVMValueRefVar> = allocArray(args.size)
            LLVMGetParams(function, array)
            for (i in 0 until args.size) {
                LLVMSetValueName2(array[i], args[i], args[i].length.toULong())
            }
        }
        return function
    }
}

data class Function(val proto: FunctionProto, val body: ASTBase) : Llvm {
    @ExperimentalUnsignedTypes
    override fun codegen(data: LlvmData): LLVMValueRef? {
        Logger.debug("Generate function")
        functionProtos[proto.name] = proto
        val function = getFunction(proto.name, data) ?: proto.codegen(data)

        val basicBlock = LLVMAppendBasicBlockInContext(context, function, "entry")
        LLVMPositionBuilderAtEnd(data.builder, basicBlock)

        data.namedValues.clear()
        memScoped {
            val lengthPtr = alloc<ULongVar>().ptr
            val args = proto.args
            val array: CArrayPointer<LLVMValueRefVar> = allocArray(args.size)
            LLVMGetParams(function, array)
            data.namedValues.clear()
            for (i in 0 until args.size) {
                val name = LLVMGetValueName2(array[i], lengthPtr)?.toKString()
                if (name != null) {
                    data.namedValues[name] = array[i]
                }
            }
        }

        val retVal = body.codegen(data)
        return if (retVal != null) {
            LLVMBuildRet(data.builder, retVal)
            LLVMVerifyFunction(function, LLVMVerifierFailureAction.LLVMPrintMessageAction)
            if (data.optimization) {
                LLVMRunFunctionPassManager(data.fpm, function)
            }
            function
        } else {
            LLVMDeleteFunction(function)
            null
        }
    }
}
