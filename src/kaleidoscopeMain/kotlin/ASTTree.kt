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
        val argList = List(args.size) { LLVMInt64TypeInContext(context) }
        val arguments = argList.toCValues()
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

data class IfExprAst(val cond: ASTBase, val then: ASTBase, val elseCode: ASTBase) : ASTBase() {
    @ExperimentalUnsignedTypes
    override fun codegen(data: LlvmData): LLVMValueRef? {
        val condition = cond.codegen(data)
        val condRes = LLVMBuildICmp(
            data.builder,
            LLVMIntNE,
            condition,
            LLVMConstInt(LLVMInt64TypeInContext(context), 0u, 1),
            "ifcond"
        )

        val function = LLVMGetBasicBlockParent(LLVMGetInsertBlock(data.builder))
        var thenBb = LLVMAppendBasicBlockInContext(context, function, "then")
        var elseBb = LLVMAppendBasicBlockInContext(context, function, "else")
        val mergeBb = LLVMAppendBasicBlockInContext(context, function, "ifcont")
        LLVMBuildCondBr(data.builder, condRes, thenBb, elseBb)

        // Create then branch
        LLVMPositionBuilderAtEnd(data.builder, thenBb)
        val thenV = then.codegen(data)
        LLVMBuildBr(data.builder, mergeBb)

        thenBb = LLVMGetInsertBlock(data.builder)

        // Create else branch
        LLVMPositionBuilderAtEnd(data.builder, elseBb)
        val elseV = elseCode.codegen(data)
        LLVMBuildBr(data.builder, mergeBb)

        elseBb = LLVMGetInsertBlock(data.builder)

        // Create merge
        LLVMPositionBuilderAtEnd(data.builder, mergeBb)
        val phi = LLVMBuildPhi(data.builder, LLVMInt64TypeInContext(context), "iftmp")

        val incomingValues = listOf(thenV, elseV).toCValues()
        val incomingBlocks = listOf(thenBb, elseBb).toCValues()

        LLVMAddIncoming(phi, incomingValues, incomingBlocks, 2u)
        return phi
    }
}

data class ForExprAst(
    val varName: String,
    val start: ASTBase,
    val end: ASTBase,
    val step: ASTBase?,
    val body: ASTBase
) : ASTBase() {
    @ExperimentalUnsignedTypes
    override fun codegen(data: LlvmData): LLVMValueRef? {
        val startVal = start.codegen(data)

        val function = LLVMGetBasicBlockParent(LLVMGetInsertBlock(data.builder))
        val preheaderBb = LLVMGetInsertBlock(data.builder)
        val loopBb = LLVMAppendBasicBlockInContext(context, function, "loop")
        LLVMBuildBr(data.builder, loopBb)

        // Create loop
        LLVMPositionBuilderAtEnd(data.builder, loopBb)
        val phi = LLVMBuildPhi(data.builder, LLVMInt64TypeInContext(context), varName)
        LLVMAddIncoming(phi, listOf(startVal).toCValues(), listOf(preheaderBb).toCValues(), 1u)

        // Within the loop, the variable is defined equal to the PHI node.  If it
        // shadows an existing variable, we have to restore it, so save it now.
        val oldVar = data.namedValues[varName]
        data.namedValues[varName] = phi

        // Generate body
        body.codegen(data)

        // Emit the step value.
        val step = if (step != null) step.codegen(data) else LLVMConstInt(LLVMInt64TypeInContext(context), 1U, 1)
        val nextVar = LLVMBuildAdd(data.builder, phi, step, "nextvar")

        // End condition
        val end = end.codegen(data)
        val endCondition = LLVMBuildICmp(
            data.builder,
            LLVMIntNE,
            end,
            LLVMConstInt(LLVMInt64TypeInContext(context), 0u, 1),
            "loopcond"
        )

        // Create the "after loop" block and insert it.
        val loopEndBb = LLVMGetInsertBlock(data.builder)
        val afterBb = LLVMAppendBasicBlockInContext(context, function, "afterloop")

        // Insert the conditional branch into the end of LoopEndBB.
        LLVMBuildCondBr(data.builder, endCondition, loopBb, afterBb)

        // Any new code will be inserted in AfterBB.
        LLVMPositionBuilderAtEnd(data.builder, afterBb)

        // Add a new entry to the PHI node for the backedge.
        LLVMAddIncoming(phi, listOf(nextVar).toCValues(), listOf(loopEndBb).toCValues(), 1u)

        // Restore the unshadowed variable.
        if (oldVar != null) data.namedValues[varName] = oldVar else data.namedValues.remove(varName)

        // for expr always returns 0.0.
        return LLVMConstNull(LLVMInt64TypeInContext(context))
    }
}
