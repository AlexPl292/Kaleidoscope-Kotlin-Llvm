/**
 * @author Alex Plate
 */

data class Program(private val functions: List<Llvm>)

interface Llvm

sealed class ASTBase : Llvm

data class NumberExpr(val value: Int) : ASTBase()

data class VarExpr(val name: String) : ASTBase()

data class BinaryExpr(val operator: Char, val left: ASTBase, val right: ASTBase) : ASTBase()

data class CallExpr(val name: String, val args: List<ASTBase>) : ASTBase()

data class FunctionProto(val name: String, val args: List<String>) : Llvm

data class Function(val proto: FunctionProto, val body: ASTBase) : Llvm
