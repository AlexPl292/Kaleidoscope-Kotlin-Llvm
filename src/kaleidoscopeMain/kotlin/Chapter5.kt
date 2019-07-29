@file:Suppress("PackageDirectoryMismatch", "DuplicatedCode")

package ch5

import EOF
import InputModel
import KaleidoscopeJIT
import Logger
import ch5.Token.Companion.tok_def
import ch5.Token.Companion.tok_else
import ch5.Token.Companion.tok_eof
import ch5.Token.Companion.tok_extern
import ch5.Token.Companion.tok_for
import ch5.Token.Companion.tok_identifier
import ch5.Token.Companion.tok_if
import ch5.Token.Companion.tok_in
import ch5.Token.Companion.tok_number
import ch5.Token.Companion.tok_then
import isAscii
import kotlinx.cinterop.*
import llvm.*
import toLog

/**
 * @author Alex Plate
 */


//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
private class Token {
    companion object {
        const val tok_eof = -1

        // Commands
        const val tok_def = -2
        const val tok_extern = -3

        // Primary
        const val tok_identifier = -4
        const val tok_number = -5

        // Control
        const val tok_if = -6
        const val tok_then = -7
        const val tok_else = -8
        const val tok_for = -9
        const val tok_in = -10
    }
}

private var identifierString: String = ""   // Filled in if tok_identifier
private var numVal: Double = 0.0            // Filled in if tok_number

private var lastChar = ' '
private fun getTok(): Int {
    Logger.log(Logger.LogPart.LEXER, "Get next token")

    while (lastChar.isWhitespace()) lastChar = InputModel.getChar()

    Logger.log(Logger.LogPart.LEXER, "Whitespaces skipped")
    if (lastChar.isLetter()) { // Identifier: [a-zA-Z][a-zA-Z0-9]*
        identifierString = lastChar.toString()
        lastChar = InputModel.getChar()
        while (lastChar.isLetterOrDigit()) {
            identifierString += lastChar
            lastChar = InputModel.getChar()
        }

        Logger.log(Logger.LogPart.LEXER, "Parse identifier: $identifierString")
        return when (identifierString) {
            "def" -> tok_def
            "extern" -> tok_extern
            "if" -> tok_if
            "then" -> tok_then
            "else" -> tok_else
            "for" -> tok_for
            "in" -> tok_in
            else -> tok_identifier
        }
    }

    if (lastChar.isDigit() || lastChar == '.') { // Number: [0-9.]+
        var numberString = ""
        do {
            numberString += lastChar
            lastChar = InputModel.getChar()
        } while (lastChar.isDigit() || lastChar == '.')
        numVal = numberString.toDouble()
        Logger.log(Logger.LogPart.LEXER, "Parse number: $numVal")
        return tok_number
    }

    if (lastChar == '#') { // Comment until end of line
        Logger.log(Logger.LogPart.LEXER, "Parse comment")
        do {
            lastChar = InputModel.getChar()
        } while (lastChar != EOF && lastChar != '\n')

        if (lastChar != EOF) return getTok()
    }

    // Check for end of file.
    if (lastChar == EOF) {
        Logger.log(Logger.LogPart.LEXER, "EOF reached")
        lastChar = ' '
        return tok_eof
    }

    Logger.log(Logger.LogPart.LEXER, "Return character $lastChar")
    // Otherwise, just return the character as its ascii value.
    val thisChar = lastChar
    lastChar = InputModel.getChar()
    return thisChar.toInt()
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

private val theContext = LLVMContextCreate()
private val builder = LLVMCreateBuilder()
private lateinit var theModule: LLVMModuleRef
private lateinit var theFPM: LLVMPassManagerRef
@ExperimentalUnsignedTypes
private lateinit var theJIT: KaleidoscopeJIT
private val namedValues = mutableMapOf<String, LLVMValueRef?>()
@ExperimentalUnsignedTypes
private val functionProtos = mutableMapOf<String, PrototypeAST>()

@ExperimentalUnsignedTypes
private fun getFunction(name: String): LLVMValueRef? {
    // First, see if the function has already been added to the current module.
    // If not, check whether we can codegen the declaration from some existing prototype.
    // If no existing prototype exists, return null.
    return LLVMGetNamedFunction(theModule, name) ?: functionProtos[name]?.codegen()
}

/// ExprAST - Base class for all expression nodes.
private interface ExprAST {
    fun codegen(): LLVMValueRef?
}

/// NumberExprAST - Expression class for numeric literals like "1.0".
private data class NumberExprAST(private val value: Double) : ExprAST {
    override fun codegen(): LLVMValueRef? {
        Logger.log(Logger.LogPart.CODE_GENERATION, "Generate float - $value")
        return LLVMConstReal(LLVMDoubleTypeInContext(theContext), value)
    }
}

/// VariableExprAST - Expression class for referencing a variable, like "a".
private data class VariableExprAST(private val name: String) : ExprAST {
    // Look this variable up in the function
    override fun codegen(): LLVMValueRef? = namedValues[name] ?: logErrorV("Unknown variable name")
}

/// BinaryExprAST - Expression class for a binary operator.
private data class BinaryExprAST(private val op: Char, private val LHS: ExprAST, private val RHS: ExprAST) : ExprAST {
    override fun codegen(): LLVMValueRef? {
        val l = LHS.codegen() ?: return null
        val r = RHS.codegen() ?: return null

        Logger.log(Logger.LogPart.CODE_GENERATION, "Generate binary operation $op")
        return when (op) {
            '+' -> LLVMBuildFAdd(builder, l, r, "addtmp")
            '-' -> LLVMBuildFSub(builder, l, r, "subtmp")
            '*' -> LLVMBuildFMul(builder, l, r, "multmp")
            '<' -> {
                val v = LLVMBuildFCmp(builder, LLVMRealPredicate.LLVMRealULT, l, r, "cmptmp")
                // Convert bool 0/1 to double 0.0 or 1.0
                LLVMBuildUIToFP(builder, v, LLVMDoubleTypeInContext(theContext), "booltmp")
            }
            else -> logErrorV("Invalid binary operator")
        }
    }
}

/// CallExprAST - Expression class for function calls.
@ExperimentalUnsignedTypes
private data class CallExprAST(private val callee: String, private val args: List<ExprAST>) : ExprAST {
    override fun codegen(): LLVMValueRef? {
        // Look up the name in the global module table
        val calleeF = getFunction(callee) ?: return logErrorV("Unknown function referenced")

        // If argument mismatch error
        if (llvm.LLVMCountParams(calleeF).toInt() != args.size) return logErrorV("Incorrect # of arguments passed")

        val codeArgs: CValuesRef<LLVMValueRefVar>? = args.map { it.codegen() }.toCValues()
        return LLVMBuildCall(builder, calleeF, codeArgs, args.size.toUInt(), "calltmp")
    }
}

/// IfExprAST - Expression class for if/then/else.
@ExperimentalUnsignedTypes
private data class IfExprAST(
    private val cond: ExprAST,
    private val then: ExprAST,
    private val else_body: ExprAST
) : ExprAST {
    override fun codegen(): LLVMValueRef? {
        val condition = cond.codegen() ?: return null

        // Convert condition to a bool by comparing non-equal to 0.0.
        val condRes = LLVMBuildFCmp(
            builder,
            LLVMRealPredicate.LLVMRealONE,
            condition,
            LLVMConstReal(LLVMDoubleTypeInContext(theContext), 0.0),
            "ifcond"
        )

        val theFunction = LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder))

        // Create blocks for the then and else cases.  Insert the 'then' block at the
        // end of the function.
        var thenBb = LLVMAppendBasicBlockInContext(theContext, theFunction, "then")
        var elseBb = LLVMAppendBasicBlockInContext(theContext, theFunction, "else")
        val mergeBb = LLVMAppendBasicBlockInContext(theContext, theFunction, "ifcont")

        LLVMBuildCondBr(builder, condRes, thenBb, elseBb)

        // Emit then value.
        LLVMPositionBuilderAtEnd(builder, thenBb)
        val thenV = then.codegen() ?: return null

        LLVMBuildBr(builder, mergeBb)

        // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
        thenBb = LLVMGetInsertBlock(builder)

        // Emit else block.
        LLVMPositionBuilderAtEnd(builder, elseBb)
        val elseV = else_body.codegen() ?: return null
        LLVMBuildBr(builder, mergeBb)

        // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
        elseBb = LLVMGetInsertBlock(builder)

        // Emit merge block.
        LLVMPositionBuilderAtEnd(builder, mergeBb)
        val phi = LLVMBuildPhi(builder, LLVMDoubleTypeInContext(theContext), "iftmp")

        val incomingValues = listOf(thenV, elseV).toCValues()
        val incomingBlocks = listOf(thenBb, elseBb).toCValues()

        LLVMAddIncoming(phi, incomingValues, incomingBlocks, 2u)
        return phi
    }
}

// Output for-loop as:
//   ...
//   start = startexpr
//   goto loop
// loop:
//   variable = phi [start, loopheader], [nextvariable, loopend]
//   ...
//   bodyexpr
//   ...
// loopend:
//   step = stepexpr
//   nextvariable = variable + step
//   endcond = endexpr
//   br endcond, loop, endloop
// outloop:
@ExperimentalUnsignedTypes
private data class ForExprAST(
    private val varName: String,
    private val start: ExprAST,
    private val end: ExprAST,
    private val step: ExprAST?,
    private val body: ExprAST
) : ExprAST {
    override fun codegen(): LLVMValueRef? {
        // Emit the start code first, without 'variable' in scope.
        val startVal = start.codegen() ?: return null

        // Make the new basic block for the loop header, inserting after current
        // block.
        val function = LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder))
        val preheaderBb = LLVMGetInsertBlock(builder)
        val loopBb = LLVMAppendBasicBlockInContext(theContext, function, "loop")

        // Insert an explicit fall through from the current block to the LoopBB.
        LLVMBuildBr(builder, loopBb)

        // Start insertion in LoopBB.
        LLVMPositionBuilderAtEnd(builder, loopBb)

        // Start the PHI node with an entry for Start.
        val variable = LLVMBuildPhi(builder, LLVMDoubleTypeInContext(theContext), varName)
        LLVMAddIncoming(variable, listOf(startVal).toCValues(), listOf(preheaderBb).toCValues(), 1u)

        // Within the loop, the variable is defined equal to the PHI node.  If it
        // shadows an existing variable, we have to restore it, so save it now.
        val oldVar = namedValues[varName]
        namedValues[varName] = variable

        // Emit the body of the loop.  This, like any other expr, can change the
        // current BB.  Note that we ignore the value computed by the body, but don't
        // allow an error.
        body.codegen() ?: return null

        // Emit the step value.
        val step = if (step != null) {
            step.codegen() ?: return null
        } else {
            // If not specified, use 1.0.
            LLVMConstReal(LLVMDoubleTypeInContext(theContext), 1.0)
        }
        val nextVar = LLVMBuildFAdd(builder, variable, step, "nextvar")

        // Compute the end condition.
        val end = end.codegen() ?: return null

        // Convert condition to a bool by comparing non-equal to 0.0.
        val endCondition = LLVMBuildFCmp(
            builder, LLVMRealPredicate.LLVMRealONE, end,
            LLVMConstReal(LLVMDoubleTypeInContext(theContext), 0.0),
            "loopcond"
        )

        // Create the "after loop" block and insert it.
        val loopEndBb = LLVMGetInsertBlock(builder)
        val afterBb = LLVMAppendBasicBlockInContext(theContext, function, "afterloop")

        // Insert the conditional branch into the end of LoopEndBB.
        LLVMBuildCondBr(builder, endCondition, loopBb, afterBb)

        // Any new code will be inserted in AfterBB.
        LLVMPositionBuilderAtEnd(builder, afterBb)

        // Add a new entry to the PHI node for the backedge.
        LLVMAddIncoming(variable, listOf(nextVar).toCValues(), listOf(loopEndBb).toCValues(), 1u)

        // Restore the unshadowed variable.
        if (oldVar != null) namedValues[varName] = oldVar else namedValues.remove(varName)

        // for expr always returns 0.0.
        return LLVMConstNull(LLVMDoubleTypeInContext(theContext))
    }
}

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
@ExperimentalUnsignedTypes
private data class PrototypeAST(val name: String, val args: List<String>) {
    fun codegen(): LLVMValueRef? {
        // Make the function type:  double(double,double) etc.
        val arguments = List(args.size) { LLVMDoubleTypeInContext(theContext) }.toCValues()
        val functionType = LLVMFunctionType(LLVMDoubleTypeInContext(theContext), arguments, args.size.toUInt(), 0)
        val function = LLVMAddFunction(theModule, name, functionType)

        // Set names for all arguments.
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

/// FunctionAST - This class represents a function definition itself.
@ExperimentalUnsignedTypes
private data class FunctionAST(private val proto: PrototypeAST, private val body: ExprAST) {
    fun codegen(): LLVMValueRef? {
        // Transfer ownership of the prototype to the FunctionProtos map, but keep a
        // reference to it for use below.
        functionProtos[proto.name] = proto
        val theFunction = getFunction(proto.name) ?: return null

        // Create a new basic block to start insertion into.
        val bb = LLVMAppendBasicBlockInContext(theContext, theFunction, "entry")
        LLVMPositionBuilderAtEnd(builder, bb)

        // Record the function arguments in the NamedValues map.
        namedValues.clear()
        memScoped {
            val lengthPtr = alloc<ULongVar>().ptr
            val array: CArrayPointer<LLVMValueRefVar> = allocArray(proto.args.size)
            LLVMGetParams(theFunction, array)
            for (i in 0..proto.args.lastIndex) {
                LLVMGetValueName2(array[i], lengthPtr)?.toKString()?.let { namedValues[it] = array[i] }
            }
        }

        val retVal = body.codegen()
        Logger.log(Logger.LogPart.CODE_GENERATION, "Generated body of the function")
        return if (retVal != null) {
            // Finish off the function
            LLVMBuildRet(builder, retVal)
            // Validate the generated code, checking for consistency.
            LLVMVerifyFunction(theFunction, LLVMVerifierFailureAction.LLVMPrintMessageAction)
            Logger.log(Logger.LogPart.CODE_GENERATION, "Function verified")
            // Optimize the function.
            LLVMRunFunctionPassManager(theFPM, theFunction)
            Logger.log(Logger.LogPart.CODE_GENERATION, "Function optimized")
            theFunction
        } else {
            LLVMDeleteFunction(theFunction)
            null
        }
    }
}


//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// curTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
private var curTok: Int = 0

private fun getNextToken(): Int {
    curTok = getTok()
    return curTok
}

/// binopPrecedence - This holds the precedence for each binary operator that is
/// defined.
private val binopPrecedence = mutableMapOf<Char, Int>()

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
private fun getTokPrecedence(): Int {
    if (!curTok.isAscii()) return -1

    return binopPrecedence.getOrElse(curTok.toChar()) { -1 }
}

/// logError* - These are little helper functions for error handling.
private fun logError(str: String): ExprAST? {
    println("Error: $str")
    return null
}

@ExperimentalUnsignedTypes
private fun logErrorP(str: String): PrototypeAST? {
    println("Error: $str")
    return null
}

private fun logErrorV(str: String): LLVMValueRef? {
    println("Error: $str")
    return null
}

/// numberexpr ::= number
private fun parseNumberExpr(): ExprAST = NumberExprAST(numVal).also { getNextToken() }

/// parenexpr ::= '(' expression ')'
@ExperimentalUnsignedTypes
private fun parseParenExpr(): ExprAST? {
    getNextToken() // eat (
    val v = parseExpression() ?: return null

    if (curTok.toChar() != ')') return logError("Expected ')'")
    getNextToken()   // Eat )
    return v
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
@ExperimentalUnsignedTypes
private fun parseIdentifierExpr(): ExprAST? {
    val idName = identifierString
    Logger.log(Logger.LogPart.PARSER, "Parse identifier: $idName")

    getNextToken() // Eat identifier

    if (curTok.toChar() != '(') { // Simple variable ref
        return VariableExprAST(idName)
    }

    // Call
    getNextToken() // Eat (
    val args = mutableListOf<ExprAST>()
    if (curTok.toChar() != ')') {
        while (true) {
            args += parseExpression() ?: return null

            if (curTok.toChar() == ')') break

            if (curTok.toChar() != ',') return logError("Expected ')' or ',' in argument list")
            getNextToken()
        }
    }

    // Eat the `)`
    getNextToken()
    return CallExprAST(idName, args)
}

/// ifexpr ::= 'if' expression 'then' expression 'else' expression
@ExperimentalUnsignedTypes
private fun parseIfExpr(): ExprAST? {
    getNextToken()   // eat the if.

    val condition = parseExpression() ?: return null

    if (curTok != tok_then) return logError("Then expected")
    getNextToken() // eat the then

    val then = parseExpression() ?: return null

    if (curTok != tok_else) return logError("Else expected")
    getNextToken() // eat the else

    val else_body = parseExpression() ?: return null

    return IfExprAST(condition, then, else_body)
}

/// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
@ExperimentalUnsignedTypes
private fun parseForExpr(): ExprAST? {
    getNextToken() // Eat the for

    if (curTok != tok_identifier) return logError("expected identifier after for")

    val idName = identifierString
    getNextToken() // Eat identifier

    if (curTok != '='.toInt()) return logError("expected '=' after for")
    getNextToken() // Eat =

    val start = parseExpression() ?: return null

    if (curTok != ','.toInt()) return logError("expected ',' after for start value")
    getNextToken()

    val end = parseExpression() ?: return null

    // The step value is optional.
    val step = if (curTok == ','.toInt()) {
        getNextToken()
        parseExpression() ?: return null
    } else null

    if (curTok != tok_in) return logError("expected 'in' after for")
    getNextToken() // Eat in

    val body = parseExpression() ?: return null

    return ForExprAST(idName, start, end, step, body)
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
///   ::= ifexpr
///   ::= forexpr
@ExperimentalUnsignedTypes
private fun parsePrimary(): ExprAST? {
    Logger.log(Logger.LogPart.PARSER, "Parse primary")
    return when (curTok) {
        tok_identifier -> parseIdentifierExpr()
        tok_number -> parseNumberExpr()
        '('.toInt() -> parseParenExpr()
        tok_if -> parseIfExpr()
        tok_for -> parseForExpr()
        else -> logError("Unknown token when expecting an expression")
    }
}

/// binoprhs
///   ::= ('+' primary)*
@ExperimentalUnsignedTypes
private fun parseBinOpRHS(exprPrec: Int, _LHS: ExprAST): ExprAST? {
    var lhs = _LHS
    // If this is a binop, find its precedence.
    while (true) {
        val tokPrec = getTokPrecedence()

        // If this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (tokPrec < exprPrec) return lhs

        // Okay, we know this is a binop.
        val binOp = curTok.toChar()
        getNextToken() // Eat binop

        // Parse the primary expression after the binary operator.
        var rhs = parsePrimary() ?: return null

        // If BinOp binds less tightly with RHS than the operator after RHS, let
        // the pending operator take RHS as its LHS.
        val nextPrec = getTokPrecedence()
        if (tokPrec < nextPrec) {
            rhs = parseBinOpRHS(tokPrec + 1, rhs) ?: return null
        }

        // Merge LHS/RHS.
        lhs = BinaryExprAST(binOp, lhs, rhs)
    }
}

@ExperimentalUnsignedTypes
private fun parseExpression(): ExprAST? {
    Logger.log(Logger.LogPart.PARSER, "Parse expression")
    val lhs = parsePrimary() ?: return null
    return parseBinOpRHS(0, lhs)
}

/// prototype
///   ::= id '(' id* ')'
@ExperimentalUnsignedTypes
private fun parsePrototype(): PrototypeAST? {
    Logger.log(Logger.LogPart.PARSER, "Parse prototype")
    if (curTok != tok_identifier) return logErrorP("Expected function name in prototype")

    val fnName = identifierString
    Logger.log(Logger.LogPart.PARSER, "Parsed function name $fnName")
    getNextToken()

    if (curTok.toChar() != '(') return logErrorP("Expected '(' in prototype. Got ${curTok.toLog()}")

    val argNames = mutableListOf<String>()
    while (getNextToken() == tok_identifier) argNames += identifierString
    Logger.log(Logger.LogPart.PARSER, "Parsed args $argNames")

    if (curTok.toChar() != ')') return logErrorP("Expected ')' in prototype. Got ${curTok.toLog()}")

    // Success
    getNextToken() // Eat )

    return PrototypeAST(fnName, argNames)
}

/// definition ::= 'def' prototype expression
@ExperimentalUnsignedTypes
private fun parseDefinition(): FunctionAST? {
    Logger.log(Logger.LogPart.PARSER, "Parse definition")
    getNextToken() // Eat def
    val proto = parsePrototype() ?: return null
    Logger.log(Logger.LogPart.PARSER, "Function prototype parsed")

    val e = parseExpression() ?: return null
    Logger.log(Logger.LogPart.PARSER, "Function body parsed")
    return FunctionAST(proto, e)
}

/// toplevelexpr ::= expression
@ExperimentalUnsignedTypes
private fun parseTopLevelExpr(): FunctionAST? {
    Logger.log(Logger.LogPart.PARSER, "Parse top level expression")
    val e = parseExpression() ?: return null
    // Make an anonymous proto
    val proto = PrototypeAST("__anon_expr", emptyList())
    return FunctionAST(proto, e)
}

/// external ::= 'extern' prototype
@ExperimentalUnsignedTypes
private fun parseExtern(): PrototypeAST? {
    getNextToken() // Eat extern
    return parsePrototype()
}

//===----------------------------------------------------------------------===//
// Top-Level parsing
//===----------------------------------------------------------------------===//

private fun initializeModuleAndPassManager() {
    // Open a new module
    theModule = LLVMModuleCreateWithNameInContext("Kaleidoscope-kotlin", theContext)
        ?: throw RuntimeException("Cannot initialize module")

    theFPM = LLVMCreateFunctionPassManagerForModule(theModule) ?: throw RuntimeException("Cannot create pass manager")

    // Do simple "peephole" optimizations and bit-twiddling optzns.
    LLVMAddInstructionCombiningPass(theFPM)
    // Reassociate expressions.
    LLVMAddReassociatePass(theFPM)
    // Eliminate Common SubExpressions.
    LLVMAddGVNPass(theFPM)
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    LLVMAddCFGSimplificationPass(theFPM)

    LLVMInitializeFunctionPassManager(theFPM)
}

@ExperimentalUnsignedTypes
private fun handleDefinition() {
    val definition = parseDefinition()?.codegen()
    if (definition != null) {
        println("Read function definition:")
        println(LLVMPrintValueToString(definition)?.toKString())
        theJIT.addModule(theModule)
        initializeModuleAndPassManager()
    } else {
        // Skip token for error recovery.
        getNextToken()
    }
}

@ExperimentalUnsignedTypes
private fun handleExtern() {
    val extern = parseExtern()
    val parseExtern = extern?.codegen()
    if (parseExtern != null) {
        println("Read an extern:")
        println(LLVMPrintValueToString(parseExtern)?.toKString())
        functionProtos[extern.name] = extern
    } else {
        // Skip token for error recovery.
        getNextToken()
    }
}

@ExperimentalUnsignedTypes
private fun handleTopLevel() {
    Logger.log(Logger.LogPart.OTHER, "Handle top level")
    val topLevelExpr = parseTopLevelExpr()?.codegen()
    if (topLevelExpr != null) {
        println("Parsed a top-level expr")

        theJIT.addModule(theModule)
        initializeModuleAndPassManager()

        val function = theJIT.findSymbol("__anon_expr")
        val res = memScoped {
            val args = alloc<LLVMGenericValueRefVar>()
            LLVMRunFunction(theJIT.executionEngine, function, 0u, args.ptr)
        }
        println(LLVMPrintValueToString(topLevelExpr)?.toKString())
        println("Evaluated to " + LLVMGenericValueToFloat(LLVMDoubleTypeInContext(theContext), res))

        theJIT.removeModule(theModule)
    } else {
        // Skip token for error recovery.
        getNextToken()
    }
}

@ExperimentalUnsignedTypes
private fun mainLoop() {
    loop@ while (true) {
        when (curTok) {
            tok_eof -> {
                getNextToken()
                continue@loop
            }
            ';'.toInt() -> {
                getNextToken()
                continue@loop
            }
            tok_def -> handleDefinition()
            tok_extern -> handleExtern()
            else -> handleTopLevel()
        }
        print("ready> ")
    }
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

@ExperimentalUnsignedTypes
fun main(args: Array<String>) {
    Logger.setUp(args)
    // Install standard binary operators.
    // 1 is lowest precedence.
    binopPrecedence['<'] = 10
    binopPrecedence['+'] = 20
    binopPrecedence['-'] = 20
    binopPrecedence['*'] = 40

    print("ready> ")
    getNextToken()

    LLVMLinkInMCJIT()
    LLVMInitializeNativeTarget()
    LLVMInitializeNativeAsmPrinter()

    theJIT = KaleidoscopeJIT(theContext)
    initializeModuleAndPassManager()

    mainLoop()
}
