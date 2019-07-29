@file:Suppress("PackageDirectoryMismatch", "DuplicatedCode")

package ch2

import EOF
import InputModel
import Logger
import ch2.Token.Companion.tok_def
import ch2.Token.Companion.tok_eof
import ch2.Token.Companion.tok_extern
import ch2.Token.Companion.tok_identifier
import ch2.Token.Companion.tok_number
import isAscii
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

/// ExprAST - Base class for all expression nodes.
private interface ExprAST

/// NumberExprAST - Expression class for numeric literals like "1.0".
private data class NumberExprAST(private val value: Double) : ExprAST

/// VariableExprAST - Expression class for referencing a variable, like "a".
private data class VariableExprAST(private val name: String) : ExprAST

/// BinaryExprAST - Expression class for a binary operator.
private data class BinaryExprAST(private val op: Char, private val LHS: ExprAST, private val RHS: ExprAST) : ExprAST

/// CallExprAST - Expression class for function calls.
private data class CallExprAST(private val callee: String, private val args: List<ExprAST>) : ExprAST

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
private data class PrototypeAST(private val name: String, private val args: List<String>)

/// FunctionAST - This class represents a function definition itself.
private data class FunctionAST(private val proto: PrototypeAST, private val body: ExprAST)


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

private fun logErrorP(str: String): PrototypeAST? {
    println("Error: $str")
    return null
}


/// numberexpr ::= number
private fun parseNumberExpr(): ExprAST = NumberExprAST(numVal).also { getNextToken() }

/// parenexpr ::= '(' expression ')'
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

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
private fun parsePrimary(): ExprAST? {
    Logger.log(Logger.LogPart.PARSER, "Parse primary")
    return when (curTok) {
        tok_identifier -> parseIdentifierExpr()
        tok_number -> parseNumberExpr()
        '('.toInt() -> parseParenExpr()
        else -> logError("Unknown token when expecting an expression")
    }
}

/// binoprhs
///   ::= ('+' primary)*
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

private fun parseExpression(): ExprAST? {
    Logger.log(Logger.LogPart.PARSER, "Parse expression")
    val lhs = parsePrimary() ?: return null
    return parseBinOpRHS(0, lhs)
}

/// prototype
///   ::= id '(' id* ')'
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
private fun parseTopLevelExpr(): FunctionAST? {
    Logger.log(Logger.LogPart.PARSER, "Parse top level expression")
    val e = parseExpression() ?: return null
    // Make an anonymous proto
    val proto = PrototypeAST("", emptyList())
    return FunctionAST(proto, e)
}

/// external ::= 'extern' prototype
private fun parseExtern(): PrototypeAST? {
    getNextToken() // Eat extern
    return parsePrototype()
}

//===----------------------------------------------------------------------===//
// Top-Level parsing
//===----------------------------------------------------------------------===//

private fun handleDefinition() {
    val definition = parseDefinition()
    if (definition != null) {
        println("Parsed a function definition.")
    } else {
        // Skip token for error recovery.
        getNextToken()
    }
}

private fun handleExtern() {
    val parseExtern = parseExtern()
    if (parseExtern != null) {
        println("Parsed an extern")
    } else {
        // Skip token for error recovery.
        getNextToken()
    }
}

private fun handleTopLevel() {
    Logger.log(Logger.LogPart.OTHER, "Handle top level")
    val topLevelExpr = parseTopLevelExpr()
    if (topLevelExpr != null) {
        println("Parsed a top-level expr")
    } else {
        // Skip token for error recovery.
        getNextToken()
    }
}

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

    mainLoop()
}
