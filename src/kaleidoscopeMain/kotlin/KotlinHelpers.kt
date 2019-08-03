/**
 * @author Alex Plate
 */

@ThreadLocal
object InputModel {
    private var value = ""
    private var pointer = 0

    fun getChar(): Char {
        if (value.lastIndex >= pointer) {
            return this.value[pointer++]
        }
        value = (readLine() ?: "") + EOF
        Logger.log(Logger.LogPart.INPUT_MODEL, "Read new line: $value")
        pointer = 0
        return value[pointer++]
    }
}

fun Int.isAscii(): Boolean = this in 0..255
fun Int.toLog() = if (this.isAscii()) this.toChar() else (this - Char.MAX_VALUE.toInt()).toString()

const val EOF = '\u001a'

@ThreadLocal
object Logger {
    enum class LogPart {
        INPUT_MODEL,
        OTHER,
        CODE_GENERATION,
        PARSER,
        LEXER
    }

    private val enabledLoggers = mutableSetOf<LogPart>()

    fun log(part: LogPart, msg: String) {
        if (part !in enabledLoggers) return
        println("-".repeat(part.ordinal) + " $part : $msg")
    }

    fun setUp(args: Array<String>) {
        if ("log" in args) {
            enabledLoggers += LogPart.values()
            return
        }
        args.forEach { logPart ->
            enumValues<LogPart>().find { it.name == logPart }?.also { enabledLoggers += it }
        }
    }
}
