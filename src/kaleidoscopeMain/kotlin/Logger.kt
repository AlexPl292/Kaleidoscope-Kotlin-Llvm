/**
 * @author Alex Plate
 */
class Logger {
    companion object {
        fun debug(msg: String) {
            println(msg)
        }
    }
}


fun error(msg: String): Nothing {
    Logger.debug(msg)
    throw RuntimeException(msg)
}