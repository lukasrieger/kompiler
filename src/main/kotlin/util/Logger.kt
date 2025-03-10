package util

const val ANSI_RESET = "\u001B[0m"
const val ANSI_BLACK = "\u001B[30m"
const val ANSI_RED = "\u001B[31m"
const val ANSI_GREEN = "\u001B[32m"
const val ANSI_YELLOW = "\u001B[33m"
const val ANSI_BLUE = "\u001B[34m"
const val ANSI_PURPLE = "\u001B[35m"
const val ANSI_CYAN = "\u001B[36m"
const val ANSI_WHITE = "\u001B[37m"

val colors = listOf(
    ANSI_BLUE,
    ANSI_CYAN,
    ANSI_GREEN,
    ANSI_YELLOW,
    ANSI_PURPLE,
    ANSI_RED
)


object Logger {
    fun debug(color: Int, domain: String, text: String): Unit =
        println("${colors[color]}[$domain]$ANSI_RESET :: $text")
}