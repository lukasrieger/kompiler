package frontend
sealed class CompilerError(val reason: String, val location: String) {
    open class Severe(reason: String, location: String) : CompilerError(reason, location)
    class Continue(reason: String, location: String) : CompilerError(reason, location)
}

