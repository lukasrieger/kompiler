package frontend

import arrow.core.Either
import arrow.core.Nel
import arrow.core.flatMap
import frontend.Stage.Companion.COMPOSED_STAGE
import kotlin.time.ExperimentalTime
import kotlin.time.measureTimedValue

data class StageIdentifier(
    val name: String,
    val canonicalOrder: Int
)

enum class LogLevel {
    None,
    ErrorsOnly,
    Verbose,

}

interface CompilerConfiguration {
    val logLevel: LogLevel
}

object StandardConfiguration : CompilerConfiguration {
    const val TYPE = "DEBUG"

    override val logLevel: LogLevel = LogLevel.Verbose
}


typealias EitherNel<L, R> = Either<Nel<L>, R>


interface Stage<I, O> {
    companion object {
        const val COMPOSED_STAGE = -1
    }

    val identifier: StageIdentifier

    @OptIn(ExperimentalTime::class)
    operator fun invoke(input: I, config: CompilerConfiguration = StandardConfiguration): Either<CompilerError, O> {
        val (result, duration) = measureTimedValue { run(input, config) }
        val resultTemplate = result.fold({ "failed" }, { "successful" })

        if (config.logLevel == LogLevel.Verbose) {
            println("[${identifier.name}] Stage took $duration. Execution $resultTemplate")
        }

        return result
    }

    fun run(input: I, config: CompilerConfiguration): Either<CompilerError, O>
}


infix fun <N, I, O> Stage<I, O>.compose(next: Stage<O, N>) = object : Stage<I, N> {

    override fun run(input: I, config: CompilerConfiguration): Either<CompilerError, N> =
        this@compose(input, config).flatMap { next(it, config) }

    override val identifier: StageIdentifier = StageIdentifier(
        name = "${this@compose.identifier.name} -> ${next.identifier.name}",
        canonicalOrder = COMPOSED_STAGE
    )

}
