package frontend

import arrow.core.Either
import arrow.core.flatMap
import frontend.StageIdentifier.Companion.INTERIM_STAGE
import util.Logger
import kotlin.time.Duration
import kotlin.time.ExperimentalTime
import kotlin.time.measureTimedValue

data class StageIdentifier(
    val name: String,
    val canonicalOrder: Int
) {
    companion object {
        val INTERIM_STAGE = StageIdentifier(name = "PROXY", canonicalOrder = -1)
    }
}

enum class LogLevel {
    None,
    ErrorsOnly,
    Verbose,

}

interface CompilerConfiguration {
    val logLevel: LogLevel

    @OptIn(ExperimentalTime::class)
    val stateDuration: Duration
}

object StandardConfiguration : CompilerConfiguration {
    override val logLevel: LogLevel = LogLevel.Verbose

    @OptIn(ExperimentalTime::class)
    override val stateDuration: Duration
        get() = Duration.ZERO
}

data class CompilerOutput<T> @ExperimentalTime constructor(val value: T, val duration: Duration)

interface Stage<I, O> {
    companion object {
        const val COMPOSED_STAGE = -1
    }

    val identifier: StageIdentifier

    @OptIn(ExperimentalTime::class)
    operator fun invoke(input: I, config: CompilerConfiguration = StandardConfiguration): Either<CompilerError, O> {
        val (result, duration) = measureTimedValue { run(input, config) }

        val resultTemplate = result.fold({ "failed" }, { "successful" })

        if (config.logLevel == LogLevel.Verbose && identifier != INTERIM_STAGE) {
            Logger.debug(
                color = identifier.canonicalOrder,
                domain = this.identifier.name,
                text = "Stage took ${duration}. Execution $resultTemplate."
            )

        }


        return result
    }

    fun run(input: I, config: CompilerConfiguration): Either<CompilerError, O>
}


@OptIn(ExperimentalTime::class)
infix fun <N, I, O> Stage<I, O>.compose(next: Stage<O, N>) = object : Stage<I, N> {

    override fun run(input: I, config: CompilerConfiguration): Either<CompilerError, N> {
        return this@compose(input, config).flatMap {
            next(it, config)
        }
    }

    override val identifier: StageIdentifier = INTERIM_STAGE
}


