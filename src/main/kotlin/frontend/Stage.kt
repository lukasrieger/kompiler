package frontend

import arrow.core.Either
import arrow.core.Nel
import arrow.core.flatMap


interface CompilerConfiguration

object StandardConfiguration : CompilerConfiguration {
    const val TYPE = "DEBUG"
}


typealias EitherNel<L, R> = Either<Nel<L>, R>

interface Stage<I, O> {
    operator fun invoke(input: I, config: CompilerConfiguration = StandardConfiguration): Either<CompilerError, O> =
        run(input, config)

    fun run(input: I, config: CompilerConfiguration = StandardConfiguration): Either<CompilerError, O>
}

infix fun <N, I, O> Stage<I, O>.compose(next: Stage<O, N>) = object : Stage<I, N> {
    override fun run(input: I, config: CompilerConfiguration) =
        this@compose(input, config).flatMap { next(it, config) }

}
