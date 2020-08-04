package frontend

import arrow.core.Either
import arrow.core.Nel
import arrow.core.flatMap


interface CompilerConfiguration

object StandardConfiguration : CompilerConfiguration {
    const val TYPE = "DEBUG"
}


typealias EitherNel<L, R> = Either<Nel<L>, R>

abstract class Stage<I, O> {
    abstract operator fun invoke(input: I, config: CompilerConfiguration = StandardConfiguration): EitherNel<CompilerError, O>
}

infix fun <N, I, O> Stage<I, O>.compose(next: Stage<O, N>) = object : Stage<I, N>() {
    override operator fun invoke(input: I, config: CompilerConfiguration) =
        this@compose(input, config).flatMap { next(it, config) }

}
