package frontend.typecheck

import arrow.core.Nel


sealed class StateF<S, T>(open val value: T) {

    data class Err<S, T>(val errors: Nel<S>, override val value: T): StateF<S,T>(value)

    data class Succ<S,T>(override val value: T): StateF<S,T>(value)
}

fun <S, T> just(value: T) = StateF.Succ<S,T>(value)

infix fun <S, T> S.toT(value: T) = StateF.Err(Nel(this), value)

infix fun <S,T> List<S>.toT(value: T) = StateF.Err(Nel.fromListUnsafe(this), value)

fun <S,T> T.success() = just<S,T>(this)


interface StateFEffect<S, T> {
    var carried: List<S>

    operator fun <B> StateF<S, B>.invoke(): B = when(this) {
        is StateF.Err -> {
            carried = (carried + errors)
            value
        }
        is StateF.Succ -> value
    }

    fun shift(value: T) = carried toT value

}

@Suppress("ClassName")
object evaltype {
    inline operator fun <E, A> invoke(fn: StateFEffect<E, *>.() -> A): StateF<E, A> =
        object : StateFEffect<E, A> {
            override var carried = emptyList<E>()
        }.run { shift(fn()) }
}