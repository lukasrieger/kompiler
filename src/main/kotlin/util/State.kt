package util


import arrow.continuations.Effect
import arrow.continuations.generic.DelimitedScope
import arrow.continuations.generic.RestrictedScope
import arrow.core.Tuple2
import arrow.core.toT
import kotlinx.coroutines.runBlocking


fun interface StateS<S, A> {
    val runF get() = ::invoke
    operator fun invoke(st: S): Tuple2<S, A>
}

fun <S, T> just(t: T) = StateS<S, T> { st -> st toT t }

inline fun <S, T, R> StateS<S, T>.map(crossinline f: (T) -> R): StateS<S, R> = flatMap { t -> just(f(t)) }

inline fun <S, T, F> StateS<S, T>.flatMap(crossinline f: (T) -> StateS<S, F>): StateS<S, F> =
    StateS { st -> this@flatMap.runF(st).let { (stT, a) -> f(a)(stT) } }


class StateEffect<S, A>(
    private val scope: RestrictedScope<StateS<S, A>>
) : Effect<StateS<S, A>> {

    override fun control(): DelimitedScope<StateS<S, A>> = scope

    suspend operator fun <B> StateS<S, B>.invoke(): B =
        scope.shift { cb -> flatMap { st -> runBlocking { cb(st) } } }
}

inline fun <S, A> state(crossinline c: suspend StateEffect<S, *>.() -> A): StateS<S, A> =
    Effect.restricted(
        eff = { StateEffect(it as RestrictedScope<StateS<S, A>>) },
        just = { just(it) },
        f = c
    )
