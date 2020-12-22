package asm.i386


import arrow.core.AndThen
import arrow.core.Tuple2
import arrow.core.toT


data class Test(val s: List<String>)

fun Test.emit(e: String): StateS<Test, String> = TODO()

fun interface StateS<S, A> {
    val runF
        get() = ::invoke

    operator fun invoke(st: S): Tuple2<S, A>
}

inline fun <S, T> just(t: T) = StateS<S, T> { it toT t }

inline fun <S, T, R> StateS<S, T>.map(crossinline f: (T) -> R): StateS<S, R> = flatMap { t -> just(f(t)) }

inline fun <S, T, F> StateS<S, T>.flatMap(noinline f: (T) -> StateS<S, F>): StateS<S, F> =
    StateS(
        AndThen(runF).andThenF(
            AndThen(f).compose { (_, a): Tuple2<S, T> -> a }
                .flatMap { st -> AndThen(st.runF).compose { (s, _): Tuple2<S, T> -> s } }

        ))

// DOESNT WORK
//
//fun <S> monad(): Monad<StateS<S,*>> = object : Monad<StateS<S,*>> {
//    override fun <A> just(a: A): Kind<StateS<S, *>, A> = StateS { it toT a }
//
//    override fun <A, B> tailRecM(a: A, f: (A) -> Kind<StateS<S, *>, Either<A, B>>): Kind<StateS<S, *>, B> {
//        TODO("Not yet implemented")
//    }
//
//    override fun <A, B> Kind<StateS<S, *>, A>.flatMap(f: (A) -> Kind<StateS<S, *>, B>): Kind<StateS<S, *>, B> {
//        TODO("Not yet implemented")
//    }
//
//}