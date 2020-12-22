package frontend.typecheck

import arrow.core.Nel


sealed class TypeStateM<L, R>(open val value: R) {

    data class Failed<L, R>(val errors: Nel<L>, override val value: R) : TypeStateM<L, R>(value)

    data class Success<L, R>(override val value: R) : TypeStateM<L, R>(value)
}

fun <A, B> A.failed(value: B): TypeStateM<A, B> = TypeStateM.Failed(Nel(this), value)

fun <A, B> A.success(): TypeStateM<B, A> = TypeStateM.Success(this)


interface TypeStateEffect<L, R> {
    var carried: List<L>

    operator fun <B> TypeStateM<L, B>.invoke(): B = when (this) {
        is TypeStateM.Failed -> {
            carried = (carried + errors)
            value
        }
        is TypeStateM.Success -> value
    }


    fun shift(value: R) = if (carried.isEmpty()) {
        TypeStateM.Success(value)
    } else {
        TypeStateM.Failed(Nel.fromListUnsafe(carried), value)
    }

}

@Suppress("ClassName")
object evaltype {
    inline operator fun <E, A> invoke(fn: TypeStateEffect<E, *>.() -> A): TypeStateM<E, A> =
        object : TypeStateEffect<E, A> {
            override var carried = emptyList<E>()
        }.run { shift(fn()) }
}