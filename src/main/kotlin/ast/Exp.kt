package ast

import arrow.core.Either
import arrow.core.right

sealed class ExpK

data class Exp(val exp: ExpF<Exp>) : ExpK()

data class TExp(val exp: ExpF<TExp>, val type: Type) : ExpK()

sealed class ExpF<T : ExpK> {

    class Read<T : ExpK> : ExpF<T>()
    data class This<T : ExpK>(val typeRef: Symbol<T>) : ExpF<T>()
    data class New<T : ExpK>(val typeRef: Symbol<T>) : ExpF<T>()
    data class Bool<T : ExpK>(val value: Boolean) : ExpF<T>()

    data class Constant<T : ExpK>(val value: Int) : ExpF<T>()
    data class ArrayLength<T : ExpK>(val array: T) : ExpF<T>()
    data class ArrayGet<T : ExpK>(val array: T, val index: T) : ExpF<T>()
    data class Negate<T : ExpK>(val expr: T) : ExpF<T>()
    data class NewArray<T : ExpK>(val size: T) : ExpF<T>()

    data class Symbol<T : ExpK>(val name: NamedRef) : ExpF<T>()

    data class BinaryOp<T : ExpK>(
        val left: T,
        val op: Operator,
        val right: T
    ) : ExpF<T>()

    data class Invoke<T : ExpK>(
        val obj: T,
        val method: Symbol<T>,
        val arguments: List<T>
    ) : ExpF<T>()
}

infix fun <T : ExpF<TExp>> T.typeOf(type: Type): Either.Right<TExp> =
    TExp(this, type).right() as Either.Right<TExp>


infix fun <T : ExpF<TExp>> T.of(type: Type): TExp = TExp(this, type)
