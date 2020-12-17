package ast


data class Exp(val exp: ExpF<Exp>)

data class TExp(val exp: ExpF<TExp>, val type: Type)

sealed class ExpF<T> {

    class Read<T> : ExpF<T>()
    data class This<T>(val typeRef: Identifier<T>) : ExpF<T>()
    data class New<T>(val typeRef: Identifier<T>) : ExpF<T>()
    data class Bool<T>(val value: Boolean) : ExpF<T>()

    data class Constant<T>(val value: Int) : ExpF<T>()
    data class ArrayLength<T>(val array: T) : ExpF<T>()
    data class ArrayGet<T>(val array: T, val index: T) : ExpF<T>()
    data class Negate<T>(val expr: T) : ExpF<T>()
    data class NewArray<T>(val size: T) : ExpF<T>()

    data class Identifier<T>(
        val name: Name
    ) : ExpF<T>()

    data class BinaryOp<T>(
        val left: T,
        val op: Operator,
        val right: T
    ) : ExpF<T>()

    data class Invoke<T>(
        val obj: T,
        val method: Identifier<T>,
        val arguments: List<T>
    ) : ExpF<T>()
}

infix fun <T : ExpF<TExp>> T.typeOf(type: Type): TExp = TExp(this, type)
