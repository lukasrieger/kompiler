package frontend.syntax

import arrow.optics.optics


@optics
sealed class Expression(open val type: Type? = null) {

    @optics
    data class ArrayGet(override val type: Type? = null, val array: Expression, val index: Expression) :
        Expression(type) {
        companion object
    }

    @optics
    data class ArrayLength(val array: Expression) : Expression(Type.IntType) {
        companion object
    }

    @optics
    data class BinaryOp(
        override val type: Type? = null,
        val left: Expression,
        val op: Operator,
        val right: Expression
    ) : Expression(type) {
        companion object
    }

    @optics
    data class Id (override val type: Type? = null, val id: String) : Expression(type) {
        companion object
    }

    @optics
    data class Constant(override val type: Type? = null, val value: Int) : Expression(type) {
        companion object
    }

    @optics
    data class Invoke(
        override val type: Type? = null,
        val obj: Expression,
        val method: Id,
        val arguments: List<Expression>?
    ) : Expression(type) {
        companion object
    }

    @optics
    data class Negate(val expression: Expression) : Expression(Type.BooleanType) {
        companion object
    }

    @optics
    data class New(override val type: Type? = null, val className: Id) : Expression(type) {
        companion object
    }

    @optics
    data class NewArray(override val type: Type? = null, val size: Expression) : Expression(type) {
        companion object
    }

    @optics
    data class Read(override val type: Type? = null) : Expression(type) {
        companion object
    }

    @optics
    data class This(override val type: Type? = null) : Expression(type) {
        companion object
    }

    object True : Expression(type = Type.BooleanType)
    object False : Expression(type = Type.BooleanType)
}


