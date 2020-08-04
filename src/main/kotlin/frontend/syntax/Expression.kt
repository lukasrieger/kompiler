package frontend.syntax

import arrow.core.None
import arrow.core.Option
import arrow.core.Some
import arrow.optics.optics

@optics
sealed class Expression(open val type: Option<Type> = None) {
    @optics
    data class ArrayGet(override val type: Option<Type> = None, val array: Expression, val index: Expression) :
        Expression(type) {
        companion object
    }

    @optics
    data class ArrayLength(override val type: Option<Type> = None, val array: Expression) : Expression() {
        companion object
    }

    @optics
    data class BinaryOp(
        override val type: Option<Type> = None,
        val left: Expression,
        val op: Operator,
        val right: Expression
    ) : Expression(type) {
        companion object
    }

    @optics
    data class Id(override val type: Option<Type> = None, val id: String) : Expression(type) {
        companion object
    }

    @optics
    data class Constant(override val type: Option<Type> = None, val value: Int) : Expression(type) {
        companion object
    }

    @optics
    data class Invoke(
        override val type: Option<Type> = None,
        val obj: Expression,
        val method: Id,
        val arguments: List<Expression>?
    ) : Expression(type) {
        companion object
    }

    @optics
    data class Negate(override val type: Option<Type> = None, val expression: Expression) : Expression(type) {
        companion object
    }

    @optics
    data class New(override val type: Option<Type> = None, val className: Id) : Expression(type) {
        companion object
    }

    @optics
    data class NewArray(override val type: Option<Type> = None, val size: Expression) : Expression(type) {
        companion object
    }

    @optics
    data class Read(override val type: Option<Type> = None) : Expression(type) {
        companion object
    }

    @optics
    data class This(override val type: Option<Type> = None) : Expression(type) {
        companion object
    }

    object True : Expression(type = Some(Type.BooleanType))
    object False : Expression(type = Some(Type.BooleanType))
}


