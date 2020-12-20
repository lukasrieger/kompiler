package ast

import frontend.CompilerError


sealed class TypeError : CompilerError.Severe("", "") {
    data class TypeMismatch(
        val expected: Type,
        val actual: Type,
        val expr: Any
    ) : TypeError()

    data class BinaryMismatch(
        val left: Type,
        val right: Type,
        val expr: ExpF<TExp>
    ) : TypeError()

    data class IncompatibleArguments(
        val expected: List<Type>,
        val actual: List<Type>,
        val expr: ExpF<TExp>
    ) : TypeError()

    data class UnknownReference(val ref: NamedRef) : TypeError()

}

interface Type

sealed class TypeState {
    open class Valid : TypeState()
    open class Invalid : TypeState()
}

interface Typed : Type {
    object Boolean : Typed, TypeState.Valid()
    object Int : Typed, TypeState.Valid()
    object Void : Typed, TypeState.Valid()
    object Array : Typed, TypeState.Valid()

    data class Class(val name: NamedRef) : Typed, TypeState.Valid()
    data class Error(val err: TypeError) : Typed, TypeState.Invalid()
}

object Untyped : Type


