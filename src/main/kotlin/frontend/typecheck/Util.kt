package frontend.typecheck

import arrow.core.Either
import ast.Name
import ast.Type
import ast.typed.TypedExpr
import ast.untyped.UntypedExpr
import frontend.CompilerError

fun UntypedExpr.Identifier.toTyped(type: Type? = null) =
    TypedExpr.Identifier(
        name = name,
        type = type ?: Type.ClassType(name)
    )

sealed class TypeError : CompilerError.Severe("", "") {
    data class TypeMismatch(
        val expected: Type,
        val actual: Type,
        val expr: Any
    ) : TypeError()

    data class BinaryMismatch(
        val left: Type,
        val right: Type,
        val expr: TypedExpr
    ) : TypeError()

    data class IncompatibleArguments(
        val expected: List<Type>,
        val actual: List<Type>,
        val expr: TypedExpr
    ) : TypeError()

    data class UnknownReference(val ref: Name) : TypeError()

}

internal fun TypedExpr.classTypeOf(typeRef: Type.ClassType): Either<TypeError, TypedExpr> = Either.conditionally(
    type == typeRef, ifFalse = {
        TypeError.TypeMismatch(
            expected = typeRef,
            actual = type,
            expr = this
        )
    }, ifTrue = { this }
)

internal inline fun <reified T : Type> TypedExpr.typeOf(alt: Type = Type.UnknownType): Either<TypeError, TypedExpr> =
    Either.conditionally(
        type is T, ifFalse = {
            TypeError.TypeMismatch(
                expected = T::class.objectInstance ?: alt,
                actual = type,
                expr = this
            )
        }, ifTrue = { this })

