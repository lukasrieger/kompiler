package frontend.typecheck

import arrow.core.Either
import ast.*
import frontend.ir.TypedExpr

fun ExpF.Identifier<Exp>.toTyped(): ExpF.Identifier<TExp> = ExpF.Identifier(name = name)


internal fun TypedExpr.classTypeOf(typeRef: Typed.Class): Either<TypeError, TypedExpr> = Either.conditionally(
    type == typeRef, ifFalse = {
        TypeError.TypeMismatch(
            expected = typeRef,
            actual = type,
            expr = this
        )
    }, ifTrue = { this }
)

internal inline fun <reified T : Type> TExp.ensureType(alt: Type = Untyped): Either<Pair<TExp, TypeError>, TExp> =
    Either.conditionally(
        type is T, ifFalse = {
            this to TypeError.TypeMismatch(
                expected = T::class.objectInstance ?: alt,
                actual = type,
                expr = this
            )
        }, ifTrue = { this })

