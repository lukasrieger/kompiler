package frontend.typecheck

import arrow.core.Either
import ast.*

fun ExpF.Symbol<Exp>.toTyped(): ExpF.Symbol<TExp> = ExpF.Symbol(name = name)

data class TExpErr(val exp: TExp, val error: TypeError)


internal inline fun <reified T : Type> TExp.ensureType(alt: Type = Untyped): Either<TExpErr, TExp> =
    Either.conditionally(
        type is T, ifFalse = {
            TExpErr(
                this, TypeError.TypeMismatch(
                    expected = T::class.objectInstance ?: alt,
                    actual = type,
                    expr = this
                )
            )
        }, ifTrue = { this })

