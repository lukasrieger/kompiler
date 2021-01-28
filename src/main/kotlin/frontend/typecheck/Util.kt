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


internal inline fun <reified T : Type> TExp.ensureType2(alt: Type = Untyped): StateF<TypeError, TExp> =
    when (type) {
        is T -> this.success()
        else -> TypeError.TypeMismatch(
            expected = T::class.objectInstance ?: alt,
            actual = type,
            expr = this
        ).let { it toT this }
    }

internal fun TExp.typeEq(type: Type): StateF<TypeError, TExp> = when (type) {
    this.type -> this.success()
    else -> TypeError.TypeMismatch(
        expected = type,
        actual = this.type,
        expr = this
    ).let { it toT this }
}

internal fun StmtF.Assign<TStmt, TExp>.typeEq(type: Type): StateF<TypeError, StmtF.Assign<TStmt, TExp>> =
    when (val r = value.typeEq(type)) {
        is StateF.Err -> StateF.Err(r.errors, this)
        is StateF.Succ -> this.success()
    }