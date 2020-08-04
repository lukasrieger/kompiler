package frontend.typecheck

import arrow.core.Valid
import arrow.core.ValidatedNel
import frontend.syntax.*
import frontend.syntax.Expression.*

object TypeChecker {

    fun check(program: Program): ValidatedNel<Nothing, Program> = TODO()
    fun checkExpression(expression: Expression): ValidatedNel<Nothing, Expression> = when (val exp = expression) {
        is ArrayGet -> TODO()
        is ArrayLength -> TODO()
        is BinaryOp -> TODO()
        is Id -> TODO()
        is Constant -> TODO()
        is Invoke -> TODO()
        is Negate -> checkExpression(exp.expression)
        is New -> TODO()
        is NewArray -> TODO()
        is Read -> TODO()
        is This -> TODO()
        True -> Valid(exp).toValidatedNel()
        False -> Valid(exp).toValidatedNel()
    }
}