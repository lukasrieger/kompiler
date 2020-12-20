package canonize

import frontend.ir.ast.IRExp
import frontend.ir.ast.IRStmt
import frontend.ir.ast.TempVar

data class CanonizedExp(val body: List<IRStmt>, val exp: IRExp) {
    constructor(exp: IRExp) : this(emptyList(), exp)
}

fun CanonizedExp.map(fn: (IRExp) -> IRExp): CanonizedExp = copy(exp = fn(exp))

fun combine(first: CanonizedExp, second: CanonizedExp, fn: (IRExp, IRExp) -> IRExp): CanonizedExp =
    (first compose second.body).run { CanonizedExp(body, fn(exp, second.exp)) }

fun combine(first: CanonizedExp, tail: List<CanonizedExp>, fn: (IRExp, List<IRExp>) -> IRExp): CanonizedExp =
    tail.asReversed()
        .fold(listOf<IRExp>() to listOf<IRStmt>()) { (joined, stmts), current ->
            val composed = current compose stmts

            (listOf(composed.exp) + joined) to composed.body
        }.let { (joined, stmts) ->
            val composedFirst = first compose stmts
            CanonizedExp(composedFirst.body, fn(composedFirst.exp, joined))
        }


fun CanonizedExp.combineToStmt(second: CanonizedExp, fn: (IRExp, IRExp) -> IRStmt): List<IRStmt> =
    compose(second.body).run { body + fn(exp, second.exp) }

fun CanonizedExp.toStmt(fn: (IRExp) -> IRStmt): List<IRStmt> = body + fn(exp)

infix fun IRStmt.commute(exp: IRExp): Boolean = exp is IRExp.Name || exp is IRExp.Const

infix fun List<IRStmt>.commute(exp: IRExp): Boolean = all { it commute exp }

infix fun CanonizedExp.compose(statements: List<IRStmt>): CanonizedExp = when (statements) {
    emptyList<IRStmt>() -> this
    else -> if (statements commute exp) {
        CanonizedExp(body + statements, exp)
    } else {
        val temp = IRExp.Temp(TempVar())
        CanonizedExp(body + listOf(IRStmt.Move(temp, exp)) + statements, temp)
    }
}