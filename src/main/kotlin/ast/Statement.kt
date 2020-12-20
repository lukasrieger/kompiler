package ast

import arrow.core.Either
import arrow.core.right

sealed class StmtK

data class Stmt(val stmt: StmtF<Stmt, Exp>) : StmtK()

data class TStmt(val stmt: StmtF<TStmt, TExp>, val type: Type) : StmtK()

sealed class StmtF<T : StmtK, E : ExpK> {

    data class ArrayAssign<T : StmtK, E : ExpK>(
        val id: ExpF.Symbol<E>,
        val index: E,
        val value: E
    ) : StmtF<T, E>()

    data class Assign<T : StmtK, E : ExpK>(
        val id: ExpF.Symbol<E>,
        val value: E
    ) : StmtF<T, E>()

    data class If<T : StmtK, E : ExpK>(
        val condition: E,
        val trueBranch: T,
        val falseBranch: T
    ) : StmtF<T, E>()

    data class While<T : StmtK, E : ExpK>(
        val condition: E,
        val body: T
    ) : StmtF<T, E>()

    data class Print<T : StmtK, E : ExpK>(val output: E) : StmtF<T, E>()
    data class Write<T : StmtK, E : ExpK>(val output: E) : StmtF<T, E>()
    data class SequenceOf<T : StmtK, E : ExpK>(val statements: List<T>) : StmtF<T, E>()

}


infix fun <T : StmtF<TStmt, TExp>> T.typeOf(type: Type): Either.Right<TStmt> =
    TStmt(this, type).right() as Either.Right<TStmt>
