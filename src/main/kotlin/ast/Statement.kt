package ast

data class Stmt(val stmt: StmtF<Stmt, Exp>)

data class TStmt(val stmt: StmtF<TStmt, TExp>, val type: Type)

sealed class StmtF<T, E> {

    data class ArrayAssign<T, E>(
        val id: ExpF.Identifier<E>,
        val index: E,
        val value: E
    ) : StmtF<T, E>()

    data class Assign<T, E>(
        val id: ExpF.Identifier<E>,
        val value: E
    ) : StmtF<T, E>()

    data class If<T, E>(
        val condition: E,
        val trueBranch: T,
        val falseBranch: T
    ) : StmtF<T, E>()

    data class While<T, E>(
        val condition: E,
        val body: T
    ) : StmtF<T, E>()

    data class Print<T, E>(val output: E) : StmtF<T, E>()
    data class Write<T, E>(val output: E) : StmtF<T, E>()
    data class SequenceOf<T, E>(val statements: List<T>) : StmtF<T, E>()

}


infix fun <T : StmtF<TStmt, TExp>> T.typeOf(type: Type): TStmt = TStmt(this, type)
