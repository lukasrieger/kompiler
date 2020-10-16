package ast.typed

sealed class TypedStatement {

    data class ArrayAssign(
        val id: TypedExpr.Identifier,
        val index: TypedExpr,
        val value: TypedExpr
    ) : TypedStatement()

    data class Assign(
        val id: TypedExpr.Identifier,
        val value: TypedExpr
    ) : TypedStatement()

    data class If(
        val condition: TypedExpr,
        val trueBranch: TypedStatement,
        val falseBranch: TypedStatement
    ) : TypedStatement()

    data class While(
        val condition: TypedExpr,
        val body: TypedStatement
    ) : TypedStatement()

    data class Print(val output: TypedExpr) : TypedStatement()
    data class Write(val output: TypedExpr) : TypedStatement()
    data class SequenceOf(val statements: List<TypedStatement>) : TypedStatement()

}