package ast.untyped


sealed class UntypedStatement {

    data class ArrayAssign(
        val id: UntypedExpr.Identifier,
        val index: UntypedExpr,
        val value: UntypedExpr
    ) : UntypedStatement()

    data class Assign(
        val id: UntypedExpr.Identifier,
        val value: UntypedExpr
    ) : UntypedStatement()

    data class If(
        val condition: UntypedExpr,
        val trueBranch: UntypedStatement,
        val falseBranch: UntypedStatement
    ) : UntypedStatement()

    data class While(
        val condition: UntypedExpr,
        val body: UntypedStatement
    ) : UntypedStatement()

    data class Print(val output: UntypedExpr) : UntypedStatement()
    data class Write(val output: UntypedExpr) : UntypedStatement()
    data class SequenceOf(val statements: List<UntypedStatement>) : UntypedStatement()

}