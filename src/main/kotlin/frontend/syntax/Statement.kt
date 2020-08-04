package frontend.syntax

sealed class Statement
data class ArrayAssign(val id: Expression.Id, val index: Expression, val value: Expression) : Statement()
data class Assign(val id: Expression.Id, val value: Expression) : Statement()
data class If(val condition: Expression, val trueBranch: Statement, val falseBranch: Statement) : Statement()
data class Print(val output: Expression) : Statement()
data class SequenceOf(val statements: List<Statement>) : Statement()
data class While(val condition: Expression, val body: Statement) : Statement()
data class Write(val output: Expression) : Statement()



