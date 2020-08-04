package frontend.syntax



data class Args(val type: Type, val id: Expression.Id)
data class MethodArgs(val args: List<Args>)

data class MethodDefinition(
    val type: Type,
    val name: Expression.Id,
    val args: MethodArgs?,
    val vars: List<VariableDefinition>,
    val body: Statement,
    val returnExpression: Expression
)