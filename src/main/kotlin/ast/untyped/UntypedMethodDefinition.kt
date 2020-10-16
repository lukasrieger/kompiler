package ast.untyped

import ast.typed.TypedExpr


inline class MethodArgs(val args: List<UntypedExpr.Identifier>)
inline class VariableDefinitions(val definitions: List<TypedExpr.Identifier>)

data class UntypedMethodDefinition(
    val name: UntypedExpr.Identifier,
    val arguments: MethodArgs,
    val variables: VariableDefinitions,
    val body: UntypedStatement,
    val returnExpression: UntypedExpr
)