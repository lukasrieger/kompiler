package ast.untyped

import ast.Type
import ast.typed.TypedExpr


inline class MethodArgs(val args: List<TypedExpr.Identifier>)
inline class VariableDefinitions(val definitions: List<TypedExpr.Identifier>)

data class UntypedMethodDefinition(
    val name: UntypedExpr.Identifier,
    val arguments: MethodArgs,
    val variables: VariableDefinitions,
    val body: UntypedStatement,
    val returnExpression: UntypedExpr,
    val returnType: Type
)