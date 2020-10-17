package ast.typed

import ast.Type

inline class MethodArgs(val args: List<TypedExpr.Identifier>)
inline class VariableDefinitions(val definitions: List<TypedExpr.Identifier>)

data class TypedMethodDefinition(
    val name: TypedExpr.Identifier,
    val arguments: MethodArgs,
    val variables: VariableDefinitions,
    val body: TypedStatement,
    val returnExpression: TypedExpr,
    val returnType: Type
)


data class MethodTypeDescriptor(
    val name: TypedExpr.Identifier,
    val arguments: MethodArgs,
    val returnType: Type
)