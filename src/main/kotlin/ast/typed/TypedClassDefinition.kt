package ast.typed

import ast.Name

data class TypedClassDefinition(
    val name: TypedExpr.Identifier,
    val superName: TypedExpr.Identifier?,
    val fields: VariableDefinitions,
    val methods: List<TypedMethodDefinition>,
)


data class ClassTypeDescriptor(
    val name: TypedExpr.Identifier,
    val superName: TypedExpr.Identifier?,
    val methods: Map<Name, MethodTypeDescriptor>
)
