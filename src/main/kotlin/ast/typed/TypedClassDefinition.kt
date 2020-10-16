package ast.typed

data class TypedClassDefinition(
    val name: TypedExpr.Identifier,
    val superName: TypedExpr.Identifier?,
    val fields: VariableDefinitions,
    val methods: List<TypedMethodDefinition>,
)