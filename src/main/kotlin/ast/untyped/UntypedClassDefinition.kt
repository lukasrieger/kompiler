package ast.untyped


data class UntypedClassDefinition(
    val name: UntypedExpr.Identifier,
    val superName: UntypedExpr.Identifier?,
    val fields: VariableDefinitions,
    val methods: List<UntypedMethodDefinition>
)