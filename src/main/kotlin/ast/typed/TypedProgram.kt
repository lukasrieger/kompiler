package ast.typed

class TypedProgram(
    val mainClass: TypedClassDefinition,
    val classes: List<TypedClassDefinition>
)