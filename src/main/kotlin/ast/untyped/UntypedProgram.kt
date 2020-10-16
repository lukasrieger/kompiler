package ast.untyped

class UntypedProgram(
    val mainClass: UntypedClassDefinition,
    val classes: List<UntypedClassDefinition>
)