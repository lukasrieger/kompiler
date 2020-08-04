package frontend.syntax

data class Value(val id: String, val type: Type)


data class ClassDefinition(
    val name: Expression.Id,
    val superName: Expression.Id?,
    val fields: List<VariableDefinition>,
    val methods: List<MethodDefinition>,
    val symbolTable: SymbolTable = buildSymbolTable()
)

data class MainClassDefinition(
    val name: Expression.Id,
    val argsName: Expression.Id,
    val body: Statement
)

fun buildSymbolTable(): Nothing = TODO()