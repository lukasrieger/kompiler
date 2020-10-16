package ast

data class Name(
    val simpleName: String,
    val moduleName: String
) {
    val qualifiedName: String
        get() = "$moduleName.$simpleName"
}

