package ast

private const val BLANK_MODULE = "UNDEF_MODULE"

data class Name(
    val simpleName: String,
    val moduleName: String
) {
    val qualifiedName: String
        get() = "${moduleName.ifBlank { BLANK_MODULE }}.$simpleName"
}

