package ast

private const val BLANK_MODULE = "UNDEF_MODULE"

data class NamedRef(
    val simpleName: String,
    val moduleName: String = BLANK_MODULE
) {
    val qualifiedName: String = "${moduleName.ifBlank { BLANK_MODULE }}.$simpleName"
}

