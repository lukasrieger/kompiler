package frontend.syntax

sealed class Type {
    object BooleanType : Type()
    object IntType : Type()
    object VoidType : Type()
    data class ClassType(val name: String) : Type()
    object ArrayType : Type()
}