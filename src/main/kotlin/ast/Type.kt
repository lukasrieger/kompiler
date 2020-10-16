package ast

sealed class Type {
    object BooleanType : Type()
    object IntType : Type()
    object VoidType : Type()
    data class ClassType(val name: Name) : Type()
    object ArrayType : Type()

    object UnknownType : Type()
}