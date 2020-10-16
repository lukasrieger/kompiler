package ast

import ast.Operator.*

enum class Operator { Plus, Minus, Times, Div, And, Lt, Gt }

fun Operator.typeOf() = when(this) {
    Plus, Minus, Times, Div -> Type.IntType
    And, Lt, Gt -> Type.BooleanType
}