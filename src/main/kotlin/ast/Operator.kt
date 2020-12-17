package ast

import ast.Operator.*

enum class Operator { Plus, Minus, Times, Div, And, Lt, Gt }

fun Operator.typeOf(): Typed = when (this) {
    Plus, Minus, Times, Div -> Typed.Int
    And, Lt, Gt -> Typed.Boolean
}