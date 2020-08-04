package frontend.syntax



enum class Operator { Plus, Minus, Times, Div, And, Lt, Gt }

interface Symbol {
    val id: String
}

interface SymbolTable