package frontend.ir.ast

data class Label(val name: String) {
    companion object {
        private var currentId = 0

        fun id() = currentId++
        fun reset(): Unit { currentId = 0 }
    }

    constructor() : this("L$$${id()}")
}