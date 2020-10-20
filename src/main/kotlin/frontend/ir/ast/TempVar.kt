package frontend.ir.ast

data class TempVar(val id: Int = id()) {
    companion object {
        private var currentId = 0

        fun id() = currentId++
        fun reset(): Unit { currentId = 0 }
    }
}