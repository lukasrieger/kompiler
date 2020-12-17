package frontend.ir.ast


data class Label private constructor(val id: Int, val name: String) : Comparable<Label> {

    companion object {

        operator fun invoke(name: String? = null): Label {
            val id = id()
            return Label(id = id, name = name ?: "L$$${id}")
        }

        private var currentId = 0

        internal fun id() = currentId++
        internal fun reset() {
            currentId = 0
        }
    }

    override fun compareTo(other: Label): Int = id - other.id
}