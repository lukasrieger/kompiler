package frontend.ir.ast


data class Label private constructor(val id: Int, val name: String) : Comparable<Label> {

    companion object {

        operator fun invoke(name: String? = null): Label {
            val id = id()
            return Label(id = id, name = name ?: "L$$${id}")
        }

        private var currentId = 0

        private fun id() = currentId++

    }

    override fun compareTo(other: Label): Int = id - other.id
}