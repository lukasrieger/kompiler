package frontend.ir.ast

data class IRProgram(
    val functions: List<IRFunction>
) : Iterable<IRFunction> by functions