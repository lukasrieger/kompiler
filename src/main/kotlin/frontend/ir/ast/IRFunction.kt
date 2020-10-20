package frontend.ir.ast

data class IRFunction(
    val name: Label,
    val paramCount: Int,
    val body: List<IRStmt>,
    val returnVar: TempVar,
    val locals: List<TempVar>
) : Iterable<IRStmt> by body

