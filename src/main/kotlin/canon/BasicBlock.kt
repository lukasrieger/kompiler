package canon

import frontend.ir.ast.IRStmt
import frontend.ir.ast.Label


data class BasicBlock(val body: List<IRStmt>) {
    val entry = (body.first() as? IRStmt.IRLabel)?.label ?: error("First element of basic block must be a Label.")
    val relay = body.last()
}

data class BlockContainer(
    val blocks: Map<Label, BasicBlock>,
    val start: Label,
    val end: Label,
    val isVirtual: Boolean
) {

    fun consumeFirst(): BasicBlock =
        (blocks as MutableMap).remove(start)!!

    fun first(): BasicBlock =
        blocks[start] ?: error("Can't retrieve first basic block at label: $start")

    fun last(): BasicBlock =
        blocks[end] ?: error("Can't retrieve last basic block at label: $end")

    operator fun get(index: Label): BasicBlock = blocks.getValue(index)
}