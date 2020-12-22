package asm.i386

import asm.MachineInstruction
import frontend.ir.ast.Label
import frontend.ir.ast.TempVar

data class UnaryInstruction(val kind: Kind, val op: Operand) : MachineInstruction {
    enum class Kind {
        PUSH, POP, NEG, NOT, INC, DEC, IDIV, DBG
    }

    override val isFallThrough: Boolean = true

    override fun use(): Iterable<TempVar> = when (kind) {
        Kind.PUSH, Kind.NEG, Kind.NOT, Kind.INC, Kind.DEC -> op.use()
        Kind.IDIV -> op.use() + listOf(EDX.reg)
        else -> emptyList()
    }

    override fun def(): Iterable<TempVar> = when (kind) {
        Kind.POP, Kind.NEG, Kind.NOT, Kind.INC, Kind.DEC -> op.use()
        Kind.IDIV -> listOf(EAX.reg, EDX.reg)
        else -> emptyList()
    }

    override fun jumps(): Iterable<Label> = emptyList()

    override fun moveBetweenJumps(): Pair<TempVar, TempVar>? = null

    override fun label(): Label? = null

    override fun rename(sigma: (TempVar) -> TempVar): MachineInstruction = copy(op = op.rename(sigma))
}