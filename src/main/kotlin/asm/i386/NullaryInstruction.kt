package asm.i386

import asm.MachineInstruction
import frontend.ir.ast.Label
import frontend.ir.ast.TempVar

data class NullaryInstruction(val kind: Kind) : MachineInstruction {
    enum class Kind {
        RET, LEAVE, NOP, DBG, CDQ
    }

    override val isFallThrough: Boolean = when (kind) {
        Kind.RET -> false
        else -> true
    }

    override fun use(): Iterable<TempVar> = when (kind) {
        Kind.RET -> listOf(EBX.reg, EDI.reg, ESI.reg, EAX.reg)
        else -> emptyList()
    }

    override fun def(): Iterable<TempVar> = emptyList()

    override fun jumps(): Iterable<Label> = emptyList()

    override fun moveBetweenJumps(): Pair<TempVar, TempVar>? = null

    override fun label(): Label? = null

    override fun rename(sigma: (TempVar) -> TempVar): MachineInstruction = this
}