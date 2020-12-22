package asm.i386

import asm.MachineInstruction
import frontend.ir.ast.Label
import frontend.ir.ast.TempVar


data class LabelInstruction(val label: Label) : MachineInstruction {
    override val isFallThrough: Boolean = false

    override fun use(): Iterable<TempVar> = emptyList()

    override fun def(): Iterable<TempVar> = emptyList()

    override fun jumps(): Iterable<Label> = emptyList()

    override fun moveBetweenJumps(): Pair<TempVar, TempVar>? = null

    override fun label(): Label? = null

    override fun rename(sigma: (TempVar) -> TempVar): MachineInstruction = this
}