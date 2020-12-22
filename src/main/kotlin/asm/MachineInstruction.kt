package asm

import frontend.ir.ast.Label
import frontend.ir.ast.TempVar

interface MachineInstruction {
    val isFallThrough: Boolean

    fun use(): Iterable<TempVar>

    fun def(): Iterable<TempVar>

    fun jumps(): Iterable<Label>

    fun moveBetweenJumps(): Pair<TempVar, TempVar>?

    fun label(): Label?

    fun rename(sigma: (TempVar) -> TempVar): MachineInstruction
}