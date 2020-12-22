package asm.i386

import asm.MachineInstruction
import frontend.ir.ast.Label
import frontend.ir.ast.TempVar

data class JumpInstruction(
    val kind: Kind,
    val label: Label?,
    val dest: Operand?,
    val cond: Cond?
) : MachineInstruction {

    constructor(kind: Kind, label: Label) : this(kind, label, null, null)
    constructor(kind: Kind, dest: Operand) : this(kind, null, dest, null)
    constructor(cond: Cond, label: Label) : this(Kind.J, label, null, cond)


    enum class Kind {
        JMP, J, CALL, DBG
    }

    enum class Cond {
        E, NE, L, LE, G, GE, Z, A, B, DBG
    }

    override val isFallThrough: Boolean = kind != Kind.JMP

    override fun use(): Iterable<TempVar> = dest?.use() ?: emptyList()

    override fun def(): Iterable<TempVar> = when (kind) {
        Kind.CALL -> listOf(EAX.reg, EDX.reg, ECX.reg) // caller save registers
        else -> emptyList()
    }

    override fun jumps(): Iterable<Label> = listOfNotNull(label)

    override fun moveBetweenJumps(): Pair<TempVar, TempVar>? = null

    override fun label(): Label? = null

    override fun rename(sigma: (TempVar) -> TempVar): MachineInstruction = copy(dest = dest?.rename(sigma))
}