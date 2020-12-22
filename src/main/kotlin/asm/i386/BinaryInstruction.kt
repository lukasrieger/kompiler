package asm.i386

import asm.MachineInstruction
import frontend.ir.ast.Label
import frontend.ir.ast.TempVar

data class BinaryInstruction(
    val kind: Kind,
    val dst: Operand,
    val src: Operand
) : MachineInstruction {

    enum class Kind {
        MOV, ADD, SUB, SHL, SHR, SAL, SAR, AND, OR, XOR, TEST, CMP, LEA, IMUL, DBG
    }

    override val isFallThrough: Boolean = true


    override fun use(): Iterable<TempVar> = src.use().let {
        when {
            kind == Kind.MOV && (dst is Operand.Mem) -> it + dst.use()
            kind != Kind.MOV -> it + dst.use()
            else -> it
        }
    }


    override fun def(): Iterable<TempVar> = when {
        kind == Kind.CMP -> TODO()
        dst is Operand.Reg -> dst.use()
        else -> emptyList()
    }

    override fun jumps(): Iterable<Label> = emptyList()

    override fun moveBetweenJumps(): Pair<TempVar, TempVar>? = when {
        kind == Kind.MOV && src is Operand.Reg && dst is Operand.Reg ->
            dst.reg to src.reg
        else -> null
    }

    override fun label(): Label? = null

    override fun rename(sigma: (TempVar) -> TempVar): MachineInstruction = copy(
        src = src.rename(sigma),
        dst = dst.rename(sigma)
    )
}