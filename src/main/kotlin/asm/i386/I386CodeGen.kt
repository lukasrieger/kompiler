package asm.i386

import asm.CodeGen
import asm.MachineProgram
import frontend.ir.ast.IRProgram
import frontend.ir.ast.TempVar

val EAX = Operand.Reg(TempVar(-1))
val EBX = Operand.Reg(TempVar(-2))
val ECX = Operand.Reg(TempVar(-3))
val EDX = Operand.Reg(TempVar(-4))
val ESI = Operand.Reg(TempVar(-5))
val EDI = Operand.Reg(TempVar(-6))
val ESP = Operand.Reg(TempVar(-7))
val EBP = Operand.Reg(TempVar(-8))

object I386CodeGen : CodeGen {
    override val allRegisters: List<TempVar> = (-1 downTo -8).map(::TempVar)
    override val generalPurposeRegisters: List<TempVar> = (-1 downTo -6).map(::TempVar)

    override fun IRProgram.toMachineProgram(): MachineProgram = TODO()
}