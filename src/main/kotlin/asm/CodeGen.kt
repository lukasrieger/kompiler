package asm

import frontend.ir.ast.IRProgram
import frontend.ir.ast.TempVar

interface CodeGen {

    val allRegisters: List<TempVar>
    val generalPurposeRegisters: List<TempVar>

    fun IRProgram.toMachineProgram(): MachineProgram
}