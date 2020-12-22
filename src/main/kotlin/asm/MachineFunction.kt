package asm

import frontend.ir.ast.TempVar

interface MachineFunction : Iterable<MachineInstruction> {

    fun rename(sigma: (TempVar) -> TempVar): MachineFunction

    fun spill(toSpill: List<TempVar>): MachineFunction
}