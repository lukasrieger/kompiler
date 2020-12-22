package asm

interface MachineProgram : Iterable<MachineFunction> {
    fun assembly(): String
}