package asm.i386

import asm.MachineFunction
import asm.MachineInstruction
import frontend.ir.ast.Label
import frontend.ir.ast.TempVar

data class I386Function(
    val name: Label,
    val body: List<MachineInstruction>,
    val frameSize: Int
) : MachineFunction, Iterable<MachineInstruction> by body {

    override fun rename(sigma: (TempVar) -> TempVar): MachineFunction =
        copy(body = map { it.rename(sigma) })

    override fun spill(toSpill: List<TempVar>): MachineFunction =
        copy(body = toSpill.fold(body) { acc, current -> spillHelper(acc, current) })


    private fun spillHelper(body: List<MachineInstruction>, toSpill: TempVar): List<MachineInstruction> {
        val newTemp = TempVar()
        return body.fold(listOf()) { acc, t ->
            val isUsed = t.use().any(toSpill::equals)
            val isDefined = t.def().any(toSpill::equals)

            val used = BinaryInstruction(
                BinaryInstruction.Kind.MOV,
                Operand.Reg(newTemp), Operand.Mem(EBP.reg, null, null, -frameSize)
            ).takeIf { isUsed }


            val renamed = if (isUsed || isDefined) t.rename { if (it == toSpill) newTemp else it } else t

            val defined = BinaryInstruction(
                BinaryInstruction.Kind.MOV,
                Operand.Mem(EBP.reg, null, null, -frameSize),
                Operand.Reg(newTemp)
            ).takeIf { isDefined }

            val updateEsp = BinaryInstruction(BinaryInstruction.Kind.SUB, ESP, Operand.Imm(frameSize))

            val next = listOfNotNull(used) + acc + listOf(renamed) + listOfNotNull(defined)

            //ouch
            (next as MutableList).add(2, updateEsp)

            next
        }
    }
}