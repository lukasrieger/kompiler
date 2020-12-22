package asm.i386

import frontend.ir.ast.Label
import frontend.ir.ast.TempVar

sealed class Operand {
    open fun use(): List<TempVar> = emptyList()
    abstract fun rename(sigma: (TempVar) -> TempVar): Operand

    data class Imm(val const: Int) : Operand() {
        override fun rename(sigma: (TempVar) -> TempVar): Operand = this
    }

    data class Reg(val reg: TempVar = TempVar()) : Operand() {
        override fun rename(sigma: (TempVar) -> TempVar): Operand = Reg(sigma(reg))

        override fun use(): List<TempVar> = listOf(reg)
    }

    data class Mem(val base: TempVar?, val scale: Int?, val index: TempVar?, val displacement: Int) : Operand() {
        constructor(base: TempVar) : this(base, null, null, 0)

        override fun use(): List<TempVar> = listOfNotNull(
            base?.takeIf { it != ESP.reg && it != EBP.reg },
            index?.takeIf { it != ESP.reg && it != EBP.reg }
        )

        override fun rename(sigma: (TempVar) -> TempVar): Operand =
            Mem(base?.let(sigma), scale, index?.let(sigma), displacement)
    }

    data class ByLabel(val name: Label) : Operand() {
        override fun rename(sigma: (TempVar) -> TempVar): Operand = this
    }
}