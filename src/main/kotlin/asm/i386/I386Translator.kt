package asm.i386

import asm.MachineInstruction
import frontend.CompilerError
import frontend.ir.ast.*
import util.StateS
import util.state

data class AsmError(val err: Throwable) : CompilerError.Severe("Can't translate program to assembler", "")

typealias InstructionState = List<MachineInstruction>

//object AsmTranslate : Stage<IRProgram, MachineProgram> {
//    override val identifier: StageIdentifier =
//        StageIdentifier("AsmTranslate", 5)
//
//    override fun run(input: IRProgram, config: CompilerConfiguration): Either<CompilerError, MachineProgram> =
//        try {
//            Right(input.toI386())
//        } catch (err: Throwable) {
//            Left(AsmError(err))
//        }
//}



object TranslationContext {
    fun emit(instr: MachineInstruction): StateS<InstructionState, Unit> = TODO()
        //StateS { (it + instr) toT Unit }
}

//fun IRProgram.toI386(): MachineProgram =
//    functions.map { it.toI386() }.let { TODO() }

//fun IRFunction.toI386(): MachineFunction = with(TranslationContext) {
//
//    val (instrState, _) = state<InstructionState, Unit> { body.map { toI386(it)() } }(emptyList())
//
//    I386Function(
//        name = name,
//        body = instrState,
//        frameSize = -1
//    )
//}


fun TranslationContext.toI386(exp: IRExp): StateS<InstructionState, Operand> = state {

    when (exp) {
        is IRExp.BinOp -> {
            val reg = Operand.Reg()
            val right = Operand.Reg()
            val left = Operand.Reg()

            val leftOp = toI386(exp.left)()
            emit(BinaryInstruction(BinaryInstruction.Kind.MOV, left, leftOp))()

            val rightOp = toI386(exp.right)()
            emit(BinaryInstruction(BinaryInstruction.Kind.MOV, right, rightOp))()

            val next = Operand.Reg()

            when (exp.op) {
                IROp.PLUS -> {
                    emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))()
                    emit(BinaryInstruction(BinaryInstruction.Kind.ADD, reg, right))()
                }
                IROp.MINUS -> {
                    emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))()
                    emit(BinaryInstruction(BinaryInstruction.Kind.SUB, reg, right))()
                }
                IROp.MUL -> {
                    emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))()
                    emit(BinaryInstruction(BinaryInstruction.Kind.IMUL, reg, right))()
                }
                IROp.DIV -> {
                    emit(BinaryInstruction(BinaryInstruction.Kind.MOV, EAX, next))()
                    emit(NullaryInstruction(NullaryInstruction.Kind.CDQ))()
                    emit(UnaryInstruction(UnaryInstruction.Kind.IDIV, right))()
                    emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, EAX))()
                }
                IROp.AND -> {
                    emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))()
                    emit(BinaryInstruction(BinaryInstruction.Kind.AND, reg, right))()
                }
                IROp.OR -> {
                    emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))()
                    emit(BinaryInstruction(BinaryInstruction.Kind.OR, reg, right))()
                }
                IROp.LSHIFT -> {
                    emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))()
                    emit(BinaryInstruction(BinaryInstruction.Kind.SHL, reg, right))()
                }
                IROp.RSHIFT -> {
                    emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))()
                    emit(BinaryInstruction(BinaryInstruction.Kind.SHR, reg, right))()
                }
                IROp.ARSHIFT -> {
                    emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))()
                    emit(BinaryInstruction(BinaryInstruction.Kind.SHL, reg, right))()
                }
                IROp.XOR -> {
                    emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))()
                    emit(BinaryInstruction(BinaryInstruction.Kind.XOR, reg, right))()
                }
            }

            reg

        }
        is IRExp.Call -> TODO()
        is IRExp.Const -> Operand.Imm(exp.value)
        is IRExp.EStmtSeq -> {
            toI386(exp.stm)()
            toI386(exp.exp)()
        }
        is IRExp.Mem -> {
            val addr = toI386(exp.address)()
            val temp = TempVar()

            when (addr) {
                is Operand.Reg -> Operand.Mem(addr.reg)
                is Operand.Mem -> {
                    emit(BinaryInstruction(BinaryInstruction.Kind.MOV, Operand.Reg(temp), addr))()
                    Operand.Mem(temp)
                }
                else -> error("Encountered untranslatable operand: $addr")
            }

        }
        is IRExp.Name -> Operand.ByLabel(exp.label)
        is IRExp.Param -> Operand.Mem(EBP.reg, null, null, 8 + 4 * exp.index)
        is IRExp.Temp -> Operand.Reg(exp.temp)
    }
}


fun TranslationContext.toI386(stmt: IRStmt): StateS<InstructionState, Unit> = state {
    when (stmt) {
        is IRStmt.Jump -> stmt.targets
            .filter { toI386(stmt.dest).toString() == it.toString() }
            .forEach { emit(JumpInstruction(JumpInstruction.Kind.JMP, it)) }
        is IRStmt.CJump -> {
            val left = Operand.Reg()
            val right = Operand.Reg()

            emit(BinaryInstruction(BinaryInstruction.Kind.MOV, left, toI386(stmt.left)()))
            emit(BinaryInstruction(BinaryInstruction.Kind.MOV, right, toI386(stmt.right)()))
            emit(BinaryInstruction(BinaryInstruction.Kind.MOV, left, right))

            when (stmt.rel) {
                Rel.EQ -> {
                    emit(JumpInstruction(JumpInstruction.Cond.E, stmt.trueLabel))()
                    emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))()
                }

                Rel.NE -> {
                    emit(JumpInstruction(JumpInstruction.Cond.NE, stmt.trueLabel))()
                    emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))()
                }
                Rel.LT -> {
                    emit(JumpInstruction(JumpInstruction.Cond.L, stmt.trueLabel))()
                    emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))()
                }
                Rel.GT -> {
                    emit(JumpInstruction(JumpInstruction.Cond.G, stmt.trueLabel))()
                    emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))()
                }
                Rel.LE -> {
                    emit(JumpInstruction(JumpInstruction.Cond.LE, stmt.trueLabel))()
                    emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))()
                }
                Rel.GE -> {
                    emit(JumpInstruction(JumpInstruction.Cond.GE, stmt.trueLabel))()
                    emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))()
                }
                Rel.ULT -> {
                    emit(JumpInstruction(JumpInstruction.Cond.B, stmt.trueLabel))()
                    emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))()
                }
                Rel.ULE -> {
                    emit(JumpInstruction(JumpInstruction.Cond.NE, stmt.trueLabel))()
                    emit(BinaryInstruction(BinaryInstruction.Kind.CMP, left, right))()
                    emit(JumpInstruction(JumpInstruction.Cond.A, stmt.falseLabel))()
                    emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))()
                }
                Rel.UGT -> {
                    emit(JumpInstruction(JumpInstruction.Cond.A, stmt.trueLabel))()
                    emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))()
                }
                Rel.UGE -> {
                    emit(JumpInstruction(JumpInstruction.Cond.NE, stmt.trueLabel))()
                    emit(BinaryInstruction(BinaryInstruction.Kind.CMP, left, right))()
                    emit(JumpInstruction(JumpInstruction.Cond.B, stmt.falseLabel))()
                    emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))()
                }
            }
        }
        is IRStmt.IRLabel -> emit(LabelInstruction(stmt.label))()
        is IRStmt.Move -> {
            val dest = toI386(stmt.dest)()
            val src = toI386(stmt.src)()

            when {
                dest is Operand.Mem && src is Operand.Mem -> {
                    val reg = Operand.Reg()
                    emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, src))()
                    emit(BinaryInstruction(BinaryInstruction.Kind.MOV, dest, reg))()
                }
                else -> emit(BinaryInstruction(BinaryInstruction.Kind.MOV, dest, src))()

            }
        }
        is IRStmt.StmSeq -> stmt.statements.forEach { toI386(it)() }
    }
}

