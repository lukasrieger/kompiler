package asm.i386

import arrow.core.Either
import arrow.core.extensions.either.monad.monad
import asm.MachineFunction
import asm.MachineInstruction
import frontend.ir.ast.*


class TranslationContext {
    private val instructions: MutableList<MachineInstruction> = mutableListOf()

    fun emit(instruction: MachineInstruction) {
        instructions += instruction
    }
}

fun IRFunction.toI386(): MachineFunction = TODO()

fun withState(exp: IRExp) = Either.monad<>()

fun TranslationContext.toI386(exp: IRExp): Operand = when (exp) {
    is IRExp.BinOp -> {
        val reg = Operand.Reg()
        val right = Operand.Reg()
        val left = Operand.Reg()

        val leftOp = toI386(exp.left)
        emit(BinaryInstruction(BinaryInstruction.Kind.MOV, left, leftOp))

        val rightOp = toI386(exp.right)
        emit(BinaryInstruction(BinaryInstruction.Kind.MOV, right, rightOp))

        val next = Operand.Reg()

        when (exp.op) {
            IROp.PLUS -> {
                emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))
                emit(BinaryInstruction(BinaryInstruction.Kind.ADD, reg, right))
            }
            IROp.MINUS -> {
                emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))
                emit(BinaryInstruction(BinaryInstruction.Kind.SUB, reg, right))
            }
            IROp.MUL -> {
                emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))
                emit(BinaryInstruction(BinaryInstruction.Kind.IMUL, reg, right))
            }
            IROp.DIV -> {
                emit(BinaryInstruction(BinaryInstruction.Kind.MOV, EAX, next))
                emit(NullaryInstruction(NullaryInstruction.Kind.CDQ))
                emit(UnaryInstruction(UnaryInstruction.Kind.IDIV, right))
                emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, EAX))
            }
            IROp.AND -> {
                emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))
                emit(BinaryInstruction(BinaryInstruction.Kind.AND, reg, right))
            }
            IROp.OR -> {
                emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))
                emit(BinaryInstruction(BinaryInstruction.Kind.OR, reg, right))
            }
            IROp.LSHIFT -> {
                emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))
                emit(BinaryInstruction(BinaryInstruction.Kind.SHL, reg, right))
            }
            IROp.RSHIFT -> {
                emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))
                emit(BinaryInstruction(BinaryInstruction.Kind.SHR, reg, right))
            }
            IROp.ARSHIFT -> {
                emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))
                emit(BinaryInstruction(BinaryInstruction.Kind.SHL, reg, right))
            }
            IROp.XOR -> {
                emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, next))
                emit(BinaryInstruction(BinaryInstruction.Kind.XOR, reg, right))
            }
        }

        reg

    }
    is IRExp.Call -> TODO()
    is IRExp.Const -> Operand.Imm(exp.value)
    is IRExp.EStmtSeq -> {
        toI386(exp.stm)
        toI386(exp.exp)
    }
    is IRExp.Mem -> {
        val addr = toI386(exp.address)
        val temp = TempVar()

        when (addr) {
            is Operand.Reg -> Operand.Mem(addr.reg)
            is Operand.Mem -> {
                emit(BinaryInstruction(BinaryInstruction.Kind.MOV, Operand.Reg(temp), addr))
                Operand.Mem(temp)
            }
            else -> error("Encountered untranslatable operand: $addr")
        }

    }
    is IRExp.Name -> Operand.ByLabel(exp.label)
    is IRExp.Param -> Operand.Mem(EBP.reg, null, null, 8 + 4 * exp.index)
    is IRExp.Temp -> Operand.Reg(exp.temp)
}

fun TranslationContext.toI386(stmt: IRStmt): Unit = when (stmt) {
    is IRStmt.Jump -> stmt.targets
        .filter { toI386(stmt.dest).toString() == it.toString() }
        .forEach { emit(JumpInstruction(JumpInstruction.Kind.JMP, it)) }
    is IRStmt.CJump -> {
        val left = Operand.Reg()
        val right = Operand.Reg()

        emit(BinaryInstruction(BinaryInstruction.Kind.MOV, left, toI386(stmt.left)))
        emit(BinaryInstruction(BinaryInstruction.Kind.MOV, right, toI386(stmt.right)))
        emit(BinaryInstruction(BinaryInstruction.Kind.MOV, left, right))

        when (stmt.rel) {
            Rel.EQ -> {
                emit(JumpInstruction(JumpInstruction.Cond.E, stmt.trueLabel))
                emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))
            }

            Rel.NE -> {
                emit(JumpInstruction(JumpInstruction.Cond.NE, stmt.trueLabel))
                emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))
            }
            Rel.LT -> {
                emit(JumpInstruction(JumpInstruction.Cond.L, stmt.trueLabel))
                emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))
            }
            Rel.GT -> {
                emit(JumpInstruction(JumpInstruction.Cond.G, stmt.trueLabel))
                emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))
            }
            Rel.LE -> {
                emit(JumpInstruction(JumpInstruction.Cond.LE, stmt.trueLabel))
                emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))
            }
            Rel.GE -> {
                emit(JumpInstruction(JumpInstruction.Cond.GE, stmt.trueLabel))
                emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))
            }
            Rel.ULT -> {
                emit(JumpInstruction(JumpInstruction.Cond.B, stmt.trueLabel))
                emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))
            }
            Rel.ULE -> {
                emit(JumpInstruction(JumpInstruction.Cond.NE, stmt.trueLabel))
                emit(BinaryInstruction(BinaryInstruction.Kind.CMP, left, right))
                emit(JumpInstruction(JumpInstruction.Cond.A, stmt.falseLabel))
                emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))
            }
            Rel.UGT -> {
                emit(JumpInstruction(JumpInstruction.Cond.A, stmt.trueLabel))
                emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))
            }
            Rel.UGE -> {
                emit(JumpInstruction(JumpInstruction.Cond.NE, stmt.trueLabel))
                emit(BinaryInstruction(BinaryInstruction.Kind.CMP, left, right))
                emit(JumpInstruction(JumpInstruction.Cond.B, stmt.falseLabel))
                emit(JumpInstruction(JumpInstruction.Kind.JMP, stmt.falseLabel))
            }
        }
    }
    is IRStmt.IRLabel -> emit(LabelInstruction(stmt.label))
    is IRStmt.Move -> {
        val dest = toI386(stmt.dest)
        val src = toI386(stmt.src)

        when {
            dest is Operand.Mem && src is Operand.Mem -> {
                val reg = Operand.Reg()
                emit(BinaryInstruction(BinaryInstruction.Kind.MOV, reg, src))
                emit(BinaryInstruction(BinaryInstruction.Kind.MOV, dest, reg))
            }
            else -> emit(BinaryInstruction(BinaryInstruction.Kind.MOV, dest, src))

        }
    }
    is IRStmt.StmSeq -> stmt.statements.forEach(::toI386)
}