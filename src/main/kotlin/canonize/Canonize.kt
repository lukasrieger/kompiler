package canonize

import arrow.core.Either
import arrow.core.Left
import arrow.core.Right
import frontend.CompilerConfiguration
import frontend.CompilerError
import frontend.Stage
import frontend.StageIdentifier
import frontend.ir.ast.*

data class CanonizeError(val err: Throwable) : CompilerError.Severe("Can't canonize program", "")

object IRCanonize : Stage<IRProgram, IRProgram> {
    override val identifier: StageIdentifier = StageIdentifier("IRCanonize", 3)

    override fun run(input: IRProgram, config: CompilerConfiguration): Either<CompilerError, IRProgram> =
        try {
            Right(input.canonize())
        } catch (err: Throwable) {
            Left(CanonizeError(err))
        }

}

fun IRProgram.canonize() = copy(functions = map { it.canonize() })

fun IRFunction.canonize(): IRFunction = copy(body = flatMap { it.canonize() })

fun IRStmt.canonize(): List<IRStmt> = when (this) {
    is IRStmt.Jump -> dest
        .canonNoTopCall()
        .toStmt { IRStmt.Jump(it, targets) }
    is IRStmt.IRLabel -> listOf(this)
    is IRStmt.StmSeq -> statements.flatMap { it.canonize() }
    is IRStmt.CJump -> left
        .canonNoTopCall()
        .combineToStmt(right.canonNoTopCall()) { l, r ->
            IRStmt.CJump(rel, l, r, trueLabel, falseLabel)
        }
    is IRStmt.Move -> when (dest) {
        is IRExp.EStmtSeq -> IRStmt.StmSeq(dest.stm, IRStmt.Move(dest.exp, src)).canonize()
        is IRExp.Mem -> dest.address
            .canonNoTopCall()
            .combineToStmt(src.canonNoTopCall()) { eaddr, esrc -> IRStmt.Move(IRExp.Mem(eaddr), esrc) }
        is IRExp.Temp, is IRExp.Param -> src.canonize().toStmt { IRStmt.Move(dest = dest, src = it) }
        else -> error("Left-hand side of MOVE must be TEMP, PARAM, MEM or ESEQ.")
    }
}

fun IRExp.canonize(): CanonizedExp = when (this) {
    is IRExp.BinOp -> combine(left.canonize(), right.canonize()) { l, r -> IRExp.BinOp(l, op, r) }
    is IRExp.Call -> combine(this.func.canonNoTopCall(), args.map { it.canonNoTopCall() }, IRExp::Call)
    is IRExp.Mem -> address.canonNoTopCall().map(IRExp::Mem)
    is IRExp.Const, is IRExp.Param, is IRExp.Temp, is IRExp.Name -> CanonizedExp(this)
    is IRExp.EStmtSeq -> combine(
        first = CanonizedExp(stm.canonize(), IRExp.Const(0)),
        second = exp.canonNoTopCall()
    ) { _, e -> e }
}


fun IRExp.canonNoTopCall(): CanonizedExp = canonize().let { canon ->
    when (canon.exp) {
        is IRExp.Call -> {
            val tempStm = IRExp.Temp(TempVar())
            CanonizedExp(
                body = canon.body + IRStmt.Move(tempStm, canon.exp),
                exp = tempStm
            )
        }
        else -> canon
    }
}

