package frontend.ir.ast

sealed class IRStmt {
    data class Jump(val dest: IRExp, val targets: List<Label>) : IRStmt() {
        constructor(label: Label) : this(IRExp.Name(label), listOf(label))
    }

    data class CJump(
        val rel: Rel,
        val left: IRExp,
        val right: IRExp,
        val trueLabel: Label,
        val falseLabel: Label
    ) : IRStmt()

    data class IRLabel(val label: frontend.ir.ast.Label) : IRStmt()

    data class Move(val dest: IRExp, val src: IRExp) : IRStmt()

    data class StmSeq(val statements: List<IRStmt>) : IRStmt() {
        constructor(vararg statements: IRStmt) : this(statements.toList())
    }
}

