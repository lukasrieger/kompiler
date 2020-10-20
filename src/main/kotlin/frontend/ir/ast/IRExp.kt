package frontend.ir.ast

enum class IROp {
    PLUS, MINUS, MUL, DIV, AND, OR, LSHIFT, RSHIFT, ARSHIFT, XOR
}

fun ast.Operator.toIR(): IROp = TODO()

sealed class IRExp {

    data class BinOp(val left: IRExp, val op: IROp, val right: IRExp) : IRExp()

    data class Call(val func: IRExp, val args: List<IRExp>) : IRExp()

    data class Const(val value: Int) : IRExp()

    data class EStmtSeq(val stm: IRStmt, val exp: IRExp) : IRExp()

    data class Mem(val address: IRExp) : IRExp()

    data class Name(val label: Label) : IRExp() {
        constructor(name: String) : this(Label(name))

    }

    data class Param(val index: Int) : IRExp()

    data class Temp(val temp: TempVar) : IRExp()

}