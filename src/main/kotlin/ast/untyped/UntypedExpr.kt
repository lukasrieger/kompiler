package ast.untyped

import ast.Name
import ast.Operator


sealed class UntypedExpr {
    object True : UntypedExpr()
    object False : UntypedExpr()

    object Read : UntypedExpr()
    object This : UntypedExpr()

    data class Identifier(val name: Name) : UntypedExpr()
    data class Constant(val value: Int) : UntypedExpr()

    data class BinaryOp(
        val left: UntypedExpr,
        val op: Operator,
        val right: UntypedExpr
    ) : UntypedExpr()

    data class ArrayLength(val array: UntypedExpr) : UntypedExpr()
    data class ArrayGet(val array: UntypedExpr, val index: UntypedExpr) : UntypedExpr()

    data class Invoke(
        val obj: UntypedExpr,
        val method: Identifier,
        val arguments: List<UntypedExpr>
    ) : UntypedExpr()

    data class Negate(val expr: UntypedExpr) : UntypedExpr()
    data class New(val typeRef: Identifier) : UntypedExpr()

    data class NewArray(val size: UntypedExpr) : UntypedExpr()

}