package ast.typed

import ast.Name
import ast.Operator
import ast.Type
import ast.typeOf

data class MethodRef(
    val name: TypedExpr.Identifier,
    val returnType: Type,
    val argumentTypes: List<TypedExpr.Identifier>
)

sealed class TypedExpr(open val type: Type) {

    object True : TypedExpr(Type.BooleanType)
    object False : TypedExpr(Type.BooleanType)
    object Read : TypedExpr(Type.IntType)

    class This(typeRef: Identifier) : TypedExpr(typeRef.type)
    class New(typeRef: Identifier) : TypedExpr(typeRef.type)

    data class Constant(val value: Int) : TypedExpr(Type.IntType)
    data class ArrayLength(val array: TypedExpr) : TypedExpr(Type.IntType)
    data class ArrayGet(val array: TypedExpr) : TypedExpr(Type.IntType)
    data class Negate(val expr: TypedExpr) : TypedExpr(Type.BooleanType)
    data class NewArray(val size: TypedExpr) : TypedExpr(Type.ArrayType)

    data class Identifier(
        override val type: Type,
        val name: Name
    ) : TypedExpr(type)

    data class BinaryOp(
        val left: TypedExpr,
        val op: Operator,
        val right: TypedExpr
    ) : TypedExpr(op.typeOf())

    data class Invoke(
        val obj: TypedExpr,
        val method: MethodRef,
        val arguments: List<TypedExpr>
    ) : TypedExpr(method.returnType)


}