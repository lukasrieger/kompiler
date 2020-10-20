package frontend.typecheck

import arrow.core.Either
import arrow.core.left
import arrow.core.right
import ast.Name
import ast.Type
import ast.typed.ClassTypeDescriptor
import ast.typed.MethodRef
import ast.typed.MethodTypeDescriptor
import ast.typed.TypedExpr
import ast.untyped.UntypedClassDefinition
import ast.untyped.UntypedExpr
import ast.untyped.UntypedMethodDefinition


data class Context(
    val types: Map<Name, ClassTypeDescriptor>,
    val outer: Context?,
    val scope: Map<Name, TypedExpr.Identifier>
) {

    companion object {
        val THIS = Name("this", "")
    }

    private fun MethodTypeDescriptor.toMethodRef(): MethodRef = MethodRef(
        name = name,
        returnType = returnType,
        argumentTypes = arguments.args
    )

    fun resolve(id: UntypedExpr.Identifier): Either<TypeError, TypedExpr.Identifier> =
        scope[id.name]?.right() ?: outer?.resolve(id) ?: TypeError.UnknownReference(id.name).left()

    fun resolveRef(typeRef: Type.ClassType, method: UntypedExpr.Identifier): Either<TypeError, MethodRef> =
        types[typeRef.name]?.let { classDef ->
            classDef.methods[method.name]?.toMethodRef()?.right()
        } ?: TypeError.UnknownReference(typeRef.name).left()
}


fun Context.scoped(classRef: UntypedClassDefinition): Context = Context(
    types = types,
    outer = this,
    scope = (classRef.fields.definitions + listOf(
        TypedExpr.Identifier(
            name = Name("this", ""),
            type = Type.ClassType(classRef.name.name)
        )
    )).map { it.name to it }.toMap()
)

fun Context.scoped(methodRef: UntypedMethodDefinition): Context = Context(
    types = types,
    outer = this,
    scope = (methodRef.variables.definitions + methodRef.arguments.args).map { it.name to it }.toMap()
)
