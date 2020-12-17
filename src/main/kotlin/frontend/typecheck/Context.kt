package frontend.typecheck

import arrow.core.Either
import arrow.core.left
import arrow.core.right
import ast.*

data class ContextId(val name: Name, val type: Type)

fun ContextId.toTypedRef() = TypedRef(
    exp = ExpF.Identifier(name),
    type = type
)


data class TypedRef(val exp: ExpF.Identifier<TExp>, val type: Type)

data class Context(
    val types: Map<Name, ClassTypeDescriptor<TExp>>,
    val outer: Context?,
    val scope: Map<Name, ContextId>
) {

    companion object {
        val THIS = Name("this", "")
    }

    private fun MethodTypeDescriptor.toMethodRef(): MethodRef = MethodRef(
        name = name,
        returnType = returnType,
        argumentTypes = arguments.args.map { it.type }
    )

    fun resolve(id: ExpF.Identifier<Exp>): Either<TypeError, TypedRef> =
        scope[id.name]?.toTypedRef()?.right() ?: outer?.resolve(id) ?: TypeError.UnknownReference(id.name).left()

    fun resolveRef(typeRef: Typed.Class, method: ExpF.Identifier<Exp>): Either<TypeError, MethodRef> =
        types[typeRef.name]?.let { classDef ->
            classDef.methods[method.name]?.toMethodRef()?.right()
        } ?: TypeError.UnknownReference(typeRef.name).left()
}


fun Context.scoped(classRef: ClassDefinition<Stmt, Exp>): Context = Context(
    types = types,
    outer = this,
    scope = (classRef.fields.definitions.map { ContextId(it.name.name, it.type) } +
            listOf(
                ContextId(
                    name = Name("this", ""),
                    type = Typed.Class(classRef.name.name)
                )
            )).map { it.name to ContextId(it.name, it.type) }.toMap()
)

fun Context.scoped(methodRef: MethodDefinition<Stmt, Exp>): Context = Context(
    types = types,
    outer = this,
    scope = (methodRef.variables.definitions + methodRef.arguments.args)
        .map { it.name.name to ContextId(it.name.name, it.type) }
        .toMap()
)
