package frontend.typecheck

import arrow.core.Either
import arrow.core.left
import arrow.core.right
import ast.*
import frontend.typecheck.Context.Companion.THIS


data class SymbolRef<E : ExpK>(val ref: ExpF.Symbol<E>, val type: Type)

data class Context(
    val types: Map<NamedRef, ClassDescriptor<TExp>>,
    val outer: Context?,
    val scope: Map<NamedRef, SymbolRef<TExp>>
) {

    companion object {
        val THIS = NamedRef("this", "")
    }

    fun resolve2(id: ExpF.Symbol<Exp>): StateF<TypeError, SymbolRef<TExp>> =
        scope[id.name]?.success() ?: types[id.name]?.name?.let { SymbolRef(it, Typed.Class(it.name)) }?.success()
        ?: outer?.resolve2(id) ?:
        TypeError.UnknownReference(id.name) toT SymbolRef(ref = ExpF.Symbol(id.name), type = Untyped)


    fun resolve(id: ExpF.Symbol<Exp>): Either<TypeError, SymbolRef<TExp>> =
        scope[id.name]?.right() ?: types[id.name]?.name?.let { SymbolRef(it, Typed.Class(it.name)) }?.right()
        ?: outer?.resolve(id) ?: TypeError.UnknownReference(id.name).left()

    fun resolveRef(typeRef: Typed.Class, method: ExpF.Symbol<Exp>): Either<TypeError, MethodDescriptor> =
        types[typeRef.name]?.let { classDef ->
            classDef.methods[method.name]?.right()
        } ?: TypeError.UnknownReference(typeRef.name).left()
}


fun Context.scoped(classRef: ClassDef<Stmt, Exp>): Context = Context(
    types = types,
    outer = this,
    scope = (classRef.fields.map { SymbolRef(it.ref.name.toTypedSymbol(), it.type) } +
            listOf(
                SymbolRef(
                    ref = THIS.toTypedSymbol(),
                    type = Typed.Class(classRef.name.name)
                )
            )).map { it.ref.name to it }.toMap()
)

fun Context.scoped(methodRef: MethodDef<Stmt, Exp>): Context = Context(
    types = types,
    outer = this,
    scope = (methodRef.variables + methodRef.arguments)
        .map { it.ref.name to SymbolRef(it.ref.name.toTypedSymbol(), it.type) }
        .toMap()
)

fun NamedRef.toTypedSymbol() = ExpF.Symbol<TExp>(this)






















