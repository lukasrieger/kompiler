package ast

import frontend.typecheck.SymbolRef

data class ClassDef<T : StmtK, E : ExpK>(
    val name: ExpF.Symbol<E>,
    val superName: ExpF.Symbol<E>?,
    val fields: List<SymbolRef<E>>,
    val methods: List<MethodDef<T, E>>,
)


data class ClassDescriptor<E : ExpK>(
    val name: ExpF.Symbol<E>,
    val superName: ExpF.Symbol<E>?,
    val methods: Map<NamedRef, MethodDescriptor>
) : Type
