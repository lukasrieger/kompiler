package ast

import frontend.typecheck.SymbolRef


data class MethodDef<T : StmtK, E : ExpK>(
    val name: ExpF.Symbol<E>,
    val arguments: List<SymbolRef<E>>,
    val variables: List<SymbolRef<E>>,
    val body: T,
    val returnExpression: E,
    val returnType: Type
)


data class MethodDescriptor(
    val name: ExpF.Symbol<TExp>,
    val arguments: List<SymbolRef<TExp>>,
    val returnType: Type
) : Type
