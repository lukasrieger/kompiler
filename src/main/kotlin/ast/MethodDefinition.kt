package ast

inline class MethodArgs<E>(val args: List<VariableDescriptor<E>>)
inline class VariableDefinitions<E>(val definitions: List<VariableDescriptor<E>>)

data class MethodDefinition<T, E>(
    val name: ExpF.Identifier<E>,
    val arguments: MethodArgs<E>,
    val variables: VariableDefinitions<E>,
    val body: T,
    val returnExpression: E,
    val returnType: Type
)


data class MethodTypeDescriptor(
    val name: ExpF.Identifier<TExp>,
    val arguments: MethodArgs<TExp>,
    val returnType: Type
)

data class VariableDescriptor<E>(
    val name: ExpF.Identifier<E>,
    val type: Type
)

data class MethodRef(
    val name: ExpF.Identifier<TExp>,
    val returnType: Type,
    val argumentTypes: List<Type>
) : Type

