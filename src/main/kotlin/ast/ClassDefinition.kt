package ast

data class ClassDefinition<T, E>(
    val name: ExpF.Identifier<E>,
    val superName: ExpF.Identifier<E>?,
    val fields: VariableDefinitions<E>,
    val methods: List<MethodDefinition<T, E>>,
)


data class ClassTypeDescriptor<E>(
    val name: ExpF.Identifier<E>,
    val superName: ExpF.Identifier<E>?,
    val methods: Map<Name, MethodTypeDescriptor>
)
