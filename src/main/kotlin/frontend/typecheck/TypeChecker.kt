package frontend.typecheck

import arrow.core.Either
import arrow.core.computations.either
import arrow.core.left
import arrow.core.right
import ast.Name
import ast.Type
import ast.typed.*
import ast.typed.VariableDefinitions
import ast.untyped.*
import frontend.CompilerConfiguration
import frontend.CompilerError
import frontend.Stage
import frontend.typecheck.ExpressionTyper.typed

sealed class TypeError : CompilerError.Severe("", "") {
    data class TypeMismatch(val expected: Type, val actual: Type, val expr: Any) : TypeError()
    data class BinaryMismatch(val left: Type, val right: Type, val expr: TypedExpr) : TypeError()
    data class IncompatibleArguments(val expected: List<Type>, val actual: List<Type>, val expr: TypedExpr) :
        TypeError()

    data class UnknownReference(val ref: Name) : TypeError()

}

private fun TypedExpr.classTypeOf(typeRef: Type.ClassType): Either<TypeError, TypedExpr> = Either.conditionally(
    type == typeRef, ifFalse = {
        TypeError.TypeMismatch(
            expected = typeRef,
            actual = type,
            expr = this
        )
    }, ifTrue = { this }
)

private inline fun <reified T : Type> TypedExpr.typeOf(alt: Type = Type.UnknownType): Either<TypeError, TypedExpr> =
    Either.conditionally(
        type is T, ifFalse = {
            TypeError.TypeMismatch(
                expected = T::class.objectInstance ?: alt,
                actual = type,
                expr = this
            )
        }, ifTrue = { this })

data class TypeRegistry(
    val classes: Map<TypedExpr.Identifier, TypedClassDefinition>
) : Map<TypedExpr.Identifier, TypedClassDefinition> by classes {
    companion object
}

data class Context(
    val typeRegistry: TypeRegistry,
    val outer: Context?,
    val scope: Map<Name, TypedExpr.Identifier>
) {
    companion object {}

    private fun TypedMethodDefinition.toMethodRef(): MethodRef = MethodRef(
        name = name,
        returnType = returnType,
        argumentTypes = arguments.args
    )

    fun resolve(id: UntypedExpr.Identifier): Either<TypeError, TypedExpr.Identifier> =
        scope[id.name]?.right() ?: outer?.resolve(id) ?: TypeError.UnknownReference(id.name).left()

    fun resolveRef(typeRef: Type.ClassType, method: UntypedExpr.Identifier): Either<TypeError, MethodRef> =
        typeRegistry[TypedExpr.Identifier(typeRef, typeRef.name)]?.let { classDef ->
            classDef.methods.find { it.name.name == method.name }?.toMethodRef()?.right()
        } ?: TypeError.UnknownReference(typeRef.name).left()
}


fun TypeRegistry.Companion.of(prg: UntypedProgram): Either<TypeError, TypeRegistry> = either.eager {
    TypeRegistry(
        classes = !ToplevelTyper.run(prg.classes)
    )
}




fun Context.scoped(classRef: TypedMethodDefinition): Context = TODO()


object TypeChecker : Stage<UntypedProgram, TypedProgram> {

    override fun run(input: UntypedProgram, config: CompilerConfiguration): Either<TypeError, TypedProgram> =
        either.eager {
            val programContext = !TypeRegistry.of(input)

            TypedProgram(programContext[Ident], programContext.classes.values.toList())


        }
}

typealias PartialTypeRegistry = Map<TypedExpr.Identifier, TypedClassDefinition>


object ToplevelTyper {

    /**
     * TODO:
     * Implement a Global context that just holds the name mappings of all classes;
     * Then focus in on each class by Context.of
     * Then for each class focus in on each method with Context.of
     */

    fun run(classes: List<UntypedClassDefinition>): Either<TypeError, Map<TypedExpr.Identifier, TypedClassDefinition>> =
        either.eager {
            classes.fold(mutableMapOf()) { acc, current ->
                val typedIdent = TypedExpr.Identifier(
                    name = current.name.name,
                    type = Type.ClassType(current.name.name)
                )
                val fields  = VariableDefinitions(current.fields.definitions)
                val context = Context(
                    TypeRegistry(acc),

                )
                val methods = current.methods.map { acc.runMethodShallow(it) }

                acc[typedIdent] = TypedClassDefinition(
                    TypedExpr.Identifier(
                        name = current.name.name,
                        type = Type.ClassType(current.name.name)
                    ),
                    superName = current.superName?.let { (name) ->
                        val typed = TypedExpr.Identifier(Type.ClassType(name), name)
                        acc[typed]?.name
                    },
                    methods = methods,
                    fields = fields

                )

                acc
            }
        }

    private fun PartialTypeRegistry.runMethodShallow(method: UntypedMethodDefinition): Either<TypeError,TypedMethodDefinition> {

    }

    private fun PartialTypeRegistry.runFieldShallow(field: UntypedExpr.Identifier): TypedExpr.Identifier {}


    fun runMain(mainClass: UntypedClassDefinition): Either<TypeError, TypedClassDefinition> = TODO()
}

object StatementTyper {

    fun Context.typed(stmt: UntypedStatement): Either<TypeError, TypedStatement> = either.eager {
        when (stmt) {
            is UntypedStatement.ArrayAssign -> {
                val resolved = !resolve(stmt.id)
                val index = !typed(stmt.index).bind().typeOf<Type.IntType>()
                val value = !typed(stmt.value)
                val typedStmt = TypedStatement.ArrayAssign(resolved, index, value)

                !Either.conditionally(
                    resolved.type == index.type,
                    ifFalse = { TypeError.TypeMismatch(resolved.type, index.type, typedStmt) },
                    ifTrue = { typedStmt }
                )
            }
            is UntypedStatement.Assign -> {
                val resolved = !resolve(stmt.id)
                val typedVal = !typed(stmt.value)
                val typedStmt = TypedStatement.Assign(resolved, typedVal)

                !Either.conditionally(
                    resolved.type == typedVal.type,
                    ifFalse = { TypeError.TypeMismatch(resolved.type, typedVal.type, typedStmt) },
                    ifTrue = { typedStmt }
                )
            }
            is UntypedStatement.If -> TypedStatement.If(
                condition = !typed(stmt.condition).bind().typeOf<Type.BooleanType>(),
                trueBranch = !typed(stmt.trueBranch),
                falseBranch = !typed(stmt.falseBranch)
            )
            is UntypedStatement.While -> TypedStatement.While(
                condition = !typed(stmt.condition).bind().typeOf<Type.BooleanType>(),
                body = !typed(stmt.body)
            )
            is UntypedStatement.SequenceOf -> TypedStatement.SequenceOf(
                statements = stmt.statements.map { !typed(it) }
            )
            is UntypedStatement.Print -> TypedStatement.Print(output = !typed(stmt.output))
            is UntypedStatement.Write -> TypedStatement.Write(output = !typed(stmt.output))
        }
    }
}

object ExpressionTyper {


    fun Context.typed(expr: UntypedExpr): Either<TypeError, TypedExpr> = either.eager {
        when (expr) {
            is UntypedExpr.True -> TypedExpr.True
            is UntypedExpr.False -> TypedExpr.False
            is UntypedExpr.Read -> TypedExpr.Read
            is UntypedExpr.This -> TypedExpr.This(
                !resolve(
                    UntypedExpr.Identifier(Name("this", ""))
                )
            )
            is UntypedExpr.Identifier -> !resolve(expr)
            is UntypedExpr.Constant -> TypedExpr.Constant(expr.value)
            is UntypedExpr.ArrayLength -> TypedExpr.ArrayLength(
                !typed(expr.array).bind().typeOf<Type.IntType>()
            )
            is UntypedExpr.ArrayGet -> TypedExpr.ArrayGet(
                !typed(expr.array).bind().typeOf<Type.IntType>()
            )
            is UntypedExpr.Negate -> TypedExpr.Negate(
                !typed(expr.expr).bind().typeOf<Type.BooleanType>()
            )
            is UntypedExpr.New -> TypedExpr.New(
                !resolve(expr.typeRef).bind().typeOf<Type.ClassType>().map { it as TypedExpr.Identifier }

            )
            is UntypedExpr.NewArray -> TypedExpr.NewArray(
                !typed(expr.size).bind().typeOf<Type.IntType>()
            )
            is UntypedExpr.BinaryOp -> TypedExpr.BinaryOp(
                left = !typed(expr.left),
                op = expr.op,
                right = !typed(expr.right)
            ).let { e ->
                !Either.conditionally(
                    e.left.type is Type.BooleanType &&
                            e.right.type is Type.BooleanType,
                    ifFalse = { TypeError.BinaryMismatch(e.left.type, e.right.type, e) },
                    ifTrue = { e })
            }
            is UntypedExpr.Invoke -> {
                val resolved = !typed(expr.obj).bind().typeOf<Type.ClassType>()
                val methodRef = !resolveRef(resolved.type as Type.ClassType, expr.method)
                val arguments = expr.arguments.map { !typed(it) }

                val typesCombined = methodRef
                    .argumentTypes.map { it.type }
                    .zip(arguments.map { it.type })

                val typesMatch = typesCombined.all { (t1, t2) -> t1 == t2 }

                !Either.conditionally(
                    typesCombined.size == methodRef.argumentTypes.size && typesMatch,
                    ifFalse = {
                        TypeError.IncompatibleArguments(
                            methodRef.argumentTypes.map { it.type },
                            arguments.map { it.type },
                            TypedExpr.Invoke(
                                obj = resolved,
                                method = methodRef,
                                arguments = arguments
                            )
                        )
                    }, ifTrue = {
                        TypedExpr.Invoke(
                            obj = resolved,
                            method = methodRef,
                            arguments = arguments
                        )
                    })

            }

        }
    }
}