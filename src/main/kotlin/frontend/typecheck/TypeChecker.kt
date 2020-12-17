package frontend.typecheck

import arrow.core.Either
import arrow.core.computations.either
import ast.Type
import ast.typed.*
import ast.typed.TypedExpr.*
import ast.typed.TypedStatement.*
import ast.untyped.UntypedClassDefinition
import ast.untyped.UntypedExpr
import ast.untyped.UntypedProgram
import ast.untyped.UntypedStatement
import frontend.CompilerConfiguration
import frontend.CompilerError
import frontend.Stage
import frontend.StageIdentifier

private typealias PartialUntypedPrg = List<UntypedClassDefinition>

object TypeChecker : Stage<UntypedProgram, TypedProgram> {
    override fun run(input: UntypedProgram, config: CompilerConfiguration): Either<CompilerError, TypedProgram> =
        input.classes.typed()

    override val identifier: StageIdentifier = StageIdentifier(
        name = "TypeChecker",
        canonicalOrder = 2
    )

}



fun PartialUntypedPrg.typed(): Either<TypeError, TypedProgram> =
    either.eager {
        val globalContext = this@typed.buildGlobalContext()

        val typedClasses = this@typed.map { untyped ->
            val classContext = globalContext.scoped(untyped)
            val resolvedClass = globalContext.resolve(untyped.name)()

            typedClassDefinition(resolvedClass, untyped, globalContext, classContext)()
        }

        TypedProgram(classes = typedClasses)
    }

private fun typedClassDefinition(
    resolvedClass: Identifier,
    untyped: UntypedClassDefinition,
    globalContext: Context,
    classContext: Context
): Either<TypeError, TypedClassDefinition> = either.eager {
    TypedClassDefinition(
        name = resolvedClass,
        superName = untyped.superName?.let { globalContext.resolve(it)() },
        fields = VariableDefinitions(untyped.fields.definitions),
        methods = untyped.methods.map { methodDef ->
            val methodContext = classContext.scoped(methodDef)

            TypedMethodDefinition(
                name = methodContext
                    .resolveRef(resolvedClass.type as Type.ClassType, methodDef.name)().name,
                arguments = MethodArgs(methodDef.arguments.args),
                variables = VariableDefinitions(methodDef.variables.definitions),
                body = methodContext.typed(methodDef.body)(),
                returnExpression = methodContext.typed(methodDef.returnExpression)(),
                returnType = methodDef.returnType
            )
        }
    )
}


/**
 * TODO: Typechecks!
 */
private fun PartialUntypedPrg.buildGlobalContext(): Context =
    Context(
        types = map { untyped ->
            untyped.name.name to ClassTypeDescriptor(
                name = untyped.name.toTyped(),
                superName = untyped.superName?.let { sup ->
                    Identifier(name = sup.name, type = Type.ClassType(sup.name))
                },
                methods = untyped.methods.map { method ->
                    method.name.name to MethodTypeDescriptor(
                        name = method.name.toTyped(method.returnType),
                        arguments = MethodArgs(method.arguments.args),
                        returnType = method.returnType
                    )
                }.toMap()
            )
        }.toMap(),
        outer = null,
        scope = mapOf()
    )


private fun Context.typed(stmt: UntypedStatement): Either<TypeError, TypedStatement> = either.eager {
    when (stmt) {
        is UntypedStatement.ArrayAssign -> {
            val resolved = resolve(stmt.id)()
            val index = typed(stmt.index)().typeOf<Type.IntType>()()
            val value = typed(stmt.value)()
            val typedStmt = ArrayAssign(resolved, index, value)

            Either.conditionally(
                resolved.type == index.type,
                ifFalse = { TypeError.TypeMismatch(resolved.type, index.type, typedStmt) },
                ifTrue = { typedStmt }
            )()
        }
        is UntypedStatement.Assign -> {
            val resolved = resolve(stmt.id)()
            val typedVal = typed(stmt.value)()
            val typedStmt = Assign(resolved, typedVal)

            Either.conditionally(
                resolved.type == typedVal.type,
                ifFalse = { TypeError.TypeMismatch(resolved.type, typedVal.type, typedStmt) },
                ifTrue = { typedStmt }
            )()
        }
        is UntypedStatement.If -> If(
            condition = typed(stmt.condition)().typeOf<Type.BooleanType>()(),
            trueBranch = typed(stmt.trueBranch)(),
            falseBranch = typed(stmt.falseBranch)()
        )
        is UntypedStatement.While -> While(
            condition = typed(stmt.condition)().typeOf<Type.BooleanType>()(),
            body = typed(stmt.body)()
        )
        is UntypedStatement.SequenceOf -> SequenceOf(
            statements = stmt.statements.map { typed(it)() }
        )
        is UntypedStatement.Print -> Print(output = typed(stmt.output)())
        is UntypedStatement.Write -> Write(output = typed(stmt.output)())
    }
}


private fun Context.typed(expr: UntypedExpr): Either<TypeError, TypedExpr> = either.eager {
    when (expr) {
        is UntypedExpr.True -> True
        is UntypedExpr.False -> False
        is UntypedExpr.Read -> Read
        is UntypedExpr.This -> This(
            resolve(
                UntypedExpr.Identifier(Context.THIS)
            )()
        )
        is UntypedExpr.Identifier -> resolve(expr)()
        is UntypedExpr.Constant -> Constant(expr.value)
        is UntypedExpr.ArrayLength -> ArrayLength(
            typed(expr.array)().typeOf<Type.ArrayType>()()
        )
        is UntypedExpr.ArrayGet -> ArrayGet(
            typed(expr.array)().typeOf<Type.ArrayType>()(),
            typed(expr.index)().typeOf<Type.IntType>()()
        )
        is UntypedExpr.Negate -> Negate(
            typed(expr.expr)().typeOf<Type.BooleanType>()()
        )
        is UntypedExpr.New -> New(
            resolve(expr.typeRef)()
                .typeOf<Type.ClassType>()
                .map { it as Identifier }()

        )
        is UntypedExpr.NewArray -> NewArray(
            typed(expr.size)().typeOf<Type.IntType>()()
        )
        is UntypedExpr.BinaryOp -> BinaryOp(
            left = typed(expr.left)(),
            op = expr.op,
            right = typed(expr.right)()
        ).let { e ->
            Either.conditionally(
                e.left.type is Type.BooleanType &&
                        e.right.type is Type.BooleanType,
                ifFalse = { TypeError.BinaryMismatch(e.left.type, e.right.type, e) },
                ifTrue = { e })()
        }
        is UntypedExpr.Invoke -> {
            val resolved = typed(expr.obj)().typeOf<Type.ClassType>()()
            val methodRef = resolveRef(resolved.type as Type.ClassType, expr.method)()
            val arguments = expr.arguments.map { typed(it)() }
            val typedExpr = Invoke(
                obj = resolved,
                method = methodRef,
                arguments = arguments
            )

            val typesCombined = methodRef
                .argumentTypes.map { it.type }
                .zip(arguments.map { it.type })

            val typesMatch = typesCombined.all { (t1, t2) -> t1 == t2 }

            Either.conditionally(
                typesCombined.size == methodRef.argumentTypes.size && typesMatch,
                ifFalse = {
                    TypeError.IncompatibleArguments(
                        methodRef.argumentTypes.map { it.type },
                        arguments.map { it.type },
                        typedExpr
                    )
                }, ifTrue = { typedExpr }
            )()
        }

    }
}
