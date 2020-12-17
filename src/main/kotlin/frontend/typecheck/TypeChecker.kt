package frontend.typecheck

import arrow.core.Either
import arrow.core.computations.either
import ast.*
import frontend.CompilerConfiguration
import frontend.CompilerError
import frontend.Stage
import frontend.StageIdentifier

private typealias PartialUntypedPrg = List<ClassDefinition<Stmt, Exp>>


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
    resolvedClass: TypedRef,
    untyped: ClassDefinition<Stmt, Exp>,
    globalContext: Context,
    classContext: Context
): Either<TypeError, ClassDefinition<TStmt, TExp>> = either.eager {

    ClassDefinition(
        name = resolvedClass.exp,
        superName = untyped.superName?.let { globalContext.resolve(it)().exp },
        fields = VariableDefinitions(untyped.fields.definitions.map {
            val expT = globalContext.typed(Exp(it.name))
            VariableDescriptor(expT.exp as ExpF.Identifier<TExp>, expT.type)
        }),
        methods = untyped.methods.map { methodDef ->
            val methodContext = classContext.scoped(methodDef)

            MethodDefinition(
                name = methodContext
                    .resolveRef(resolvedClass.type as Typed.Class, methodDef.name)().name,
                arguments = methodDef.arguments.toTyped(),
                variables = methodDef.variables.toTyped(),
                body = methodContext.typed(methodDef.body),
                returnExpression = methodContext.typed(methodDef.returnExpression),
                returnType = methodDef.returnType
            )
        }
    )
}


private fun PartialUntypedPrg.buildGlobalContext(): Context =
    Context(
        types = map { untyped ->
            untyped.name.name to ClassTypeDescriptor(
                name = untyped.name.toTyped(),
                superName = untyped.superName?.let { sup ->
                    ExpF.Identifier(name = sup.name)
                },
                methods = untyped.methods.map { method ->
                    method.name.name to MethodTypeDescriptor(
                        name = method.name.toTyped(),
                        arguments = MethodArgs(method.arguments.args).toTyped(),
                        returnType = method.returnType
                    )
                }.toMap()
            )
        }.toMap(),
        outer = null,
        scope = mapOf()
    )


private fun MethodArgs<Exp>.toTyped() = MethodArgs<TExp>(
    args.map {
        VariableDescriptor(ExpF.Identifier(it.name.name), it.type)
    }
)

private fun VariableDefinitions<Exp>.toTyped() =
    VariableDefinitions<TExp>(
        definitions.map {
            VariableDescriptor(ExpF.Identifier(it.name.name), it.type)
        }
    )

private fun Context.typed(statement: Stmt): TStmt = when (val stmt = statement.stmt) {
    is StmtF.ArrayAssign -> TODO()
    is StmtF.Assign -> resolve(stmt.id).fold(
        ifLeft = {
            StmtF.Assign<TStmt, TExp>(ExpF.Identifier(stmt.id.name), typed(stmt.value)) typeOf Typed.Void
        },
        ifRight = { (field, type) ->
            val typedVal = typed(stmt.value)
            val typedStmt = StmtF.Assign<TStmt, TExp>(field, typedVal)
            val typeT = type.takeIf { type == typedVal.type } ?: Typed.Error(
                TypeError.TypeMismatch(type, typedVal.type, typedStmt)
            )

            typedStmt typeOf typeT
        }
    )
    is StmtF.If -> typed(stmt.condition).ensureType<Typed.Boolean>().fold(
        ifLeft = { (con, err) ->
            StmtF.If(con, typed(stmt.trueBranch), typed(stmt.falseBranch)) typeOf Typed.Error(err)
        },
        ifRight = { StmtF.If(it, typed(stmt.trueBranch), typed(stmt.falseBranch)) typeOf Typed.Void }
    )
    is StmtF.While -> typed(stmt.condition).ensureType<Typed.Boolean>().fold(
        ifLeft = { (con, err) -> StmtF.While(con, typed(stmt.body)) typeOf Typed.Error(err) },
        ifRight = { StmtF.While(it, typed(stmt.body)) typeOf Typed.Void }
    )
    is StmtF.Print -> StmtF.Print<TStmt, TExp>(typed(stmt.output)) typeOf Typed.Void
    is StmtF.Write -> StmtF.Write<TStmt, TExp>(typed(stmt.output)) typeOf Typed.Void
    is StmtF.SequenceOf -> StmtF.SequenceOf<TStmt, TExp>(
        statements = stmt.statements.map { typed(it) }
    ) typeOf Typed.Void
}


private fun Context.typed(expr: Exp): TExp =
    when (val exp = expr.exp) {
        is ExpF.Bool -> ExpF.Bool<TExp>(exp.value) typeOf Typed.Boolean
        is ExpF.Read -> ExpF.Read<TExp>() typeOf Typed.Int
        is ExpF.This -> resolve(ExpF.Identifier(Context.THIS)).fold(
            ifLeft = {
                ExpF.This(ExpF.Identifier<TExp>(exp.typeRef.name)) typeOf
                        Typed.Error(TypeError.UnknownReference(exp.typeRef.name))
            },
            ifRight = { (exp, type) -> ExpF.This(exp) typeOf type }
        )
        is ExpF.Constant -> ExpF.Constant<TExp>(exp.value) typeOf Typed.Int
        is ExpF.ArrayLength -> typed(exp.array).ensureType<Typed.Array>().fold(
            ifLeft = { (exp, err) -> ExpF.ArrayLength(exp) typeOf Typed.Error(err) },
            ifRight = { ExpF.ArrayLength(it) typeOf Typed.Int }
        )

        is ExpF.Negate -> typed(exp.expr).ensureType<Typed.Boolean>().fold(
            ifLeft = { (exp, err) -> ExpF.Negate(exp) typeOf Typed.Error(err) },
            ifRight = { ExpF.Negate(it) typeOf Typed.Boolean }
        )

        is ExpF.Identifier -> resolve(exp).fold(
            ifLeft = { ExpF.Identifier<TExp>(exp.name) typeOf Typed.Error(it) },
            ifRight = { (exp, type) -> exp typeOf type }
        )
        is ExpF.BinaryOp -> {
            val left = typed(exp.left)
            val right = typed(exp.right)
            val expT = ExpF.BinaryOp(left, exp.op, right)

            when (left.type) {
                right.type -> expT typeOf exp.op.typeOf()
                else -> expT typeOf Typed.Error(TypeError.BinaryMismatch(left.type, right.type, expT))
            }
        }


        is ExpF.New -> resolve(exp.typeRef).fold(
            ifLeft = { ExpF.Identifier<TExp>(exp.typeRef.name) typeOf Typed.Error(it) },
            ifRight = { (exp, type) -> ExpF.New(exp) typeOf type }
        )

        is ExpF.Invoke -> exp.arguments.map(::typed).let { args ->
            typed(exp.obj).ensureType<Typed.Class>().fold(
                ifLeft = { (obj, err) ->
                    ExpF.Invoke(
                        obj = obj,
                        method = ExpF.Identifier(exp.method.name),
                        arguments = args
                    ) typeOf Typed.Error(err)
                },
                ifRight = { (obj, type) ->
                    val expT = ExpF.Invoke(
                        obj = obj typeOf type as Typed.Class,
                        method = ExpF.Identifier(exp.method.name),
                        arguments = args
                    )

                    resolveRef(type, exp.method).fold(
                        ifLeft = { expT typeOf Typed.Error(it) },
                        ifRight = { ref ->
                            val typesCombined = ref.argumentTypes
                                .zip(args.map { it.type })

                            val typesMatch = typesCombined.all { (t1, t2) -> t1 == t2 }
                            val argCondition = (typesCombined.size == ref.argumentTypes.size) && typesMatch

                            (expT typeOf ref.returnType).takeIf { argCondition } ?: expT typeOf Typed.Error(
                                TypeError.IncompatibleArguments(
                                    ref.argumentTypes,
                                    args.map { it.type },
                                    expT
                                )
                            )

                        }
                    )


                }
            )
        }
        is ExpF.NewArray -> typed(exp.size).ensureType<Typed.Int>().fold(
            ifLeft = { (exp, err) -> ExpF.NewArray(exp) typeOf Typed.Error(err) },
            ifRight = { ExpF.NewArray(it) typeOf Typed.Array }
        )

        is ExpF.ArrayGet -> typed(exp.array).ensureType<Typed.Array>().fold(
            ifLeft = { (arr, err) -> ExpF.ArrayGet(arr, typed(exp.index)) typeOf Typed.Error(err) },
            ifRight = { arr ->
                typed(exp.index).ensureType<Typed.Int>().fold(
                    ifLeft = { (i, err) -> ExpF.ArrayGet(arr, i) typeOf Typed.Error(err) },
                    ifRight = { i -> ExpF.ArrayGet(arr, i) typeOf Typed.Int }
                )
            }
        )
    }
