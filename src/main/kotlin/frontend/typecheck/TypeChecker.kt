package frontend.typecheck

import arrow.core.*
import arrow.core.computations.either
import ast.*
import ast.ExpF.*
import ast.StmtF.*
import frontend.CompilerConfiguration
import frontend.CompilerError
import frontend.Stage
import frontend.StageIdentifier
import kotlin.collections.mapOf

private typealias PartialUntypedPrg = List<ClassDef<Stmt, Exp>>

data class TypeCheckFailed(val errors: Nel<TypeError>) : CompilerError.Severe("TypeCheck failed.", "")

object TypeChecker : Stage<UntypedProgram, TypedProgram> {
    override fun run(input: UntypedProgram, config: CompilerConfiguration): Either<CompilerError, TypedProgram> =
        input.classes.typed().mapLeft(::TypeCheckFailed)

    override val identifier: StageIdentifier = StageIdentifier(
        name = "TypeChecker",
        canonicalOrder = 2
    )

}


fun PartialUntypedPrg.typed(): Either<Nel<TypeError>, TypedProgram> =
    either.eager {
        val globalContext = this@typed.buildGlobalContext()

        val typedClasses = this@typed.map { untyped ->
            val classContext = globalContext.scoped(untyped)
            val resolvedClass = globalContext.resolve(untyped.name).toValidatedNel()()

            typedClassDefinition(resolvedClass, untyped, globalContext, classContext)()
        }

        TypedProgram(classes = typedClasses)
    }

private fun typedClassDefinition(
    resolvedClass: SymbolRef<TExp>,
    untyped: ClassDef<Stmt, Exp>,
    globalContext: Context,
    classContext: Context
): Either<Nel<TypeError>, ClassDef<TStmt, TExp>> = either.eager {

    ClassDef(
        name = resolvedClass.ref,
        superName = untyped.superName?.let { globalContext.resolve(it).toValidatedNel()().ref },
        fields = untyped.fields.map { (ref, ty) ->
            val expT = globalContext.typed(Exp(ref))()
            SymbolRef(expT.exp as Symbol<TExp>, ty)
        },
        methods = untyped.methods.map { methodDef ->
            val methodContext = classContext.scoped(methodDef)

            MethodDef(
                name = methodContext
                    .resolveRef(resolvedClass.type as Typed.Class, methodDef.name).toValidatedNel()().name,
                arguments = methodDef.arguments.map { (ref, ty) -> SymbolRef(ref.toTyped(), ty) },
                variables = methodDef.variables.map { (ref, ty) -> SymbolRef(ref.toTyped(), ty) },
                body = methodContext.typed(methodDef.body)(),
                returnExpression = methodContext.typed(methodDef.returnExpression)(),
                returnType = methodDef.returnType
            )
        }
    )

}


private fun PartialUntypedPrg.buildGlobalContext(): Context =
    Context(
        types = map { untyped ->
            untyped.name.name to ClassDescriptor(
                name = untyped.name.toTyped(),
                superName = untyped.superName?.let { sup ->
                    Symbol(name = sup.name)
                },
                methods = untyped.methods.map { method ->
                    method.name.name to MethodDescriptor(
                        name = method.name.toTyped(),
                        arguments = method.arguments.map { (ref, ty) -> SymbolRef(ref.toTyped(), ty) },
                        returnType = method.returnType
                    )
                }.toMap()
            )
        }.toMap(),
        outer = null,
        scope = mapOf()
    )


private fun Context.typed(statement: Stmt): Validated<Nel<TypeError>, TStmt> =
    either.eager<Nel<TypeError>, TStmt> {
        when (val stmt = statement.stmt) {
            is ArrayAssign -> TODO()
            is Assign -> resolve(stmt.id).fold(
                ifLeft = {
                    (Assign<TStmt, TExp>(Symbol(stmt.id.name), typed(stmt.value)()) typeOf Typed.Void)()
                    it.invalidNel()<TStmt>()
                },
                ifRight = { (field, type) ->
                    val typedVal = typed(stmt.value)()
                    val typedStmt = Assign<TStmt, TExp>(field, typedVal)
                    val typeT = type.takeIf { type == typedVal.type } ?: Typed.Error(
                        TypeError.TypeMismatch(type, typedVal.type, typedStmt)
                    )

                    (typedStmt typeOf typeT)()
                }
            )
            is If -> typed(stmt.condition)().ensureType<Typed.Boolean>().fold(
                ifLeft = { (con, err) ->
                    (If(con, typed(stmt.trueBranch)(), typed(stmt.falseBranch)()) typeOf Typed.Error(err))()
                    err.invalidNel()<TStmt>()
                },
                ifRight = {
                    (If(
                        it,
                        typed(stmt.trueBranch)(),
                        typed(stmt.falseBranch)()
                    ) typeOf Typed.Void)()
                }
            )
            is While -> typed(stmt.condition)().ensureType<Typed.Boolean>().fold(
                ifLeft = { (con, err) ->
                    While(con, typed(stmt.body)()).typeOf(Typed.Error(err))()
                    err.invalidNel()<TStmt>()
                },
                ifRight = { (While(it, typed(stmt.body)()) typeOf Typed.Void)() }
            )
            is Print -> (Print<TStmt, TExp>(typed(stmt.output)()) typeOf Typed.Void)()
            is Write -> (Write<TStmt, TExp>(typed(stmt.output)()) typeOf Typed.Void)()
            is SequenceOf -> (SequenceOf<TStmt, TExp>(
                statements = stmt.statements.map { typed(it)() }
            ) typeOf Typed.Void)()
        }
    }.toValidated()


private fun Context.typed(expr: Exp): Validated<Nel<TypeError>, TExp> = either.eager<Nel<TypeError>, TExp> {
    when (val exp = expr.exp) {

        is Bool -> Bool<TExp>(exp.value).typeOf(Typed.Boolean)()

        is Read -> Read<TExp>().typeOf(Typed.Int)()

        is This -> resolve(Symbol(Context.THIS)).fold(
            ifLeft = {
                This(Symbol<TExp>(exp.typeRef.name))
                    .typeOf(Typed.Error(TypeError.UnknownReference(exp.typeRef.name)))()

                it.invalidNel()<TExp>()
            },
            ifRight = { (exp, type) -> This(exp).typeOf(type)() }
        )

        is Constant -> Constant<TExp>(exp.value).typeOf(Typed.Int)()

        is ArrayLength -> typed(exp.array)().ensureType<Typed.Array>().fold(
            ifLeft = { (exp, err) -> ArrayLength(exp).typeOf(Typed.Error(err))() },
            ifRight = { ArrayLength(it).typeOf(Typed.Int)() }
        )

        is Negate -> typed(exp.expr)().ensureType<Typed.Boolean>().fold(
            ifLeft = { (exp, err) ->
                Negate(exp).typeOf(Typed.Error(err))()
                err.invalidNel()<TExp>()
            },
            ifRight = { Negate(it).typeOf(Typed.Boolean)() }
        )

        is Symbol -> resolve(exp).fold(
            ifLeft = {
                Symbol<TExp>(exp.name) typeOf Typed.Error(it)
                it.invalidNel()<TExp>()
            },
            ifRight = { (exp, type) -> exp.typeOf(type)() }
        )
        is BinaryOp -> {
            val left = typed(exp.left)()
            val right = typed(exp.right)()
            val expT = BinaryOp(left, exp.op, right)

            when (left.type) {
                right.type -> expT.typeOf(exp.op.typeOf())()
                else -> expT.typeOf(Typed.Error(TypeError.BinaryMismatch(left.type, right.type, expT)))()

            }
        }


        is New -> resolve(exp.typeRef).fold(
            ifLeft = { Symbol<TExp>(exp.typeRef.name).typeOf(Typed.Error(it))() },
            ifRight = { (exp, type) -> New(exp).typeOf(type)() }
        )

        is Invoke -> exp.arguments.map { typed(it)() }.let { args ->
            typed(exp.obj)().ensureType<Typed.Class>().fold(
                ifLeft = { (obj, err) ->
                    Invoke(
                        obj = obj,
                        method = Symbol(exp.method.name),
                        arguments = args
                    ).typeOf(Typed.Error(err))()
                },
                ifRight = { (obj, type) ->
                    val expT = Invoke(
                        obj = (obj typeOf type as Typed.Class)(),
                        method = Symbol(exp.method.name),
                        arguments = args
                    )

                    resolveRef(type as Typed.Class, exp.method).fold(
                        ifLeft = { expT.typeOf(Typed.Error(it))() },
                        ifRight = { ref ->
                            val argTypes = ref.arguments.map { it.type }
                            val typesCombined = argTypes.zip(args.map { it.type })

                            val typesMatch = typesCombined.all { (t1, t2) -> t1 == t2 }
                            val argCondition = (typesCombined.size == argTypes.size) && typesMatch

                            (expT.typeOf(ref.returnType)()).takeIf { argCondition } ?: expT.typeOf(
                                Typed.Error(
                                    TypeError.IncompatibleArguments(
                                        argTypes,
                                        args.map { it.type },
                                        expT
                                    )
                                )
                            )()
                        }
                    )
                }
            )
        }
        is NewArray -> typed(exp.size)().ensureType<Typed.Int>().fold(
            ifLeft = { (exp, err) -> NewArray(exp).typeOf(Typed.Error(err))() },
            ifRight = { NewArray(it).typeOf(Typed.Array)() }
        )

        is ArrayGet -> typed(exp.array)().ensureType<Typed.Array>().fold(
            ifLeft = { (arr, err) -> ArrayGet(arr, typed(exp.index)()).typeOf(Typed.Error(err))() },
            ifRight = { arr ->
                typed(exp.index)().ensureType<Typed.Int>().fold(
                    ifLeft = { (i, err) -> ArrayGet(arr, i).typeOf(Typed.Error(err))() },
                    ifRight = { i -> ArrayGet(arr, i).typeOf(Typed.Int)() }
                )
            }
        )
    }
}.toValidated()


