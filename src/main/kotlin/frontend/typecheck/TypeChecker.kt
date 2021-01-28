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
        canonicalOrder = 1
    )

}


fun PartialUntypedPrg.typed(): Either<Nel<TypeError>, TypedProgram> = evaltype<TypeError, TypedProgram> {
    val globalContext = this@typed.buildGlobalContext()
    val typedClasses = this@typed.map { untyped ->

        val classContext = globalContext.scoped(untyped)
        val resolvedClass = globalContext.resolve2(untyped.name)()

        typedClassDefinition(resolvedClass, untyped, globalContext, classContext)()
    }

    TypedProgram(classes = typedClasses)
}.let {
    when (it) {
        is StateF.Err -> it.errors.left()
        is StateF.Succ -> it.value.right()
    }
}


private fun typedClassDefinition(
    resolvedClass: SymbolRef<TExp>,
    untyped: ClassDef<Stmt, Exp>,
    globalContext: Context,
    classContext: Context
): StateF<TypeError, ClassDef<TStmt, TExp>> = evaltype {

    ClassDef(
        name = resolvedClass.ref,
        superName = untyped.superName?.let { globalContext.resolve2(it)().ref },
        fields = untyped.fields.map { (ref, ty) ->
            val expT = classContext.typedTest(Exp(ref))()
            SymbolRef(expT.exp as Symbol<TExp>, ty)

        },
        methods = untyped.methods.map { methodDef ->
            val methodContext = classContext.scoped(methodDef)

            MethodDef(
                name = methodDef.name.toTyped(),
                arguments = methodDef.arguments.map { (ref, ty) -> SymbolRef(ref.toTyped(), ty) },
                variables = methodDef.variables.map { (ref, ty) -> SymbolRef(ref.toTyped(), ty) },
                body = methodContext.typedTest(methodDef.body)(),
                returnExpression = methodContext.typedTest(methodDef.returnExpression)().typeEq(methodDef.returnType)(),
                returnType = methodDef.returnType
            )


        }
    )

}

//TODO UNIFY Context.resolve and resolve ref!!! Lookup may fail because the identifier points to a class

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


private fun Context.typedTest(statement: Stmt): StateF<TypeError, TStmt> = evaltype {
    when (val stmt = statement.stmt) {
        is ArrayAssign -> {
            val resolved = resolve2(stmt.id)()

            ArrayAssign<TStmt, TExp>(
                resolved.ref,
                typedTest(stmt.index)().ensureType2<Typed.Int>()(),
                typedTest(stmt.value)().ensureType2<Typed.Int>()()
            ) of Typed.Void
        }
        is Assign -> {
            val resolved = resolve2(stmt.id)()
            val type = resolved.type
            val typedVal = typedTest(stmt.value)()
            val typedStmt = Assign<TStmt, TExp>(resolved.ref, typedTest(stmt.value)())

            val typeT = type.takeIf { type == typedVal.type } ?: Typed.Error(
                TypeError.TypeMismatch(type, typedVal.type, typedStmt)
            )

            typedStmt.typeEq(type)() of typeT
        }
        is If -> {
            val cond = typedTest(stmt.condition)().ensureType2<Typed.Boolean>()()

            If(cond, typedTest(stmt.trueBranch)(), typedTest(stmt.falseBranch)()) of Typed.Void

        }
        is While -> {
            val cond = typedTest(stmt.condition)().ensureType2<Typed.Boolean>()()

            While(cond, typedTest(stmt.body)()) of Typed.Void

        }
        is Print -> Print<TStmt, TExp>(typedTest(stmt.output)()) of Typed.Int
        is Write -> Write<TStmt, TExp>(typedTest(stmt.output)()) of Typed.Int
        is SequenceOf -> SequenceOf<TStmt, TExp>(stmt.statements.map { typedTest(it)() }) of Typed.Void
    }
}

private fun Context.typedTest(expr: Exp): StateF<TypeError, TExp> = evaltype {
    when (val exp = expr.exp) {
        is Read -> Read<TExp>() of Typed.Int
        is This -> {
            val (ref, type) = resolve2(Symbol(Context.THIS))()

            This(ref) of type
        }
        is New -> {
            val (ref, type) = resolve2(exp.typeRef)()

            New(ref) of type
        }
        is Bool -> Bool<TExp>(exp.value) of Typed.Boolean
        is Constant -> Constant<TExp>(exp.value) of Typed.Int
        is ArrayLength -> {
            val arrTyped = typedTest(exp.array)().ensureType2<Typed.Array>()()

            ArrayLength(arrTyped) of Typed.Int
        }

        is Negate -> {
            val neg = typedTest(exp.expr)().ensureType2<Typed.Boolean>()()

            Negate(neg) of Typed.Boolean
        }

        is Symbol -> {
            val (symbol, type) = resolve2(exp)()

            symbol of type
        }
        is BinaryOp -> {
            val left = typedTest(exp.left)()
            val right = typedTest(exp.right)()
            val expT = BinaryOp(left, exp.op, right)

            when (left.type) {
                right.type -> expT of exp.op.typeOf()
                else -> expT of Typed.Error(TypeError.BinaryMismatch(left.type, right.type, expT))
            }
        }
        is Invoke -> {
            val args = exp.arguments.map { typedTest(it)() }

            when (val obj = typedTest(exp.obj)().ensureType2<Typed.Class>()) {
                is StateF.Err -> {
                    val ref = obj()
                    Invoke(ref, Symbol(exp.method.name), args) of Typed.Error(obj.errors.head)
                }
                is StateF.Succ -> {
                    val (objRef, type) = obj()
                    val expT = Invoke(
                        obj = objRef of type as Typed.Class,
                        method = Symbol(exp.method.name),
                        arguments = args
                    )

                    resolveRef(type, exp.method).fold(
                        ifLeft = { expT of Typed.Error(it) },
                        ifRight = { ref ->
                            val argTypes = ref.arguments.map { it.type }
                            val typesCombined = argTypes.zip(args.map { it.type })

                            val typesMatch = typesCombined.all { (t1, t2) -> t1 == t2 }
                            val argCondition = (typesCombined.size == argTypes.size) && typesMatch

                            (expT of ref.returnType).takeIf { argCondition } ?: expT of
                            Typed.Error(
                                TypeError.IncompatibleArguments(
                                    argTypes,
                                    args.map { it.type },
                                    expT
                                )
                            )
                        }
                    )
                }
            }


        }

        is ArrayGet -> {
            val arr = typedTest(exp.array)().ensureType2<Typed.Array>()()
            val index = typedTest(exp.index)().ensureType2<Typed.Int>()()

            ArrayGet(arr, index) of Typed.Int
        }
        is NewArray -> {
            val size = typedTest(exp.size)().ensureType2<Typed.Int>()()

            NewArray(size) of Typed.Array
        }
    }
}


private fun Context.typed(expr: Exp): Validated<Nel<TypeError>, TExp> = either.eager<Nel<TypeError>, TExp> {
    when (val exp = expr.exp) {


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


    }
}.toValidated()


private fun Context.typed(statement: Stmt): Validated<Nel<TypeError>, TStmt> =
    either.eager<Nel<TypeError>, TStmt> {
        when (val stmt = statement.stmt) {

            is While -> typed(stmt.condition)().ensureType<Typed.Boolean>().fold(
                ifLeft = { (con, err) ->
                    While(con, typed(stmt.body)()).typeOf(Typed.Error(err))()
                    err.invalidNel()<TStmt>()
                },
                ifRight = { (While(it, typed(stmt.body)()) typeOf Typed.Void)() }
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
            is Assign -> resolve(stmt.id).fold(
                ifLeft = {
                    it.invalidNel()<TStmt>()
                    (Assign<TStmt, TExp>(Symbol(stmt.id.name), typed(stmt.value)()) typeOf Typed.Void)()

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
            is ArrayAssign -> resolve(stmt.id).fold(
                ifLeft = {
                    ArrayAssign<TStmt, TExp>(
                        Symbol(stmt.id.name),
                        typed(stmt.index)(),
                        typed(stmt.value)()
                    ).typeOf(Typed.Void)

                    it.invalidNel()<TStmt>()
                },
                ifRight = {
                    ArrayAssign<TStmt, TExp>(
                        it.ref,
                        typed(stmt.index)(),
                        typed(stmt.value)()
                    ).typeOf(Typed.Void)()
                }
            )


            is Print -> (Print<TStmt, TExp>(typed(stmt.output)()) typeOf Typed.Void)()
            is Write -> (Write<TStmt, TExp>(typed(stmt.output)()) typeOf Typed.Void)()
            is SequenceOf -> (SequenceOf<TStmt, TExp>(
                statements = stmt.statements.map { typed(it)() }
            ) typeOf Typed.Void)()
        }
    }.toValidated()



