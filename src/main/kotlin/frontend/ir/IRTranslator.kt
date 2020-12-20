package frontend.ir

import arrow.core.Either
import arrow.core.computations.either
import arrow.core.left
import arrow.core.right
import ast.*
import ast.ExpF.*
import ast.Operator.*
import ast.StmtF.*
import frontend.CompilerConfiguration
import frontend.CompilerError
import frontend.Stage
import frontend.StageIdentifier
import frontend.ir.IRContext.Companion.THIS
import frontend.ir.ast.*
import frontend.ir.ast.IRExp.*
import frontend.ir.ast.IRStmt.*

private typealias TypedClasses = List<ClassDef<TStmt, TExp>>

sealed class IRError : CompilerError.Severe("", "") {
    data class MissingIdentifier(val id: String) : IRError()

    data class MissingClassSize(val name: String) : IRError()

    data class NonNegatableExpression(val exp: IRExp) : IRError()
}

object IRTranslator : Stage<TypedProgram, IRProgram> {

    override fun run(input: TypedProgram, config: CompilerConfiguration): Either<CompilerError, IRProgram> =
        either.eager {
            val irContext = IRContext.global(input.classes)

            IRProgram(input.classes.translate(irContext)().flatten())
        }

    override val identifier = StageIdentifier(
        name = "IRTranslator",
        canonicalOrder = 3
    )


}

private typealias IRClass = List<IRFunction>

fun TypedClasses.translate(globalContext: IRContext): Either<IRError, List<IRClass>> = either.eager {
    map { typedClass ->
        val runtimeRaise = Label()
        val afterRaise = Label()

        val raiseStmt = StmSeq(
            Jump(afterRaise),
            IRLabel(runtimeRaise),
            Move(
                Temp(TempVar()),
                Call(
                    IRContext.RAISE, listOf(Const(-1))
                )
            ),
            Jump(runtimeRaise),
            IRLabel(afterRaise)
        )

        typedClass.methods.map { typedMethod ->
            val scopedContext = globalContext.scoped(
                classDef = typedClass,
                methodDef = typedMethod
            )

            val localTemps = typedMethod.variables.map {
                (scopedContext.environment[it.ref.name] as Temp).temp
            }

            val translatedBody = scopedContext.translate(typedMethod.body)()
            val returnTemp = Temp(TempVar())
            val returnStmt = Move(
                returnTemp,
                scopedContext.translate(typedMethod.returnExpression)()
            )

            val expandedBody = StmSeq(
                translatedBody,
                raiseStmt,
                returnStmt
            )

            IRFunction(
                name = Label("${typedClass.name.name.qualifiedName}\$${typedMethod.name.name.simpleName}"),
                paramCount = typedMethod.arguments.size + 1,
                body = listOf(expandedBody),
                returnVar = returnTemp.temp,
                locals = localTemps
            )
        }
    }
}

fun IRContext.Companion.global(classes: List<ClassDef<TStmt, TExp>>) = object : IRContext {
    override val memoryLookup = classes.map { typedClass ->
        typedClass
            .name
            .name to (typedClass.fields.size * WORD_SIZE + 4)
    }.toMap()

    override val environment: Map<NamedRef, IRExp> = emptyMap()
}

interface IRContext {
    companion object {
        const val WORD_SIZE = 8

        val THIS = NamedRef("this", "")
        val RAISE = Name(Label("_raise"))
        val READ = Name(Label("_read"))
        val ALLOC = Name(Label("_halloc"))
        val PRINT = Name(Label("_println_int"))
        val WRITE = Name(Label("_write"))

    }

    val memoryLookup: Map<NamedRef, Int>
    val environment: Map<NamedRef, IRExp>
}

fun IRContext.scoped(classDef: ClassDef<TStmt, TExp>, methodDef: MethodDef<TStmt, TExp>) =
    object : IRContext {
        val thisRef = Param(0)
        override val memoryLookup = this@scoped.memoryLookup
        override val environment: Map<NamedRef, IRExp> = mapOf(THIS to thisRef) +
                methodDef.arguments.mapIndexed { index, id -> id.ref.name to Param(index) }
                    .toMap() +
                classDef.fields.mapIndexed { index, id ->
                    id.ref.name to Mem(
                        BinOp(
                            op = IROp.PLUS,
                            left = thisRef,
                            right = Const(index + 1)
                        )
                    )
                } + methodDef.variables.map { it.ref.name to Temp(TempVar()) }

    }

fun IRContext.lookup(id: NamedRef): Either<IRError, IRExp> =
    environment[id]?.right() ?: IRError.MissingIdentifier(id.qualifiedName).left()

fun IRContext.sizeOf(className: NamedRef): Either<IRError, Int> =
    memoryLookup[className]?.right() ?: IRError.MissingClassSize(className.qualifiedName).left()


fun IRContext.translate(expr: TExp): Either<IRError, IRExp> = either.eager {
    when (val exp = expr.exp) {

        is Read -> Call(IRContext.READ, emptyList())
        is This -> Param(0)
        is New -> Call(IRContext.ALLOC, listOf(Const(sizeOf(exp.typeRef.name)())))
        is Constant -> Const(exp.value)
        is ArrayLength -> Mem(address = translate(exp.array)())
        is ArrayGet -> {
            val array = translate(exp.array)()
            val index = translate(exp.index)()
            val getLbl = Label()

            val checkBounds = CJump(
                rel = Rel.GE,
                left = index,
                right = Mem(array),
                trueLabel = IRContext.RAISE.label,
                falseLabel = getLbl
            )
            EStmtSeq(
                StmSeq(
                    checkBounds,
                    IRLabel(getLbl)
                ),
                Mem(
                    BinOp(
                        op = IROp.PLUS,
                        left = BinOp(
                            op = IROp.MUL,
                            left = BinOp(index, IROp.PLUS, Const(1)),
                            right = Const(IRContext.WORD_SIZE)
                        ),
                        right = array
                    )
                )
            )
        }
        is Negate -> negateExp(translate(exp.expr)())()
        is NewArray -> {
            val size = translate(exp.size)()
            val memSize = BinOp(
                op = IROp.PLUS,
                left = BinOp(size, IROp.MUL, Const(IRContext.WORD_SIZE)),
                right = Const(IRContext.WORD_SIZE)
            )
            val allocExp = Call(IRContext.ALLOC, listOf(memSize))
            val mem = Temp(TempVar())

            EStmtSeq(
                StmSeq(
                    Move(mem, allocExp),
                    Move(Mem(mem), size)
                ),
                mem
            )
        }
        is Symbol -> lookup(exp.name)()
        is BinaryOp -> when (exp.op) {
            Plus, Minus, Times, Div, Gt -> BinOp(
                left = translate(exp.left)(),
                op = exp.op.toIR(),
                right = translate(exp.right)()
            )
            And -> handleBinOpAndCase(exp)()
            Lt -> handleBinOpLtCase(exp)()
        }
        is Invoke -> Call(
            func = Name(exp.mangledName()),
            args = exp.arguments.map { translate(it)() }
        )
        is Bool -> exp.value.takeIf { it }?.let { Const(1) } ?: Const(0)
    }
}

fun IRContext.translate(statement: TStmt): Either<IRError, IRStmt> = either.eager {
    when (val stmt = statement.stmt) {
        is ArrayAssign -> {
            val array = lookup(stmt.id.name)()
            val index = translate(stmt.index)()
            val value = translate(stmt.value)()
            val lblAssign = Label()

            val checkBounds = CJump(
                rel = Rel.GE,
                index,
                Mem(array),
                IRContext.RAISE.label,
                lblAssign
            )

            StmSeq(
                checkBounds,
                IRLabel(lblAssign),
                Move(
                    Mem(
                        BinOp(
                            op = IROp.PLUS,
                            left = BinOp(
                                op = IROp.MUL,
                                left = BinOp(
                                    op = IROp.PLUS,
                                    left = index,
                                    right = Const(1)
                                ),
                                right = Const(IRContext.WORD_SIZE)
                            ),
                            right = array
                        )
                    ),
                    value
                )
            )
        }
        is Assign -> Move(lookup(stmt.id.name)(), translate(stmt.value)())
        is If -> {
            val lblTrue = Label()
            val lblFalse = Label()
            val lblAfter = Label()

            StmSeq(
                CJump(
                    rel = Rel.EQ,
                    left = translate(stmt.condition)(),
                    right = Const(1),
                    trueLabel = lblTrue,
                    falseLabel = lblFalse
                ),
                IRLabel(lblTrue),
                translate(stmt.trueBranch)(),
                Jump(lblAfter),
                IRLabel(lblFalse),
                translate(stmt.falseBranch)(),
                IRLabel(lblAfter)
            )
        }
        is While -> {
            val lblBefore = Label()
            val lblBody = Label()
            val lblAfter = Label()

            StmSeq(
                IRLabel(lblBefore),
                CJump(
                    rel = Rel.EQ,
                    left = translate(stmt.condition)(),
                    right = Const(1),
                    trueLabel = lblBody,
                    falseLabel = lblAfter
                ),
                IRLabel(lblBody),
                translate(stmt.body)(),
                Jump(lblBefore),
                IRLabel(lblAfter)
            )
        }
        is Print -> Move(
            Temp(TempVar()),
            Call(IRContext.PRINT, listOf(translate(stmt.output)()))
        )
        is Write -> Move(
            Temp(TempVar()),
            Call(IRContext.WRITE, listOf(translate(stmt.output)()))
        )
        is SequenceOf -> StmSeq(stmt.statements.map { translate(it)() })
    }
}


private fun IRContext.negateExp(exp: IRExp): Either<IRError, IRExp> = either.eager {
    when (exp) {
        is BinOp, is Call, is Temp, is Param, is Mem -> BinOp(Const(1), IROp.MINUS, exp)
        is Const -> Const(1 - exp.value)
        is EStmtSeq -> EStmtSeq(exp.stm, negateExp(exp.exp)())
        is Name -> IRError.NonNegatableExpression(exp).left().invoke<IRExp>()
    }
}

private fun IRContext.handleBinOpLtCase(exp: BinaryOp<TExp>): Either<IRError, IRExp> = either.eager {
    val ltRes = TempVar()
    val lblTrue = Label()
    val lblFalse = Label()

    EStmtSeq(
        StmSeq(
            Move(Temp(ltRes), Const(0)),
            CJump(
                rel = Rel.LT,
                left = translate(exp.left)(),
                right = translate(exp.right)(),
                trueLabel = lblTrue,
                falseLabel = lblFalse
            ),
            IRLabel(lblTrue),
            Move(Temp(ltRes), Const(1)),
            IRLabel(lblFalse)
        ),
        Temp(ltRes)
    )
}

private fun IRContext.handleBinOpAndCase(exp: BinaryOp<TExp>): Either<IRError, IRExp> = either.eager {
    val andRes = TempVar()
    val testLbl = Label()
    val lblTrue = Label()
    val lblFalse = Label()

    EStmtSeq(
        StmSeq(
            Move(Temp(andRes), Const(0)),
            CJump(
                rel = Rel.EQ,
                left = translate(exp.left)(),
                right = Const(1),
                trueLabel = testLbl,
                falseLabel = lblFalse
            ),
            IRLabel(testLbl),
            CJump(
                rel = Rel.EQ,
                left = translate(exp.right)(),
                right = Const(1),
                trueLabel = lblTrue,
                falseLabel = lblFalse
            ),
            IRLabel(lblTrue),
            Move(Temp(andRes), Const(1)),
            IRLabel(lblFalse)
        ),
        Temp(andRes)
    )

}

private fun Invoke<TExp>.mangledName() =
    (obj.type as Typed.Class).let {
        "${it.name.qualifiedName}\$${method.name.qualifiedName}"
    }