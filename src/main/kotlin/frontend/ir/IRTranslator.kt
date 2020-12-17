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
import frontend.ir.ast.*

private typealias TypedClasses = List<ClassDefinition<TStmt, TExp>>

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

        val raiseStmt = IRStmt.StmSeq(
            IRStmt.Jump(afterRaise),
            IRStmt.IRLabel(runtimeRaise),
            IRStmt.Move(
                IRExp.Temp(TempVar()),
                IRExp.Call(
                    IRContext.RAISE, listOf(IRExp.Const(-1))
                )
            ),
            IRStmt.Jump(runtimeRaise),
            IRStmt.IRLabel(afterRaise)
        )

        typedClass.methods.map { typedMethod ->
            val scopedContext = globalContext.scoped(
                classDef = typedClass,
                methodDef = typedMethod
            )

            val localTemps = typedMethod.variables.definitions.map {
                (scopedContext.environment[it.name.name.qualifiedName] as IRExp.Temp).temp
            }

            val translatedBody = scopedContext.translate(typedMethod.body)()
            val returnTemp = IRExp.Temp(TempVar())
            val returnStmt = IRStmt.Move(
                returnTemp,
                scopedContext.translate(typedMethod.returnExpression)()
            )

            val expandedBody = IRStmt.StmSeq(
                translatedBody,
                raiseStmt,
                returnStmt
            )

            IRFunction(
                name = Label("${typedClass.name.name.qualifiedName}\$${typedMethod.name.name.simpleName}"),
                paramCount = typedMethod.arguments.args.size + 1,
                body = listOf(expandedBody),
                returnVar = returnTemp.temp,
                locals = localTemps
            )
        }
    }
}

fun IRContext.Companion.global(classes: List<ClassDefinition<TStmt, TExp>>) = object : IRContext {
    override val memoryLookup: Map<String, Int> =
        classes.map { typedClass ->
            typedClass
                .name
                .name
                .qualifiedName to (typedClass.fields.definitions.size * WORD_SIZE + 4)
        }.toMap()

    override val environment: Map<String, IRExp> = emptyMap()
}

interface IRContext {
    companion object {
        const val WORD_SIZE = 8

        val RAISE = IRExp.Name(Label("_raise"))
        val READ  = IRExp.Name(Label("_read"))
        val ALLOC = IRExp.Name(Label("_halloc"))
        val PRINT = IRExp.Name(Label("_println_int"))
        val WRITE = IRExp.Name(Label("_write"))

    }

    val memoryLookup: Map<String, Int>
    val environment: Map<String, IRExp>
}

fun IRContext.scoped(classDef: ClassDefinition<TStmt, TExp>, methodDef: MethodDefinition<TStmt, TExp>) =
    object : IRContext {
        val thisRef = IRExp.Param(0)
        override val memoryLookup: Map<String, Int> = this@scoped.memoryLookup
        override val environment: Map<String, IRExp> = mapOf("this" to thisRef) +
                methodDef.arguments.args.mapIndexed { index, id -> id.name.name.qualifiedName to IRExp.Param(index) }
                    .toMap() +
                classDef.fields.definitions.mapIndexed { index, id ->
                    id.name.name.qualifiedName to IRExp.Mem(
                        IRExp.BinOp(
                            op = IROp.PLUS,
                            left = thisRef,
                            right = IRExp.Const(index + 1)
                        )
                    )
                } + methodDef.variables.definitions.map { it.name.name.qualifiedName to IRExp.Temp(TempVar()) }

    }

fun IRContext.lookup(id: Name): Either<IRError, IRExp> =
    environment[id.qualifiedName]?.right() ?: IRError.MissingIdentifier(id.qualifiedName).left()

fun IRContext.sizeOf(className: Name): Either<IRError, Int> =
    memoryLookup[className.qualifiedName]?.right() ?: IRError.MissingClassSize(className.qualifiedName).left()

typealias TypedExpr = TExp
typealias TypedStatement = TStmt

fun IRContext.translate(expr: TypedExpr): Either<IRError, IRExp> = either.eager {
    when (val exp = expr.exp) {

        is Read -> IRExp.Call(IRContext.READ, emptyList())
        is This -> IRExp.Param(0)
        is New -> IRExp.Call(IRContext.ALLOC, listOf(IRExp.Const(sizeOf(exp.typeRef.name)())))
        is Constant -> IRExp.Const(exp.value)
        is ArrayLength -> IRExp.Mem(address = translate(exp.array)())
        is ArrayGet -> {
            val array = translate(exp.array)()
            val index = translate(exp.index)()
            val getLbl = Label()

            val checkBounds = IRStmt.CJump(
                rel = Rel.GE,
                left = index,
                right = IRExp.Mem(array),
                trueLabel = IRContext.RAISE.label,
                falseLabel = getLbl
            )
            IRExp.EStmtSeq(
                IRStmt.StmSeq(
                    checkBounds,
                    IRStmt.IRLabel(getLbl)
                ),
                IRExp.Mem(
                    IRExp.BinOp(
                        op = IROp.PLUS,
                        left = IRExp.BinOp(
                            op = IROp.MUL,
                            left = IRExp.BinOp(index, IROp.PLUS, IRExp.Const(1)),
                            right = IRExp.Const(IRContext.WORD_SIZE)
                        ),
                        right = array
                    )
                )
            )
        }
        is Negate -> negateExp(translate(exp.expr)())()
        is NewArray -> {
            val size = translate(exp.size)()
            val memSize = IRExp.BinOp(
                op = IROp.PLUS,
                left = IRExp.BinOp(size, IROp.MUL, IRExp.Const(IRContext.WORD_SIZE)),
                right = IRExp.Const(IRContext.WORD_SIZE)
            )
            val allocExp = IRExp.Call(IRContext.ALLOC, listOf(memSize))
            val mem = IRExp.Temp(TempVar())

            IRExp.EStmtSeq(
                IRStmt.StmSeq(
                    IRStmt.Move(mem, allocExp),
                    IRStmt.Move(IRExp.Mem(mem), size)
                ),
                mem
            )
        }
        is Identifier -> lookup(exp.name)()
        is BinaryOp -> when (exp.op) {
            Plus, Minus, Times, Div, Gt -> IRExp.BinOp(
                left = translate(exp.left)(),
                op = exp.op.toIR(),
                right = translate(exp.right)()
            )
            And -> handleBinOpAndCase(exp)()
            Lt -> handleBinOpLtCase(exp)()
        }
        is Invoke -> IRExp.Call(
            func = IRExp.Name(exp.mangledName()),
            args = exp.arguments.map { translate(it)() }
        )
        is Bool -> exp.value.takeIf { it }?.let { IRExp.Const(1) } ?: IRExp.Const(0)
    }
}

fun IRContext.translate(statement: TypedStatement): Either<IRError, IRStmt> = either.eager {
    when (val stmt = statement.stmt) {
        is ArrayAssign -> {
            val array = lookup(stmt.id.name)()
            val index = translate(stmt.index)()
            val value = translate(stmt.value)()
            val lblAssign = Label()

            val checkBounds = IRStmt.CJump(
                rel = Rel.GE,
                index,
                IRExp.Mem(array),
                IRContext.RAISE.label,
                lblAssign
            )

            IRStmt.StmSeq(
                checkBounds,
                IRStmt.IRLabel(lblAssign),
                IRStmt.Move(
                    IRExp.Mem(
                        IRExp.BinOp(
                            op = IROp.PLUS,
                            left = IRExp.BinOp(
                                op = IROp.MUL,
                                left = IRExp.BinOp(
                                    op = IROp.PLUS,
                                    left = index,
                                    right = IRExp.Const(1)
                                ),
                                right = IRExp.Const(IRContext.WORD_SIZE)
                            ),
                            right = array
                        )
                    ),
                    value
                )
            )
        }
        is Assign -> IRStmt.Move(lookup(stmt.id.name)(), translate(stmt.value)())
        is If -> {
            val lblTrue = Label()
            val lblFalse = Label()
            val lblAfter = Label()

            IRStmt.StmSeq(
                IRStmt.CJump(
                    rel = Rel.EQ,
                    left = translate(stmt.condition)(),
                    right = IRExp.Const(1),
                    trueLabel = lblTrue,
                    falseLabel = lblFalse
                ),
                IRStmt.IRLabel(lblTrue),
                translate(stmt.trueBranch)(),
                IRStmt.Jump(lblAfter),
                IRStmt.IRLabel(lblFalse),
                translate(stmt.falseBranch)(),
                IRStmt.IRLabel(lblAfter)
            )
        }
        is While -> {
            val lblBefore = Label()
            val lblBody = Label()
            val lblAfter = Label()

            IRStmt.StmSeq(
                IRStmt.IRLabel(lblBefore),
                IRStmt.CJump(
                    rel = Rel.EQ,
                    left = translate(stmt.condition)(),
                    right = IRExp.Const(1),
                    trueLabel = lblBody,
                    falseLabel = lblAfter
                ),
                IRStmt.IRLabel(lblBody),
                translate(stmt.body)(),
                IRStmt.Jump(lblBefore),
                IRStmt.IRLabel(lblAfter)
            )
        }
        is Print -> IRStmt.Move(
            IRExp.Temp(TempVar()),
            IRExp.Call(IRContext.PRINT, listOf(translate(stmt.output)()))
        )
        is Write -> IRStmt.Move(
            IRExp.Temp(TempVar()),
            IRExp.Call(IRContext.WRITE, listOf(translate(stmt.output)()))
        )
        is SequenceOf -> IRStmt.StmSeq(stmt.statements.map { translate(it)() })
    }
}


private fun IRContext.negateExp(exp: IRExp): Either<IRError, IRExp> = either.eager {
    when (exp) {
        is IRExp.BinOp, is IRExp.Call, is IRExp.Temp, is IRExp.Param, is IRExp.Mem ->
            IRExp.BinOp(IRExp.Const(1), IROp.MINUS, exp)
        is IRExp.Const -> IRExp.Const(1 - exp.value)
        is IRExp.EStmtSeq -> IRExp.EStmtSeq(exp.stm, negateExp(exp.exp)())
        // should already be caught by type checking!
        is IRExp.Name -> IRError.NonNegatableExpression(exp).left().invoke<IRExp>()
    }
}

private fun IRContext.handleBinOpLtCase(exp: BinaryOp<TExp>): Either<IRError, IRExp> = either.eager {
    val ltRes = TempVar()
    val lblTrue = Label()
    val lblFalse = Label()

    IRExp.EStmtSeq(
        IRStmt.StmSeq(
            IRStmt.Move(IRExp.Temp(ltRes), IRExp.Const(0)),
            IRStmt.CJump(
                rel = Rel.LT,
                left = translate(exp.left)(),
                right = translate(exp.right)(),
                trueLabel = lblTrue,
                falseLabel = lblFalse
            ),
            IRStmt.IRLabel(lblTrue),
            IRStmt.Move(IRExp.Temp(ltRes), IRExp.Const(1)),
            IRStmt.IRLabel(lblFalse)
        ),
        IRExp.Temp(ltRes)
    )
}

private fun IRContext.handleBinOpAndCase(exp: BinaryOp<TExp>): Either<IRError, IRExp> = either.eager {
    val andRes = TempVar()
    val testLbl = Label()
    val lblTrue = Label()
    val lblFalse = Label()

    IRExp.EStmtSeq(
        IRStmt.StmSeq(
            IRStmt.Move(IRExp.Temp(andRes), IRExp.Const(0)),
            IRStmt.CJump(
                rel = Rel.EQ,
                left = translate(exp.left)(),
                right = IRExp.Const(1),
                trueLabel = testLbl,
                falseLabel = lblFalse
            ),
            IRStmt.IRLabel(testLbl),
            IRStmt.CJump(
                rel = Rel.EQ,
                left = translate(exp.right)(),
                right = IRExp.Const(1),
                trueLabel = lblTrue,
                falseLabel = lblFalse
            ),
            IRStmt.IRLabel(lblTrue),
            IRStmt.Move(IRExp.Temp(andRes), IRExp.Const(1)),
            IRStmt.IRLabel(lblFalse)
        ),
        IRExp.Temp(andRes)
    )

}

private fun Invoke<TExp>.mangledName() =
    (obj.type as Typed.Class).let {
        "${it.name.qualifiedName}\$${method.name.qualifiedName}"
    }