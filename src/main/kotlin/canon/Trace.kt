package canon


import arrow.core.Either
import arrow.core.right
import frontend.CompilerConfiguration
import frontend.CompilerError
import frontend.Stage
import frontend.StageIdentifier
import frontend.ir.ast.*

data class Traced(val body: List<BasicBlock>, val virtualEnd: Label?) : List<BasicBlock> by body

private data class PartialBlock(val label: Label, val statements: List<IRStmt> = listOf()) : List<IRStmt> by statements

private fun PartialBlock.endsWithJump() = last().let { it is IRStmt.Jump || it is IRStmt.CJump }

object IRTracer : Stage<IRProgram, IRProgram> {

    override fun run(input: IRProgram, config: CompilerConfiguration): Either<CompilerError, IRProgram> =
        input.trace().right()

    override val identifier = StageIdentifier(
        name = "IRTracer",
        canonicalOrder = 4
    )

}

fun IRProgram.trace(): IRProgram =
    copy(functions = map { it.trace() })

fun IRFunction.trace(): IRFunction = toBlockContainer()
    .trace()
    .toIR()
    .let { copy(body = it) }

fun IRFunction.toBlockContainer(): BlockContainer =
    fold(mapOf<Label, BasicBlock>() to initPartial()) { (acc, currentBlock), stmt ->
        when (stmt) {
            is IRStmt.Jump, is IRStmt.CJump -> {
                acc + (currentBlock.label to currentBlock.toBasicBlock()) to currentBlock
            }
            is IRStmt.IRLabel -> {
                val withJump = currentBlock.ensureJumpExists(stmt)

                (acc + (withJump.label to withJump.toBasicBlock())) to PartialBlock(
                    label = stmt.label
                )
            }
            // if the last basic-block ended with a jump, the currentBlock is still intact. This allows us to check for
            // dead code: If the current block has ended, yet we landed in this branch -> skip the current statement.
            else -> if (currentBlock.endsWithJump()) {
                acc to currentBlock
            } else {
                acc to PartialBlock(currentBlock.label, currentBlock + stmt)
            }

        }
    }.let { (acc, lastBlock) ->
        val startLabel = acc.keys.minOf { it }
        val endLabel = Label()

        val finalBlock = lastBlock.takeIf { it.isNotEmpty() && !it.endsWithJump() }
            ?.let { PartialBlock(it.label, it + IRStmt.Jump(endLabel)) }

        val blocks = finalBlock?.let {
            acc + (it.label to it.toBasicBlock())
        } ?: acc

        BlockContainer(
            blocks = blocks,
            start = startLabel,
            end = endLabel,
            isVirtual = finalBlock?.let { true } ?: false
        )
    }


private fun PartialBlock.toBasicBlock(): BasicBlock =
    BasicBlock(statements)

private fun PartialBlock.ensureJumpExists(lbl: IRStmt.IRLabel): PartialBlock =
    this.takeIf { endsWithJump() } ?: PartialBlock(label, statements + IRStmt.Jump(lbl.label))

private fun IRFunction.initPartial(): PartialBlock {
    val label = (first() as? IRStmt.IRLabel) ?: IRStmt.IRLabel(Label())

    return PartialBlock(
        label = label.label,
        statements = listOf(label)
    )
}

data class TraceState(val traced: List<BasicBlock>, val untraced: Map<Label, BasicBlock>) :
    Collection<BasicBlock> by untraced.values

fun BlockContainer.initialTraceState(): TraceState =
    TraceState(
        traced = listOf(),
        untraced = blocks
    )

tailrec fun BlockContainer.traceHelper(state: TraceState, current: BasicBlock): TraceState = when {
    state.traced.contains(current) -> state
    else -> {
        val nextState = TraceState(traced = state.traced + current, untraced = state.untraced - current.entry)
        when (val relay = current.relay) {
            is IRStmt.Jump -> {
                val dst = relay.targets[0]

                when {
                    state.untraced.contains(dst) -> traceHelper(nextState, state.untraced.getValue(dst))
                    else -> state
                }

            }
            is IRStmt.CJump -> {
                val falseLabel = relay.falseLabel
                val trueLabel = relay.trueLabel

                when {
                    state.untraced.contains(falseLabel) -> traceHelper(nextState, state.untraced.getValue(falseLabel))
                    state.untraced.containsKey(trueLabel) -> {
                        val negated = IRStmt.CJump(
                            !relay.rel,
                            relay.left,
                            relay.right,
                            relay.falseLabel,
                            relay.trueLabel
                        )

                        val updatedState = nextState.copy(
                            traced = (nextState.traced - current) +
                                    current.copy(body = current.body.dropLast(1) + negated)
                        )

                        traceHelper(updatedState, state.untraced.getValue(trueLabel))
                    }

                    else -> {
                        val dummyLabel = Label()
                        val newJump = IRStmt.CJump(
                            relay.rel,
                            relay.left,
                            relay.right,
                            relay.trueLabel,
                            dummyLabel
                        )

                        val updatedState = nextState.copy(
                            traced = (nextState.traced - current) +
                                    current.copy(body = current.body.dropLast(1) + newJump)
                        )

                        traceHelper(
                            state = updatedState,
                            current = BasicBlock(
                                IRStmt.IRLabel(dummyLabel),
                                IRStmt.Jump(relay.falseLabel)
                            )
                        )
                    }
                }
            }
            else -> error("Unreachable.")
        }
    }
}

tailrec fun BlockContainer.traceRec(state: TraceState): List<BasicBlock> = when {
    state.untraced.isEmpty() -> state.traced
    else -> traceRec(traceHelper(state, first()))
}


fun BlockContainer.trace(): Traced = Traced(
    body = traceRec(initialTraceState()),
    virtualEnd = end.takeIf { isVirtual }
)


fun Traced.toIR(): List<IRStmt> = flatMap {
    it.body
} + listOfNotNull(virtualEnd?.let { IRStmt.IRLabel(it) })

