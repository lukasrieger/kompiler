package dsl

interface TreeDslSyntax {

    operator fun TreeExpression.plus(other: TreeExpression) =
        TreeExpression.BinOp(this, Operator.PLUS, other)

    operator fun TreeExpression.minus(other: TreeExpression) =
        TreeExpression.BinOp(this, Operator.MINUS, other)

    operator fun TreeExpression.times(other: TreeExpression) =
        TreeExpression.BinOp(this, Operator.TIMES, other)

    operator fun TreeExpression.div(other: TreeExpression) =
        TreeExpression.BinOp(this, Operator.DIV, other)

    infix fun TreeExpression.moveFrom(from: TreeExpression) =
        TreeExpression.Move(this, from)

    infix fun TreeExpression.Call.with(parameters: List<TreeExpression>) =
        TreeExpression.Call(name, parameters)

    fun String.name() = TreeExpression.Name(Label(this))
}

interface TreeStatementSyntax

interface TreeStatementSeqDslSyntax : TreeStatementSyntax {
    val statements: List<TreeStatement>
}

fun TreeDslSyntax.seq(f: TreeStatementSeqDslSyntax.() -> TreeStatement): TreeStatement {
    val rec = object : TreeStatementSeqDslSyntax {
        override val statements: List<TreeStatement> = listOf()
    }.also { it.f() }

    return TreeStatement.Sequence(rec.statements)
}

fun TreeDslSyntax.seq(vararg statements: TreeStatement) =
    TreeStatement.Sequence(listOf(*statements))


fun TreeDslSyntax.const(value: Int): TreeExpression = TreeExpression.Constant(value)

fun TreeDslSyntax.memoryAt(address: TreeExpression) = TreeExpression.MemoryIndex(address)

fun TreeDslSyntax.call(name: Label): TreeExpression.Call =
    TreeExpression.Call(TreeExpression.Name(name))

fun TreeDslSyntax.call(name: String): TreeExpression.Call =
    TreeExpression.Call(TreeExpression.Name(Label(name)))

object DefaultTreeDslSyntax : TreeDslSyntax

enum class Operator {
    PLUS,
    TIMES,
    DIV,
    MINUS
}

data class Label(val name: String)

sealed class TreeExpression {
    object Dummy : TreeExpression()
    data class BinOp(val left: TreeExpression, val op: Operator, val right: TreeExpression) : TreeExpression()
    data class MemoryIndex(val address: TreeExpression) : TreeExpression()
    data class Constant(val value: Int) : TreeExpression()
    data class Move(val destination: TreeExpression, val source: TreeExpression) : TreeExpression()
    data class Name(val name: Label) : TreeExpression()
    data class Call(val name: Name, val args: List<TreeExpression> = emptyList()) : TreeExpression()
}

sealed class TreeStatement {
    object Dummy : TreeStatement()
    data class Sequence(val statements: List<TreeStatement>) : TreeStatement()
}

fun tree(f: TreeDslSyntax.() -> TreeExpression): TreeExpression =
    DefaultTreeDslSyntax.f()


fun test() = tree {
    val address =
        TreeExpression.Dummy + (TreeExpression.Dummy * (TreeExpression.Dummy + const(1)))

    seq {
        TreeStatement.Dummy
    }

    call("_print") with listOf("Hello".name(), "World!".name())

    memoryAt(address) moveFrom TreeExpression.Dummy


}