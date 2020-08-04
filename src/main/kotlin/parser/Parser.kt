package parser

import frontend.CompilerConfiguration
import frontend.CompilerError
import frontend.EitherNel
import frontend.Stage
import arrow.core.*
import arrow.core.extensions.sequence.foldable.isEmpty
import com.github.h0tk3y.betterParse.combinators.*
import com.github.h0tk3y.betterParse.grammar.Grammar
import com.github.h0tk3y.betterParse.grammar.parser
import com.github.h0tk3y.betterParse.grammar.tryParseToEnd
import com.github.h0tk3y.betterParse.parser.ErrorResult
import com.github.h0tk3y.betterParse.parser.Parsed
import com.github.h0tk3y.betterParse.parser.Parser
import frontend.syntax.*
import frontend.syntax.Type.*

data class Parsed(val value: String)


object MiniJavaParser : Stage<String, Program>() {
    override fun invoke(input: String, config: CompilerConfiguration): EitherNel<CompilerError, Program> =
        when (val r = MiniJavaGrammar.tryParseToEnd(input)) {
            is Parsed -> {
                val (success, rest) = r
                Either.cond(rest.isEmpty(),
                    ifTrue = { success },
                    ifFalse = { CompilerError.Severe("Unparsed input left.", "").nel() }
                )
            }
            is ErrorResult -> {
                CompilerError.Severe("Unparseable input: $r", "").nel().left()
            }
        }


}

object MiniJavaGrammar : Grammar<Program>() {

    private val LParenToken by token("\\(")
    private val RParenToken by token("\\)")
    private val LCParenToken by token("\\{")
    private val RCParenToken by token("}")
    private val LBrackToken by token("\\[")
    private val RBrackToken by token("]")
    private val EqToken by token("=")

    private val BinOpTokens by token("&&") or
            token("<") or
            token(">") or
            token("\\+") or
            token("-") or
            token("\\*") or
            token("/") use {
        when (text) {
            "<" -> Operator.Lt
            ">" -> Operator.Gt
            "+" -> Operator.Plus
            "-" -> Operator.Minus
            "*" -> Operator.Times
            "/" -> Operator.Div
            "&&" -> Operator.And
            else -> throw RuntimeException("Unknown operator!")
        }
    }

    private val CharCastToken by LParenToken * token("char") * RParenToken
    private val CommaToken by token(",")
    private val IfToken by token("if")
    private val ElseToken by token("else")
    private val WhileToken by token("while")
    private val PrintLnToken by token("System.out.println")
    private val PrintToken by token("System.out.print")

    private val IntArrayTypeP by token("int\\[]") asJust ArrayType
    private val BooleanTypeP by token("boolean") asJust BooleanType
    private val IntTypeP by token("int") asJust IntType


    private val TypeP by IntArrayTypeP or BooleanTypeP or IntTypeP

    private val ClassToken by token("class")
    private val ExtendsToken by token("extends")
    private val StaticToken by token("static")
    private val PublicToken by token("public")
    private val VoidToken by token("void")

    private val SemicolonToken by token(";")

    private val ReturnToken by token("return")


    private val IdentifierP by token("\\w+") use { Expression.Id(None, text) }

    private val TrueExprP by token("true") asJust Expression.True
    private val FalseExprP by token("false") asJust Expression.False
    private val ThisExprP by token("this") asJust Expression.This(None)
    private val NotExprP: Parser<Expression.Negate> by skip(token("!")) *
            parser { ExprP } map {
        Expression.Negate(None, it)
    }
    private val IntExprP by token("[0-9]+") use {
        Expression.Constant(IntType.some(), text.toInt())
    }
    private val LengthExprP: Parser<Expression.ArrayLength> by parser { ExprP } *
            skip(token("\\.")) *
            skip(token("length")) map {
        Expression.ArrayLength(IntType.some(), it)
    }

    private val BinOpExprP: Parser<Expression.BinaryOp> by parser { ExprP } *
            BinOpTokens *
            parser { ExprP } map { (left, op, right) ->
        Expression.BinaryOp(None, left, op, right)
    }

    private val IdExprP = IdentifierP
    private val NewArrayExprP: Parser<Expression.NewArray> by skip(token("new")) *
            skip(token("int")) *
            skip(LBrackToken) *
            parser { ExprP } *
            skip(RBrackToken) map { Expression.NewArray(None, it) }

    private val NewExprP by skip(token("new")) *
            IdentifierP *
            skip(LParenToken) * skip(RParenToken) map { Expression.New(None, it) }

    private val ArrayGetExprP: Parser<Expression.ArrayGet> by parser { ExprP } *
            skip(LBrackToken) *
            parser { ExprP } *
            skip(RBrackToken) map { (arr, index) -> Expression.ArrayGet(None, arr, index) }

    private val MethodCallExprP: Parser<Expression.Invoke> by parser { ExprP } *
            skip(token("\\.")) *
            IdentifierP *
            skip(LParenToken) *
            optional(parser { ExprP } * zeroOrMore(skip(CommaToken) * parser { ExprP })) *
            skip(RParenToken) map { (obj, methodName, args) ->
        val allArgs = args?.let { (firstArg, tail) -> listOf(firstArg) + tail }
        Expression.Invoke(None, obj, methodName, allArgs)
    }

    private val ExprP: Parser<Expression> by TrueExprP or
            FalseExprP or
            ThisExprP or
            NotExprP or
            IntExprP or
            LengthExprP or
            BinOpExprP or
            IdExprP or
            NewArrayExprP or
            NewExprP or
            ArrayGetExprP or
            MethodCallExprP


    private val VarDeclP by TypeP * IdentifierP * skip(SemicolonToken) map { (type, name) ->
        VariableDefinition(type, name)
    }

    private val ClassDeclP by skip(ClassToken) *
            IdentifierP *
            optional(skip(ExtendsToken) and IdentifierP) *
            skip(LCParenToken) *
            zeroOrMore(VarDeclP) *
            zeroOrMore(parser { MethodDeclP }) *
            skip(RCParenToken) map { (className, extends, decls, methods) ->
        ClassDefinition(className, extends, decls, methods)
    }

    private val MethodArgsP by skip(LParenToken) *
            optional(TypeP * IdentifierP * zeroOrMore(CommaToken * TypeP * IdentifierP)) *
            skip(RParenToken) map { args ->
        when (args) {
            null -> null
            else -> {
                val (firstType, firstId, tail) = args
                val allArgs = listOf(Args(firstType, firstId)) + tail.map { (_, type, id) ->
                    Args(type, id)
                }

                MethodArgs(allArgs)
            }

        }

    }

    private val IfStatementP by skip(IfToken) *
            skip(LParenToken) *
            ExprP *
            skip(RParenToken) *
            parser { StatementP } *
            skip(ElseToken) *
            parser { StatementP } map { (condition, trueBranch, falseBranch) ->
        If(condition, trueBranch, falseBranch)
    }

    private val WhileStatementP by skip(WhileToken) *
            skip(LParenToken) *
            ExprP *
            skip(RParenToken) *
            parser { StatementP } map { (condition, statements) ->
        While(condition, statements)
    }

    private val PrintLnStatementP by skip(PrintLnToken) *
            skip(LParenToken) *
            ExprP *
            skip(RParenToken) *
            skip(SemicolonToken) map { Print(it) }

    private val PrintStatementP by skip(PrintToken) *
            skip(LParenToken) *
            skip(CharCastToken) *
            ExprP *
            skip(RParenToken) *
            skip(SemicolonToken) map { Write(it) }

    private val AssignStatementP by IdentifierP *
            skip(EqToken) *
            ExprP *
            skip(SemicolonToken) map { (id, valueExpr) ->
        Assign(id, valueExpr)
    }

    private val ArrayAssignStatementP by IdentifierP *
            skip(LBrackToken) *
            ExprP *
            skip(RBrackToken) *
            skip(EqToken) *
            ExprP *
            skip(SemicolonToken) map { (arr, id, valueExpr) ->
        ArrayAssign(arr, id, valueExpr)
    }

    private val BlockStatementP by skip(LCParenToken) *
            zeroOrMore(parser { StatementP }) *
            skip(RCParenToken) map { SequenceOf(it) }

    private val StatementP: Parser<Statement> by BlockStatementP or
            IfStatementP or
            WhileStatementP or
            PrintLnStatementP or
            PrintStatementP or
            AssignStatementP or
            ArrayAssignStatementP

    private val MethodDeclP by skip(PublicToken) *
            TypeP *
            IdentifierP *
            MethodArgsP *
            skip(LCParenToken) *
            zeroOrMore(VarDeclP) *
            zeroOrMore(StatementP) *
            skip(ReturnToken) *
            ExprP *
            skip(SemicolonToken) *
            skip(RCParenToken) map { (type, name, args, vars, stmts, returnVal) ->
        MethodDefinition(type, name, args, vars, SequenceOf(stmts), returnVal)
    }


    private val MainClass by skip(ClassToken) *
            IdentifierP *
            skip(LCParenToken) *
            skip(PublicToken) *
            skip(StaticToken) *
            skip(VoidToken) *
            skip(token("main")) *
            skip(LParenToken) *
            skip(token("String")) *
            skip(LBrackToken) *
            skip(RBrackToken) *
            IdentifierP *
            skip(RParenToken) *
            skip(LCParenToken) *
            StatementP *
            skip(RCParenToken) *
            skip(RCParenToken) map { (className, argsName, body) ->
        MainClassDefinition(className, argsName, body)

    }


    override val rootParser by MainClass * zeroOrMore(ClassDeclP) map { (main, classes) ->
        Program(main, classes)
    }

}
