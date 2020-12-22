package parser

import arrow.core.Either
import arrow.core.computations.either
import arrow.core.left
import arrow.core.right
import arrow.core.rightIfNotNull
import ast.*
import com.strumenta.antlrkotlin.examples.MiniJavaLexer
import com.strumenta.antlrkotlin.examples.MiniJavaParser
import frontend.CompilerConfiguration
import frontend.CompilerError
import frontend.Stage
import frontend.StageIdentifier
import frontend.typecheck.SymbolRef
import org.antlr.v4.kotlinruntime.CharStreams
import org.antlr.v4.kotlinruntime.CommonTokenStream
import java.io.InputStream

object MissingReturn : CompilerError.Severe("Missing return expression", "")
object UnknownToken : CompilerError.Severe("Encountered an unknown token", "")
data class UnknownContext(val cause: Any) : CompilerError.Severe("Encountered an unknown ParserContext", "")
data class UnknownType(val tokenText: String) : CompilerError.Severe("Encountered an unknown TypeContext", "")

typealias UntypedPrg = Program<Stmt, Exp>


object Parser : Stage<InputStream, UntypedPrg> {
    override val identifier: StageIdentifier = StageIdentifier("Parser", 0)

    override fun run(input: InputStream, config: CompilerConfiguration): Either<CompilerError, UntypedPrg> =
        MiniJavaParser(
            CommonTokenStream(
                MiniJavaLexer(CharStreams.fromStream(input))
            )
        ).invoke()

}


operator fun MiniJavaParser.invoke(): Either<CompilerError, UntypedPrg> = either.eager {
    try {
        val prg = program()
        Program(classes = prg.others.map { it.toAst()() })
    } catch (e: Throwable) {
        UnknownContext(e).left()<UntypedPrg>()
    }

}

fun MiniJavaParser.VarDeclarationContext.toSymRef(): Either<CompilerError, SymbolRef<Exp>> = either.eager {
    val rawName = varName?.text.rightIfNotNull { UnknownToken }()
    val sym = ExpF.Symbol<Exp>(NamedRef(rawName))
    val type = varType?.toAst().rightIfNotNull { UnknownToken }()()

    SymbolRef(sym, type)
}

//fun MiniJavaParser.MainClassContext.toAst() = TODO()
val STATIC_RETURN = ExpF.Symbol<Exp>(NamedRef("__RETURN_STATIC_REF"))
fun MiniJavaParser.ClassDeclarationContext.toAst(): Either<CompilerError, ClassDef<Stmt, Exp>> = either.eager {

    val name = ExpF.Symbol<Exp>(NamedRef(className?.text.rightIfNotNull { UnknownToken }()))

    val superName = superClass?.text?.let { ExpF.Symbol<Exp>(NamedRef(it)) }

    val fields = vars.map { it.toSymRef()() }

    val methods = methods.map { m ->
        val methodName = m.methodName?.text.rightIfNotNull { UnknownToken }()

        val parameters = m.paramList?.params?.map {

            val paramName = it.paramName?.text.rightIfNotNull { UnknownToken }()

            val paramType = it.paramType?.toAst().rightIfNotNull { UnknownToken }()()


            SymbolRef(ref = ExpF.Symbol<Exp>(NamedRef(paramName)), type = paramType)
        } ?: emptyList()


        val variables = m.vars.map { it.toSymRef()() }

        val rawStmts = m.stmts.map { it.toAst()() }
        val statements = Stmt(
            stmt = StmtF.SequenceOf(rawStmts.dropLast(1))
        )


        val retExp = (rawStmts.last().stmt as? StmtF.Assign)?.takeIf { it.id == STATIC_RETURN }
            .rightIfNotNull { MissingReturn }()


        val returnType = m.returnType?.toAst().rightIfNotNull { UnknownToken }()()


        MethodDef(
            name = methodName.let { ExpF.Symbol(NamedRef(it)) },
            arguments = parameters,
            variables = variables,
            body = statements,
            returnExpression = retExp.value,
            returnType = returnType
        )
    }

    ClassDef(
        name = name,
        superName = superName,
        fields = fields,
        methods = methods
    )

}


fun MiniJavaParser.TypeContext.toAst(): Either<CompilerError, Type> = either.eager {

    when (this@toAst) {
        is MiniJavaParser.IntTypeContext -> Typed.Int
        is MiniJavaParser.BoolTypeContext -> Typed.Boolean
        is MiniJavaParser.ArrayTypeContext -> Typed.Array
        is MiniJavaParser.ClassTypeContext -> {
            val typeName = Identifier()?.text.rightIfNotNull { UnknownToken }()

            Typed.Class(NamedRef(typeName))
        }
        else -> UnknownType(this@toAst.text).left()<Type>()

    }
}


fun MiniJavaParser.StatementContext.toAst(): Either<CompilerError, Stmt> = either.eager {
    when (this@toAst) {

        is MiniJavaParser.ReturnContext -> {

            Stmt(StmtF.Assign(STATIC_RETURN, value?.toAst().rightIfNotNull { UnknownToken }()()))
        }

        is MiniJavaParser.IfContext -> {
            val cond = cond?.toAst().rightIfNotNull { UnknownToken }()()
            val thenBlock = thenBlock?.toAst().rightIfNotNull { UnknownToken }()()
            val elseBlock = elseBlock?.toAst().rightIfNotNull { UnknownToken }()()

            Stmt(StmtF.If(cond, thenBlock, elseBlock))
        }

        is MiniJavaParser.WhileContext -> {
            val cond = cond?.toAst().rightIfNotNull { UnknownToken }()()
            val block = block?.toAst().rightIfNotNull { UnknownToken }()()

            Stmt(StmtF.While(cond, block))
        }

        is MiniJavaParser.PrintlnContext -> {
            val exp = toPrint?.toAst().rightIfNotNull { UnknownToken }()()

            Stmt(StmtF.Print(exp))
        }

        is MiniJavaParser.AssignContext -> {
            val variable = Identifier()?.symbol?.text.rightIfNotNull { UnknownToken }()
            val value = value?.toAst().rightIfNotNull { UnknownToken }()()

            Stmt(StmtF.Assign(ExpF.Symbol(NamedRef(variable)), value))
        }

        is MiniJavaParser.ArrayAssignContext -> {
            val array = Identifier()?.symbol?.text.rightIfNotNull { UnknownToken }()
            val index = index?.toAst().rightIfNotNull { UnknownToken }()()
            val value = arrValue?.toAst().rightIfNotNull { UnknownToken }()()

            Stmt(StmtF.ArrayAssign(ExpF.Symbol(NamedRef(array)), index, value))
        }

        else -> UnknownContext(this@toAst).left()<Stmt>()
    }
}


fun MiniJavaParser.ExpressionContext.toAst(): Either<CompilerError, Exp> = either.eager {
    when (this@toAst) {
        is MiniJavaParser.NewContext -> {
            val identifier = Identifier()?.symbol?.text.rightIfNotNull { UnknownToken }()

            Exp(ExpF.New(ExpF.Symbol(NamedRef(identifier))))
        }
        is MiniJavaParser.BoolContext -> {
            val bool = Boolean()?.symbol?.text.rightIfNotNull { UnknownToken }()

            Exp(ExpF.Bool(bool.toBoolean()))
        }
        is MiniJavaParser.BinOPContext -> {
            val left = left?.toAst().rightIfNotNull { UnknownToken }()()
            val right = right?.toAst().rightIfNotNull { UnknownToken }()()
            val op = op?.text.rightIfNotNull { UnknownToken }().toOperator()()

            Exp(ExpF.BinaryOp(left, op, right))
        }
        is MiniJavaParser.CallContext -> {
            val obj = obj?.toAst().rightIfNotNull { UnknownToken }()()
            val fn = fn?.text.rightIfNotNull { UnknownToken }()
                .let { ExpF.Symbol<Exp>(NamedRef(it)) }
            val args = args.map { it.toAst()() }

            Exp(ExpF.Invoke(obj, fn, args))
        }
        is MiniJavaParser.IndexContext -> {
            val array = arrExp?.toAst().rightIfNotNull { UnknownToken }()()
            val index = indexExp?.toAst().rightIfNotNull { UnknownToken }()()

            Exp(ExpF.ArrayGet(array, index))
        }
        is MiniJavaParser.IdentiferContext -> {
            val ident = Identifier()?.symbol?.text.rightIfNotNull { UnknownToken }()

            Exp(ExpF.Symbol(NamedRef(ident)))
        }
        is MiniJavaParser.IntContext -> {
            val const = IntegerLiteral()?.symbol?.text.rightIfNotNull { UnknownToken }()

            Exp(ExpF.Constant(const.toInt()))
        }
        is MiniJavaParser.NewArrayContext -> {
            val arr = arrSize?.toAst().rightIfNotNull { UnknownToken }()()

            Exp(ExpF.NewArray(arr))
        }
        is MiniJavaParser.DecimalContext -> {
            val const = Decimal()?.symbol?.text.rightIfNotNull { UnknownToken }()

            Exp(ExpF.Constant(const.toInt()))
        }
        is MiniJavaParser.LengthContext -> {
            val arr = lenExp?.toAst().rightIfNotNull { UnknownToken }()()

            Exp(ExpF.ArrayLength(arr))
        }
        is MiniJavaParser.ThisContext -> {
            Exp(ExpF.This(ExpF.Symbol(NamedRef("NONE"))))
        }
        is MiniJavaParser.ListContext -> {
            val exp = findExpression()?.toAst().rightIfNotNull { UnknownToken }()()
            exp
        }
        is MiniJavaParser.NegateContext -> {
            val exp = exp?.toAst().rightIfNotNull { UnknownToken }()()

            Exp(ExpF.Negate(exp))
        }
        else -> UnknownContext(this@toAst).left()<Exp>()
    }
}


fun String.toOperator(): Either<CompilerError, Operator> = when (this) {
    "&&" -> Operator.And.right()
    "+" -> Operator.Plus.right()
    "-" -> Operator.Minus.right()
    "*" -> Operator.Times.right()
    "/" -> Operator.Div.right()
    "<" -> Operator.Gt.right()
    ">" -> Operator.Lt.right()
    else -> UnknownToken.left()
}
