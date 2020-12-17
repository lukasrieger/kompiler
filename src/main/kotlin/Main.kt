import arrow.core.Either
import ast.UntypedProgram
import canon.IRTracer
import frontend.*
import frontend.ir.IRTranslator
import frontend.typecheck.TypeChecker

object ParsingStageProxy : Stage<String, UntypedProgram> {
    override val identifier: StageIdentifier
        get() = StageIdentifier(
            name = "NOOP",
            canonicalOrder = 0
        )

    override fun run(input: String, config: CompilerConfiguration): Either<CompilerError, UntypedProgram> {
        TODO("Not yet implemented")
    }

}

fun main() {
    val chain = ParsingStageProxy compose
            TypeChecker compose
            IRTranslator compose
            IRTracer

    println("Done.")
    println(chain.identifier)
}


