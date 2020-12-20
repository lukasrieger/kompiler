import arrow.core.Either
import ast.UntypedProgram
import canonize.IRCanonize
import frontend.*
import frontend.ir.IRTranslator
import frontend.typecheck.TypeChecker
import trace.IRTracer

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
            IRCanonize compose
            IRTracer

    println("Done.")
    println(chain.identifier)
}


