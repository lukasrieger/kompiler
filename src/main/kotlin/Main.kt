import ast.untyped.UntypedProgram
import frontend.Stage
import frontend.compose
import frontend.ir.IRTranslator
import frontend.typecheck.TypeChecker

object ParsingStageProxy : Stage<String, UntypedProgram> by TODO()

fun main(): Unit {
    val progress =
        ParsingStageProxy compose
                TypeChecker compose
                IRTranslator

    val result = progress("")

    println("Done.")
}


