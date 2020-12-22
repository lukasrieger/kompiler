
import canonize.IRCanonize
import frontend.compose
import frontend.ir.IRTranslator
import frontend.typecheck.TypeChecker
import parser.Parser
import trace.IRTracer
import java.io.File


fun main() {

    val testFile =
        File("C:\\Users\\Lukas Rieger\\IdeaProjects\\kompiler\\MiniJava_Examples\\Small\\ArrayAccess.java")
            .inputStream()
    val chain = Parser compose
            TypeChecker compose
            IRTranslator compose
            IRCanonize compose
            IRTracer


    println(chain.identifier)

    val result = chain(testFile)

    println("Done.")

    println(result)


}


