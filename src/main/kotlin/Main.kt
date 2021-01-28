
import arrow.core.toT
import kotlinx.coroutines.runBlocking
import util.StateS
import util.state

fun appendHello() = StateS<String, Int> { st -> (st + "Hello") toT 0 }

fun appendWorld() = StateS<String, Int> { st -> (st + "World") toT 0 }

fun main() = runBlocking {

//    val testFile =
//        File("C:\\Users\\Lukas Rieger\\IdeaProjects\\kompiler\\MiniJava_Examples\\Small\\ArrayAccess.java")
//            .inputStream()
//    val chain = Parser compose
//            TypeChecker compose
//            IRTranslator compose
//            IRCanonize compose
//            IRTracer
//
//
//    println(chain.identifier)
//
//    val result = chain(testFile)
//
//    println("Done.")
//
//    println(result)

    val x = state<String, Unit> {
        println("Soos")
        appendHello()()
        println("Saas")
        //appendWorld()()
        print("Sees")

    }

    println(x("Result is: ").a)


}


