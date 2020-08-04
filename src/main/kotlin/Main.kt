import arrow.core.Either
import parser.MiniJavaParser

fun main(): Unit {
    val parsed = MiniJavaParser("""
        class ArrayAccess {
            public static void main(String[] argv) {
                System.out.println(new AA().run());
            }
        }

        class AA {

            public int run() {

                int[] arr;

                arr = new int[2];

                arr[0] = 5;
                arr[1] = 10;
                return arr[0];
            }

        }
    """.trimIndent())

    when(parsed) {
        is Either.Right -> println(parsed.b)
        is Either.Left -> println("An error occured!: $parsed")
    }
}
