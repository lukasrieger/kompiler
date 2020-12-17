package ast


class Program<T, E>(
    //val mainClass: ClassDefinition<T, E>,
    val classes: List<ClassDefinition<T, E>>
)

typealias UntypedProgram = Program<Stmt, Exp>
typealias TypedProgram = Program<TStmt, TExp>