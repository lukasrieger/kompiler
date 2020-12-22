package ast


class Program<T : StmtK, E : ExpK>(
    //val mainClass: ClassDef<T, E>,
    val classes: List<ClassDef<T, E>>
)

typealias UntypedProgram = Program<Stmt, Exp>
typealias TypedProgram = Program<TStmt, TExp>