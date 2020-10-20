package frontend.ir.ast

enum class Rel {
    EQ, NE, LT, GT, LE, GE, ULT, ULE, UGT, UGE
}

operator fun Rel.not(): Rel = when(this) {
    Rel.EQ -> Rel.NE
    Rel.NE -> Rel.EQ
    Rel.LT -> Rel.GE
    Rel.GT -> Rel.LE
    Rel.LE -> Rel.GT
    Rel.GE -> Rel.LT
    Rel.ULT -> Rel.UGE
    Rel.ULE -> Rel.UGT
    Rel.UGT -> Rel.ULE
    Rel.UGE -> Rel.ULT
}