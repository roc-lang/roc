/// Low-level operations that get translated directly into e.g. LLVM instructions.
/// These are always wrapped when exposed to end users, and can only make it
/// into an Expr when added directly by can::builtins
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LowLevel {
    ListLen,
    ListGetUnsafe,
    ListSet,
    ListSetInPlace,
    ListSingle,
    NumAdd,
    NumSub,
    NumMul,
    NumGt,
    NumGte,
    NumLt,
    NumLte,
    NumDivUnchecked,
    NumRemUnchecked,
    NumAbs,
    NumNeg,
    NumSin,
    NumCos,
    NumSqrtUnchecked,
    NumRound,
    NumToFloat,
    Eq,
    NotEq,
    And,
    Or,
    Not,
}
