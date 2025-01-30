use roc_module::symbol::Symbol;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HigherOrder {
    ListSortWith { xs: Symbol },
}

impl HigherOrder {
    pub fn function_arity(&self) -> usize {
        match self {
            HigherOrder::ListSortWith { .. } => 2,
        }
    }

    /// Index in the array of arguments of the symbol that is the closure data
    /// (captured environment for this function)
    pub const fn closure_data_index(&self) -> usize {
        use HigherOrder::*;

        match self {
            ListSortWith { .. } => 2,
        }
    }

    /// Index of the function symbol in the argument list
    pub const fn function_index(&self) -> usize {
        self.closure_data_index() - 1
    }
}

#[allow(dead_code)]
enum FirstOrder {
    StrConcat,
    StrJoinWith,
    StrIsEmpty,
    StrStartsWith,
    StrEndsWith,
    StrSplit,
    StrFromInt,
    StrFromUtf8,
    StrFromUtf8Range,
    StrToUtf8,
    StrRepeat,
    StrFromFloat,
    ListLenU64,
    ListLenUsize,
    ListGetUnsafe,
    ListSublist,
    ListDropAt,
    ListConcat,
    ListAppend,
    ListPrepend,
    ListSwap,
    NumAdd,
    NumAddWrap,
    NumAddChecked,
    NumSub,
    NumSubWrap,
    NumSubChecked,
    NumMul,
    NumMulWrap,
    NumMulSaturated,
    NumMulChecked,
    NumGt,
    NumGte,
    NumLt,
    NumLte,
    NumCompare,
    NumDivUnchecked,
    NumRemUnchecked,
    NumIsMultipleOf,
    NumAbs,
    NumNeg,
    NumSin,
    NumCos,
    NumTan,
    NumSqrtUnchecked,
    NumLogUnchecked,
    NumRound,
    NumToFrac,
    NumPow,
    NumCeiling,
    NumPowInt,
    NumFloor,
    NumIsNan,
    NumIsInfinite,
    NumIsFinite,
    NumAtan,
    NumAcos,
    NumAsin,
    NumBitwiseAnd,
    NumBitwiseXor,
    NumBitwiseOr,
    NumShiftLeftBy,
    NumShiftRightBy,
    NumShiftRightZfBy,
    NumIntCast,
    NumFloatCast,
    Eq,
    NotEq,
    And,
    Or,
    Not,
    Hash,
}
