use roc_module::symbol::Symbol;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HigherOrder {
    ListMap {
        xs: Symbol,
    },
    ListMap2 {
        xs: Symbol,
        ys: Symbol,
    },
    ListMap3 {
        xs: Symbol,
        ys: Symbol,
        zs: Symbol,
    },
    ListMap4 {
        xs: Symbol,
        ys: Symbol,
        zs: Symbol,
        ws: Symbol,
    },
    ListSortWith {
        xs: Symbol,
    },
}

impl HigherOrder {
    pub fn function_arity(&self) -> usize {
        match self {
            HigherOrder::ListMap { .. } => 1,
            HigherOrder::ListMap2 { .. } => 2,
            HigherOrder::ListMap3 { .. } => 3,
            HigherOrder::ListMap4 { .. } => 4,
            HigherOrder::ListSortWith { .. } => 2,
        }
    }

    /// Index in the array of arguments of the symbol that is the closure data
    /// (captured environment for this function)
    pub const fn closure_data_index(&self) -> usize {
        use HigherOrder::*;

        match self {
            ListMap { .. } | ListSortWith { .. } => 2,
            ListMap2 { .. } => 3,
            ListMap3 { .. } => 4,
            ListMap4 { .. } => 5,
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
    StrStartsWithScalar,
    StrEndsWith,
    StrSplit,
    StrCountGraphemes,
    StrFromInt,
    StrFromUtf8,
    StrFromUtf8Range,
    StrToUtf8,
    StrRepeat,
    StrFromFloat,
    ListLen,
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
    NumBytesToU16,
    NumBytesToU32,
    NumBytesToU64,
    NumBytesToU128,
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
