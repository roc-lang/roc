/// Low-level operations that get translated directly into e.g. LLVM instructions.
/// These are always wrapped when exposed to end users, and can only make it
/// into an Expr when added directly by can::builtins
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LowLevel {
    StrConcat,
    StrJoinWith,
    StrIsEmpty,
    StrStartsWith,
    StrStartsWithCodePt,
    StrEndsWith,
    StrSplit,
    StrCountGraphemes,
    StrFromInt,
    StrFromUtf8,
    StrFromUtf8Range,
    StrToUtf8,
    StrRepeat,
    StrFromFloat,
    StrTrim,
    ListLen,
    ListGetUnsafe,
    ListSet,
    ListSingle,
    ListRepeat,
    ListReverse,
    ListConcat,
    ListContains,
    ListAppend,
    ListPrepend,
    ListJoin,
    ListRange,
    ListMap,
    ListMap2,
    ListMap3,
    ListMap4,
    ListMapWithIndex,
    ListKeepIf,
    ListWalk,
    ListWalkUntil,
    ListWalkBackwards,
    ListKeepOks,
    ListKeepErrs,
    ListSortWith,
    ListTakeFirst,
    ListDrop,
    ListDropAt,
    ListSwap,
    ListAny,
    DictSize,
    DictEmpty,
    DictInsert,
    DictRemove,
    DictContains,
    DictGetUnsafe,
    DictKeys,
    DictValues,
    DictUnion,
    DictIntersection,
    DictDifference,
    DictWalk,
    SetFromList,
    NumAdd,
    NumAddWrap,
    NumAddChecked,
    NumSub,
    NumSubWrap,
    NumSubChecked,
    NumMul,
    NumMulWrap,
    NumMulChecked,
    NumGt,
    NumGte,
    NumLt,
    NumLte,
    NumCompare,
    NumDivUnchecked,
    NumDivCeilUnchecked,
    NumRemUnchecked,
    NumIsMultipleOf,
    NumAbs,
    NumNeg,
    NumSin,
    NumCos,
    NumSqrtUnchecked,
    NumLogUnchecked,
    NumRound,
    NumToFloat,
    NumPow,
    NumCeiling,
    NumPowInt,
    NumFloor,
    NumIsFinite,
    NumAtan,
    NumAcos,
    NumAsin,
    NumBytesToU16,
    NumBytesToU32,
    NumBitwiseAnd,
    NumBitwiseXor,
    NumBitwiseOr,
    NumShiftLeftBy,
    NumShiftRightBy,
    NumShiftRightZfBy,
    NumIntCast,
    Eq,
    NotEq,
    And,
    Or,
    Not,
    Hash,
    ExpectTrue,
}

macro_rules! first_order {
    () => {
        StrConcat
            | StrJoinWith
            | StrIsEmpty
            | StrStartsWith
            | StrStartsWithCodePt
            | StrEndsWith
            | StrSplit
            | StrCountGraphemes
            | StrFromInt
            | StrFromUtf8
            | StrFromUtf8Range
            | StrToUtf8
            | StrRepeat
            | StrTrim
            | StrFromFloat
            | ListLen
            | ListGetUnsafe
            | ListSet
            | ListTakeFirst
            | ListDrop
            | ListDropAt
            | ListSingle
            | ListRepeat
            | ListReverse
            | ListConcat
            | ListContains
            | ListAppend
            | ListPrepend
            | ListJoin
            | ListRange
            | ListSwap
            | DictSize
            | DictEmpty
            | DictInsert
            | DictRemove
            | DictContains
            | DictGetUnsafe
            | DictKeys
            | DictValues
            | DictUnion
            | DictIntersection
            | DictDifference
            | SetFromList
            | NumAdd
            | NumAddWrap
            | NumAddChecked
            | NumSub
            | NumSubWrap
            | NumSubChecked
            | NumMul
            | NumMulWrap
            | NumMulChecked
            | NumGt
            | NumGte
            | NumLt
            | NumLte
            | NumCompare
            | NumDivUnchecked
            | NumDivCeilUnchecked
            | NumRemUnchecked
            | NumIsMultipleOf
            | NumAbs
            | NumNeg
            | NumSin
            | NumCos
            | NumSqrtUnchecked
            | NumLogUnchecked
            | NumRound
            | NumToFloat
            | NumPow
            | NumCeiling
            | NumPowInt
            | NumFloor
            | NumIsFinite
            | NumAtan
            | NumAcos
            | NumAsin
            | NumBitwiseAnd
            | NumBitwiseXor
            | NumBitwiseOr
            | NumShiftLeftBy
            | NumShiftRightBy
            | NumBytesToU16
            | NumBytesToU32
            | NumShiftRightZfBy
            | NumIntCast
            | Eq
            | NotEq
            | And
            | Or
            | Not
            | Hash
            | ExpectTrue
    };
}

macro_rules! higher_order {
    () => {
        ListMap
            | ListMap2
            | ListMap3
            | ListMap4
            | ListMapWithIndex
            | ListKeepIf
            | ListWalk
            | ListWalkUntil
            | ListWalkBackwards
            | ListKeepOks
            | ListKeepErrs
            | ListSortWith
            | ListAny
            | DictWalk
    };
}

impl LowLevel {
    /// is one of the arguments always a function?
    /// An example is List.map.
    pub fn is_higher_order(&self) -> bool {
        use LowLevel::*;

        match self {
            first_order!() => false,
            higher_order!() => true,
        }
    }

    pub fn function_argument_position(&self) -> usize {
        use LowLevel::*;

        match self {
            first_order!() => unreachable!(),
            ListMap => 1,
            ListMap2 => 2,
            ListMap3 => 3,
            ListMap4 => 4,
            ListMapWithIndex => 1,
            ListKeepIf => 1,
            ListWalk => 2,
            ListWalkUntil => 2,
            ListWalkBackwards => 2,
            ListKeepOks => 1,
            ListKeepErrs => 1,
            ListSortWith => 1,
            ListAny => 1,
            DictWalk => 2,
        }
    }
}
