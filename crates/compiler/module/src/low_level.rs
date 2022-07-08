use crate::symbol::Symbol;

/// Low-level operations that get translated directly into e.g. LLVM instructions.
/// These are always wrapped when exposed to end users, and can only make it
/// into an Expr when added directly by can::builtins
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LowLevel {
    StrConcat,
    StrJoinWith,
    StrIsEmpty,
    StrStartsWith,
    StrStartsWithScalar,
    StrEndsWith,
    StrSplit,
    StrCountGraphemes,
    StrCountUtf8Bytes,
    StrFromInt,
    StrFromUtf8Range,
    StrToUtf8,
    StrRepeat,
    StrFromFloat,
    StrTrim,
    StrTrimLeft,
    StrTrimRight,
    StrToNum,
    StrToScalars,
    StrGetUnsafe,
    StrSubstringUnsafe,
    StrReserve,
    StrAppendScalar,
    StrGetScalarUnsafe,
    ListLen,
    ListWithCapacity,
    ListReserve,
    ListAppendUnsafe,
    ListGetUnsafe,
    ListReplaceUnsafe,
    ListConcat,
    ListPrepend,
    ListMap,
    ListMap2,
    ListMap3,
    ListMap4,
    ListSortWith,
    ListSublist,
    ListDropAt,
    ListSwap,
    ListIsUnique,
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
    SetToDict,
    NumAdd,
    NumAddWrap,
    NumAddChecked,
    NumAddSaturated,
    NumSub,
    NumSubWrap,
    NumSubChecked,
    NumSubSaturated,
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
    NumToFrac,
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
    NumToFloatCast,
    NumToIntChecked,
    NumToFloatChecked,
    NumToStr,
    Eq,
    NotEq,
    And,
    Or,
    Not,
    Hash,
    PtrCast,
    RefCountInc,
    RefCountDec,
    BoxExpr,
    UnboxExpr,
    Unreachable,
}

macro_rules! higher_order {
    () => {
        ListMap | ListMap2 | ListMap3 | ListMap4 | ListSortWith | DictWalk
    };
}

impl LowLevel {
    /// is one of the arguments always a function?
    /// An example is List.map.
    pub fn is_higher_order(&self) -> bool {
        use LowLevel::*;

        matches!(self, higher_order!())
    }

    pub fn function_argument_position(&self) -> usize {
        use LowLevel::*;

        match self {
            ListMap => 1,
            ListMap2 => 2,
            ListMap3 => 3,
            ListMap4 => 4,
            ListSortWith => 1,
            DictWalk => 2,
            _ => unreachable!(),
        }
    }
}

/// Some wrapper functions can just be replaced by lowlevels in the backend for performance.
/// For example, Num.add should be an instruction, not a function call.
/// Variant names are chosen to help explain what to do when adding new lowlevels
pub enum LowLevelWrapperType {
    /// This wrapper function contains no logic and we can remove it in code gen
    CanBeReplacedBy(LowLevel),
    /// This wrapper function contains important logic and we cannot remove it in code gen
    WrapperIsRequired,
    NotALowLevelWrapper,
}

impl LowLevelWrapperType {
    pub fn from_symbol(symbol: Symbol) -> LowLevelWrapperType {
        for_symbol_help(symbol)
    }
}

macro_rules! more_macro_magic {
    ($($lowlevel:ident <= $symbol:ident)+) => { more_macro_magic!{$($lowlevel <= $symbol),+} };
    ($($lowlevel:ident <= $symbol:ident),*) => {

        fn for_symbol_help(symbol: Symbol) -> LowLevelWrapperType {
            use $crate::low_level::LowLevelWrapperType::*;

            match symbol {
                $(
                Symbol::$symbol => CanBeReplacedBy(LowLevel::$lowlevel),
                )*

                Symbol::LIST_MAP => WrapperIsRequired,
                Symbol::LIST_MAP2 => WrapperIsRequired,
                Symbol::LIST_MAP3 => WrapperIsRequired,
                Symbol::LIST_MAP4 => WrapperIsRequired,
                Symbol::DICT_WALK => WrapperIsRequired,

                Symbol::LIST_SORT_WITH => WrapperIsRequired,
                Symbol::DICT_GET => WrapperIsRequired,

                _ => NotALowLevelWrapper,
            }
        }

        fn _enforce_exhaustiveness(lowlevel: LowLevel) -> Symbol {
            match lowlevel {
                $(
                LowLevel::$lowlevel => Symbol::$symbol,
                )*

                // these are higher-order lowlevels, hence need the surrounding function to provide
                // enough type information for code generation
                LowLevel::ListMap => unreachable!(),
                LowLevel::ListMap2 => unreachable!(),
                LowLevel::ListMap3 => unreachable!(),
                LowLevel::ListMap4 => unreachable!(),
                LowLevel::ListSortWith => unreachable!(),
                LowLevel::DictWalk => unreachable!(),

                // The BoxExpr operation is turned into mono Expr
                LowLevel::BoxExpr => unreachable!(),
                LowLevel::UnboxExpr => unreachable!(),

                // these are implemented explicitly in for_symbol because they are polymorphic
                LowLevel::NumIntCast => unreachable!(),
                LowLevel::NumToFloatCast => unreachable!(),
                LowLevel::NumToIntChecked => unreachable!(),
                LowLevel::NumToFloatChecked => unreachable!(),
                LowLevel::NumDivUnchecked => unreachable!(),
                LowLevel::DictEmpty => unreachable!(),

                // these are used internally and not tied to a symbol
                LowLevel::Hash => unimplemented!(),
                LowLevel::PtrCast => unimplemented!(),
                LowLevel::RefCountInc => unimplemented!(),
                LowLevel::RefCountDec => unimplemented!(),

                // these are not implemented, not sure why
                LowLevel::StrFromInt => unimplemented!(),
                LowLevel::StrFromFloat => unimplemented!(),
                LowLevel::NumIsFinite => unimplemented!(),

            }
        }
    };
}

more_macro_magic! {
    StrConcat <= STR_CONCAT,
    StrJoinWith <= STR_JOIN_WITH,
    StrIsEmpty <= STR_IS_EMPTY,
    StrStartsWith <= STR_STARTS_WITH,
    StrStartsWithScalar <= STR_STARTS_WITH_SCALAR,
    StrEndsWith <= STR_ENDS_WITH,
    StrSplit <= STR_SPLIT,
    StrCountGraphemes <= STR_COUNT_GRAPHEMES,
    StrCountUtf8Bytes <= STR_COUNT_UTF8_BYTES,
    StrFromUtf8Range <= STR_FROM_UTF8_RANGE_LOWLEVEL,
    StrToUtf8 <= STR_TO_UTF8,
    StrRepeat <= STR_REPEAT,
    StrTrim <= STR_TRIM,
    StrTrimLeft <= STR_TRIM_LEFT,
    StrTrimRight <= STR_TRIM_RIGHT,
    StrToScalars <= STR_TO_SCALARS,
    StrGetUnsafe <= STR_GET_UNSAFE,
    StrSubstringUnsafe <= STR_SUBSTRING_UNSAFE,
    StrReserve <= STR_RESERVE,
    StrAppendScalar <= STR_APPEND_SCALAR_UNSAFE,
    StrGetScalarUnsafe <= STR_GET_SCALAR_UNSAFE,
    StrToNum <= STR_TO_NUM,
    ListLen <= LIST_LEN,
    ListWithCapacity <= LIST_WITH_CAPACITY,
    ListReserve <= LIST_RESERVE,
    ListIsUnique <= LIST_IS_UNIQUE,
    ListAppendUnsafe <= LIST_APPEND_UNSAFE,
    ListPrepend <= LIST_PREPEND,
    ListGetUnsafe <= LIST_GET_UNSAFE,
    ListReplaceUnsafe <= LIST_REPLACE_UNSAFE,
    ListConcat <= LIST_CONCAT,
    ListSublist <= LIST_SUBLIST_LOWLEVEL,
    ListDropAt <= LIST_DROP_AT,
    ListSwap <= LIST_SWAP,
    DictSize <= DICT_LEN,
    DictInsert <= DICT_INSERT,
    DictRemove <= DICT_REMOVE,
    DictContains <= DICT_CONTAINS,
    DictGetUnsafe <= DICT_GET_LOWLEVEL,
    DictKeys <= DICT_KEYS,
    DictValues <= DICT_VALUES,
    DictUnion <= DICT_UNION,
    DictIntersection <= DICT_INTERSECTION,
    DictDifference <= DICT_DIFFERENCE,
    SetFromList <= SET_FROM_LIST,
    SetToDict <= SET_TO_DICT,
    NumAdd <= NUM_ADD,
    NumAddWrap <= NUM_ADD_WRAP,
    NumAddChecked <= NUM_ADD_CHECKED_LOWLEVEL,
    NumAddSaturated <= NUM_ADD_SATURATED,
    NumSub <= NUM_SUB,
    NumSubWrap <= NUM_SUB_WRAP,
    NumSubChecked <= NUM_SUB_CHECKED_LOWLEVEL,
    NumSubSaturated <= NUM_SUB_SATURATED,
    NumMul <= NUM_MUL,
    NumMulWrap <= NUM_MUL_WRAP,
    NumMulSaturated <= NUM_MUL_SATURATED,
    NumMulChecked <= NUM_MUL_CHECKED_LOWLEVEL,
    NumGt <= NUM_GT,
    NumGte <= NUM_GTE,
    NumLt <= NUM_LT,
    NumLte <= NUM_LTE,
    NumCompare <= NUM_COMPARE,
    NumDivCeilUnchecked <= NUM_DIV_CEIL,
    NumRemUnchecked <= NUM_REM,
    NumIsMultipleOf <= NUM_IS_MULTIPLE_OF,
    NumAbs <= NUM_ABS,
    NumNeg <= NUM_NEG,
    NumSin <= NUM_SIN,
    NumCos <= NUM_COS,
    NumSqrtUnchecked <= NUM_SQRT,
    NumLogUnchecked <= NUM_LOG,
    NumRound <= NUM_ROUND,
    NumToFrac <= NUM_TO_FRAC,
    NumPow <= NUM_POW,
    NumCeiling <= NUM_CEILING,
    NumPowInt <= NUM_POW_INT,
    NumFloor <= NUM_FLOOR,
    NumAtan <= NUM_ATAN,
    NumAcos <= NUM_ACOS,
    NumAsin <= NUM_ASIN,
    NumBytesToU16 <= NUM_BYTES_TO_U16_LOWLEVEL,
    NumBytesToU32 <= NUM_BYTES_TO_U32_LOWLEVEL,
    NumBitwiseAnd <= NUM_BITWISE_AND,
    NumBitwiseXor <= NUM_BITWISE_XOR,
    NumBitwiseOr <= NUM_BITWISE_OR,
    NumShiftLeftBy <= NUM_SHIFT_LEFT,
    NumShiftRightBy <= NUM_SHIFT_RIGHT,
    NumShiftRightZfBy <= NUM_SHIFT_RIGHT_ZERO_FILL,
    NumToStr <= NUM_TO_STR,
    Eq <= BOOL_EQ,
    NotEq <= BOOL_NEQ,
    And <= BOOL_AND,
    Or <= BOOL_OR,
    Not <= BOOL_NOT,
    Unreachable <= LIST_UNREACHABLE
}
