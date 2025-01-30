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
    StrEndsWith,
    StrSplitOn,
    StrCountUtf8Bytes,
    StrFromInt,
    StrFromUtf8,
    StrFromUtf8Lossy,
    StrToUtf8,
    StrRepeat,
    StrFromFloat,
    StrTrim,
    StrTrimStart,
    StrTrimEnd,
    StrToNum,
    StrGetUnsafe,
    StrSubstringUnsafe,
    StrReserve,
    StrWithCapacity,
    StrReleaseExcessCapacity,
    StrWithAsciiLowercased,
    StrWithAsciiUppercased,
    StrCaselessAsciiEquals,
    ListLenUsize,
    ListLenU64,
    ListWithCapacity,
    ListReserve,
    ListReleaseExcessCapacity,
    ListAppendUnsafe,
    ListGetUnsafe,
    ListReplaceUnsafe,
    ListConcat,
    ListPrepend,
    ListSortWith,
    ListSublist,
    ListDropAt,
    ListSwap,
    ListGetCapacity,
    ListIsUnique,
    ListClone,
    ListConcatUtf8,
    ListIncref,
    ListDecref,
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
    NumDivFrac,
    NumDivTruncUnchecked,
    NumDivCeilUnchecked,
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
    NumToFloatCast,
    NumToIntChecked,
    NumToFloatChecked,
    NumToStr,
    NumCountLeadingZeroBits,
    NumCountTrailingZeroBits,
    NumCountOneBits,
    NumWithoutDecimalPoint,
    NumWithDecimalPoint,
    NumF32ToParts,
    NumF64ToParts,
    NumF32FromParts,
    NumF64FromParts,
    Eq,
    NotEq,
    Not,
    Hash,
    PtrCast,
    PtrStore,
    PtrLoad,
    PtrClearTagId,
    RefCountIncRcPtr,
    RefCountDecRcPtr,
    RefCountIncDataPtr,
    RefCountDecDataPtr,
    RefCountIsUnique,
    BoxExpr,
    UnboxExpr,
    Unreachable,
    DictPseudoSeed,
    SetJmp,
    LongJmp,
    SetLongJmpBuffer,
}

macro_rules! higher_order {
    () => {
        ListSortWith
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
            ListSortWith => 1,
            _ => unreachable!(),
        }
    }
}

/// Some wrapper functions can just be replaced by lowlevels in the backend for performance.
/// For example, Num.add should be an instruction, not a function call.
/// Variant names are chosen to help explain what to do when adding new lowlevels
#[derive(PartialEq, Eq)]
pub enum LowLevelWrapperType {
    /// This wrapper function contains no logic and we can remove it in code gen
    CanBeReplacedBy(LowLevel),
    NotALowLevelWrapper,
}

impl LowLevelWrapperType {
    pub fn from_symbol(symbol: Symbol) -> LowLevelWrapperType {
        for_symbol_help(symbol)
    }
}

/// We use a rust macro to ensure that every LowLevel gets handled
macro_rules! map_symbol_to_lowlevel {
    ($($lowlevel:ident <= $($symbol:ident),+);* $(;)?) => {

        fn for_symbol_help(symbol: Symbol) -> LowLevelWrapperType {
            use $crate::low_level::LowLevelWrapperType::*;

            // expands to a big (but non-exhaustive) match on symbols and maps them to a lowlevel
            match symbol {
                $(
                $(Symbol::$symbol)|+ => CanBeReplacedBy(LowLevel::$lowlevel),
                )*

                _ => NotALowLevelWrapper,
            }
        }

        fn _enforce_exhaustiveness(lowlevel: LowLevel) -> &'static [Symbol] {
            // when adding a new lowlevel, this match will stop being exhaustive, and give a
            // compiler error. Most likely, you are adding a new lowlevel that maps directly to a
            // symbol. For instance, you want to have `List.foo` to stand for the `ListFoo`
            // lowlevel. In that case, see below in the invocation of `map_symbol_to_lowlevel!`
            //
            // Below, we explicitly handle some exceptions to the pattern where a lowlevel maps
            // directly to a symbol. If you are unsure if your lowlevel is an exception, assume
            // that it isn't and just see if that works.
            match lowlevel {
                $(
                LowLevel::$lowlevel => &[$(Symbol::$symbol),+],
                )*

                // these are higher-order lowlevels. these need the surrounding
                // function to provide enough type information for code generation
                LowLevel::ListSortWith => unreachable!(),

                // (un)boxing is handled in a custom way
                LowLevel::BoxExpr => unreachable!(),
                LowLevel::UnboxExpr => unreachable!(),

                // these functions return polymorphic values
                LowLevel::NumIntCast => unreachable!(),
                LowLevel::NumToFloatCast => unreachable!(),
                LowLevel::NumToIntChecked => unreachable!(),
                LowLevel::NumToFloatChecked => unreachable!(),


                // these are used internally and not tied to a symbol
                LowLevel::Hash => unimplemented!(),
                LowLevel::PtrCast => unimplemented!(),
                LowLevel::PtrStore => unimplemented!(),
                LowLevel::PtrLoad => unimplemented!(),
                LowLevel::PtrClearTagId => unimplemented!(),
                LowLevel::RefCountIncRcPtr => unimplemented!(),
                LowLevel::RefCountDecRcPtr=> unimplemented!(),
                LowLevel::RefCountIncDataPtr => unimplemented!(),
                LowLevel::RefCountDecDataPtr=> unimplemented!(),
                LowLevel::RefCountIsUnique => unimplemented!(),
                LowLevel::ListIncref => unimplemented!(),
                LowLevel::ListDecref => unimplemented!(),

                LowLevel::SetJmp => unimplemented!(),
                LowLevel::LongJmp => unimplemented!(),
                LowLevel::SetLongJmpBuffer => unimplemented!(),

                // these are not implemented, not sure why
                LowLevel::StrFromInt => unimplemented!(),
                LowLevel::StrFromFloat => unimplemented!(),
            }
        }
    };
}

// here is where we actually specify the mapping for the fast majority of cases that follow the
// pattern of a symbol mapping directly to a lowlevel. In other words, most lowlevels (left) are generated
// by only one specific symbol (right)
map_symbol_to_lowlevel! {
    StrConcat <= STR_CONCAT;
    StrJoinWith <= STR_JOIN_WITH;
    StrIsEmpty <= STR_IS_EMPTY;
    StrStartsWith <= STR_STARTS_WITH;
    StrEndsWith <= STR_ENDS_WITH;
    StrSplitOn <= STR_SPLIT_ON;
    StrCountUtf8Bytes <= STR_COUNT_UTF8_BYTES;
    StrFromUtf8 <= STR_FROM_UTF8_LOWLEVEL;
    StrFromUtf8Lossy <= STR_FROM_UTF8_LOSSY;
    StrToUtf8 <= STR_TO_UTF8;
    StrRepeat <= STR_REPEAT;
    StrTrim <= STR_TRIM;
    StrTrimStart <= STR_TRIM_START;
    StrTrimEnd <= STR_TRIM_END;
    StrGetUnsafe <= STR_GET_UNSAFE;
    StrSubstringUnsafe <= STR_SUBSTRING_UNSAFE;
    StrReserve <= STR_RESERVE;
    StrToNum <= STR_TO_NUM;
    StrWithCapacity <= STR_WITH_CAPACITY;
    StrReleaseExcessCapacity <= STR_RELEASE_EXCESS_CAPACITY;
    StrWithAsciiLowercased <= STR_WITH_ASCII_LOWERCASED;
    StrWithAsciiUppercased <= STR_WITH_ASCII_UPPERCASED;
    StrCaselessAsciiEquals <= STR_CASELESS_ASCII_EQUALS;
    ListLenU64 <= LIST_LEN_U64;
    ListLenUsize <= LIST_LEN_USIZE;
    ListGetCapacity <= LIST_CAPACITY;
    ListWithCapacity <= LIST_WITH_CAPACITY;
    ListReserve <= LIST_RESERVE;
    ListReleaseExcessCapacity <= LIST_RELEASE_EXCESS_CAPACITY;
    ListIsUnique <= LIST_IS_UNIQUE;
    ListClone <= LIST_CLONE;
    ListAppendUnsafe <= LIST_APPEND_UNSAFE;
    ListPrepend <= LIST_PREPEND;
    ListGetUnsafe <= LIST_GET_UNSAFE, DICT_LIST_GET_UNSAFE;
    ListReplaceUnsafe <= LIST_REPLACE_UNSAFE;
    ListConcat <= LIST_CONCAT;
    ListSublist <= LIST_SUBLIST_LOWLEVEL;
    ListDropAt <= LIST_DROP_AT;
    ListSwap <= LIST_SWAP;
    ListConcatUtf8 <= LIST_CONCAT_UTF8;
    NumAdd <= NUM_ADD;
    NumAddWrap <= NUM_ADD_WRAP;
    NumAddChecked <= NUM_ADD_CHECKED_LOWLEVEL;
    NumAddSaturated <= NUM_ADD_SATURATED;
    NumSub <= NUM_SUB;
    NumSubWrap <= NUM_SUB_WRAP;
    NumSubChecked <= NUM_SUB_CHECKED_LOWLEVEL;
    NumSubSaturated <= NUM_SUB_SATURATED;
    NumMul <= NUM_MUL;
    NumMulWrap <= NUM_MUL_WRAP;
    NumMulSaturated <= NUM_MUL_SATURATED;
    NumMulChecked <= NUM_MUL_CHECKED_LOWLEVEL;
    NumGt <= NUM_GT;
    NumGte <= NUM_GTE;
    NumLt <= NUM_LT;
    NumLte <= NUM_LTE;
    NumCompare <= NUM_COMPARE;
    NumDivFrac <= NUM_DIV_FRAC;
    NumDivCeilUnchecked <= NUM_DIV_CEIL;
    NumDivTruncUnchecked <= NUM_DIV_TRUNC_UNCHECKED;
    NumRemUnchecked <= NUM_REM_UNCHECKED;
    NumIsMultipleOf <= NUM_IS_MULTIPLE_OF;
    NumAbs <= NUM_ABS;
    NumNeg <= NUM_NEG;
    NumSin <= NUM_SIN;
    NumCos <= NUM_COS;
    NumTan <= NUM_TAN;
    NumSqrtUnchecked <= NUM_SQRT;
    NumLogUnchecked <= NUM_LOG;
    NumRound <= NUM_ROUND;
    NumToFrac <= NUM_TO_FRAC;
    NumIsNan <= NUM_IS_NAN;
    NumIsInfinite <= NUM_IS_INFINITE;
    NumIsFinite <= NUM_IS_FINITE;
    NumPow <= NUM_POW;
    NumCeiling <= NUM_CEILING;
    NumPowInt <= NUM_POW_INT;
    NumFloor <= NUM_FLOOR;
    NumAtan <= NUM_ATAN;
    NumAcos <= NUM_ACOS;
    NumAsin <= NUM_ASIN;
    NumBitwiseAnd <= NUM_BITWISE_AND;
    NumBitwiseXor <= NUM_BITWISE_XOR;
    NumBitwiseOr <= NUM_BITWISE_OR;
    NumShiftLeftBy <= NUM_SHIFT_LEFT;
    NumShiftRightBy <= NUM_SHIFT_RIGHT;
    NumShiftRightZfBy <= NUM_SHIFT_RIGHT_ZERO_FILL;
    NumToStr <= NUM_TO_STR;
    NumCountLeadingZeroBits <= NUM_COUNT_LEADING_ZERO_BITS;
    NumCountTrailingZeroBits <= NUM_COUNT_TRAILING_ZERO_BITS;
    NumCountOneBits <= NUM_COUNT_ONE_BITS;
    NumWithoutDecimalPoint <= NUM_WITHOUT_DECIMAL_POINT;
    NumWithDecimalPoint <= NUM_WITH_DECIMAL_POINT;
    NumF32ToParts <= NUM_F32_TO_PARTS;
    NumF64ToParts <= NUM_F64_TO_PARTS;
    NumF32FromParts <= NUM_F32_FROM_PARTS;
    NumF64FromParts <= NUM_F64_FROM_PARTS;
    Eq <= BOOL_STRUCTURAL_EQ;
    NotEq <= BOOL_STRUCTURAL_NOT_EQ;
    Not <= BOOL_NOT;
    Unreachable <= LIST_UNREACHABLE;
    DictPseudoSeed <= DICT_PSEUDO_SEED;
}
