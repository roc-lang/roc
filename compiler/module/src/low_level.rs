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
    StrTrimLeft,
    StrTrimRight,
    StrToNum,
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
    ListSublist,
    ListDropAt,
    ListSwap,
    ListAny,
    ListAll,
    ListFindUnsafe,
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
    NumToStr,
    Eq,
    NotEq,
    And,
    Or,
    Not,
    Hash,
    ExpectTrue,
    PtrCast,
    RefCountInc,
    RefCountDec,
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
            | ListAll
            | ListFindUnsafe
            | DictWalk
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
            ListMapWithIndex => 1,
            ListKeepIf => 1,
            ListWalk => 2,
            ListWalkUntil => 2,
            ListWalkBackwards => 2,
            ListKeepOks => 1,
            ListKeepErrs => 1,
            ListSortWith => 1,
            ListAny => 1,
            ListAll => 1,
            ListFindUnsafe => 1,
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
        use LowLevel::*;
        use LowLevelWrapperType::*;

        match symbol {
            Symbol::STR_CONCAT => CanBeReplacedBy(StrConcat),
            Symbol::STR_JOIN_WITH => CanBeReplacedBy(StrJoinWith),
            Symbol::STR_IS_EMPTY => CanBeReplacedBy(StrIsEmpty),
            Symbol::STR_STARTS_WITH => CanBeReplacedBy(StrStartsWith),
            Symbol::STR_STARTS_WITH_CODE_PT => CanBeReplacedBy(StrStartsWithCodePt),
            Symbol::STR_ENDS_WITH => CanBeReplacedBy(StrEndsWith),
            Symbol::STR_SPLIT => CanBeReplacedBy(StrSplit),
            Symbol::STR_COUNT_GRAPHEMES => CanBeReplacedBy(StrCountGraphemes),
            Symbol::STR_FROM_UTF8 => WrapperIsRequired,
            Symbol::STR_FROM_UTF8_RANGE => WrapperIsRequired,
            Symbol::STR_TO_UTF8 => CanBeReplacedBy(StrToUtf8),
            Symbol::STR_REPEAT => CanBeReplacedBy(StrRepeat),
            Symbol::STR_TRIM => CanBeReplacedBy(StrTrim),
            Symbol::STR_TRIM_LEFT => CanBeReplacedBy(StrTrimLeft),
            Symbol::STR_TRIM_RIGHT => CanBeReplacedBy(StrTrimRight),
            Symbol::STR_TO_DEC => WrapperIsRequired,
            Symbol::STR_TO_F64 => WrapperIsRequired,
            Symbol::STR_TO_F32 => WrapperIsRequired,
            Symbol::STR_TO_NAT => WrapperIsRequired,
            Symbol::STR_TO_U128 => WrapperIsRequired,
            Symbol::STR_TO_I128 => WrapperIsRequired,
            Symbol::STR_TO_U64 => WrapperIsRequired,
            Symbol::STR_TO_I64 => WrapperIsRequired,
            Symbol::STR_TO_U32 => WrapperIsRequired,
            Symbol::STR_TO_I32 => WrapperIsRequired,
            Symbol::STR_TO_U16 => WrapperIsRequired,
            Symbol::STR_TO_I16 => WrapperIsRequired,
            Symbol::STR_TO_U8 => WrapperIsRequired,
            Symbol::STR_TO_I8 => WrapperIsRequired,
            Symbol::LIST_LEN => CanBeReplacedBy(ListLen),
            Symbol::LIST_GET => WrapperIsRequired,
            Symbol::LIST_SET => WrapperIsRequired,
            Symbol::LIST_SINGLE => CanBeReplacedBy(ListSingle),
            Symbol::LIST_REPEAT => CanBeReplacedBy(ListRepeat),
            Symbol::LIST_REVERSE => CanBeReplacedBy(ListReverse),
            Symbol::LIST_CONCAT => CanBeReplacedBy(ListConcat),
            Symbol::LIST_CONTAINS => CanBeReplacedBy(ListContains),
            Symbol::LIST_APPEND => CanBeReplacedBy(ListAppend),
            Symbol::LIST_PREPEND => CanBeReplacedBy(ListPrepend),
            Symbol::LIST_JOIN => CanBeReplacedBy(ListJoin),
            Symbol::LIST_RANGE => CanBeReplacedBy(ListRange),
            Symbol::LIST_MAP => CanBeReplacedBy(ListMap),
            Symbol::LIST_MAP2 => CanBeReplacedBy(ListMap2),
            Symbol::LIST_MAP3 => CanBeReplacedBy(ListMap3),
            Symbol::LIST_MAP4 => CanBeReplacedBy(ListMap4),
            Symbol::LIST_MAP_WITH_INDEX => CanBeReplacedBy(ListMapWithIndex),
            Symbol::LIST_KEEP_IF => CanBeReplacedBy(ListKeepIf),
            Symbol::LIST_WALK => CanBeReplacedBy(ListWalk),
            Symbol::LIST_WALK_UNTIL => CanBeReplacedBy(ListWalkUntil),
            Symbol::LIST_WALK_BACKWARDS => CanBeReplacedBy(ListWalkBackwards),
            Symbol::LIST_KEEP_OKS => CanBeReplacedBy(ListKeepOks),
            Symbol::LIST_KEEP_ERRS => CanBeReplacedBy(ListKeepErrs),
            Symbol::LIST_SORT_WITH => CanBeReplacedBy(ListSortWith),
            Symbol::LIST_SUBLIST => CanBeReplacedBy(ListSublist),
            Symbol::LIST_DROP_AT => CanBeReplacedBy(ListDropAt),
            Symbol::LIST_SWAP => CanBeReplacedBy(ListSwap),
            Symbol::LIST_ANY => CanBeReplacedBy(ListAny),
            Symbol::LIST_ALL => CanBeReplacedBy(ListAll),
            Symbol::LIST_FIND => WrapperIsRequired,
            Symbol::DICT_LEN => CanBeReplacedBy(DictSize),
            Symbol::DICT_EMPTY => CanBeReplacedBy(DictEmpty),
            Symbol::DICT_INSERT => CanBeReplacedBy(DictInsert),
            Symbol::DICT_REMOVE => CanBeReplacedBy(DictRemove),
            Symbol::DICT_CONTAINS => CanBeReplacedBy(DictContains),
            Symbol::DICT_GET => WrapperIsRequired,
            Symbol::DICT_KEYS => CanBeReplacedBy(DictKeys),
            Symbol::DICT_VALUES => CanBeReplacedBy(DictValues),
            Symbol::DICT_UNION => CanBeReplacedBy(DictUnion),
            Symbol::DICT_INTERSECTION => CanBeReplacedBy(DictIntersection),
            Symbol::DICT_DIFFERENCE => CanBeReplacedBy(DictDifference),
            Symbol::DICT_WALK => CanBeReplacedBy(DictWalk),
            Symbol::SET_FROM_LIST => CanBeReplacedBy(SetFromList),
            Symbol::NUM_ADD => CanBeReplacedBy(NumAdd),
            Symbol::NUM_ADD_WRAP => CanBeReplacedBy(NumAddWrap),
            Symbol::NUM_ADD_CHECKED => WrapperIsRequired,
            Symbol::NUM_SUB => CanBeReplacedBy(NumSub),
            Symbol::NUM_SUB_WRAP => CanBeReplacedBy(NumSubWrap),
            Symbol::NUM_SUB_CHECKED => WrapperIsRequired,
            Symbol::NUM_MUL => CanBeReplacedBy(NumMul),
            Symbol::NUM_MUL_WRAP => CanBeReplacedBy(NumMulWrap),
            Symbol::NUM_MUL_CHECKED => WrapperIsRequired,
            Symbol::NUM_GT => CanBeReplacedBy(NumGt),
            Symbol::NUM_GTE => CanBeReplacedBy(NumGte),
            Symbol::NUM_LT => CanBeReplacedBy(NumLt),
            Symbol::NUM_LTE => CanBeReplacedBy(NumLte),
            Symbol::NUM_COMPARE => CanBeReplacedBy(NumCompare),
            Symbol::NUM_DIV_FLOAT => WrapperIsRequired,
            Symbol::NUM_DIV_CEIL => WrapperIsRequired,
            Symbol::NUM_REM => WrapperIsRequired,
            Symbol::NUM_IS_MULTIPLE_OF => CanBeReplacedBy(NumIsMultipleOf),
            Symbol::NUM_ABS => CanBeReplacedBy(NumAbs),
            Symbol::NUM_NEG => CanBeReplacedBy(NumNeg),
            Symbol::NUM_SIN => CanBeReplacedBy(NumSin),
            Symbol::NUM_COS => CanBeReplacedBy(NumCos),
            Symbol::NUM_SQRT => WrapperIsRequired,
            Symbol::NUM_LOG => WrapperIsRequired,
            Symbol::NUM_ROUND => CanBeReplacedBy(NumRound),
            Symbol::NUM_TO_FLOAT => CanBeReplacedBy(NumToFloat),
            Symbol::NUM_POW => CanBeReplacedBy(NumPow),
            Symbol::NUM_CEILING => CanBeReplacedBy(NumCeiling),
            Symbol::NUM_POW_INT => CanBeReplacedBy(NumPowInt),
            Symbol::NUM_FLOOR => CanBeReplacedBy(NumFloor),
            Symbol::NUM_TO_STR => CanBeReplacedBy(NumToStr),
            // => CanBeReplacedBy(NumIsFinite),
            Symbol::NUM_ATAN => CanBeReplacedBy(NumAtan),
            Symbol::NUM_ACOS => CanBeReplacedBy(NumAcos),
            Symbol::NUM_ASIN => CanBeReplacedBy(NumAsin),
            Symbol::NUM_BYTES_TO_U16 => WrapperIsRequired,
            Symbol::NUM_BYTES_TO_U32 => WrapperIsRequired,
            Symbol::NUM_BITWISE_AND => CanBeReplacedBy(NumBitwiseAnd),
            Symbol::NUM_BITWISE_XOR => CanBeReplacedBy(NumBitwiseXor),
            Symbol::NUM_BITWISE_OR => CanBeReplacedBy(NumBitwiseOr),
            Symbol::NUM_SHIFT_LEFT => CanBeReplacedBy(NumShiftLeftBy),
            Symbol::NUM_SHIFT_RIGHT => CanBeReplacedBy(NumShiftRightBy),
            Symbol::NUM_SHIFT_RIGHT_ZERO_FILL => CanBeReplacedBy(NumShiftRightZfBy),
            Symbol::NUM_INT_CAST => CanBeReplacedBy(NumIntCast),
            Symbol::BOOL_EQ => CanBeReplacedBy(Eq),
            Symbol::BOOL_NEQ => CanBeReplacedBy(NotEq),
            Symbol::BOOL_AND => CanBeReplacedBy(And),
            Symbol::BOOL_OR => CanBeReplacedBy(Or),
            Symbol::BOOL_NOT => CanBeReplacedBy(Not),
            // => CanBeReplacedBy(Hash),
            // => CanBeReplacedBy(ExpectTrue),
            _ => NotALowLevelWrapper,
        }
    }
}
