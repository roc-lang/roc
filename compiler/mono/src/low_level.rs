use roc_module::symbol::Symbol;

#[derive(Clone, Copy, Debug, PartialEq)]
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
    ListMapWithIndex {
        xs: Symbol,
    },
    ListKeepIf {
        xs: Symbol,
    },
    ListWalk {
        xs: Symbol,
        state: Symbol,
    },
    ListWalkUntil {
        xs: Symbol,
        state: Symbol,
    },
    ListWalkBackwards {
        xs: Symbol,
        state: Symbol,
    },
    ListKeepOks {
        xs: Symbol,
    },
    ListKeepErrs {
        xs: Symbol,
    },
    ListSortWith {
        xs: Symbol,
    },
    DictWalk {
        xs: Symbol,
        state: Symbol,
    },
}

impl HigherOrder {
    pub fn function_arity(&self) -> usize {
        match self {
            HigherOrder::ListMap { .. } => 1,
            HigherOrder::ListMap2 { .. } => 2,
            HigherOrder::ListMap3 { .. } => 3,
            HigherOrder::ListMap4 { .. } => 4,
            HigherOrder::ListMapWithIndex { .. } => 2,
            HigherOrder::ListKeepIf { .. } => 1,
            HigherOrder::ListWalk { .. } => 2,
            HigherOrder::ListWalkUntil { .. } => 2,
            HigherOrder::ListWalkBackwards { .. } => 2,
            HigherOrder::ListKeepOks { .. } => 1,
            HigherOrder::ListKeepErrs { .. } => 1,
            HigherOrder::ListSortWith { .. } => 2,
            HigherOrder::DictWalk { .. } => 2,
        }
    }
}

#[allow(dead_code)]
enum FirstOrder {
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
    ListLen,
    ListGetUnsafe,
    ListSet,
    ListDrop,
    ListDropAt,
    ListSingle,
    ListRepeat,
    ListReverse,
    ListConcat,
    ListContains,
    ListAppend,
    ListPrepend,
    ListJoin,
    ListRange,
    ListSwap,
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
    NumBitwiseAnd,
    NumBitwiseXor,
    NumBitwiseOr,
    NumShiftLeftBy,
    NumShiftRightBy,
    NumBytesToU16,
    NumBytesToU32,
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

/*
enum FirstOrHigher {
    First(FirstOrder),
    Higher(HigherOrder),
}

fn from_low_level(low_level: &LowLevel, arguments: &[Symbol]) -> FirstOrHigher {
    use FirstOrHigher::*;
    use FirstOrder::*;
    use HigherOrder::*;

    match low_level {
        LowLevel::StrConcat => First(StrConcat),
        LowLevel::StrJoinWith => First(StrJoinWith),
        LowLevel::StrIsEmpty => First(StrIsEmpty),
        LowLevel::StrStartsWith => First(StrStartsWith),
        LowLevel::StrStartsWithCodePt => First(StrStartsWithCodePt),
        LowLevel::StrEndsWith => First(StrEndsWith),
        LowLevel::StrSplit => First(StrSplit),
        LowLevel::StrCountGraphemes => First(StrCountGraphemes),
        LowLevel::StrFromInt => First(StrFromInt),
        LowLevel::StrFromUtf8 => First(StrFromUtf8),
        LowLevel::StrFromUtf8Range => First(StrFromUtf8Range),
        LowLevel::StrToUtf8 => First(StrToUtf8),
        LowLevel::StrRepeat => First(StrRepeat),
        LowLevel::StrFromFloat => First(StrFromFloat),
        LowLevel::ListLen => First(ListLen),
        LowLevel::ListGetUnsafe => First(ListGetUnsafe),
        LowLevel::ListSet => First(ListSet),
        LowLevel::ListDrop => First(ListDrop),
        LowLevel::ListDropAt => First(ListDropAt),
        LowLevel::ListSingle => First(ListSingle),
        LowLevel::ListRepeat => First(ListRepeat),
        LowLevel::ListReverse => First(ListReverse),
        LowLevel::ListConcat => First(ListConcat),
        LowLevel::ListContains => First(ListContains),
        LowLevel::ListAppend => First(ListAppend),
        LowLevel::ListPrepend => First(ListPrepend),
        LowLevel::ListJoin => First(ListJoin),
        LowLevel::ListRange => First(ListRange),
        LowLevel::ListSwap => First(ListSwap),
        LowLevel::DictSize => First(DictSize),
        LowLevel::DictEmpty => First(DictEmpty),
        LowLevel::DictInsert => First(DictInsert),
        LowLevel::DictRemove => First(DictRemove),
        LowLevel::DictContains => First(DictContains),
        LowLevel::DictGetUnsafe => First(DictGetUnsafe),
        LowLevel::DictKeys => First(DictKeys),
        LowLevel::DictValues => First(DictValues),
        LowLevel::DictUnion => First(DictUnion),
        LowLevel::DictIntersection => First(DictIntersection),
        LowLevel::DictDifference => First(DictDifference),
        LowLevel::SetFromList => First(SetFromList),
        LowLevel::NumAdd => First(NumAdd),
        LowLevel::NumAddWrap => First(NumAddWrap),
        LowLevel::NumAddChecked => First(NumAddChecked),
        LowLevel::NumSub => First(NumSub),
        LowLevel::NumSubWrap => First(NumSubWrap),
        LowLevel::NumSubChecked => First(NumSubChecked),
        LowLevel::NumMul => First(NumMul),
        LowLevel::NumMulWrap => First(NumMulWrap),
        LowLevel::NumMulChecked => First(NumMulChecked),
        LowLevel::NumGt => First(NumGt),
        LowLevel::NumGte => First(NumGte),
        LowLevel::NumLt => First(NumLt),
        LowLevel::NumLte => First(NumLte),
        LowLevel::NumCompare => First(NumCompare),
        LowLevel::NumDivUnchecked => First(NumDivUnchecked),
        LowLevel::NumRemUnchecked => First(NumRemUnchecked),
        LowLevel::NumIsMultipleOf => First(NumIsMultipleOf),
        LowLevel::NumAbs => First(NumAbs),
        LowLevel::NumNeg => First(NumNeg),
        LowLevel::NumSin => First(NumSin),
        LowLevel::NumCos => First(NumCos),
        LowLevel::NumSqrtUnchecked => First(NumSqrtUnchecked),
        LowLevel::NumLogUnchecked => First(NumLogUnchecked),
        LowLevel::NumRound => First(NumRound),
        LowLevel::NumToFloat => First(NumToFloat),
        LowLevel::NumPow => First(NumPow),
        LowLevel::NumCeiling => First(NumCeiling),
        LowLevel::NumPowInt => First(NumPowInt),
        LowLevel::NumFloor => First(NumFloor),
        LowLevel::NumIsFinite => First(NumIsFinite),
        LowLevel::NumAtan => First(NumAtan),
        LowLevel::NumAcos => First(NumAcos),
        LowLevel::NumAsin => First(NumAsin),
        LowLevel::NumBitwiseAnd => First(NumBitwiseAnd),
        LowLevel::NumBitwiseXor => First(NumBitwiseXor),
        LowLevel::NumBitwiseOr => First(NumBitwiseOr),
        LowLevel::NumShiftLeftBy => First(NumShiftLeftBy),
        LowLevel::NumShiftRightBy => First(NumShiftRightBy),
        LowLevel::NumBytesToU16 => First(NumBytesToU16),
        LowLevel::NumBytesToU32 => First(NumBytesToU32),
        LowLevel::NumShiftRightZfBy => First(NumShiftRightZfBy),
        LowLevel::NumIntCast => First(NumIntCast),
        LowLevel::Eq => First(Eq),
        LowLevel::NotEq => First(NotEq),
        LowLevel::And => First(And),
        LowLevel::Or => First(Or),
        LowLevel::Not => First(Not),
        LowLevel::Hash => First(Hash),
        LowLevel::ExpectTrue => First(ExpectTrue),
        LowLevel::ListMap => {
            debug_assert_eq!(arguments.len(), 3);
            Higher(ListMap {
                xs: arguments[0],
                function_name: arguments[1],
                function_env: arguments[2],
            })
        }
        LowLevel::ListMap2 => {
            debug_assert_eq!(arguments.len(), 4);
            Higher(ListMap2 {
                xs: arguments[0],
                ys: arguments[1],
                function_name: arguments[2],
                function_env: arguments[3],
            })
        }
        LowLevel::ListMap3 => {
            debug_assert_eq!(arguments.len(), 5);
            Higher(ListMap3 {
                xs: arguments[0],
                ys: arguments[1],
                zs: arguments[2],
                function_name: arguments[3],
                function_env: arguments[4],
            })
        }
        LowLevel::ListMap4 => {
            debug_assert_eq!(arguments.len(), 6);
            Higher(ListMap4 {
                xs: arguments[0],
                ys: arguments[1],
                zs: arguments[2],
                ws: arguments[3],
                function_name: arguments[4],
                function_env: arguments[5],
            })
        }
        LowLevel::ListMapWithIndex => {
            debug_assert_eq!(arguments.len(), 3);
            Higher(ListMapWithIndex {
                xs: arguments[0],
                function_name: arguments[1],
                function_env: arguments[2],
            })
        }
        LowLevel::ListKeepIf => {
            debug_assert_eq!(arguments.len(), 3);
            Higher(ListKeepIf {
                xs: arguments[0],
                function_name: arguments[1],
                function_env: arguments[2],
            })
        }
        LowLevel::ListWalk => {
            debug_assert_eq!(arguments.len(), 3);
            Higher(ListWalk {
                xs: arguments[0],
                function_name: arguments[1],
                function_env: arguments[2],
            })
        }
        LowLevel::ListWalkUntil => {
            debug_assert_eq!(arguments.len(), 3);
            Higher(ListWalkUntil {
                function_name: arguments[1],
                function_env: arguments[2],
            })
        }
        LowLevel::ListWalkBackwards => {
            debug_assert_eq!(arguments.len(), 3);
            Higher(ListWalkBackwards {
                function_name: arguments[1],
                function_env: arguments[2],
            })
        }
        LowLevel::ListKeepOks => {
            debug_assert_eq!(arguments.len(), 3);
            Higher(ListKeepOks {
                function_name: arguments[1],
                function_env: arguments[2],
            })
        }
        LowLevel::ListKeepErrs => {
            debug_assert_eq!(arguments.len(), 3);
            Higher(ListKeepErrs {
                function_name: arguments[1],
                function_env: arguments[2],
            })
        }
        LowLevel::ListSortWith => {
            debug_assert_eq!(arguments.len(), 3);
            Higher(ListSortWith {
                function_name: arguments[1],
                function_env: arguments[2],
            })
        }
        LowLevel::DictWalk => {
            debug_assert_eq!(arguments.len(), 3);
            Higher(DictWalk {
                function_name: arguments[1],
                function_env: arguments[2],
            })
        }
    }
}
*/
