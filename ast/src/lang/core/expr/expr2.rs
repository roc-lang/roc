use arrayvec::ArrayString;
use roc_types::subs::Variable;

use crate::{
    lang::core::{fun_def::FunctionDef, pattern::Pattern2, val_def::ValueDef},
    mem_pool::{pool::NodeId, pool_str::PoolStr, pool_vec::PoolVec},
};
use roc_can::expr::Recursive;
use roc_module::called_via::CalledVia;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;

use super::record_field::RecordField;

pub const ARR_STRING_CAPACITY: usize = 24;
pub type ArrString = ArrayString<ARR_STRING_CAPACITY>;

// TODO make the inner types private?
pub type ExprId = NodeId<Expr2>;

/// An Expr that fits in 32B.
/// It has a 1B discriminant and variants which hold payloads of at most 31B.
#[derive(Debug)]
pub enum Expr2 {
    /// A negative number literal without a dot
    SmallInt {
        number: IntVal,  // 16B
        var: Variable,   // 4B
        style: IntStyle, // 1B
        text: PoolStr,   // 8B
    },
    // TODO(rvcas): rename this eventually
    /// A large (over 64-bit) negative number literal without a dot.
    /// This variant can't use IntVal because if IntVal stored 128-bit
    /// integers, it would be 32B on its own because of alignment.
    I128 {
        number: i128,    // 16B
        var: Variable,   // 4B
        style: IntStyle, // 1B
        text: PoolStr,   // 8B
    },
    // TODO(rvcas): rename this eventually
    /// A large (over 64-bit) nonnegative number literal without a dot
    /// This variant can't use IntVal because if IntVal stored 128-bit
    /// integers, it would be 32B on its own because of alignment.
    U128 {
        number: u128,    // 16B
        var: Variable,   // 4B
        style: IntStyle, // 1B
        text: PoolStr,   // 8B
    },
    /// A floating-point literal (with a dot)
    Float {
        number: FloatVal, // 16B
        var: Variable,    // 4B
        text: PoolStr,    // 8B
    },
    /// string literals of length up to 30B
    SmallStr(ArrString), // 31B
    /// string literals of length 31B or more
    Str(PoolStr), // 8B
    // Lookups
    Var(Symbol),            // 8B
    InvalidLookup(PoolStr), // 8B

    List {
        elem_var: Variable,     // 4B
        elems: PoolVec<ExprId>, // 8B
    },
    If {
        cond_var: Variable,                  // 4B
        expr_var: Variable,                  // 4B
        branches: PoolVec<(ExprId, ExprId)>, // 8B
        final_else: ExprId,                  // 4B
    },
    When {
        cond_var: Variable,            // 4B
        expr_var: Variable,            // 4B
        branches: PoolVec<WhenBranch>, // 8B
        cond: ExprId,                  // 4B
    },
    LetRec {
        defs: PoolVec<FunctionDef>, // 8B
        body_var: Variable,         // 8B
        body_id: ExprId,            // 4B
    },
    LetFunction {
        def_id: NodeId<FunctionDef>, // 4B
        body_var: Variable,          // 8B
        body_id: ExprId,             // 4B
    },
    LetValue {
        def_id: NodeId<ValueDef>, // 4B
        body_id: ExprId,          // 4B
        body_var: Variable,       // 4B
    },
    Call {
        args: PoolVec<(Variable, ExprId)>, // 8B
        expr_id: ExprId,                   // 4B
        expr_var: Variable,                // 4B
        fn_var: Variable,                  // 4B
        closure_var: Variable,             // 4B
        called_via: CalledVia,             // 2B
    },
    RunLowLevel {
        op: LowLevel,                      // 1B
        args: PoolVec<(Variable, ExprId)>, // 8B
        ret_var: Variable,                 // 4B
    },
    Closure {
        args: PoolVec<(Variable, NodeId<Pattern2>)>, // 8B
        uniq_symbol: Symbol, // 8B This is a globally unique symbol for the closure
        body_id: ExprId,     // 4B
        function_type: Variable, // 4B
        recursive: Recursive, // 1B
        extra: NodeId<ClosureExtra>, // 4B
    },
    // Product Types
    Record {
        record_var: Variable,         // 4B
        fields: PoolVec<RecordField>, // 8B
    },
    /// Empty record constant
    EmptyRecord,
    /// Look up exactly one field on a record, e.g. (expr).foo.
    Access {
        field: PoolStr,       // 4B
        expr: ExprId,         // 4B
        record_var: Variable, // 4B
        ext_var: Variable,    // 4B
        field_var: Variable,  // 4B
    },

    /// field accessor as a function, e.g. (.foo) expr
    Accessor {
        function_var: Variable, // 4B
        closure_var: Variable,  // 4B
        field: PoolStr,         // 4B
        record_var: Variable,   // 4B
        ext_var: Variable,      // 4B
        field_var: Variable,    // 4B
    },
    Update {
        symbol: Symbol,                // 8B
        updates: PoolVec<RecordField>, // 8B
        record_var: Variable,          // 4B
        ext_var: Variable,             // 4B
    },

    // Sum Types
    GlobalTag {
        name: PoolStr,                          // 4B
        variant_var: Variable,                  // 4B
        ext_var: Variable,                      // 4B
        arguments: PoolVec<(Variable, ExprId)>, // 8B
    },
    PrivateTag {
        name: Symbol,                           // 8B
        variant_var: Variable,                  // 4B
        ext_var: Variable,                      // 4B
        arguments: PoolVec<(Variable, ExprId)>, // 8B
    },
    Blank, // Rendered as empty box in editor

    // Compiles, but will crash if reached
    RuntimeError(/* TODO make a version of RuntimeError that fits in 15B */),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Problem {
    RanOutOfNodeIds,
}

pub type Res<T> = Result<T, Problem>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IntStyle {
    Decimal,
    Octal,
    Hex,
    Binary,
}

impl IntStyle {
    pub fn from_base(base: roc_parse::ast::Base) -> Self {
        use roc_parse::ast::Base;
        match base {
            Base::Decimal => Self::Decimal,
            Base::Octal => Self::Octal,
            Base::Hex => Self::Hex,
            Base::Binary => Self::Binary,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IntVal {
    I64(i64),
    U64(u64),
    I32(i32),
    U32(u32),
    I16(i16),
    U16(u16),
    I8(i8),
    U8(u8),
}

#[test]
fn size_of_intval() {
    assert_eq!(std::mem::size_of::<IntVal>(), 16);
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatVal {
    F64(f64),
    F32(f32),
}

#[derive(Debug)]
pub struct WhenBranch {
    pub patterns: PoolVec<Pattern2>, // 4B
    pub body: ExprId,                // 3B
    pub guard: Option<ExprId>,       // 4B
}

/// This is overflow data from a Closure variant, which needs to store
/// more than 32B of total data
#[derive(Debug)]
pub struct ClosureExtra {
    pub return_type: Variable,                         // 4B
    pub captured_symbols: PoolVec<(Symbol, Variable)>, // 8B
    pub closure_type: Variable,                        // 4B
    pub closure_ext_var: Variable,                     // 4B
}
