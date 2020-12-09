use crate::pool::{NodeId, PoolStr, PoolVec};
use arraystring::{typenum::U30, ArrayString};
use roc_can::def::Annotation;
use roc_can::expr::{Field, Recursive};
use roc_module::ident::Lowercase;
use roc_module::low_level::LowLevel;
use roc_module::operator::CalledVia;
use roc_module::symbol::Symbol;
use roc_types::subs::Variable;
use roc_types::types::Alias;

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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatVal {
    F64(f64),
    F32(f32),
}

#[test]
fn size_of_intval() {
    assert_eq!(std::mem::size_of::<IntVal>(), 16);
}

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
    /// A large (over 64-bit) negative number literal without a dot.
    /// This variant can't use IntVal because if IntVal stored 128-bit
    /// integers, it would be 32B on its own because of alignment.
    I128 {
        number: i128,    // 16B
        var: Variable,   // 4B
        style: IntStyle, // 1B
        text: PoolStr,   // 8B
    },
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
    },
    /// string literals of length up to 30B
    SmallStr(ArrayString<U30>), // 31B
    /// string literals of length 31B or more
    Str(PoolStr), // 8B
    // Lookups
    Var(Symbol), // 8B

    /// Separate from List because BuckeList must be non-empty, and in this case
    /// the list literal has no elements
    EmptyList {
        list_var: Variable, // 4B - required for uniqueness of the list
        elem_var: Variable, // 4B
    },
    List {
        list_var: Variable,         // 4B - required for uniqueness of the list
        elem_var: Variable,         // 4B
        elems: PoolVec<Expr2>, // 8B
    },
    If {
        cond_var: Variable,                // 4B
        expr_var: Variable,                // 4B
        branches: PoolVec<(Expr2, Expr2)>, // 8B
        final_else: NodeId<Expr2>,         // 4B
    },
    When {
        cond_var: Variable,            // 4B
        expr_var: Variable,            // 4B
        branches: PoolVec<WhenBranch>, // 8B
        cond: NodeId<Expr2>,           // 4B
    },
    LetRec {
        // TODO need to make this Alias type here page-friendly, which will be hard!
        aliases: PoolVec<(Symbol, Alias)>, // 8B
        defs: PoolVec<Def>,                // 8B
        body_var: Variable,                // 8B
        body_id: NodeId<Expr2>,            // 4B
    },
    LetNonRec {
        // TODO need to make this Alias type here page-friendly, which will be hard!
        aliases: PoolVec<(Symbol, Alias)>, // 8B
        def_id: NodeId<Def>,               // 4B
        body_id: NodeId<Expr2>,            // 4B
        body_var: Variable,                // 4B
    },
    Call {
        args: PoolVec<(Variable, NodeId<Expr2>)>, // 8B
        expr: NodeId<Expr2>,                      // 4B
        expr_var: Variable,                       // 4B
        fn_var: Variable,                         // 4B
        closure_var: Variable,                    // 4B
        called_via: CalledVia,                    // 2B
    },
    RunLowLevel {
        op: LowLevel,                             // 1B
        args: PoolVec<(Variable, NodeId<Expr2>)>, // 8B
        ret_var: Variable,                        // 4B
    },
    Closure {
        args: PoolVec<(Variable, NodeId<Pat2>)>, // 8B
        name: Symbol,                            // 8B
        body: NodeId<Expr2>,                     // 4B
        function_type: Variable,                 // 4B
        recursive: Recursive,                    // 1B
        extra: NodeId<ClosureExtra>,             // 4B
    },
    // Product Types
    Record {
        record_var: Variable,                                // 4B
        fields: PoolVec<(PoolStr, Variable, NodeId<Expr2>)>, // 8B
    },
    /// Empty record constant
    EmptyRecord,
    /// Look up exactly one field on a record, e.g. (expr).foo.
    Access {
        field: NodeId<PoolStr>,   // 4B
        expr: NodeId<Expr2>,      // 4B
        vars: NodeId<AccessVars>, // 4B
    },

    /// field accessor as a function, e.g. (.foo) expr
    Accessor {
        record_vars_id: NodeId<RecordVars>, // 4B
        function_var: Variable,             // 4B
        closure_var: Variable,              // 4B
        field_id: NodeId<PoolStr>,          // 4B
    },
    Update {
        symbol: Symbol,                       // 8B
        updates: PoolVec<(Lowercase, Field)>, // 8B
        vars_id: NodeId<UpdateVars>,          // 4B
    },

    // Sum Types
    Tag {
        // NOTE: A PoolStr node is a 2B length and then 14B bytes,
        // plus more bytes in adjacent nodes if necessary. Thus we have
        // a hard cap of 4094 bytes as the maximum length of tags and fields.
        name_id: NodeId<PoolStr>,                      // 4B
        variant_var: Variable,                         // 4B
        ext_var: Variable,                             // 4B
        arguments: PoolVec<(Variable, NodeId<Expr2>)>, // 8B
    },

    // Compiles, but will crash if reached
    RuntimeError(/* TODO make a version of RuntimeError that fits in 15B */),
}

#[derive(Debug)]
pub struct Def {
    pub pattern: NodeId<Pat2>, // 3B
    pub expr: NodeId<Expr2>,   // 3B
    // TODO maybe need to combine these vars behind a pointer?
    pub expr_var: Variable,                        // 4B
    pub pattern_vars: PoolVec<(Symbol, Variable)>, // 4B
    // TODO how big is an annotation? What about an Option<Annotation>?
    pub annotation: Option<Annotation>, // ???
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Pat2 {
    Todo,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct UpdateVars {
    record_var: Variable, // 4B
    ext_var: Variable,    // 4B
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct RecordVars {
    record_var: Variable, // 4B
    ext_var: Variable,    // 4B
    field_var: Variable,  // 4B
}

/// This is 15B, so it fits in a Node slot.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct AccessVars {
    record_var: Variable, // 4B
    ext_var: Variable,    // 4B
    field_var: Variable,  // 4B
}

/// This is overflow data from a Closure variant, which needs to store
/// more than 32B of total data
#[derive(Debug)]
pub struct ClosureExtra {
    return_type: Variable,                         // 4B
    captured_symbols: PoolVec<(Symbol, Variable)>, // 8B
    closure_type: Variable,                        // 4B
    closure_ext_var: Variable,                     // 4B
}

#[derive(Debug)]
pub struct WhenBranch {
    pub patterns: PoolVec<Pat2>,      // 4B
    pub body: NodeId<Expr2>,          // 3B
    pub guard: Option<NodeId<Expr2>>, // 4B
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PatternId {
    /// TODO: PatternPoolId
    page_id: ExprPoolId,
    /// TODO: PatternPoolSlot
    slot: ExprPoolSlot,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PatId {
    page_id: ExprPoolId, // TODO PatPoolId
    slot: ExprPoolSlot,  // TODO PatPoolSlot
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExprId {
    page_id: ExprPoolId,
    slot: ExprPoolSlot,
}

// We have a maximum of 65K pages.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExprPoolId(u16);

/// Each of these is the index of one 16B node inside a page's 4096B
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExprPoolSlot(u8);

#[test]
fn size_of_expr() {
    assert_eq!(std::mem::size_of::<Expr2>(), crate::pool::NODE_BYTES);
}
