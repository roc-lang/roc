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

/// An Expr that fits in 32B.
/// It has a 1B discriminant and variants which hold payloads of at most 31B.
#[derive(Debug)]
pub enum Expr2 {
    /// A number literal (without a dot) containing no underscores
    Num {
        number: i64,     // 8B
        var: Variable,   // 4B
        style: IntStyle, // 1B
    },
    /// A floating-point literal (with a dot) containing no underscores
    Float {
        number: f64,   // 8B
        var: Variable, // 4B
    },
    /// A number literal (without a dot) containing underscores
    NumWithUnderscores {
        number: i64,           // 8B
        var: Variable,         // 4B
        text: NodeId<PoolStr>, // 8B
    },
    /// A float literal (with a dot) containing underscores
    FloatWithUnderscores {
        number: f64,           // 8B
        var: Variable,         // 4B
        text: NodeId<PoolStr>, // 8B
    },
    /// string literals of length up to 30B
    SmallStr(ArrayString<U30>), // 31B
    /// string literals of length 31B or more
    Str(NodeId<PoolStr>), // 8B
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
        first_elem: PoolVec<Expr2>, // 16B
    },
    If {
        cond_var: Variable,                // 4B
        expr_var: Variable,                // 4B
        branches: PoolVec<(Expr2, Expr2)>, // 16B
        final_else: NodeId<Expr2>,         // 8B
    },
    // When {
    //     cond_var: Variable,            // 4B
    //     expr_var: Variable,            // 4B
    //     branches: PoolVec<WhenBranch>, // 9B
    //     cond: NodeId<Expr2>,           // 8B
    // },
    // LetRec {
    //     // TODO need to make this Alias type here page-friendly, which will be hard!
    //     aliases: PoolVec<(Symbol, Alias)>, // 9B
    //     defs: PoolVec<Def>,                // 9B
    //     body_var: Variable,                // 4B
    //     body_id: NodeId<Expr2>,            // 8B
    // },
    // LetNonRec {
    //     // TODO need to make this Alias type here page-friendly, which will be hard!
    //     aliases: PoolVec<(Symbol, Alias)>, // 9B
    //     def_id: NodeId<Def>,               // 8B
    //     body_id: NodeId<Expr2>,            // 8B
    //     body_var: Variable,                // 4B
    // },
    // Call {
    //     /// NOTE: the first elem in this list is the expression and its variable.
    //     /// The others are arguments. This is because we didn't have room for
    //     /// both the expr and its variable otherwise.
    //     expr_and_args: PoolVec<(Variable, NodeId<Expr2>)>, // 9B
    //     fn_var: Variable,      // 4B
    //     closure_var: Variable, // 4B
    //     /// Cached outside expr_and_args so we don't have to potentially
    //     /// traverse that whole linked list chain to count all the args.
    //     arity: usize, // 8B - could make this smaller if need be
    //     called_via: CalledVia, // 2B
    // },
    // RunLowLevel {
    //     op: LowLevel,                             // 1B
    //     args: PoolVec<(Variable, NodeId<Expr2>)>, // 9B
    //     ret_var: Variable,                        // 4B
    // },
    // Closure {
    //     captured_symbols: PoolVec<(Symbol, Variable)>, // 9B
    //     args: PoolVec<(Variable, NodeId<Pat2>)>,       // 9B
    //     recursive: Recursive,                          // 1B
    //     extra: NodeId<ClosureExtra>,                   // 8B
    // },
    // Product Types
    // Record {
    //     record_var: Variable,                                // 4B
    //     fields: PoolVec<(PoolStr, Variable, NodeId<Expr2>)>, // 9B
    // },
    /// Empty record constant
    // EmptyRecord,
    // /// Look up exactly one field on a record, e.g. (expr).foo.
    // Access {
    //     field: NodeId<PoolStr>,   // 8B
    //     expr: NodeId<Expr2>,      // 8B
    //     vars: NodeId<AccessVars>, // 8B
    // },

    // /// field accessor as a function, e.g. (.foo) expr
    // Accessor {
    //     record_vars_id: NodeId<RecordVars>, // 8B
    //     function_var: Variable,             // 4B
    //     closure_var: Variable,              // 4B
    //     field_id: NodeId<PoolStr>,          // 8B
    // },
    // Update {
    //     symbol: Symbol,                       // 8B
    //     updates: PoolVec<(Lowercase, Field)>, // 9B
    //     vars_id: NodeId<UpdateVars>,          // 8B
    // },

    // Sum Types
    // Tag {
    //     // NOTE: A PoolStr node is a 2B length and then 14B bytes,
    //     // plus more bytes in adjacent nodes if necessary. Thus we have
    //     // a hard cap of 4094 bytes as the maximum length of tags and fields.
    //     name_id: NodeId<PoolStr>,                      // 8B
    //     variant_var: Variable,                         // 4B
    //     ext_var: Variable,                             // 4B
    //     arguments: PoolVec<(Variable, NodeId<Expr2>)>, // 9B
    // },

    // Compiles, but will crash if reached
    RuntimeError(/* TODO make a version of RuntimeError that fits in 15B */),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// It's critical that this fit in 1 byte. If it takes 2B, Expr::Call is too big.
/// That's why we have all the variants in here, instead of having separate
/// UnaryOp and Binary
pub enum CalledVia2 {
    /// Calling with space, e.g. (foo bar)
    Space,

    /// (-), e.g. (-x)
    Negate,
    /// (!), e.g. (!x)
    Not,

    // highest precedence binary op
    Caret,
    Star,
    Slash,
    DoubleSlash,
    Percent,
    DoublePercent,
    Plus,
    Minus,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEq,
    GreaterThanOrEq,
    And,
    Or,
    Pizza, // lowest precedence binary op
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

/// This is 32B, so it fits in a Node slot.
#[derive(Debug)]
pub struct ClosureExtra {
    name: Symbol,              // 8B
    body: NodeId<Expr2>,       // 8B
    function_type: Variable,   // 4B
    closure_type: Variable,    // 4B
    closure_ext_var: Variable, // 4B
    return_type: Variable,     // 4B
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
    assert_eq!(std::mem::size_of::<Expr2>(), 32);
}

#[test]
fn size_of_called_via() {
    assert_eq!(std::mem::size_of::<CalledVia2>(), 1);
}
