use crate::ir::{CallSpecId, UpdateModeId};
use crate::layout::TagIdIntType;
use crate::layout_soa::{FunctionLayout, Index, Layout, Layouts, Slice};
use roc_module::ident::{ForeignSymbol, TagName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_std::RocDec;

static_assertions::assert_eq_size!(Stmt, [u8; 20]);
static_assertions::assert_eq_size!(Expr, [u8; 40]);

use bumpalo::collections::String;
use bumpalo::collections::Vec;

#[derive(Clone, Debug)]
pub struct Proc<'a> {
    pub name: Symbol,
    pub args: &'a [(Index<Layout>, Symbol)],
    pub body: Index<Stmt>,
    /// Is the closure data passed as an argument
    pub closure_data_layout: Option<Index<Layout>>,
    pub ret_layout: Index<Layout>,
    // pub is_self_recursive: SelfRecursive,
    // pub must_own_arguments: bool,
    // pub host_exposed_layouts: HostExposedLayouts<'a>,
}

struct Module<'a> {
    procs: Vec<'a, Proc<'a>>,

    stmts: Vec<'a, Stmt>,
    stmt_symbols: Vec<'a, Symbol>,

    exprs: Vec<'a, Expr>,
    expr_symbols: Vec<'a, Symbol>,

    literals: Vec<'a, Literal>,

    layouts: Layouts,

    symbols: Vec<'a, Symbol>,
    branch_infos: Vec<'a, BranchInfo>,
    branches: Vec<'a, (u64, Index<BranchInfo>, Index<Stmt>)>,
    parameters: Vec<'a, Param>,
    // string literals are slices into this string
    strings: String<'a>,
    big_numbers: Vec<'a, i128>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Param {
    pub symbol: Symbol,
    pub borrow: bool,
    pub layout: Index<Layout>,
}

/// in the block below, symbol `scrutinee` is assumed be be of shape `tag_id`
#[derive(Clone, Debug, PartialEq)]
pub enum BranchInfo {
    None,
    Constructor {
        scrutinee: Symbol,
        layout: Index<Layout>,
        tag_id: TagIdIntType,
    },
}

/// When positive, this is an index into the symbols array, when negative,
/// the absolute value is an index into the literals array
#[derive(Clone, Debug, PartialEq)]
pub struct SymbolOrLiteral(i32);

#[derive(Clone, Debug)]
pub enum Literal {
    Reserved,
    Int128(Index<u128>), // we could store the sign too here?
    Int(i64),
    Float(f64),
    Decimal(Index<RocDec>),
    Str(Slice<String>),
    Bool(bool),
    Byte(u8),
}

/// A function passed to a higher-order function. e.g. `f` in `List.map f [ ... ]`
#[derive(Clone, Debug)]
pub struct PassedFunction {
    name: Symbol,
    layout: FunctionLayout,
    specialization_id: CallSpecId,

    /// some stuff related to closures; figure out if we really need this
    captured_environment: Symbol,
    owns_captured_environment: bool,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Reserved,

    /// An argument to the current function
    // argument name is implicit
    Argument {
        position: u32,
    },

    Global,

    Let {
        // symbol: Symbol is implicit
        expr: Index<Expr>,
        layout: Index<Layout>,
        // continuation: Index<Stmt>, is implicit
    },

    Switch {
        /// This *must* stand for an integer, because Switch potentially compiles to a jump table.
        // cond_symbol: Symbol is implicit
        cond_layout: Index<Layout>,
        /// The u64 in the tuple will be compared directly to the condition Expr.
        /// If they are equal, this branch will be taken.
        /// final branch in the slice is the default
        branches: Slice<(u64, BranchInfo, Stmt)>,
        /// Each branch must return a value of this type.
        ret_layout: Index<Layout>,
    },
    Ret(Index<Stmt>), // the return symbol is implicit

    /// Increment the refcount of `symbol` by `increment`
    RefcountInc {
        // symbol: Symbol, is implicit
        to_modify: Index<Stmt>,
        increment: u32,
        // continuation: Index<Stmt>, is implicit
    },

    /// Decrement the refcount of `symbol` by 1
    RefcountDec {
        to_modify: Index<Stmt>,
        // symbol: Symbol, is implicit
        // continuation: Index<Stmt>, is implicit
    },
    /// A DecRef is a non-recursive reference count decrement
    /// e.g. If we Dec a list of lists, then if the reference count of the outer list is one,
    /// a Dec will recursively decrement all elements, then free the memory of the outer list.
    /// A DecRef would just free the outer list.
    /// That is dangerous because you may not free the elements, but in our Zig builtins,
    /// sometimes we know we already dealt with the elements (e.g. by copying them all over
    /// to a new list) and so we can just do a DecRef, which is much cheaper in such a case.
    RefcountDecRef {
        to_modify: Index<Stmt>,
        // symbol: Symbol, is implicit
        // continuation: Index<Stmt>, is implicit
    },
    /// a join point `join f <params> = <continuation> in remainder`
    Join {
        // id: JoinPointId, is implicit
        parameters: Slice<Param>,

        /// body: what happens after _jumping to_ the join point
        body: Index<Stmt>,
        // continuation: what happens after _defining_ the join point
        // continuation: Index<Stmt>, is implicit
    },
    Jump(
        // JoinPointId, is implicit
        Slice<SymbolOrLiteral>,
    ),
    RuntimeError(Index<String>),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Reserved,

    Literal(Literal),

    // Functions
    CallByName {
        // name: Symbol, is implicit
        arguments: Slice<SymbolOrLiteral>,
        layouts: Slice<Layout>, // final element of the slice is the return type
        specialization_id: CallSpecId,
    },

    Foreign {
        foreign_symbol: ForeignSymbol,
        arguments: Slice<SymbolOrLiteral>,
        layouts: Slice<Layout>, // final element of the slice is the return type
    },

    LowLevel {
        op: LowLevel,
        arguments: Slice<SymbolOrLiteral>,
        update_mode: UpdateModeId,
    },

    HigherOrder {
        op: Index<crate::low_level::HigherOrder>,
        update_mode: UpdateModeId,
        passed_function: Index<PassedFunction>,
    },

    Struct(Slice<SymbolOrLiteral>),

    StructAtIndex {
        index: u32,
        field_layouts: Slice<Layout>,
        // structure: Symbol, is implicit
    },

    Tag {
        tag_layout: Slice<Slice<Layout>>,
        tag_name: Index<TagName>,
        tag_id: TagIdIntType, // currently u16
        arguments: Slice<SymbolOrLiteral>,
    },

    GetTagId {
        // structure: Symbol, is implicit
        union_layout: Slice<Slice<Layout>>,
    },

    UnionAtIndex {
        // structure: Symbol, is implicit
        tag_id: TagIdIntType,
        union_layout: Slice<Slice<Layout>>,
        index: u32,
    },

    Array {
        element_layout: Index<Layout>,
        elements: Slice<SymbolOrLiteral>,
    },

    Reuse {
        // symbol: Symbol, is implicit
        update_mode: UpdateModeId,
        update_tag_id: bool,
        // normal Tag fields
        tag_layout: Slice<Slice<Layout>>,
        tag_name: Index<TagName>,
        tag_id: TagIdIntType,
        arguments: Slice<SymbolOrLiteral>,
    },
    Reset {
        // symbol: Symbol, is implicit
        update_mode: UpdateModeId,
    },

    RuntimeErrorFunction(Index<String>),
}
