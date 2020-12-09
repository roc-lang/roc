use crate::bucket::{BucketList, BucketStr, NodeId};
use arraystring::{typenum::U30, ArrayString};
use roc_can::def::Annotation;
use roc_can::expr::{Field, Recursive};
use roc_module::ident::Lowercase;
use roc_module::low_level::LowLevel;
use roc_module::operator::CalledVia;
use roc_module::symbol::Symbol;
use roc_problem::can::RuntimeError;
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
        number: i64,             // 8B
        var: Variable,           // 4B
        text: NodeId<BucketStr>, // 8B
    },
    /// A float literal (with a dot) containing underscores
    FloatWithUnderscores {
        number: f64,             // 8B
        var: Variable,           // 4B
        text: NodeId<BucketStr>, // 8B
    },
    /// string literals of length up to 30B
    SmallStr(ArrayString<U30>), // 31B
    /// string literals of length up to 4094B
    MedStr {
        str: NodeId<BucketStr>, // 8B
    },
    /// string literals of length over 4094B, but requires calling malloc/free
    BigStr {
        pointer: *const u8, // 8B
        len: u32, // 4B, meaning maximum string literal size of 4GB. Could theoretically fit 7B here, which would get closer to the full isize::MAX
    },
    // Lookups
    Var(Symbol), // 8B

    /// Separate from List because BuckeList must be non-empty, and in this case
    /// the list literal has no elements
    EmptyList {
        list_var: Variable, // 4B - required for uniqueness of the list
        elem_var: Variable, // 4B
    },
    List {
        list_var: Variable,       // 4B - required for uniqueness of the list
        elem_var: Variable,       // 4B
        elems: BucketList<Expr2>, // 9B
    },
    If {
        cond_var: Variable,                   // 4B
        expr_var: Variable,                   // 4B
        branches: BucketList<(Expr2, Expr2)>, // 9B
        final_else: NodeId<Expr2>,            // 8B
    },
    When {
        cond_var: Variable,               // 4B
        expr_var: Variable,               // 4B
        branches: BucketList<WhenBranch>, // 9B
        cond: NodeId<Expr2>,              // 8B
    },
    LetRec {
        // TODO need to make this Alias type here bucket-friendly, which will be hard!
        aliases: BucketList<(Symbol, Alias)>, // 9B
        defs: BucketList<Def>,                // 9B
        body_var: Variable,                   // 4B
        body_id: NodeId<Expr2>,               // 8B
    },
    LetNonRec {
        // TODO need to make this Alias type here bucket-friendly, which will be hard!
        aliases: BucketList<(Symbol, Alias)>, // 9B
        def_id: NodeId<Def>,                  // 8B
        body_id: NodeId<Expr2>,               // 8B
        body_var: Variable,                   // 4B
    },
    Call {
        /// NOTE: the first elem in this list is the expression and its variable.
        /// The others are arguments. This is because we didn't have room for
        /// both the expr and its variable otherwise.
        expr_and_args: BucketList<(Variable, NodeId<Expr2>)>, // 9B
        fn_var: Variable,      // 4B
        closure_var: Variable, // 4B
        /// Cached outside expr_and_args so we don't have to potentially
        /// traverse that whole linked list chain to count all the args.
        arity: usize, // 8B - could make this smaller if need be
        called_via: CalledVia, // 2B
    },
    RunLowLevel {
        op: LowLevel,                                // 1B
        args: BucketList<(Variable, NodeId<Expr2>)>, // 9B
        ret_var: Variable,                           // 4B
    },
    Closure {
        captured_symbols: BucketList<(Symbol, Variable)>, // 9B
        args: BucketList<(Variable, NodeId<Pat2>)>,       // 9B
        recursive: Recursive,                             // 1B
        extra: NodeId<ClosureExtra>,                      // 8B
    },
    // Product Types
    Record {
        record_var: Variable,                                     // 4B
        fields: BucketList<(BucketStr, Variable, NodeId<Expr2>)>, // 9B
    },
    /// Empty record constant
    EmptyRecord,
    /// Look up exactly one field on a record, e.g. (expr).foo.
    Access {
        field: NodeId<BucketStr>, // 8B
        expr: NodeId<Expr2>,      // 8B
        vars: NodeId<AccessVars>, // 8B
    },

    /// field accessor as a function, e.g. (.foo) expr
    Accessor {
        record_vars_id: NodeId<RecordVars>, // 8B
        function_var: Variable,             // 4B
        closure_var: Variable,              // 4B
        field_id: NodeId<BucketStr>,        // 8B
    },
    Update {
        symbol: Symbol,                          // 8B
        updates: BucketList<(Lowercase, Field)>, // 9B
        vars_id: NodeId<UpdateVars>,             // 8B
    },

    // Sum Types
    Tag {
        // NOTE: A BucketStr node is a 2B length and then 14B bytes,
        // plus more bytes in adjacent nodes if necessary. Thus we have
        // a hard cap of 4094 bytes as the maximum length of tags and fields.
        name_id: NodeId<BucketStr>,                       // 8B
        variant_var: Variable,                            // 4B
        ext_var: Variable,                                // 4B
        arguments: BucketList<(Variable, NodeId<Expr2>)>, // 9B
    },

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
    pub expr_var: Variable,                           // 4B
    pub pattern_vars: BucketList<(Symbol, Variable)>, // 4B
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
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
    pub patterns: BucketList<Pat2>,   // 4B
    pub body: NodeId<Expr2>,          // 3B
    pub guard: Option<NodeId<Expr2>>, // 4B
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PatternId {
    /// TODO: PatternBucketId
    bucket_id: ExprBucketId,
    /// TODO: PatternBucketSlot
    slot: ExprBucketSlot,
}

// Each bucket has metadata and slots.
// The metadata determines things like which slots are free.
#[derive(Debug)]
pub struct ExprBucket {
    // We can store this as a u8 because whenever we create a bucket, we
    // always fill at least one slot. So there will never be 256 unused slots
    // remaining; the most there will ever be will be 255.
    //
    // Note that there can be "holes" in this as we remove nodes; those
    // are recorded in the containing struct, not here.
    //
    // Also note that we can derive from this the next unused slot.
    unused_slots_remaining: u8,
    slots: Box<ExprBucketSlots>,
}

pub struct Exprs {
    // Whenever we free a slot of a particular size, we make a note of it
    // here, so we can reuse it later. This can lead to poor data locality
    // over time, but the alternative is memory fragmentation and ever-growing
    // memory usage. We could in theory go up to free_128node_slots, but in
    // practice it seems unlikely that it would be worth the bookkeeping
    // effort to go that high.
    //
    // TODO: this could be refactored Into `free_slots: [5; Vec<ExprId>]`
    // where (2 ^ index) is the size node in that slot. It's less
    // self-documenting but might allow for better code reuse.
    pub free_1node_slots: Vec<ExprId>,
    pub free_2node_slots: Vec<ExprId>,
    pub free_4node_slots: Vec<ExprId>,
    pub free_8node_slots: Vec<ExprId>,
    pub free_16node_slots: Vec<ExprId>,
    // Note that empty_buckets is equivalent to free_256node_slots - it means
    // the entire bucket is empty, at which point we can fill it with
    // whatever we please.
    pub empty_buckets: Vec<ExprBucketId>,
    pub buckets: Vec<ExprBucket>,
}

// Each bucket has 128 slots. Each slot holds one 32B node
// This means each bucket is 4096B, which is the size of a memory page
// on typical systems where the compiler will be run.
//
// Nice things about this system include:
// * Allocating a new bucket is as simple as asking the OS for a memory page.
// * Since each node is 32B, each node's memory address will be a multiple of 16.
// * Thanks to the free lists and our consistent chunk sizes, we should
//   end up with very little fragmentation.
// * Finding a slot for a given node should be very fast: see if the relevant
//   free list has any openings; if not, try the next size up.
//
// Less nice things include:
// * This system makes it very hard to ever give a page back to the OS.
//   We could try doing the Mesh Allocator strategy: whenever we allocate
//   something, assign it to a random slot in the bucket, and then periodically
//   try to merge two pages into one (by locking and remapping them in the OS)
//   and then returning the redundant physical page back to the OS. This should
//   work in theory, but is pretty complicated, and we'd need to schedule it.
//   Keep in mind that we can't use the Mesh Allocator itself because it returns
//   usize pointers, which would be too big for us to have 16B nodes.
//   On the plus side, we could be okay with higher memory usage early on,
//   and then later use the Mesh strategy to reduce long-running memory usage.
type ExprBucketSlots = [Expr2; 128];

#[test]
fn size_of_expr_bucket() {
    assert_eq!(
        std::mem::size_of::<ExprBucketSlots>(),
        crate::bucket::BUCKET_BYTES
    );
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PatId {
    bucket_id: ExprBucketId, // TODO PatBucketId
    slot: ExprBucketSlot,    // TODO PatBucketSlot
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExprId {
    bucket_id: ExprBucketId,
    slot: ExprBucketSlot,
}

// We have a maximum of 65K buckets.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExprBucketId(u16);

/// Each of these is the index of one 16B node inside a bucket's 4096B
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExprBucketSlot(u8);

#[test]
fn size_of_expr() {
    assert_eq!(std::mem::size_of::<Expr2>(), 32);
}

#[test]
fn size_of_called_via() {
    assert_eq!(std::mem::size_of::<CalledVia2>(), 1);
}
