use crate::bucket::{BucketList, BucketStr, NodeId};
use arraystring::{typenum::U14, ArrayString};
use inlinable_string::InlinableString;
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

/// Experimental idea for an Expr that fits in 16B.
/// It has a 1B discriminant and variants which hold payloads of at most 15B.
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
        text: NodeId<BucketStr>, // 3B
    },
    /// A float literal (with a dot) containing underscores
    FloatWithUnderscores {
        number: f64,             // 8B
        var: Variable,           // 4B
        text: NodeId<BucketStr>, // 3B
    },
    /// string literals of length up to 14B
    SmallStr(ArrayString<U14>), // 15B
    /// string literals of length up to 4094B
    MedStr(NodeId<BucketStr>), // 4B
    /// string literals of length over 4094B, but requires calling malloc/free
    BigStr {
        pointer: *const u8, // 8B on 64-bit systems
        len: u32, // 4B, meaning maximum string literal size of 4GB. Could theoretically fit 7B here, which would go up to the full isize::MAX
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
        elems: BucketList<Expr2>, // 4B
    },

    If {
        cond_var: Variable,                   // 4B
        expr_var: Variable,                   // 4B
        branches: BucketList<(Expr2, Expr2)>, // 4B
        final_else: NodeId<Expr2>,            // 3B
    },
    When {
        cond_var: Variable,               // 4B
        expr_var: Variable,               // 4B
        branches: BucketList<WhenBranch>, // 4B
        cond: ExprId,                     // 3B
    },
    LetRec {
        // TODO need to make this Alias type here bucket-friendly, which will be hard!
        aliases: BucketList<(Symbol, Alias)>, // 4B
        defs: BucketList<Def>,                // 4B
        body_var: Variable,                   // 4B
        body: NodeId<Expr2>,                  // 3B
    },
    LetNonRec {
        // TODO need to make this Alias type here bucket-friendly, which will be hard!
        aliases: BucketList<(Symbol, Alias)>, // 4B
        def: NodeId<Def>,                     // 3B
        body: NodeId<Expr2>,                  // 3B
        body_var: Variable,                   // 4B
    },
    Call {
        /// NOTE: the first elem in this list is the expression and its variable.
        /// The others are arguments. This is because we didn't have room for
        /// both the expr and its variable otherwise.
        expr_and_args: BucketList<(Variable, NodeId<Expr2>)>, // 4B
        fn_var: Variable,      // 4B
        closure_var: Variable, // 4B
        called_via: CalledVia, // 1B
        /// Cached outside expr_and_args so we don't have to potentially
        /// traverse that whole linked list chain to count all the args.
        arity: u16, // 2B
    },
    RunLowLevel {
        op: LowLevel,                                // 1B
        args: BucketList<(Variable, NodeId<Expr2>)>, // 4B
        ret_var: Variable,                           // 4B
    },
    Closure {
        /// NOTE: the first elem in this list is the function's name Symbol, plus Variable::NONE
        ///
        /// This is not ideal, but there's no room for an 8-byte Symbol
        /// in a 16B node that already needs to hold this much other data.
        captured_symbols: BucketList<(Symbol, Variable)>, // 4B
        args: BucketList<(Variable, NodeId<Pat2>)>, // 4B
        body: NodeId<Expr2>,                        // 3B
        recursive: Recursive,                       // 1B
        vars: NodeId<ClosureVars>,                  // 3B
    },

    // Product Types
    Record {
        record_var: Variable,                                     // 4B
        fields: BucketList<(BucketStr, Variable, NodeId<Expr2>)>, // 4B
    },

    /// Empty record constant
    EmptyRecord,

    /// Look up exactly one field on a record, e.g. (expr).foo.
    Access {
        field: NodeId<BucketStr>, // 3B
        expr: NodeId<Expr2>,      // 3B
        vars: NodeId<AccessVars>, // 3B
    },

    /// field accessor as a function, e.g. (.foo) expr
    Accessor {
        record_vars: NodeId<RecordVars>, // 3B
        function_var: Variable,          // 4B
        closure_var: Variable,           // 4B
        field: NodeId<BucketStr>,        // 3B
    },
    Update {
        symbol: Symbol,                          // 8B
        updates: BucketList<(Lowercase, Field)>, // 4B
        vars: NodeId<UpdateVars>,                // 3B
    },

    // Sum Types
    Tag {
        // NOTE: A BucketStr node is a 2B length and then 14B bytes,
        // plus more bytes in adjacent nodes if necessary. Thus we have
        // a hard cap of 4094 bytes as the maximum length of tags and fields.
        name: NodeId<BucketStr>,                          // 3B
        variant_var: Variable,                            // 4B
        ext_var: Variable,                                // 4B
        arguments: BucketList<(Variable, NodeId<Expr2>)>, // 4B
    },

    // Compiles, but will crash if reached
    RuntimeError(RuntimeError),
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

/// This is 16B, so it fits in a Node slot.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ClosureVars {
    function_type: Variable,
    closure_type: Variable,
    closure_ext_var: Variable,
    return_type: Variable,
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

// Each bucket has 256 slots. Each slot holds one 16B node
// This means each bucket is 4096B, which is the size of a memory page
// on typical systems where the compiler will be run.
//
// Because each bucket has 256 slots, and arrays of nodes must fit inside
// a single bucket, this implies that nodes which contain arrays of nodes
// (e.g. If, When, Record, Tag, Call, Closure) can only contain at most
// 255 nodes. So functions can have at most 255 arguments, records can have
// at most 255 fields, etc.
//
// Nice things about this system include:
// * Allocating a new bucket is as simple as asking the OS for a memory page.
// * Since each node is 16B, each node's memory address will be a multiple of 16.
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
type ExprBucketSlots = [Expr2; 256];

#[test]
fn size_of_expr_bucket() {
    assert_eq!(std::mem::size_of::<ExprBucketSlots>(), 4096);
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
    assert_eq!(std::mem::size_of::<Expr2>(), 16);
}
