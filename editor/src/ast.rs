use arraystring::{typenum::U14, ArrayString};
use inlinable_string::string_ext::StringExt;
use inlinable_string::InlinableString;
use roc_types::subs::Variable;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Problem {
    RanOutOfNodeIds,
}

pub type Res<T> = Result<T, Problem>;

/// The index into a decl's array of nodes.
/// This is a u32 index because no decl is allowed to hold more than 2^32 entries,
/// and it saves space on 64-bit systems compared to a pointer.
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeId(u32);

impl NodeId {
    pub const NONE: NodeId = NodeId(std::u32::MAX);

    pub fn as_index(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Debug for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self == &NodeId::NONE {
            write!(f, "none")
        } else {
            write!(f, "#{}", self.0)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UndoAction {
    UndoEdit {
        caret_id: NodeId,
        caret_parent_id: NodeId,
        caret_child_id: NodeId,
        caret_offset: u16,
        child: Node,
        redo_action: Action,
    },
    /// Used in undo logs to mean "the next N undo actions in the log should
    /// be replayed together in a batch."
    Multiple(u16),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    Backspace,
    Paste,

    /// Used in redo logs to mean "the next N redo actions in the log should
    /// be replayed together in a batch."
    Multiple(u16),
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Nodes {
    nodes: Vec<Node>,
}

impl Nodes {
    pub fn push_expr(&mut self, parent_id: NodeId, expr: Expr) -> Res<NodeId> {
        let node_id = self.push_node(Node {
            parent_id,
            content: NodeContent::Expr(expr),
        })?;

        self.set_child(parent_id, node_id);

        Ok(node_id)
    }

    fn set_child(&mut self, parent_id: NodeId, _child_id: NodeId) {
        match self.nodes.get_mut(parent_id.as_index()) {
            Some(parent) => match &mut parent.content {
                NodeContent::Expr(Expr::Int { .. }) | NodeContent::Expr(Expr::Float { .. }) => {
                    // This node has no children. No further action needed!
                }
                NodeContent::Caret { .. } => {
                    // This node has no children. No further action needed!
                }
                other => {
                    todo!("handle set_child for {:?}", other);
                }
            },
            None => {
                // The only reason this bounds check should ever fail
                // is that we were passed no parent.
                debug_assert!(parent_id == NodeId::NONE);
            }
        }
    }

    // fn remove_node(&mut self, target_id: NodeId) {
    //     debug_assert!(
    //         target_id != NodeId::NONE,
    //         "Called remove_node on NodeId::NONE"
    //     );

    //     {
    //         let target = self
    //             .nodes
    //             .get_mut(target_id.as_index())
    //             .unwrap_or_else(|| panic!("Tried to remove nonexistant node {:?}", target_id));
    //         let opt_parent = self.nodes.get_mut(target.parent_id.as_index());

    //         match &mut target.content {
    //             NodeContent::Expr(Expr::Int { .. }) => {
    //                 // This node has no children, so remove it from its parent and move on.
    //                 match opt_parent {
    //                     Some(parent) => parent.remove_child(target_id),
    //                     None => {
    //                         // The only valid reason the node's parent_id might
    //                         // not have been in the nodes vec is that it's NONE.
    //                         debug_assert!(target.parent_id == NodeId::NONE);
    //                     }
    //                 }
    //             }

    //             NodeContent::Caret { child_id, offset } => {
    //                 match opt_parent {
    //                     Some(parent) => {
    //                         // Go into the parent and replace this caret with
    //                         // the new child_id
    //                         // parent.replace_child(target_id, child_id)
    //                     }
    //                     None => {
    //                         // The only valid reason the node's parent_id might
    //                         // not have been in the nodes vec is that it's NONE.
    //                         debug_assert!(target.parent_id == NodeId::NONE);
    //                     }
    //                 }
    //             }

    //             NodeContent::Removed => {
    //                 panic!("Tried to remove a node that was already removed.");
    //             }
    //         }
    //     }
    // }

    fn push_node(&mut self, node: Node) -> Res<NodeId> {
        // TODO check to see if we have any removed nodes, and if so, use those
        // instead of pushing to the end. This way, we can avoid needing to defrag.

        // The length *before* we pushed == the index of the node we pushed.
        let index = self.nodes.len();

        self.nodes.push(node);

        if index < std::u32::MAX as usize {
            Ok(NodeId(index as u32))
        } else {
            // u32::MAX is reserved for NodeId::NONE, so if we hit that on a
            // saturating add, we've overflowed. Game over.
            Err(Problem::RanOutOfNodeIds)
        }
    }

    pub fn replace(&mut self, node_id: NodeId, node: Node) {
        let elem = self.nodes.get_mut(node_id.as_index()).unwrap_or_else(|| {
            panic!(
                "Tried to replace element at nonexistant NodeId {:?} with {:?}",
                node_id, node
            )
        });

        *elem = node;
    }

    pub fn wrap_with_caret(&mut self, target_id: NodeId, offset: u16) -> Res<NodeId> {
        let parent_id = self.get(target_id).parent_id;
        let caret_id = {
            let content = NodeContent::Caret {
                child_id: target_id,
                offset,
            };

            self.push_node(Node { content, parent_id })?
        };

        // Now that we have the caret_id, update the old
        if parent_id != NodeId::NONE {
            self.get_mut(target_id).replace_child(target_id, caret_id);
        }

        Ok(caret_id)
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub fn split_at_mut(&mut self, index: NodeId) -> (&mut [Node], &mut [Node]) {
        self.nodes.split_at_mut(index.0 as usize)
    }

    pub fn get(&self, index: NodeId) -> &Node {
        self.nodes
            .get(index.0 as usize)
            .unwrap_or_else(|| panic!("Unable to find node at index {:?}", index.0))
    }

    pub fn get_mut(&mut self, index: NodeId) -> &mut Node {
        self.nodes
            .get_mut(index.0 as usize)
            .unwrap_or_else(|| panic!("Unable to find node at index {:?}", index.0))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    parent_id: NodeId,
    content: NodeContent,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeContent {
    Expr(Expr /* TODO should Node have a Variable? */),
    // Pattern(Pattern),
    Caret { child_id: NodeId, offset: u16 },
    // SelectionStart { offset: u16, child: NodeId },
    // SelectionEnd { offset: u16, child: NodeId },
    Removed,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IntStyle {
    Decimal,
    Octal,
    Hex,
    Binary,
}

/// Experimental idea for an Expr that fits in 16B.
/// It has a 1B discriminant and variants which hold payloads of at most 15B.
#[derive(Debug, Clone, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
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
enum Pat2 {
    Todo,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct UpdateVars {
    record_var: Variable, // 4B
    ext_var: Variable,    // 4B
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct RecordVars {
    record_var: Variable, // 4B
    ext_var: Variable,    // 4B
    field_var: Variable,  // 4B
}

/// This is 15B, so it fits in a Node slot.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct AccessVars {
    record_var: Variable, // 4B
    ext_var: Variable,    // 4B
    field_var: Variable,  // 4B
}

/// This is 16B, so it fits in a Node slot.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct ClosureVars {
    function_type: Variable,
    closure_type: Variable,
    closure_ext_var: Variable,
    return_type: Variable,
}

#[derive(Clone, Debug, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// An integer literal (without a dot)
    Int {
        text: InlinableString,
        var: Variable,
    },
    /// An floating-point literal (with a dot)
    Float {
        text: InlinableString,
        var: Variable,
    },
    // /// A partial lookup that has not yet been completed, e.g.
    // /// `Foo.` or `pkg.Foo.Bar`
    // PartialLookup {
    //     /// dot-separated sections, e.g. `Foo.Bar.` would be ["Foo", "Bar", ""]
    //     sections: Vec<InlinableString>,
    //     var: Variable,
    // },
    // Lookup {
    //     name: InlinableString,
    //     var: Variable,
    // },
    // If {
    //     conditional: NodeId,
    //     then_node: NodeId,
    //     else_node: NodeId,
    //     var: Variable,
    // },
    // Then {
    //     child: NodeId,
    //     var: Variable,
    // },
    // Else {
    //     child: NodeId,
    //     var: Variable,
    // },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Identifier { text: String, var: Variable },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Decl {
    Def(Def),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Def {
    Body { pattern: NodeId, expr: NodeId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub nodes: Nodes,
    pub decls: Vec<Decl>,
    // Use a Vec over a Set because it'll always be of small length
    pub carets: Vec<NodeId>,
    pub selections: Vec<(NodeId, NodeId)>,

    /// Because these actions store NodeId values, it's critically important
    /// that when we take an action and then redo it, everything (including Nodes)
    /// ends up back in the same state, including NodeId values.
    pub undo_log: Vec<UndoAction>,

    pub redos: Vec<Action>,
}

impl Node {
    /// Returns the number of characters removed, so that
    /// the caret can be moved back this many indices.
    /// (For example, if you backspace over an emoji, that can move the
    /// caret back multiple indices.)
    pub fn backspace(&mut self, index: u16) -> u16 {
        use Expr::*;

        match &mut self.content {
            NodeContent::Expr(Int { text, .. }) | NodeContent::Expr(Float { text, .. }) => {
                // TODO remove an entire *grapheme cluster* here, not just a
                // single character! This will require using a 3rd-party crate.
                let removed = text.remove(index as usize);

                // TODO need to re-index any other carets/selections which
                // were also in this node - they need to be moved too!
                removed.len_utf8() as u16
            }
            other => {
                todo!("handle backspace for {:?}", other);
            }
        }
    }

    // fn remove_child(&mut self, id_to_remove: NodeId) {
    //     match &mut self.content {
    //         NodeContent::Expr(Expr::Int { .. }) | NodeContent::Expr(Expr::Float { .. }) => {
    //             // This node has no children, so no action is needed.
    //         }

    //         NodeContent::Caret { child_id, .. } => {
    //             if *child_id == id_to_remove {
    //                 *child_id = NodeId::NONE;
    //             }
    //         }

    //         NodeContent::Removed => {
    //             panic!(
    //                 "Tried to remove a child node ({:?}) from a NodeContent::Removed",
    //                 id_to_remove
    //             );
    //         }
    //     }
    // }

    fn replace_child(&mut self, old_id: NodeId, new_id: NodeId) {
        match &mut self.content {
            NodeContent::Expr(Expr::Int { .. }) | NodeContent::Expr(Expr::Float { .. }) => {
                // This node has no children, so no action is needed.
            }

            NodeContent::Caret { child_id, .. } => {
                if *child_id == old_id {
                    *child_id = new_id;
                }
            }

            NodeContent::Removed => {
                panic!(
                    "Tried to replace child node ID {:?} with {:?} in a NodeContent::Removed",
                    old_id, new_id
                );
            }
        }
    }
}

impl Module {
    pub fn undo(&mut self) {
        let mut iterations_remaining: u16 = 0;

        loop {
            match self.undo_log.pop() {
                Some(action) => {
                    iterations_remaining += self.apply_undo_action(action);
                }
                None => {
                    if iterations_remaining > 0 {
                        panic!("Expected to be able to do {} more Undo iterations, but ran out of actions to undo.", iterations_remaining);
                    }
                }
            }

            if iterations_remaining == 0 {
                return;
            } else {
                iterations_remaining -= 1;
            }
        }
    }

    pub fn redo(&mut self) {
        let mut iterations_remaining: u16 = 0;

        loop {
            match self.redos.pop() {
                Some(action) => {
                    iterations_remaining += self.apply_action(action);
                }
                None => {
                    if iterations_remaining > 0 {
                        panic!("Expected to be able to do {} more Redo iterations, but ran out of actions to redo.", iterations_remaining);
                    }
                }
            }

            if iterations_remaining == 0 {
                return;
            } else {
                iterations_remaining -= 1;
            }
        }
    }

    fn apply_action(&mut self, action: Action) -> u16 {
        use Action::*;

        match action {
            Backspace => {
                self.backspace();

                0
            }
            Paste => {
                todo!("TODO support Paste action");
            }
            Multiple(iterations) => iterations,
        }
    }

    fn apply_undo_action(&mut self, action: UndoAction) -> u16 {
        use UndoAction::*;

        match action {
            UndoEdit {
                caret_id,
                caret_parent_id,
                caret_child_id,
                caret_offset,
                child,
                redo_action,
            } => {
                self.redos.push(redo_action);
                self.nodes.replace(caret_child_id, child);

                let caret = Node {
                    parent_id: caret_parent_id,
                    content: NodeContent::Caret {
                        child_id: caret_child_id,
                        offset: caret_offset,
                    },
                };

                self.nodes.replace(caret_id, caret);

                0
            }
            Multiple(iterations) => iterations,
        }
    }

    pub fn paste(&mut self, text: &str) {
        todo!(
            "TODO paste this string, taking carets and selections into account: {:?}",
            text
        );
    }

    pub fn backspace(&mut self) {
        for &caret_node_id in self.carets.iter() {
            debug_assert!(caret_node_id.as_index() <= self.nodes.len());

            // Use slices around the caret because we'll need to modify the
            // child node. Without these slices we'd need multiple simultaneous
            // mutable references to self.nodes, which is never allowed.
            let (before_caret, caret_and_after) = self.nodes.split_at_mut(caret_node_id);
            let (caret_only, after_caret) = caret_and_after.split_at_mut(1);

            let caret = caret_only.first_mut().unwrap();
            let parent_id = caret.parent_id;

            match &mut caret.content {
                NodeContent::Caret { offset, child_id } => {
                    let child_id = *child_id;
                    let offset_index = *offset;

                    if offset_index != 0 {
                        // Get the child node from the appropriate slice
                        let child_node: &mut Node = {
                            debug_assert!(
                                child_id != caret_node_id,
                                "A caret had itself as its own child: {:?}",
                                caret_node_id
                            );

                            if child_id > caret_node_id {
                                after_caret.get_mut(child_id.as_index() - (before_caret.len() + 1))
                            } else {
                                before_caret.get_mut(child_id.as_index())
                            }
                        }
                        .unwrap_or_else(|| {
                            panic!("Could not get child node for caret {:?}", caret_node_id)
                        });

                        // Add an entry to the undo log to undo this edit.
                        self.undo_log.push(UndoAction::UndoEdit {
                            caret_id: caret_node_id,
                            caret_parent_id: parent_id,
                            caret_offset: offset_index,
                            caret_child_id: child_id,
                            child: child_node.clone(),
                            redo_action: Action::Backspace,
                        });

                        // Mutate the child node to apply the backspace operation.
                        //
                        // -1 because index 0 is *before* the first character
                        // in the string (which we already ruled out using the
                        // above conditional), and child_node.backspace expects
                        // to be given the index *after* the char to be removed.
                        let chars_removed = child_node.backspace(offset_index - 1);

                        // Mutate the caret to decrement its offset
                        *offset = offset_index - chars_removed;
                    } else {
                        todo!(
                            "Backspace when caret offset is 0, into parent: {:?}",
                            parent_id
                        );
                    }
                }
                other => {
                    unreachable!("Caret pointed to a non-caret node: {:?}", other);
                }
            }
        }
    }
}

#[test]
fn single_backspace_1_caret() {
    use roc_types::subs::VarStore;

    let mut var_store = VarStore::default();
    let int_var = var_store.fresh();
    let int_node_id;
    let caret_node_id;
    let expected = {
        let mut nodes = Nodes::default();

        int_node_id = nodes
            .push_expr(
                NodeId::NONE,
                Expr::Int {
                    text: "abd".into(),
                    var: int_var,
                },
            )
            .unwrap();

        caret_node_id = nodes.wrap_with_caret(int_node_id, 2).unwrap();

        nodes
    };

    let actual = {
        let mut nodes = Nodes::default();

        let actual_node_id = nodes
            .push_expr(
                NodeId::NONE,
                Expr::Int {
                    text: "abcd".into(),
                    var: int_var,
                },
            )
            .unwrap();

        assert!(int_node_id == actual_node_id);

        let actual_node_id = nodes.wrap_with_caret(int_node_id, 3).unwrap();

        assert!(caret_node_id == actual_node_id);

        nodes
    };

    let mut module = Module {
        nodes: actual,
        decls: Vec::new(),
        carets: vec![caret_node_id],
        selections: Vec::new(),
        undo_log: Vec::new(),
        redos: Vec::new(),
    };

    let original_module = module.clone();

    module.backspace();

    let altered_module = module.clone();

    assert_eq!(expected, module.nodes);

    module.undo();

    let stashed_redos = module.redos;

    module.redos = Vec::new();

    assert_eq!(original_module, module);

    module.redos = stashed_redos;

    module.redo();

    assert_eq!(altered_module, module);
}

#[test]
fn double_backspace_1_caret() {
    use roc_types::subs::VarStore;

    let mut var_store = VarStore::default();
    let int_var = var_store.fresh();
    let int_node_id;
    let caret_node_id;
    let expected = {
        let mut nodes = Nodes::default();

        int_node_id = nodes
            .push_expr(
                NodeId::NONE,
                Expr::Int {
                    text: "ad".into(),
                    var: int_var,
                },
            )
            .unwrap();

        caret_node_id = nodes.wrap_with_caret(int_node_id, 1).unwrap();

        nodes
    };

    let actual = {
        let mut nodes = Nodes::default();
        let actual_node_id = nodes
            .push_expr(
                NodeId::NONE,
                Expr::Int {
                    text: "abcd".into(),
                    var: int_var,
                },
            )
            .unwrap();

        assert!(int_node_id == actual_node_id);
        assert!(caret_node_id == nodes.wrap_with_caret(int_node_id, 3).unwrap());

        nodes
    };

    let mut module = Module {
        nodes: actual,
        decls: Vec::new(),
        carets: vec![caret_node_id],
        selections: Vec::new(),
        undo_log: Vec::new(),
        redos: Vec::new(),
    };

    let original_module = module.clone();

    module.backspace();
    module.backspace();

    let altered_module = module.clone();

    assert_eq!(expected, module.nodes);

    module.undo();
    module.undo();

    let stashed_redos = module.redos;

    module.redos = Vec::new();

    assert_eq!(original_module, module);

    module.redos = stashed_redos;

    module.redo();
    module.redo();

    assert_eq!(altered_module, module);
}
