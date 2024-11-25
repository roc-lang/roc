use crate::{
    foreign_symbol::ForeignSymbolId, mono_module::InternedStrId, mono_num::Number,
    mono_struct::MonoFieldId, mono_type::MonoTypeId, specialize_type::Problem,
};
use roc_can::expr::Recursive;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_region::all::Region;
use soa::{Index, NonEmptySlice, Slice, Slice2, Slice3};
use std::iter;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MonoPatternId {
    inner: u32,
}

pub type IdentId = Symbol; // TODO make this an Index into an array local to this module

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Def {
    pub pattern: MonoPatternId,
    /// Named variables in the pattern, e.g. `a` in `Ok a ->`
    pub pattern_vars: Slice2<IdentId, MonoTypeId>,
    pub expr: MonoExprId,
    pub expr_type: MonoTypeId,
}

#[derive(Debug, Default)]
pub struct MonoExprs {
    // TODO convert to Vec2
    exprs: Vec<MonoExpr>,
    regions: Vec<Region>,
}

impl MonoExprs {
    pub fn new() -> Self {
        Self {
            exprs: Vec::new(),
            regions: Vec::new(),
        }
    }

    pub fn add(&mut self, expr: MonoExpr, region: Region) -> MonoExprId {
        let index = self.exprs.len() as u32;
        self.exprs.push(expr);
        self.regions.push(region);

        MonoExprId {
            inner: Index::new(index),
        }
    }

    pub fn get_expr(&self, id: MonoExprId) -> &MonoExpr {
        debug_assert!(
            self.exprs.get(id.inner.index()).is_some(),
            "A MonoExprId was not found in MonoExprs. This should never happen!"
        );

        // Safety: we should only ever hand out MonoExprIds that are valid indices into here.
        unsafe { self.exprs.get_unchecked(id.inner.index()) }
    }

    pub fn get_region(&self, id: MonoExprId) -> Region {
        debug_assert!(
            self.regions.get(id.inner.index()).is_some(),
            "A MonoExprId was not found in MonoExprs. This should never happen!"
        );

        // Safety: we should only ever hand out MonoExprIds that are valid indices into here.
        unsafe { *self.regions.get_unchecked(id.inner.index()) }
    }

    pub fn reserve_id(&mut self) -> MonoExprId {
        let answer = MonoExprId {
            inner: Index::new(self.exprs.len() as u32),
        };

        // These should all be overwritten; if they aren't, that's a problem!
        self.exprs
            .push(MonoExpr::CompilerBug(Problem::UninitializedReservedExpr));
        self.regions.push(Region::zero());

        answer
    }

    pub fn reserve_ids(&mut self, len: u16) -> Slice<MonoExprId> {
        let answer = Slice::new(self.exprs.len() as u32, len);

        // These should all be overwritten; if they aren't, that's a problem!
        self.exprs.extend(
            iter::repeat(MonoExpr::CompilerBug(Problem::UninitializedReservedExpr))
                .take(len as usize),
        );
        self.regions
            .extend(iter::repeat(Region::zero()).take(len as usize));

        answer
    }

    pub(crate) fn insert(&mut self, id: MonoExprId, mono_expr: MonoExpr, region: Region) {
        debug_assert!(
            self.exprs.get(id.inner.index()).is_some(),
            "A MonoExprId was not found in MonoExprs. This should never happen!"
        );

        debug_assert!(
            self.regions.get(id.inner.index()).is_some(),
            "A MonoExprId was not found in MonoExprs. This should never happen!"
        );

        let index = id.inner.index();

        // Safety: we should only ever hand out MonoExprIds that are valid indices into here.
        unsafe {
            *self.exprs.get_unchecked_mut(index) = mono_expr;
            *self.regions.get_unchecked_mut(index) = region;
        }
    }

    pub fn iter_slice(&self, exprs: Slice<MonoExpr>) -> impl Iterator<Item = &MonoExpr> {
        exprs.indices().map(|index| {
            debug_assert!(
                self.exprs.get(index).is_some(),
                "A Slice index was not found in MonoExprs. This should never happen!"
            );

            // Safety: we should only ever hand out MonoExprId slices that are valid indices into here.
            unsafe { self.exprs.get_unchecked(index) }
        })
    }

    pub fn extend(
        &mut self,
        exprs: impl Iterator<Item = (MonoExpr, Region)> + Clone,
    ) -> Slice<MonoExpr> {
        let start = self.exprs.len();

        self.exprs.extend(exprs.clone().map(|(expr, _region)| expr));
        self.regions.extend(exprs.map(|(_expr, region)| region));

        let len = self.exprs.len() - start;

        Slice::new(start as u32, len as u16)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MonoExprId {
    inner: Index<MonoExpr>,
}

impl MonoExprId {
    pub(crate) unsafe fn new_unchecked(inner: Index<MonoExpr>) -> Self {
        Self { inner }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MonoStmtId {
    inner: Index<MonoStmt>,
}

impl MonoStmtId {
    pub(crate) unsafe fn new_unchecked(inner: Index<MonoStmt>) -> Self {
        Self { inner }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MonoStmt {
    /// Assign to a variable.
    Assign(IdentId, MonoExprId),
    AssignRec(IdentId, MonoExprId),

    /// Introduce a variable, e.g. `var foo_` (we'll MonoStmt::Assign to it later.)
    Declare(IdentId),

    /// The `return` statement
    Return(MonoExprId),

    /// The "crash" keyword. Importantly, during code gen we must mark this as "nothing happens after this"
    Crash {
        msg: MonoExprId,
        /// The type of the `crash` expression (which will have unified to whatever's around it)
        expr_type: MonoTypeId,
    },

    Expect {
        condition: MonoExprId,
        /// If the expectation fails, we print the values of all the named variables
        /// in the final expr. These are those values.
        lookups_in_cond: Slice2<MonoTypeId, IdentId>,
    },

    Dbg {
        source_location: InternedStrId,
        source: InternedStrId,
        expr: MonoExprId,
        expr_type: MonoTypeId,
        name: IdentId,
    },

    // Call a function that has no return value (or which we are discarding due to an underscore pattern).
    CallVoid {
        fn_type: MonoTypeId,
        fn_expr: MonoExprId,
        args: Slice2<MonoTypeId, MonoExprId>,
        /// This is the type of the closure based only on canonical IR info,
        /// not considering what other closures might later influence it.
        /// Lambda set specialization may change this type later!
        capture_type: MonoTypeId,
    },

    // Branching
    When {
        /// The actual condition of the when expression.
        cond: MonoExprId,
        cond_type: MonoTypeId,
        /// Type of each branch (and therefore the type of the entire `when` expression)
        branch_type: MonoTypeId,
        /// Note: if the branches weren't exhaustive, we will have already generated a default
        /// branch which crashes if it's reached. (The compiler will have reported an error already;
        /// this is for if you want to run anyway.)
        branches: NonEmptySlice<WhenBranch>,
    },
    If {
        /// Type of each branch (and therefore the type of the entire `if` expression)
        branch_type: MonoTypeId,
        branches: Slice<(MonoStmtId, MonoStmtId)>,
        final_else: Option<MonoTypeId>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MonoExpr {
    Str(InternedStrId),
    Number(Number),
    List {
        elem_type: MonoTypeId,
        elems: Slice<MonoExprId>,
    },
    Lookup(IdentId, MonoTypeId),

    /// Like Lookup, but from a module with params
    ParameterizedLookup {
        name: IdentId,
        lookup_type: MonoTypeId,
        params_name: IdentId,
        params_type: MonoTypeId,
    },

    /// This is *only* for calling functions, not for tag application.
    /// The Tag variant contains any applied values inside it.
    Call {
        fn_type: MonoTypeId,
        fn_expr: MonoExprId,
        args: Slice2<MonoTypeId, MonoExprId>,
        /// This is the type of the closure based only on canonical IR info,
        /// not considering what other closures might later influence it.
        /// Lambda set specialization may change this type later!
        capture_type: MonoTypeId,
    },
    RunLowLevel {
        op: LowLevel,
        args: Slice<(MonoTypeId, MonoExprId)>,
        ret_type: MonoTypeId,
    },
    ForeignCall {
        foreign_symbol: ForeignSymbolId,
        args: Slice<(MonoTypeId, MonoExprId)>,
        ret_type: MonoTypeId,
    },

    Lambda {
        fn_type: MonoTypeId,
        arguments: Slice<(MonoTypeId, MonoPatternId)>,
        body: MonoExprId,
        captured_symbols: Slice<(IdentId, MonoTypeId)>,
        recursive: Recursive,
    },

    /// A record literal or a tuple literal.
    /// These have already been sorted alphabetically.
    Struct(NonEmptySlice<MonoExpr>),

    /// Look up exactly one field on a record, tuple, or tag payload.
    /// At this point we've already unified those concepts and have
    /// converted (for example) record field names to indices, and have
    /// also dropped all fields that have no runtime representation (e.g. empty records).
    ///
    /// In a later compilation phase, these indices will be re-sorted
    /// by alignment and converted to byte offsets, but we in this
    /// phase we aren't concerned with alignment or sizes, just indices.
    StructAccess {
        record_expr: MonoExprId,
        record_type: MonoTypeId,
        field_type: MonoTypeId,
        field_id: MonoFieldId,
    },

    RecordUpdate {
        record_type: MonoTypeId,
        record_name: IdentId,
        updates: Slice2<MonoFieldId, MonoExprId>,
    },

    /// Same as BigTag but with u8 discriminant instead of u16
    SmallTag {
        discriminant: u8,
        tag_union_type: MonoTypeId,
        args: Slice2<MonoTypeId, MonoExprId>,
    },

    /// Same as SmallTag but with u16 discriminant instead of u8
    BigTag {
        discriminant: u16,
        tag_union_type: MonoTypeId,
        args: Slice2<MonoTypeId, MonoExprId>,
    },

    Block {
        stmts: Slice<MonoStmtId>,
        final_expr: MonoExprId,
    },

    CompilerBug(Problem),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct WhenBranch {
    pub patterns: Slice<MonoPatternId>,
    pub body: Slice<MonoStmtId>,
    pub guard: Option<MonoExprId>,
}

#[derive(Clone, Copy, Debug)]
pub enum MonoPattern {
    Identifier(IdentId),
    As(MonoPatternId, IdentId),
    StrLiteral(InternedStrId),
    NumberLiteral(Number),
    AppliedTag {
        tag_union_type: MonoTypeId,
        tag_name: IdentId,
        args: Slice<MonoPatternId>,
    },
    StructDestructure {
        struct_type: MonoTypeId,
        destructs: Slice3<IdentId, MonoFieldId, DestructType>,
    },
    List {
        elem_type: MonoTypeId,
        patterns: Slice<MonoPatternId>,

        /// Where a rest pattern splits patterns before and after it, if it does at all.
        /// If present, patterns at index >= the rest index appear after the rest pattern.
        /// For example:
        ///   [ .., A, B ] -> patterns = [A, B], rest = 0
        ///   [ A, .., B ] -> patterns = [A, B], rest = 1
        ///   [ A, B, .. ] -> patterns = [A, B], rest = 2
        /// Optionally, the rest pattern can be named - e.g. `[ A, B, ..others ]`
        opt_rest: Option<(u16, Option<IdentId>)>,
    },
    Underscore,
}

#[derive(Clone, Copy, Debug)]
pub enum DestructType {
    Required,
    Optional(MonoTypeId, MonoExprId),
    Guard(MonoTypeId, MonoPatternId),
}
