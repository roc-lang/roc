use crate::{
    foreign_symbol::ForeignSymbolId, mono_module::InternedStrId, mono_num::Number,
    mono_struct::MonoFieldId, mono_type::MonoTypeId, specialize_type::Problem, MonoPattern,
    MonoPatternId,
};
use roc_can::expr::Recursive;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_region::all::Region;
use soa::{Index, NonEmptySlice, PairSlice, Slice, Slice2};
use std::{iter, mem::MaybeUninit};

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

    pub fn extend(&mut self, exprs: impl Iterator<Item = (MonoExpr, Region)>) -> Slice<MonoExpr> {
        let start = self.exprs.len();

        let (size_hint, _) = exprs.size_hint();

        self.exprs.reserve(size_hint);
        self.regions.reserve(size_hint);

        for (expr, region) in exprs {
            self.exprs.push(expr);
            self.regions.push(region);
        }

        let len = self.exprs.len() - start;

        Slice::new(start as u32, len as u16)
    }

    pub fn iter_pair_slice(
        &self,
        exprs: PairSlice<MonoExpr>,
    ) -> impl Iterator<Item = (&MonoExpr, &MonoExpr)> {
        exprs.indices_iter().map(|(index_a, index_b)| {
            debug_assert!(
                self.exprs.len() > index_a && self.exprs.len() > index_b,
                "A Slice index was not found in MonoExprs. This should never happen!"
            );

            // Safety: we should only ever hand out MonoExprId slices that are valid indices into here.
            unsafe {
                (
                    self.exprs.get_unchecked(index_a),
                    self.exprs.get_unchecked(index_b),
                )
            }
        })
    }

    pub fn extend_pairs(
        &mut self,
        exprs: impl Iterator<Item = ((MonoExpr, Region), (MonoExpr, Region))>,
    ) -> PairSlice<MonoExpr> {
        let start = self.exprs.len();

        let additional = exprs.size_hint().0 * 2;
        self.exprs.reserve(additional);
        self.regions.reserve(additional);

        let mut pairs = 0;

        for ((expr_a, region_a), (expr_b, region_b)) in exprs {
            self.exprs.push(expr_a);
            self.exprs.push(expr_b);
            self.regions.push(region_a);
            self.regions.push(region_b);

            pairs += 1;
        }

        PairSlice::new(start as u32, pairs)
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

    Unit,

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

    If {
        branch_type: MonoTypeId,
        branches: PairSlice<MonoExpr>,
        final_else: MonoExprId,
    },

    When {
        /// The value being matched on
        value: MonoExprId,
        /// The type of the value being matched on
        value_type: MonoTypeId,
        /// The return type of all branches and thus the whole when expression
        branch_type: MonoTypeId,
        /// The branches of the when expression
        branches: NonEmptySlice<WhenBranch>,
    },

    CompilerBug(Problem),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct WhenBranch {
    /// The pattern(s) to match the value against
    pub patterns: NonEmptySlice<MonoPattern>,
    /// A boolean expression that must be true for this branch to be taken
    pub guard: Option<MonoExprId>,
    /// The expression to produce if the pattern matches
    pub value: MonoExprId,
}

#[derive(Debug, Default)]
pub struct WhenBranches {
    branches: Vec<MaybeUninit<WhenBranch>>,
}

impl WhenBranches {
    pub fn new() -> Self {
        Self {
            branches: Vec::new(),
        }
    }

    pub fn reserve(&mut self, count: usize) -> Slice<WhenBranch> {
        let start = self.branches.len();

        let new_size = start + count;
        self.branches.reserve(count);

        unsafe {
            self.branches.set_len(new_size);
        }

        Slice::new(start as u32, count as u16)
    }

    pub fn insert(&mut self, id: Index<WhenBranch>, branch: WhenBranch) {
        debug_assert!(
            self.branches.get(id.index()).is_some(),
            "A WhenBranch index was not found in WhenBranches. This should never happen!"
        );

        // Safety: we should only ever hand out WhenBranch indices that are valid indices into here.
        unsafe {
            self.branches.get_unchecked_mut(id.index()).write(branch);
        }
    }

    pub fn iter_slice(
        &self,
        branches: Slice<WhenBranch>,
    ) -> impl Iterator<Item = &MaybeUninit<WhenBranch>> {
        branches.indices().map(|index| {
            debug_assert!(self.branches.len() > index, "Slice index out of bounds");

            unsafe { self.branches.get_unchecked(index) }
        })
    }
}
