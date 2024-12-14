use roc_region::all::Region;
use soa::{Index, Slice, Slice3};

use crate::{
    mono_ir::IdentId, InternedStrId, MonoExprId, MonoFieldId, MonoTypeId, Number, Problem,
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MonoPatternId {
    inner: Index<MonoPattern>,
}

impl MonoPatternId {
    fn new(inner: Index<MonoPattern>) -> Self {
        Self { inner }
    }
}

#[derive(Debug, Default)]
pub struct MonoPatterns {
    patterns: Vec<MonoPattern>,
    regions: Vec<Region>,
}

impl MonoPatterns {
    pub fn new() -> Self {
        Self {
            patterns: Vec::new(),
            regions: Vec::new(),
        }
    }

    pub fn reserve(&mut self, count: usize) -> Slice<MonoPattern> {
        let start = self.patterns.len();

        self.patterns.extend(
            std::iter::repeat(MonoPattern::CompilerBug(
                Problem::UninitializedReservedPattern,
            ))
            .take(count),
        );
        self.regions
            .extend(std::iter::repeat(Region::zero()).take(count));

        Slice::new(start as u32, count as u16)
    }

    pub fn insert(&mut self, id: Index<MonoPattern>, pattern: MonoPattern, region: Region) {
        debug_assert!(
            self.patterns.len() > id.index(),
            "Pattern index out of bounds"
        );

        // Safety: we should only ever hand out WhenBranch indices that are valid indices into here.
        unsafe {
            *self.patterns.get_unchecked_mut(id.index()) = pattern;
            *self.regions.get_unchecked_mut(id.index()) = region;
        }
    }

    pub fn iter_slice(&self, patterns: Slice<MonoPattern>) -> impl Iterator<Item = &MonoPattern> {
        patterns.indices().map(|index| {
            debug_assert!(
                self.patterns.len() > index,
                "MonoPattern Slice out of bounds"
            );

            unsafe { self.patterns.get_unchecked(index) }
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
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
    CompilerBug(Problem),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DestructType {
    Required,
    Optional(MonoTypeId, MonoExprId),
    Guard(MonoTypeId, MonoPatternId),
}
