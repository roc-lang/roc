/// A Scope records bindings at the top level as well as nested bindings, and returns
/// opaque ScopeId values which uniquely identify those bindings within a module's scope.
///
/// It also offers lookups, which return ScopeIds based on what has been added so far.
///
/// In all of this it takes into account Roc's shadowing rules: top-level identifiers may not shadow
/// other top-level identifiers, but nested identifiers may shadow anything.
use core::fmt::Debug;

/// TODO replace this with the real Vec2 that stores 1 length as u32 etc.
type Vec2<A, B> = Vec<(A, B)>;
/// TODO replace this with the real Vec3 that stores 1 length as u32 etc.
type Vec3<A, B, C> = Vec<(A, B, C)>;
/// TODO replace this with the real Vec3 that stores 1 length as u32 etc.
type Vec4<A, B, C, D> = Vec<(A, B, C, D)>;

/// Either a binding or a lookup.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LowercaseId(u32);

impl LowercaseId {
    fn to_index(&self) -> usize {
        self.0 as usize
    }
}

/// Either a binding or a lookup.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UppercaseId(u32);

impl UppercaseId {
    fn to_index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct ScopeLen {
    lc: u32,
    uc: u32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UppercaseKind {
    /// A structural alias is something like
    ///   List a : [Nil, Cons a (List a)]
    /// It is typed structurally, so that a `List U8` is always equal to a `[Nil]_`, for example.
    TypeAlias,
    /// An opaque alias corresponds to an opaque type from the language syntax, like
    ///   Age := U32
    /// It is type nominally, so that `Age` is never equal to `U8` - the only way to unwrap the
    /// structural type inside `Age` is to unwrap the opaque, so `Age` = `@Age U8`.
    OpaqueType,
    /// Ability definition
    Ability,
}

#[derive(Debug)]
pub struct Scope<IdentId, Region> {
    /// Bindings nested under the top level.
    nested_lc: Vec3<IdentId, Region, LowercaseId>,
    /// Uppercase bindings nested under the top level.
    nested_uc: Vec4<IdentId, UppercaseKind, Region, UppercaseId>,

    /// Whenever we pop the scope, we take the .last() of this and
    /// truncate that many entries from the end of bindings.
    scope_lengths: Vec<ScopeLen>,

    /// Each LowercaseId is an index into this.
    ident_ids_by_lc_id: Vec2<IdentId, Region>,

    /// Each UppercaseId is an index into this.
    ident_ids_by_uc_id: Vec3<IdentId, UppercaseKind, Region>,
}

impl<Region: Copy + Debug, IdentId: Copy + PartialEq + Debug> Scope<IdentId, Region> {
    /// Top-level bindings must be provided when the Scope is initialized, because whenever we add a new lookup,
    /// it's always allowed to reference top level bindings no matter where they are. We don't store the
    /// actual bindings because the first (number of top-level bindings) entries in ident_ids_by_scope_id are
    /// the IdentIds for the top level. We store the number of top-level bindings in the first scope_lengths entry.
    ///
    /// Returns Self as well as all the top-level bindings that were shadowed and therefore not recorded
    /// (since top-level bindings may not shadow each other).
    pub fn new(
        top_level_lowercase: impl Iterator<Item = (IdentId, Region)>,
        top_level_uppercase: impl Iterator<Item = (IdentId, UppercaseKind, Region)>,
    ) -> (Self, Vec2<IdentId, Region>) {
        let mut shadowed: Vec2<IdentId, Region> = Default::default();
        let mut ident_ids_by_lc_id: Vec2<IdentId, Region> = Default::default();

        for (ident_id, region) in top_level_lowercase {
            match ident_ids_by_lc_id
                .iter()
                .find(|(existing_ident_id, _)| *existing_ident_id == ident_id)
            {
                Some((existing_ident_id, existing_region)) => {
                    shadowed.push((*existing_ident_id, *existing_region));
                }
                None => {
                    ident_ids_by_lc_id.push((ident_id, region));
                }
            }
        }

        let mut ident_ids_by_uc_id: Vec3<IdentId, UppercaseKind, Region> = Default::default();

        for (ident_id, uc_kind, region) in top_level_uppercase {
            match ident_ids_by_uc_id
                .iter()
                .find(|(existing_ident_id, _, _)| *existing_ident_id == ident_id)
            {
                Some((existing_ident_id, _existing_uc_kind, existing_region)) => {
                    shadowed.push((*existing_ident_id, *existing_region));
                }
                None => {
                    ident_ids_by_uc_id.push((ident_id, uc_kind, region));
                }
            }
        }

        let mut scope_lengths = Vec::with_capacity(16);

        // This cast can never overflow because it's u32, and we only allow at most u16::MAX lines per file
        // plus u16::MAX bytes per line.
        scope_lengths.push(ScopeLen {
            lc: ident_ids_by_lc_id.len() as u32,
            uc: ident_ids_by_uc_id.len() as u32,
        });

        let scope = Self {
            nested_lc: Default::default(),
            nested_uc: Default::default(),
            scope_lengths,
            ident_ids_by_lc_id,
            ident_ids_by_uc_id,
        };

        (scope, shadowed)
    }

    /// This returns u64 because it's adding two u32s, which could overflow otherwise.
    fn num_top_level_bindings(&self) -> u64 {
        // We always push 1 onto this vec when we initialize Self, and it should never get popped.
        // (We could use get_unchecked here, but the minor perf boost doesn't seem worth the risk.)
        self.scope_lengths
            .first()
            .map(|scope_len| scope_len.lc as u64 + scope_len.uc as u64)
            .unwrap_or(0)
    }

    /// The first `num_top_level_bindings` entries in ident_ids_by_scope_id are always
    /// the top level IdentIds and Regions.
    fn top_level_lc_with_id<'a>(&'a self) -> impl Iterator<Item = (IdentId, LowercaseId)> + 'a {
        let len = self.num_top_level_bindings() as usize;

        (&self.ident_ids_by_lc_id[..len])
            .iter()
            .enumerate()
            .map(|(index, (ident_id, _region))| (*ident_id, LowercaseId(index as u32)))
    }

    pub fn nested_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.push();

        let answer = f(self);

        self.pop();

        answer
    }

    /// Create a new nested scope. This can happen inside a lambda, a when-branch, indented def, etc.
    fn push(&mut self) {
        // This can never overflow because we only have at most u16::MAX lines in the source code, and
        // at most u16::MAX bytes per line. So even if filled every line with (if x then ...) it wouldn't overflow.
        self.scope_lengths.push(ScopeLen::default());
    }

    /// End the current scope.
    fn pop(&mut self) {
        match self.scope_lengths.pop() {
            Some(scope_len) => {
                debug_assert!(
                    self.scope_lengths.len() >= 1,
                    "Popped the top-level scope. This should never happen!"
                );
                debug_assert!(self.nested_lc.len() >= scope_len.lc as usize, "Tried to pop {} off lowercase bindings with only {} entries remaining; this should never happen!", scope_len.lc, self.nested_lc.len());
                debug_assert!(self.nested_uc.len() >= scope_len.uc as usize, "Tried to pop {} off uppercase bindings with only {} entries remaining; this should never happen!", scope_len.uc, self.nested_uc.len());

                {
                    let new_len = self.nested_lc.len().saturating_sub(scope_len.lc as usize);

                    self.nested_lc.truncate(new_len);
                }

                {
                    let new_len = self.nested_uc.len().saturating_sub(scope_len.uc as usize);

                    self.nested_uc.truncate(new_len);
                }
            }
            None => {
                #[cfg(debug_assertions)]
                {
                    // TODO panic in debug builds, somehow we popped more bindings than we pushed!
                }
            }
        }
    }

    fn new_lc_id(&mut self, ident_id: IdentId, region: Region) -> LowercaseId {
        // This should never overflow because we only have u16::MAX lines.
        let scope_id = LowercaseId(self.ident_ids_by_lc_id.len() as u32);

        self.ident_ids_by_lc_id.push((ident_id, region));

        scope_id
    }

    fn new_uc_id(
        &mut self,
        ident_id: IdentId,
        uc_kind: UppercaseKind,
        region: Region,
    ) -> UppercaseId {
        // This should never overflow because we only have u16::MAX lines.
        let scope_id = UppercaseId(self.ident_ids_by_uc_id.len() as u32);

        self.ident_ids_by_uc_id.push((ident_id, uc_kind, region));

        scope_id
    }

    pub fn add_lc_binding(&mut self, ident_id: IdentId, region: Region) -> LowercaseId {
        debug_assert!(self.scope_lengths.len() > 1, "Tried to add a lowercase binding to the top level before Scope::push() was called (or if Scope::pop() had been called enough times to get back to the top level). All top-level bindings in Scope must be initialized on startup; no more can be added later!");

        // Increment the current scope lengths. This is needed so that when we pop
        // a scope, we pop the correct number of bindings.
        match self.scope_lengths.last_mut() {
            Some(lengths) => {
                // This should never overflow because it's u32 and we support at most u16::MAX lines
                // with u16::MAX bytes per line. So eve
                // this still wouldn't overflow.
                *lengths = ScopeLen {
                    lc: lengths.lc + 1,
                    uc: lengths.uc,
                };
            }
            None => {
                // TODO panic in debug mode because we're trying to add a binding with no nested scopes!
                // (All the top-level bindings should have been added on boot.)
            }
        }

        let scope_id = self.new_lc_id(ident_id, region.clone());
        let nested = &mut self.nested_lc;

        nested.push((ident_id, region.clone(), scope_id));

        scope_id
    }

    pub fn add_uc_binding(
        &mut self,
        ident_id: IdentId,
        uc_kind: UppercaseKind,
        region: Region,
    ) -> UppercaseId {
        debug_assert!(self.scope_lengths.len() > 1, "Tried to add a lowercase binding to the top level before Scope::push() was called (or if Scope::pop() had been called enough times to get back to the top level). All top-level bindings in Scope must be initialized on startup; no more can be added later!");

        // Increment the current scope lengths. This is needed so that when we pop
        // a scope, we pop the correct number of bindings.
        match self.scope_lengths.last_mut() {
            Some(lengths) => {
                // This should never overflow because it's u32 and we support at most u16::MAX lines
                // with u16::MAX bytes per line. So eve
                // this still wouldn't overflow.
                *lengths = ScopeLen {
                    lc: lengths.lc,
                    uc: lengths.uc + 1,
                };
            }
            None => {
                // TODO panic in debug mode because we're trying to add a binding with no nested scopes!
                // (All the top-level bindings should have been added on boot.)
            }
        }

        let uc_id = self.new_uc_id(ident_id, uc_kind, region.clone());
        let nested = &mut self.nested_uc;

        nested.push((ident_id, uc_kind, region.clone(), uc_id));

        uc_id
    }

    /// Lookup an unqualified lowercase identifier, such as `blah` (as opposed to a qualified one like `Foo.blah`)
    pub fn lookup_lc_unqualified(&self, ident_id: IdentId) -> Option<LowercaseId> {
        // First, look for it in the current nested scope. To resolve shadowing correctly, we must
        // check nested scope before falling back on checking the top level.
        self.lookup_lc_in_nested(ident_id).or_else(|| {
            self.lookup_lc_in_top_level(ident_id)
            // If we couldn't find it in the current nested scope, look for it in the top level.
        })
    }

    /// Lookup a qualified lowercase identifier, such as `Foo.blah` (as opposed to an unqualified one like `blah`)
    pub fn lookup_lc_qualified(&self, ident_id: IdentId) -> Option<LowercaseId> {
        // Don't bother looking for this in nested scopes, because you can't refer to those
        // in a qualified way. If we don't find it in the top level, it wasn't found.
        self.lookup_lc_in_top_level(ident_id)
    }

    fn lookup_lc_in_nested(&self, needle_ident_id: IdentId) -> Option<LowercaseId> {
        self.nested_lc
            .iter()
            // It's important to search in reverse because of shadowing. If we find a match,
            // we must prefer the *last* binding!
            .rev()
            .find_map(|(nested_ident_id, _region, lc_id)| {
                if needle_ident_id == *nested_ident_id {
                    Some(*lc_id)
                } else {
                    None
                }
            })
    }

    fn lookup_lc_in_top_level(&self, needle_ident_id: IdentId) -> Option<LowercaseId> {
        self.top_level_lc_with_id()
            .find_map(|(tl_ident_id, lc_id)| {
                if tl_ident_id == needle_ident_id {
                    Some(lc_id)
                } else {
                    None
                }
            })
    }

    pub fn ident_id_from_lc_id(&self, lc_id: LowercaseId) -> IdentId {
        unsafe { self.ident_ids_by_lc_id.get_unchecked(lc_id.to_index()).0 }
    }

    pub fn region_from_lc_id(&self, lc_id: LowercaseId) -> Region {
        unsafe { self.ident_ids_by_lc_id.get_unchecked(lc_id.to_index()).1 }
    }

    pub fn ident_id_from_uc_id(&self, uc_id: UppercaseId) -> IdentId {
        unsafe { self.ident_ids_by_lc_id.get_unchecked(uc_id.to_index()).0 }
    }

    pub fn kind_from_uc_id(&self, uc_id: UppercaseId) -> UppercaseKind {
        unsafe { self.ident_ids_by_uc_id.get_unchecked(uc_id.to_index()).1 }
    }

    pub fn region_from_uc_id(&self, uc_id: UppercaseId) -> Region {
        unsafe { self.ident_ids_by_uc_id.get_unchecked(uc_id.to_index()).2 }
    }
}

#[cfg(test)]
mod scope_tests {
    use super::{Scope, UppercaseKind, Vec2};

    type IdentId = usize;
    type Region = usize;
    type TestScope = Scope<IdentId, Region>;

    fn new_scope(
        lowercase: &[IdentId],
        uppercase: &[(IdentId, UppercaseKind)],
    ) -> (TestScope, Vec2<IdentId, Region>) {
        Scope::new(
            lowercase
                .iter()
                .enumerate()
                .map(|(region, ident_id)| (*ident_id, region)),
            uppercase
                .iter()
                .enumerate()
                .map(|(region, (ident_id, uc_kind))| (*ident_id, *uc_kind, region)),
        )
    }

    fn push_bindings(scope: &mut TestScope, ident_ids: &[IdentId]) {
        scope.push();

        for &ident_id in ident_ids {
            scope.add_lc_binding(ident_id, scope.ident_ids_by_lc_id.len());
        }
    }

    #[test]
    fn empty_top_level() {
        let (scope, shadowed) = new_scope(&[], &[]);

        assert_eq!(shadowed, Vec::new());
        assert_eq!(scope.lookup_lc_in_nested(0), None);
        assert_eq!(scope.lookup_lc_in_top_level(0), None);
    }

    #[test]
    fn top_level_lookup() {
        let top_level = &[1, 2, 3];
        let (scope, shadowed) = new_scope(top_level, &[]);

        assert_eq!(shadowed, Vec::new());

        for &ident_id in top_level {
            assert!(
                scope.lookup_lc_in_top_level(ident_id).is_some(),
                "Expected {ident_id} to be in top-level, but it wasn't."
            );
            assert!(
                scope.lookup_lc_in_nested(ident_id).is_none(),
                "Expected {ident_id} to NOT be in nested defs, but it was."
            );
        }

        assert_eq!(scope.lookup_lc_in_top_level(0), None);
        assert_eq!(scope.lookup_lc_in_top_level(top_level.len() + 1), None);

        assert_eq!(scope.lookup_lc_in_nested(0), None);
        assert_eq!(scope.lookup_lc_in_nested(top_level.len() + 1), None);
    }

    #[test]
    fn nested_lookup_not_shadowing() {
        let top_level = &[1, 2, 3];
        let (mut scope, shadowed) = new_scope(top_level, &[]);
        assert_eq!(shadowed, Vec::new());
        let nested_scope = &[4, 5, 6];

        push_bindings(&mut scope, nested_scope);

        for &ident_id in nested_scope {
            assert!(
                scope.lookup_lc_unqualified(ident_id).is_some(),
                "Expected {ident_id} to be in nested scope, but an unqualified lookup failed."
            );
        }
    }

    #[test]
    fn nested_lookup_shadowing_top_level() {
        let top_level = &[1, 2, 3];
        let (mut scope, shadowed) = new_scope(top_level, &[]);
        assert_eq!(shadowed, Vec::new());
        let nested_scope = &[2, 3, 4];

        push_bindings(&mut scope, nested_scope);

        for (index, ident_id) in nested_scope.iter().copied().enumerate() {
            let opt_lc_id = scope.lookup_lc_unqualified(ident_id);

            assert!(
                opt_lc_id.is_some(),
                "Expected {ident_id} to be in nested scope, but an unqualified lookup failed."
            );

            let lc_id = opt_lc_id.unwrap();

            assert!(
                lc_id.0 as usize >= top_level.len(),
                "Lookup for {ident_id} had a ScopeId of {}, which was not greater than or equal to number of top-level defs ({}), which was expecte because it shadowed a top-level binding.",
                lc_id.0,
                top_level.len()
            );

            assert_eq!(ident_id, scope.ident_id_from_lc_id(lc_id));
            assert_eq!(index + top_level.len(), scope.region_from_lc_id(lc_id));
        }
    }

    #[test]
    fn nested_lookup_in_out_shadowing() {
        let top_level = &[1, 2, 3];
        let (mut scope, shadowed) = new_scope(top_level, &[]);
        assert_eq!(shadowed, Vec::new());
        push_bindings(&mut scope, &[2, 3, 4, 5]);
        push_bindings(&mut scope, &[3, 4, 5, 6, 42]);

        let total_bindings_before_pop = scope.ident_ids_by_lc_id.len();

        {
            assert!(
                scope.lookup_lc_unqualified(42).is_some(),
                "Expected lookup for 42 to be in scope before the pop(), but it was not in scope."
            );

            scope.pop();

            assert!(
                scope.lookup_lc_unqualified(42).is_none(),
                "Expected lookup for 42 to be out of scope after the pop(), but it was in scope."
            );
        }

        push_bindings(&mut scope, &[1, 3, 4, 5, 7]);

        assert!(
            scope.lookup_lc_unqualified(42).is_none(),
            "Expected lookup for 42 to be out of scope after the pop() and the new bindings, but it was in scope."
        );

        assert!(
            scope.lookup_lc_unqualified(7).is_some(),
            "Expected lookup for 7 to be in scope after the new bindings, but it was not in scope."
        );

        for ident_id in [3, 4, 5] {
            let opt_lc_id = scope.lookup_lc_unqualified(ident_id);

            assert!(
                opt_lc_id.is_some(),
                "Expected {ident_id} to be in nested scope, but an unqualified lookup failed."
            );

            let lc_id = opt_lc_id.unwrap();

            assert!(
                lc_id.0 as usize >= total_bindings_before_pop,
                "Lookup for {ident_id} had a ScopeId of {}, which was not greater than or equal to number of expected bindings at this point ({total_bindings_before_pop})",
                lc_id.0,
            );
        }
    }
}
