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

/// Either a binding or a lookup.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScopeId(u32);

impl ScopeId {
    fn to_index(&self) -> usize {
        self.0 as usize
    }
}

pub struct Scope<IdentId, Region> {
    /// Bindings nested under the top level.
    nested: Vec3<IdentId, Region, ScopeId>,

    /// Whenever we pop the scope, we take the .last() of this and
    /// truncate that many entries from the end of bindings.
    scope_lengths: Vec<u32>,

    /// Each ScopeId is an index into this.
    ident_ids_by_scope_id: Vec2<IdentId, Region>,
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
        top_level_bindings: impl Iterator<Item = (IdentId, Region)>,
    ) -> (Self, Vec2<IdentId, Region>) {
        let mut shadowed: Vec2<IdentId, Region> = Default::default();
        let mut ident_ids_by_scope_id: Vec2<IdentId, Region> = Default::default();

        for (ident_id, region) in top_level_bindings {
            match ident_ids_by_scope_id
                .iter()
                .find(|(existing_ident_id, _)| *existing_ident_id == ident_id)
            {
                Some((existing_ident_id, existing_region)) => {
                    shadowed.push((*existing_ident_id, *existing_region));
                }
                None => {
                    ident_ids_by_scope_id.push((ident_id, region));
                }
            }
        }

        let mut scope_lengths = Vec::with_capacity(16);

        // This cast can never overflow because it's u32, and we only allow at most u16::MAX lines per file
        // plus u16::MAX bytes per line.
        scope_lengths.push(ident_ids_by_scope_id.len() as u32);

        let scope = Self {
            nested: Default::default(),
            scope_lengths,
            ident_ids_by_scope_id,
        };

        (scope, shadowed)
    }

    fn num_top_level_bindings(&self) -> u32 {
        // We always push 1 onto this vec when we initialize Self, and it should never get popped.
        // (We could use get_unchecked here, but the minor perf boost doesn't seem worth the risk.)
        self.scope_lengths.first().copied().unwrap_or(0)
    }

    /// The first `num_top_level_bindings` entries in ident_ids_by_scope_id are always
    /// the top level IdentIds and Regions.
    fn top_level_ident_ids_with_scope_id<'a>(
        &'a self,
    ) -> impl Iterator<Item = (IdentId, ScopeId)> + 'a {
        let len = self.num_top_level_bindings() as usize;

        (&self.ident_ids_by_scope_id[..len])
            .iter()
            .enumerate()
            .map(|(index, (ident_id, _region))| (*ident_id, ScopeId(index as u32)))
    }

    /// Create a new nested scope. This can happen inside a lambda, a when-branch, indented def, etc.
    pub fn push(&mut self) {
        // This can never overflow because we only have at most u16::MAX lines in the source code, and
        // at most u16::MAX bytes per line. So even if filled every line with (if x then ...) it wouldn't overflow.
        self.scope_lengths.push(0);
    }

    /// End the current scope.
    pub fn pop(&mut self) {
        match self.scope_lengths.pop() {
            Some(num_entries) => {
                debug_assert!(
                    self.scope_lengths.len() >= 1,
                    "Popped the top-level scope. This should never happen!"
                );
                debug_assert!(self.nested.len() >= num_entries as usize, "Tried to pop {num_entries} off a bindings with only {} entries remaining; this should never happen!", self.nested.len());

                let new_len = self.nested.len().saturating_sub(num_entries as usize);

                self.nested.truncate(new_len);
            }
            None => {
                #[cfg(debug_assertions)]
                {
                    // TODO panic in debug builds, somehow we popped more bindings than we pushed!
                }
            }
        }
    }

    fn new_scope_id(&mut self, ident_id: IdentId, region: Region) -> ScopeId {
        // This should never overflow because we only have u16::MAX lines.
        let scope_id = ScopeId(self.ident_ids_by_scope_id.len() as u32);

        self.ident_ids_by_scope_id.push((ident_id, region));

        scope_id
    }

    pub fn add_binding(&mut self, ident_id: IdentId, region: Region) -> ScopeId {
        debug_assert!(self.scope_lengths.len() > 1, "Tried to add a binding to the top level before Scope::push() was called (or if Scope::pop() had been called enough times to get back to the top level). All top-level bindings in Scope must be initialized on startup; no more can be added later!");

        // Increment the current scope lengths. This is needed so that when we pop
        // a scope, we pop the correct number of bindings.
        match self.scope_lengths.last_mut() {
            Some(lengths) => {
                // This should never overflow because it's u32 and we support at most u16::MAX lines
                // with u16::MAX bytes per line. So eve
                // this still wouldn't overflow.
                *lengths = *lengths + 1;
            }
            None => {
                // TODO panic in debug mode because we're trying to add a binding with no nested scopes!
                // (All the top-level bindings should have been added on boot.)
            }
        }

        let scope_id = self.new_scope_id(ident_id, region.clone());
        let nested = &mut self.nested;

        nested.push((ident_id, region.clone(), scope_id));

        scope_id
    }

    /// Lookup an unqualified identifier, such as `blah` (as opposed to a qualified one like `Foo.blah`)
    pub fn lookup_unqualified(&self, ident_id: IdentId) -> Option<ScopeId> {
        // First, look for it in the current nested scope. To resolve shadowing correctly, we must
        // check nested scope before falling back on checking the top level.
        self.lookup_in_nested(ident_id).or_else(|| {
            self.lookup_in_top_level(ident_id)
            // If we couldn't find it in the current nested scope, look for it in the top level.
        })
    }

    /// Lookup a qualified identifier, such as `Foo.blah` (as opposed to an unqualified one like `blah`)
    pub fn lookup_qualified(&self, ident_id: IdentId) -> Option<ScopeId> {
        // Don't bother looking for this in nested scopes, because you can't refer to those
        // in a qualified way. If we don't find it in the top level, it wasn't found.
        self.lookup_in_top_level(ident_id)
    }

    fn lookup_in_nested(&self, needle_ident_id: IdentId) -> Option<ScopeId> {
        self.nested
            .iter()
            // It's important to search in reverse because of shadowing. If we find a match,
            // we must prefer the *last* binding!
            .rev()
            .find_map(|(nested_ident_id, _region, scope_id)| {
                if needle_ident_id == *nested_ident_id {
                    Some(*scope_id)
                } else {
                    None
                }
            })
    }

    fn lookup_in_top_level(&self, needle_ident_id: IdentId) -> Option<ScopeId> {
        self.top_level_ident_ids_with_scope_id()
            .find_map(|(tl_ident_id, scope_id)| {
                if tl_ident_id == needle_ident_id {
                    Some(scope_id)
                } else {
                    None
                }
            })
    }

    pub fn ident_id_from_scope_id(&self, scope_id: ScopeId) -> IdentId {
        unsafe {
            self.ident_ids_by_scope_id
                .get_unchecked(scope_id.to_index())
                .0
        }
    }

    pub fn region_from_scope_id(&self, scope_id: ScopeId) -> Region {
        unsafe {
            self.ident_ids_by_scope_id
                .get_unchecked(scope_id.to_index())
                .1
        }
    }
}

#[cfg(test)]
mod scope_tests {
    use super::{Scope, Vec2};

    type IdentId = usize;
    type Region = usize;
    type TestScope = Scope<IdentId, Region>;

    fn new_scope(ident_ids: &[IdentId]) -> (TestScope, Vec2<IdentId, Region>) {
        Scope::new(
            ident_ids
                .iter()
                .enumerate()
                .map(|(region, ident_id)| (*ident_id, region)),
        )
    }

    fn push_bindings(scope: &mut TestScope, ident_ids: &[IdentId]) {
        scope.push();

        for &ident_id in ident_ids {
            scope.add_binding(ident_id, scope.ident_ids_by_scope_id.len());
        }
    }

    #[test]
    fn empty_top_level() {
        let (scope, shadowed) = new_scope(&[]);

        assert_eq!(shadowed, Vec::new());
        assert_eq!(scope.lookup_in_nested(0), None);
        assert_eq!(scope.lookup_in_top_level(0), None);
    }

    #[test]
    fn top_level_lookup() {
        let top_level = &[1, 2, 3];
        let (scope, shadowed) = new_scope(top_level);

        assert_eq!(shadowed, Vec::new());

        for &ident_id in top_level {
            assert!(
                scope.lookup_in_top_level(ident_id).is_some(),
                "Expected {ident_id} to be in top-level, but it wasn't."
            );
            assert!(
                scope.lookup_in_nested(ident_id).is_none(),
                "Expected {ident_id} to NOT be in nested defs, but it was."
            );
        }

        assert_eq!(scope.lookup_in_top_level(0), None);
        assert_eq!(scope.lookup_in_top_level(top_level.len() + 1), None);

        assert_eq!(scope.lookup_in_nested(0), None);
        assert_eq!(scope.lookup_in_nested(top_level.len() + 1), None);
    }

    #[test]
    fn nested_lookup_not_shadowing() {
        let top_level = &[1, 2, 3];
        let (mut scope, shadowed) = new_scope(top_level);
        assert_eq!(shadowed, Vec::new());
        let nested_scope = &[4, 5, 6];

        push_bindings(&mut scope, nested_scope);

        for &ident_id in nested_scope {
            assert!(
                scope.lookup_unqualified(ident_id).is_some(),
                "Expected {ident_id} to be in nested scope, but an unqualified lookup failed."
            );
        }
    }

    #[test]
    fn nested_lookup_shadowing_top_level() {
        let top_level = &[1, 2, 3];
        let (mut scope, shadowed) = new_scope(top_level);
        assert_eq!(shadowed, Vec::new());
        let nested_scope = &[2, 3, 4];

        push_bindings(&mut scope, nested_scope);

        for (index, ident_id) in nested_scope.iter().copied().enumerate() {
            let opt_scope_id = scope.lookup_unqualified(ident_id);

            assert!(
                opt_scope_id.is_some(),
                "Expected {ident_id} to be in nested scope, but an unqualified lookup failed."
            );

            let scope_id = opt_scope_id.unwrap();

            assert!(
                scope_id.0 as usize >= top_level.len(),
                "Lookup for {ident_id} had a ScopeId of {}, which was not greater than or equal to number of top-level defs ({}), which was expecte because it shadowed a top-level binding.",
                scope_id.0,
                top_level.len()
            );

            assert_eq!(ident_id, scope.ident_id_from_scope_id(scope_id));
            assert_eq!(
                index + top_level.len(),
                scope.region_from_scope_id(scope_id)
            );
        }
    }

    #[test]
    fn nested_lookup_in_out_shadowing() {
        let top_level = &[1, 2, 3];
        let (mut scope, shadowed) = new_scope(top_level);
        assert_eq!(shadowed, Vec::new());
        push_bindings(&mut scope, &[2, 3, 4, 5]);
        push_bindings(&mut scope, &[3, 4, 5, 6, 42]);

        let total_bindings_before_pop = scope.ident_ids_by_scope_id.len();

        {
            assert!(
                scope.lookup_unqualified(42).is_some(),
                "Expected lookup for 42 to be in scope before the pop(), but it was not in scope."
            );

            scope.pop();

            assert!(
                scope.lookup_unqualified(42).is_none(),
                "Expected lookup for 42 to be out of scope after the pop(), but it was in scope."
            );
        }

        push_bindings(&mut scope, &[1, 3, 4, 5, 7]);

        assert!(
            scope.lookup_unqualified(42).is_none(),
            "Expected lookup for 42 to be out of scope after the pop() and the new bindings, but it was in scope."
        );

        assert!(
            scope.lookup_unqualified(7).is_some(),
            "Expected lookup for 7 to be in scope after the new bindings, but it was not in scope."
        );

        for ident_id in [3, 4, 5] {
            let opt_scope_id = scope.lookup_unqualified(ident_id);

            assert!(
                opt_scope_id.is_some(),
                "Expected {ident_id} to be in nested scope, but an unqualified lookup failed."
            );

            let scope_id = opt_scope_id.unwrap();

            assert!(
                scope_id.0 as usize >= total_bindings_before_pop,
                "Lookup for {ident_id} had a ScopeId of {}, which was not greater than or equal to number of expected bindings at this point ({total_bindings_before_pop})",
                scope_id.0,
            );
        }
    }
}
