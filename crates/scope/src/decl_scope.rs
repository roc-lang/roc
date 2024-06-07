/// Scope within an individual top-level declaration.
/// It has a reference to the entire top-level scope
/// (so, it can see other top-level declarations),
/// and it can contain nested scopes.
use crate::{
    ids::{LowercaseId, ModuleId, UppercaseId},
    problem::Problem,
    tl_scope::TopLevelScope,
};
use bumpalo::{collections::vec::Vec, Bump};
use core::fmt::Debug;

/// TODO replace this with the real Vec2 that stores 1 length as u32 etc. (We can even do u16 len.)
type Vec2<'a, A, B> = Vec<'a, (A, B)>;
/// TODO replace this with the real Vec3 that stores 1 length as u32 etc. (We can even do u16 len.)
type Vec3<'a, A, B, C> = Vec<'a, (A, B, C)>;

pub struct DeclScope<'a, LcStrId, UcStrId, ModuleStrId, Region> {
    /// The top-level scope is used to verify shadowing, and as a fallback for lookups
    /// that fail here. It's valuable for correctness to store it in the structure itself
    /// rather than having each method accept it, because this ensures that it cannot
    /// be modified in between calls.
    ///
    /// Top-level scope should never be modified once we have begun canonicalizing individual
    /// declarations, because we need to be able to do lookups in top-level declarations that
    /// appear later in the file than those declarations.
    tl_scope: &'a TopLevelScope<'a, LcStrId, UcStrId, ModuleStrId, Region>,

    /// These lowercase bindings may shadow, since they aren't top-level.
    lc_bindings: Vec3<'a, ModuleId, LcStrId, Region>,

    /// Uppercase bindings may not shadow.
    uc_bindings: Vec3<'a, ModuleId, UcStrId, Region>,

    /// Module names may not shadow.
    imports: Vec2<'a, ModuleStrId, Region>,

    /// Whenever we pop the scope, we take the .last() of this and
    /// truncate that many entries from the end of bindings.
    scope_lengths: Vec<'a, ScopeLen>,
}

#[derive(Debug, Clone, Copy, Default)]
struct ScopeLen {
    lc_bindings: u32,
    uc_bindings: u32,
    imports: u32,
}

impl<
        'a,
        LcStrId: Copy + PartialEq + Debug,
        UcStrId: Copy + PartialEq + Debug,
        ModuleStrId: Copy + PartialEq + Debug,
        Region: Copy + Debug,
    > DeclScope<'a, LcStrId, UcStrId, ModuleStrId, Region>
{
    pub fn new(
        arena: &'a Bump,
        tl_scope: &'a TopLevelScope<'a, LcStrId, UcStrId, ModuleStrId, Region>,
    ) -> Self {
        Self {
            tl_scope,
            lc_bindings: Vec::new_in(arena),
            uc_bindings: Vec::new_in(arena),
            imports: Vec::new_in(arena),
            scope_lengths: Vec::new_in(arena),
        }
    }

    pub fn reset(&mut self) {
        // Do exhaustive destructuring here so if we add a field later, we don't forget
        // to include it in the reset.
        let Self {
            tl_scope: _,
            lc_bindings,
            uc_bindings,
            imports,
            scope_lengths,
        } = self;

        lc_bindings.truncate(0);
        uc_bindings.truncate(0);
        imports.truncate(0);
        scope_lengths.truncate(0);
    }

    /// If this is from `exposes` in an `import`, this should be passed that module's ModuleId.
    /// Otherwise, this should be passed the current module's ModuleId.
    ///
    /// This can never fail because it's always allowed to shadow an existing binding.
    pub fn bind_lc(&mut self, module_id: ModuleId, str_id: LcStrId, region: Region) -> LowercaseId {
        // Increment the current scope lengths. This is needed so that when we pop
        // a scope, we pop the correct number of bindings.
        match self.scope_lengths.last_mut() {
            Some(lengths) => {
                // This increment should never overflow because it's u32 and we support at most
                // u16::MAX lines with u16::MAX bytes per line.
                *lengths = ScopeLen {
                    lc_bindings: lengths.lc_bindings + 1,
                    uc_bindings: lengths.uc_bindings,
                    imports: lengths.imports,
                };
            }
            None => {
                // TODO panic in debug mode because we're trying to add a binding with no nested scopes!
            }
        }

        // This could theoretically overflow, but if it does, we'll catch that in validate() later.
        let lc_id = LowercaseId((self.lc_bindings.len() + self.tl_scope.num_lc_bindings()) as u16);

        self.lc_bindings.push((module_id, str_id, region));

        lc_id
    }

    /// If this is from `exposes` in an `import`, this should be passed that module's ModuleId.
    /// Otherwise, this should be passed the current module's ModuleId.
    ///
    /// If this would shadow an existing binding that's already in scope, returns Err with
    /// the original binding's Region.
    pub fn bind_uc(
        &mut self,
        module_id: ModuleId,
        str_id: UcStrId,
        region: Region,
    ) -> Result<UppercaseId, Region> {
        // Increment the current scope lengths. This is needed so that when we pop
        // a scope, we pop the correct number of bindings.
        match self.scope_lengths.last_mut() {
            Some(lengths) => {
                // This increment should never overflow because it's u32 and we support at most
                // u16::MAX lines with u16::MAX bytes per line.
                *lengths = ScopeLen {
                    lc_bindings: lengths.lc_bindings,
                    uc_bindings: lengths.uc_bindings + 1,
                    imports: lengths.imports,
                };
            }
            None => {
                // TODO panic in debug mode because we're trying to add a binding with no nested scopes!
            }
        }

        // This lookup should usually fail; if it succeeds then we're shadowing something!
        match self.lookup_uc_unqualified(str_id) {
            None => {
                // This should not overflow because we only allow one uppercase binding per line syntactically,
                // and we also only allow u16::MAX total lines.
                let uc_id =
                    UppercaseId((self.uc_bindings.len() + self.tl_scope.num_uc_bindings()) as u16);

                self.uc_bindings.push((module_id, str_id, region));

                Ok(uc_id)
            }
            Some((_uc_id, region)) => Err(region),
        }
    }

    /// NOTE: When handling a module importing using `as`, always pass the `as`, not the
    /// original module name. The thing after `as` is what will be added to scope; scope
    /// does not care about (or want to know about) the original!
    ///
    /// If this would shadow an existing import that's already in scope, returns Err with
    /// the original import's Region.
    pub fn import(&mut self, str_id: ModuleStrId, region: Region) -> Result<ModuleId, Region> {
        // Increment the current scope lengths. This is needed so that when we pop
        // a scope, we pop the correct number of bindings.
        match self.scope_lengths.last_mut() {
            Some(lengths) => {
                // This increment should never overflow because it's u32 and we support at most
                // u16::MAX lines with u16::MAX bytes per line.
                *lengths = ScopeLen {
                    lc_bindings: lengths.lc_bindings,
                    uc_bindings: lengths.uc_bindings,
                    imports: lengths.imports + 1,
                };
            }
            None => {
                // TODO panic in debug mode because we're trying to add a binding with no nested scopes!
            }
        }

        // This lookup should usually fail; if it succeeds then we're shadowing something!
        match self.lookup_module(str_id) {
            None => {
                // This should not overflow because we only allow one uppercase binding per line syntactically,
                // and we also only allow u16::MAX total lines.
                let module_id = ModuleId((self.imports.len() + self.tl_scope.num_imports()) as u16);

                self.imports.push((str_id, region));

                Ok(module_id)
            }
            Some((_module_id, region)) => Err(region),
        }
    }

    pub fn lookup_lc_unqualified(&self, str_id: LcStrId) -> Option<(LowercaseId, Region)> {
        self.lc_bindings
            .iter()
            .enumerate()
            // Search in reverse because of shadowing.
            // If we find a match, we must prefer the *last* binding!
            .rev()
            .find_map(|(index, (_haystack_module_id, haystack_str_id, region))| {
                // Since this is unqualified, we ignore the module_id
                if *haystack_str_id == str_id {
                    Some((
                        LowercaseId((index + self.tl_scope.num_lc_bindings()) as u16),
                        *region,
                    ))
                } else {
                    None
                }
            })
            .or_else(|| self.tl_scope.lookup_lc(ModuleId::HOME, str_id))
    }

    /// The caller should already have looked up the ModuleId based on the qualified
    /// module name string, and should have reported any "module not imported here"
    /// errors as necessary.
    pub fn lookup_lc_qualified(
        &self,
        module_id: ModuleId,
        str_id: LcStrId,
    ) -> Option<(LowercaseId, Region)> {
        self.tl_scope.lookup_lc(module_id, str_id)
    }

    pub fn lookup_uc_unqualified(&self, str_id: UcStrId) -> Option<(UppercaseId, Region)> {
        self.uc_bindings
            .iter()
            .enumerate()
            .find_map(|(index, (_haystack_module_id, haystack_str_id, region))| {
                // Since this is unqualified, we ignore the module_id
                if *haystack_str_id == str_id {
                    Some((
                        UppercaseId((index + self.tl_scope.num_uc_bindings()) as u16),
                        *region,
                    ))
                } else {
                    None
                }
            })
            .or_else(|| self.tl_scope.lookup_uc(ModuleId::HOME, str_id))
    }

    /// The caller should already have looked up the ModuleId based on the qualified
    /// module name string, and should have reported any "module not imported here"
    /// errors as necessary.
    pub fn lookup_uc_qualified(
        &self,
        module_id: ModuleId,
        str_id: UcStrId,
    ) -> Option<(UppercaseId, Region)> {
        self.tl_scope.lookup_uc(module_id, str_id)
    }

    pub fn lookup_module(&self, str_id: ModuleStrId) -> Option<(ModuleId, Region)> {
        self.imports
            .iter()
            .enumerate()
            .find_map(|(index, (haystack_str_id, region))| {
                if *haystack_str_id == str_id {
                    Some((
                        ModuleId((index + self.tl_scope.num_imports()) as u16),
                        *region,
                    ))
                } else {
                    None
                }
            })
            .or_else(|| self.tl_scope.lookup_module(str_id))
    }

    pub fn region_from_lc_id(&self, lc_id: LowercaseId) -> Option<Region> {
        self.lc_bindings
            .get(lc_id.to_index() + self.tl_scope.num_lc_bindings())
            .map(|(_module_id, _str_id, region)| *region)
    }

    pub fn str_id_from_lc_id(&self, lc_id: LowercaseId) -> Option<LcStrId> {
        self.lc_bindings
            .get(lc_id.to_index() + self.tl_scope.num_lc_bindings())
            .map(|(_module_id, str_id, _region)| *str_id)
    }

    pub fn module_id_from_lc_id(&self, lc_id: LowercaseId) -> Option<ModuleId> {
        self.lc_bindings
            .get(lc_id.to_index() + self.tl_scope.num_lc_bindings())
            .map(|(module_id, _str_id, _region)| *module_id)
    }

    pub fn region_from_uc_id(&self, uc_id: UppercaseId) -> Option<Region> {
        self.uc_bindings
            .get(uc_id.to_index() + self.tl_scope.num_uc_bindings())
            .map(|(_module_id, _str_id, region)| *region)
    }

    pub fn str_id_from_uc_id(&self, uc_id: UppercaseId) -> Option<UcStrId> {
        self.uc_bindings
            .get(uc_id.to_index() + self.tl_scope.num_uc_bindings())
            .map(|(_module_id, str_id, _region)| *str_id)
    }

    pub fn module_id_from_uc_id(&self, uc_id: UppercaseId) -> Option<ModuleId> {
        self.uc_bindings
            .get(uc_id.to_index() + self.tl_scope.num_uc_bindings())
            .map(|(module_id, _str_id, _region)| *module_id)
    }

    pub fn region_from_module_id(&self, module_id: ModuleId) -> Option<Region> {
        self.imports
            .get(module_id.to_index() + self.tl_scope.num_imports())
            .map(|(_str_id, region)| *region)
    }

    pub fn str_id_from_module_id(&self, module_id: ModuleId) -> Option<ModuleStrId> {
        self.imports
            .get(module_id.to_index() + self.tl_scope.num_imports())
            .map(|(str_id, _region)| *str_id)
    }

    /// Make sure we didn't overflow anything - e.g. that we didn't hand
    /// out more LowercaseIds than we support (which would have caused
    /// errors due to u16 casting resulting in incorrect numbers).
    ///
    /// It's essentially inconceivable that this would come up in practice,
    /// but if it does (or if someone is fuzzing the compiler), we should
    /// still handle it with a graceful error!
    pub fn validate(
        &self,
        arena: &'a Bump,
    ) -> Vec<'a, Problem<LcStrId, UcStrId, ModuleStrId, Region>> {
        let mut problems = Vec::new_in(arena);

        let total_lc_bindings = self
            .lc_bindings
            .len()
            .saturating_add(self.tl_scope.num_lc_bindings());

        if total_lc_bindings >= u16::MAX as usize {
            problems.push(Problem::TooManyBindings);
        }

        // Note: module imports and uppercase bindings each go on their own lines,
        // syntactically, so they actually cannot overflow. Only lowercase bindings
        // can possibly overflow, which could theoretically happen due to top-level destructuring.
        problems
    }
}
