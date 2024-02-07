use core::marker::PhantomData;
use std::{env::current_exe, thread::current};

use arena::{Vec16, Vec32};
use intern::{InternKey, Interns};

use crate::node::Node;

pub struct IdentId(NonZeroU16);

/// This is all SoA, which means they should all share one length and capacity.
struct ById<'c, 'f, 'b, Id> {
    // The Id type is an index into each of these SoA arrays.
    _phantom: PhantomData<Id>,

    // Region information needs to persist for reporting (and maybe in the future debug info),
    // and since only regions are used in reporting (not interns), interns don't need to live
    // past canonicalization. (In type checking we only need the intern keys and the regions,
    // not what the keys map to.)
    regions: Vec16<'b, Region>,

    // The interned name which goes with this ID.
    names: Vec16<'f, InternKey>,

    /// Bitvec indicating whether or not this is referenced by anything else. At the end of
    /// canonicalization, these get turned into reports, which are based on Region and which
    /// mean we no longer need this information.
    used: Vec16<'c, bool>,
}

/// 'c - lives for canonicalization only, and is no longer needed afterwards
/// 'f - lives for frontend, will be used after canonicalization but not by the backend
/// 'b - lives for backend, will still need to be used after type checking has finished
pub struct State<'c, 'f, 'b> {
    // Arena for things only used in canonicalization, only used in the frontend, and used in the backend.
    can_arena: Arena<'c>,
    fe_arena: Arena<'f>,
    be_arena: Arena<'f>,

    // Named functions, e.g. `foo = \…`
    // These are separate from constants because top-level functions need to be topologically sorted
    // into strongly-connected components for type checking, and because the backend needs to iterate
    // over all functions separately from all non-functions.
    tl_fns: ById<'a, TopLevelId>,
    ntl_fns: ById<'a, NonTopLevelId>,

    // Named constants, e.g. `foo = <non-lambda>`
    tl_constants: ById<'f, TopLevelId>,
    ntl_constants: ById<'a, NonTopLevelId>,

    // Named types (including opaques, aliases, abilities)
    tl_types: ById<'a, TopLevelTypeId>,
    ntl_types: ById<'a, NonTopLevelTypeId>,

    // We intern lowercase and uppercase strings separately in order to make lookups faster.
    // There's an argument for splitting them up even further, e.g.
    // - Split uppercase into type variables, expression names, and record field names
    // - Split lowercase into type variables, expression names, and record field names
    // However, type variables are often either 1 letter (and therefore stored inline) or else
    // they are the same name as variable name (e.g. `elem`). Record field names commonly
    // overlap with variable names, especially with punning and destructuring (e.g. `{ foo }`).
    // Uppercase names tend to be less numerous,
    lowercase: Interns<'a>,
    uppercase: Interns<'a>,

    /// We only care about which top-level functions reference which other top-level functions,
    /// which is necessary in order to topologically sort them into strongly-connected components.
    /// (Type checking needs them to be sorted this way, and to know which ones are mutually recursive.)
    /// Everything other than
    fn_references: Vec16<'a, TopLevelId>,

    // These go together, SoA style
    names_in_scope: Vec32<'c, InternKey>,
    ids_in_scope: Vec32<'c, ConstId>,

    /// Each nested scope can only have 65K more entries than the previous one. Seems like a safe limit.
    scope_lengths: SimdVec16<'a, u16>,

    prev_names_len: u32,

    // Whenever a lookup fails, we record it here in case it ends up being defined later at the top level.
    missing_const_names: SimdVec16<'a, InternKey>,
    missing_const_ids: SimdVec16<'a, ConstId>,
    // Each name/id may have multiple regions which were looking for it.
    missing_const_regions: SimdVec16<'a, SimdVec16<'a, Region>>,
    missing_const_is_tl_non_fn: SimdVec16<'a, SimdVec16<'a, bool>>,
}

impl<'a> State<'a> {
    /// How many levels deep we are right now.
    /// Remember that depth can increase and then decrease and then increase again!
    pub fn depth() -> u16 {
        self.scope_lengths.len()
    }

    /// Push a new scope onto the stack.
    pub fn push(&mut self, arena: &'a mut Arena<'a>) {
        let current_len = self.names.len();

        debug_assert!(current_len >= self.prev_names_len);

        let delta = current_len - self.prev_names_len;

        self.prev_names_len = current_len;
        self.scope_lengths.push(arena, delta);
    }

    /// Pop back to the previous scope.
    pub fn pop(&mut self) -> SimdVec32 {
        if let Some(len) = self.scope_lengths.pop() {
            // All the lookups at
            failed_lookup_depths.count_trailing_eq(self.depth());

            debug_assert!(self.prev_names_len >= len);

            self.scope_lengths.drop_last(len);
            self.prev_names_len -= len;
        } else {
            #[cfg(debug_assertions)]
            {
                panic!("Tried to pop a non-empty scope stack. This should never happen!");
            }
        }
    }

    /// Registering functions works differently because functions are allowed to be
    /// referenced before they're declared. (This is necessary because otherwise it
    /// would be impossible to write mutually recursive functions.) As such, when
    /// we register a function, we record
    pub fn register_fn(name: &str, arena: &'a mut Arena<'a>, interns: &'a mut Interns<'a>) -> bool {
        let intern_key = interns.key_from_str(name);
        let was_in_scope = self.names.first_index_of(intern_key).is_some();

        self.names.push(arena, name);

        was_in_scope
    }

    pub fn register_non_fn(
        name: &str,
        arena: &'a mut Arena<'a>,
        interns: &'a mut Interns<'a>,
    ) -> bool {
        let intern_key = interns.key_from_str(name);
        let was_in_scope = self.names.first_index_of(intern_key).is_some();

        self.names.push(arena, name);

        was_in_scope
    }

    pub fn register_tl_non_fn(&mut self, name: &str, region: Region) -> ConstId {
        // TODO same as register_tl_fn, *except* that for each missing_const_is_tl_non_fn
        // which corresponds to a failed lookup on this name, we immediately report their
        // Regions as errors (and remove them from those vecs), because those were necessarily
        // instances of a top-level non-function trying to refer to another top-level non-function
        // before it was defined. That is not allowed!
    }

    pub fn register_tl_fn(&mut self, name: &str, region: Region) -> ConstId {
        let key = self.lowercase.key_from_str(name);

        if let Some(index) = self.tl_fn_names.first_index_of(key) {
            // We've redeclared this top-level function. That is not allowed!
            self.problems.push(Problem::ShadowedTopLevelConst {
                original_region: self.tl_fn_regions.get_unchecked(index),
                shadowed_region: region,
            });
            return self.err_shadowed_tl_const(key);
        }

        if let Some(index) = self.tl_non_fn_names.first_index_of(key) {
            // We've redeclared this top-level value. That is not allowed!
            self.problems.push(Problem::ShadowedTopLevelConst {
                original_region: self.tl_non_fn_regions.get_unchecked(index),
                shadowed_region: region,
            });
            return self.err_shadowed_tl_const(key);
        }

        // Next, check to see if we previously had any lookups which already tried unsuccessfully
        // to reference this name.
        let const_id = match self.missing_const_names.first_index_of(key) {
            Some(index) => unsafe {
                // It turns out we already had some lookups that tried to reference this top-level
                // function, but it hadn't been defined yet. We'll use the ConstId they referred to,
                // so that they're retroactively referring to the right thing!
                //
                // Also remove this entry from missing const lookups, so future lookups don't have to
                // scan through as much. (Also so we don't end up incorrectly reporting it as missing!)
                self.missing_const_names.remove_swap_unchecked(index);
                self.missing_const_regions.remove_swap_unchecked(index);
                self.missing_const_ids.remove_swap_unchecked(index)
            },
            None => {
                // No previous lookups ever tried to reference this, so make a new ConstId for it.
                self.new_const_id()
            }
        };

        self.tl_fn_names.push(key);
        self.tl_fn_ids.push(const_id);
        self.tl_fn_regions.push(region);

        const_id
    }

    /// If it's not in scope, still returns the InternKey so it can be stored for later.
    pub fn lookup_const(
        &mut self,
        name: &str,
        region: Region,
        looking_up_from_tl_non_fn: bool,
    ) -> Node {
        let key = self.lowercase.key_from_str(name);
        let const_id;

        match self.names_in_scope.first_index_of(key) {
            Some(index) => {
                // It was in scope! Record a normal lookup for it.
                const_id = unsafe { self.ids_in_scope.get_unchecked(self.arena, index) };
            }
            None => {
                // It was not in scope. Get a ConstId (or create a new one) for it for later.
                match self.missing_const_names.first_index_of(key) {
                    Some(index) => {
                        self.missing_const_is_tl_non_fn
                            .get_unchecked(index)
                            .push(looking_up_from_tl_non_fn);
                        self.missing_const_regions.get_unchecked(index).push(region);
                        const_id = self.missing_const_ids.get_unchecked(index);
                    }
                    None => {
                        let index = self.missing_const_ids.len();

                        const_id = invalid_lookup_const(key);

                        self.missing_const_ids.push(const_id);
                        self.missing_const_regions.push(vec![region]);
                        self.missing_const_is_tl_non_fn
                            .push(vec![looking_up_from_tl_non_fn]);
                    }
                };
            }
        };

        // We always return a normal lookup, whether or not we successfully found anything.
        // This is so that later on, if it turns out there *was* a top-level value which assigned
        // this name to a value (after the fact), we can update what this ConstId refers to
        // without needing to go back and fix up all the nodes that referred to it.
        Node::LookupConst(const_id)
    }

    fn invalid_lookup_const(&mut self, key: InternKey) -> ConstId {
        // TODO need to fabricate a new one!
    }

    fn err_shadowed_tl_const(&mut self, key: InternKey) -> ConstId {
        // TODO need to fabricate a new one!
    }
}
