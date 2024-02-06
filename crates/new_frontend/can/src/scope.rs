use std::{env::current_exe, thread::current};

use arena::{Vec16, Vec32};
use intern::{InternKey, Interns};

pub struct IdentId(NonZeroU16);

pub struct Scope<'a> {
    // SoA version of Vec<{ name: InternKey, ident_id: IdentId, used: bool }>
    names: Vec32<'a, InternKey>,
    ident_ids: Vec32<'a, IdentId>,
    used: Vec32<'a, bool>,

    /// Each nested scope can only have 65K more entries than the previous one. Seems like a safe limit.
    scope_lengths: SimdVec16<'a, u16>,

    prev_names_len: u32,

    // Each failed lookup consists of both the key we tried to look up (it apparently wasn't in scope at the time)
    // as well as the depth at which the lookup occurred, and also
    failed_lookup_keys: SimdVec16<'a, InternKey>,
    failed_lookup_depths: SimdVec16<'a, u16>,
    failed_lookup_scope_ids: SimdVec16<'a, u16>,
}

impl<'a> Scope<'a> {
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

    /// If it's not in scope, still returns the InternKey so it can be stored for later.
    pub fn lookup(
        &mut self,
        name: &str,
        arena: &'a mut Arena<'a>,
        interns: &'a mut Interns<'a>,
    ) -> Option<InternKey> {
        let intern_key = interns.key_from_str(name);
        let answer = self.names.first_index_of(intern_key);

        if answer.is_none() {
            self.failed_lookup_keys.push(intern_key);
            self.failed_lookup_depths.push(self.depth());
        }

        todo!(
            "If pop the scope, check these out and be like 'ok it's fine iff this was a function'"
        );
    }
}
