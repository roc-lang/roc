use crate::{ident::{Ident, ModuleIdents}, interns::InternId};
use bumpalo::{collections::Vec, Bump};

// TODO this should probably be constructed during parsing actually.

pub struct Module<'a> {
    arena: &'a Bump,
    idents: ModuleIdents<'a>,
    scopes: Vec<'a, Scope<'a>>,
}

struct Scope<'a> {
    introduced: Vec<'a, Ident<'a>>,
    lookups: Vec<'a, Ident<'a>>,
    idents_by_intern: Vec<'a, (InternId<'a>, Ident<'a>)>,

    /// These might be naming errors, but not necessarily!
    /// They might turn out to be recursive.
    looked_up_before_declared: Vec<'a, Ident<'a>>,
}

impl<'a> Scope<'a> {
    fn with_capacity_in(arena: &'a Bump, introduced_cap: usize, lookup_cap: usize) -> Self {
        Self {
            introduced: Vec::with_capacity_in(introduced_cap, arena),
            lookups: Vec::with_capacity_in(lookup_cap, arena),
            idents_by_intern: Vec::with_capacity_in(introduced_cap, arena),

            // empty by default
            looked_up_before_declared: Vec::new_in(arena),
        }
    }
}

impl<'a> Module<'a> {
    pub fn push_scope_with_capacity(&'a mut self, introduced_cap: usize, lookup_cap: usize) {
        let scope = Scope::with_capacity_in(self.arena, introduced_cap, lookup_cap);

        self.scopes.push(scope);
    }

    pub fn pop_scope(&'a mut self) -> Option<Scope<'a>> {
        self.scopes.pop()
    }

    pub fn register_lookup(&'a mut self, lookup: InternId<'a>) -> Ident<'a> {
        let scope = self.current_scope_mut();
    }

    fn current_scope_mut(&'a mut self) -> Scope<'a> {
        // Get the latest scope. If there isn't one, push one!
        todo!()
    }

    pub fn load_from_cache(path: &'a [u8]) -> Result<Self, SaveErr> {
        let file_size_in_bytes = read_file_size()?;
        let allocation = Arena::with_capacity(file_size_in_bytes);
        let iovecs = Iovecs::from_header(fd)?;

        readv(iovecs.as_mut_ptr().cast(), iovecs.len())
    }

    pub fn save_to_cache(&self, path: &'a [u8]) -> Result<(), SaveErr> {
        let file_size = IOVECS_HEADER_SIZE +
        let iovecs = self.as_iovecs();

        iovecs.write_header()?;

        writev(iovecs.as_ptr().cast(), iovecs.len());
    }

    fn as_iovecs(&self) -> Iovecs {

    }
}

struct Iovecs {
    idents: Iovec,
    scopes: Iovec,
}
