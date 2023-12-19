use core::fmt;
use bumpalo::{collections::Vec, Bump};

/// The index within a module of a particular identifier.
/// In release builds, this is a single u32. This means that
/// Roc supports at most 2^32 (about 4 billion) identifiers per module.
///
/// In debug builds, this also includes a string for the identifier name,
/// and another Arc<String> for the module name. These are used in its Debug
/// implementation.
pub struct Ident<'a> {
    value: u32,

    #[cfg(debug_assertions)]
    ident_name: &'a str,

    #[cfg(debug_assertions)]
    module_name: &'a str,

    #[cfg(not(debug_assertions))]
    _phantom: PhantomData<'a>,
}

#[cfg(debug_assertions)]
impl<'a> fmt::Debug for Ident<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { module_name, ident_name, value } = self;

        write!(f, "{module_name}.{ident_name}({value})")
    }
}

#[cfg(not(debug_assertions))]
impl<'a> fmt::Debug for Ident<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = Self.value;

        write!(f, "Ident({value})")
    }
}

pub struct ModuleIdents<'a> {
    /// Each Ident gets its own entry in this Vec, and
    depends_on: Vec<'a, Vec<'a, Ident<'a>>>,

    #[cfg(debug_assertions)]
    module_name: &'a str,
}

impl<'a> ModuleIdents<'a> {
    #[cfg(debug_assertions)]
    pub fn with_capacity(arena: &'a Bump, cap: usize, module_name: &'a str) -> Self {
        Self { depends_on: Vec::with_capacity_in(cap, arena), module_name }
    }

    #[cfg(not(debug_assertions))]
    pub fn with_capacity(arena: &'a Bump, cap: usize) -> Self {
        Self { depends_on: Vec::with_capacity_in(cap, arena), _phantom: PhantomData::default() }
    }

    #[cfg(debug_assertions)]
    pub fn next(&mut self, ident_name: &'a str) -> Ident {
        Ident { value: self.increment(), module_name: self.module_name, ident_name }
    }

    #[cfg(not(debug_assertions))]
    pub fn next(&mut self) -> Ident {
        Ident { value: self.increment(), _phantom: PhantomData::default() }
    }

    /// Helper used internally by both the debug and release versions of next()
    fn increment(&mut self) -> u32 {
        // Safety: It should never be possible to parse more than `usize` idents
        // out of a file, so this should never overflow.
        self.next = self.next + 1;

        // This might truncate (e.g. from u64 to u32), but this is a massively common
        // operation, so we check for overflow at the end using has_overflowed.
        self.next as u32
    }

    /// For performance, next() doesn't check for overflow as it goes. Instead,
    /// has_overflowed() should be called once a module has finished canonicalization,
    /// in order to determine if overflow occurred at any point. (If overflow did occur,
    /// then some of the idents that were handed out were invalid, and the whole module
    /// must be discarded as invalid.)
    pub fn has_overflowed(&self) -> bool {
        self.next > (u32::MAX as usize)
    }
}
