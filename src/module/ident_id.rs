use inlinable_string::InlinableString;
use std::fmt;
use symbol_map::indexing::{HashIndexing, Indexing};
use symbol_map::Symbol;

#[cfg(debug_assertions)]
lazy_static! {
    /// This is used in Debug builds only, to let us have a Debug instance
    /// which displays not only the Ident ID, but also the name string which
    /// corresponds to that ID.
    pub static ref DEBUG_IDENT_ID_NAMES: std::sync::Mutex<crate::collections::MutMap<IdentId, Box<str>>> =
        std::sync::Mutex::new(crate::collections::MutMap::default());
}

/// An ID that is assigned to interned string identifiers within a module.
/// By turning these strings into numbers, post-canonicalization processes
/// like unification and optimization can run a lot faster.
///
/// This ID is unique within a given module, not globally - so to turn this back into
/// a string, you would need a ModuleId, an IdentId, and a Map<ModuleId, Map<IdentId, String>>.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct IdentId {
    value: u32,
}

impl fmt::Debug for IdentId {
    /// In debug builds, whenever we create a new IdentId, we record is name in
    /// a global interning table so that Debug can look it up later. That table
    /// needs a global mutex, so we don't do this in release builds. This means
    /// the Debug impl in release builds only shows the number, not the name (which
    /// it does not have available, due to having never stored it in the mutexed intern table.)
    #[cfg(debug_assertions)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:?}, {})", self.name(), self.value)
    }

    /// In relese builds, all we have access to is the number, so only display that.
    #[cfg(not(debug_assertions))]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

/// In Debug builds only, IdentId has a name() method that lets
/// you look up its name in a global intern table. This table is
/// behind a mutex, so it is neither populated nor available in release builds.
#[cfg(debug_assertions)]
impl IdentId {
    pub fn name(&self) -> Box<str> {
        DEBUG_IDENT_ID_NAMES
            .lock()
            .expect("Failed to acquire lock for Debug reading from DEBUG_IDENT_ID_NAMES, presumably because a thread panicked.")
            .get(self)
            .unwrap_or_else(|| panic!("Could not find a Debug name for Ident ID {:?}", self))
            .clone()
    }
}

#[derive(Debug, Default)]
pub struct IdentIds {
    store: HashIndexing<InlinableString, u32>,
}

impl IdentIds {
    pub fn get_or_insert_id(&mut self, ident_name: &InlinableString) -> IdentId {
        match self.store.get(ident_name) {
            Some(symbol) => IdentId {
                value: *symbol.id(),
            },
            None => {
                let ident_id = IdentId {
                    value: *self.store.get_or_insert(ident_name.clone()).unwrap().id(),
                };

                if cfg!(debug_assetions) {
                    Self::insert_debug_name(ident_id, &ident_name);
                }

                ident_id
            }
        }
    }

    #[cfg(debug_assertions)]
    fn insert_debug_name(ident_id: IdentId, ident_name: &InlinableString) {
        let mut names = DEBUG_IDENT_ID_NAMES.lock().expect("Failed to acquire lock for Debug interning into DEBUG_IDENT_ID_NAMES, presumably because a thread panicked.");

        names.insert(ident_id, ident_name.to_string().into());
    }

    #[cfg(not(debug_assertions))]
    fn insert_debug_name(_ident_id: IdentId, _ident_name: &InlinableString) {
        // By design, this is a no-op in release builds!
    }

    pub fn get_id(&self, ident_name: &InlinableString) -> Option<IdentId> {
        self.store.get(ident_name).map(|symbol| IdentId {
            value: *symbol.id(),
        })
    }

    pub fn get_ident(&self, id: IdentId) -> Option<&InlinableString> {
        self.store.get_symbol(&id.value).map(Symbol::data)
    }
}
