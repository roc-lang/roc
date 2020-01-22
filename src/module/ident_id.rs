use crate::collections::MutMap;
use inlinable_string::InlinableString;
use std::fmt;

lazy_static! {
    pub static ref STR_IDENTS: IdentIds = {
        // TODO populate these similarly to how we do in `impl Default for ModuleIds`,
        // including having hardcoded IdentId values for each identifier
        // in each builtin module.

        IdentIds::default()
    };
}

#[cfg(debug_assertions)]
lazy_static! {
    /// This is used in Debug builds only, to let us have a Debug instance
    /// which displays not only the Ident ID, but also the name string which
    /// corresponds to that ID.
    pub static ref DEBUG_IDENT_ID_NAMES: std::sync::Mutex<crate::collections::MutMap<u32, Box<str>>> =
        // This stores a u32 key instead of a ModuleId key so that if there's
        // a problem with ModuleId's Debug implementation, logging this for diagnostic
        // purposes won't recursively trigger ModuleId's Debug instance in the course of printing
        // this out.
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
        let names =
        DEBUG_IDENT_ID_NAMES
            .lock()
            .expect("Failed to acquire lock for Debug reading from DEBUG_IDENT_ID_NAMES, presumably because a thread panicked.");

        match names.get(&self.value) {
            Some(str_ref) => str_ref.clone(),
            None => {
                panic!(
                    "Could not find a Debug name for ident ID {} in {:?}",
                    self.value, names,
                );
            }
        }
    }
}

/// Stores a mapping between ModuleId and ModuleName.
///
/// Each module name is stored twice, for faster lookups.
/// Since these are interned strings, this shouldn't result in many total allocations in practice.
#[derive(Debug, Default)]
pub struct IdentIds {
    by_name: MutMap<InlinableString, IdentId>,
    /// Each ModuleId is an index into this Vec
    by_id: Vec<InlinableString>,
}

impl IdentIds {
    pub fn get_or_insert_id(&mut self, ident_name: &InlinableString) -> IdentId {
        match self.by_name.get(ident_name) {
            Some(id) => *id,
            None => {
                let by_id = &mut self.by_id;
                let ident_id = IdentId {
                    value: by_id.len() as u32,
                };

                by_id.push(ident_name.clone());

                self.by_name.insert(ident_name.clone(), ident_id);

                if cfg!(debug_assertions) {
                    Self::insert_debug_name(ident_id, &ident_name);
                }

                ident_id
            }
        }
    }

    #[cfg(debug_assertions)]
    fn insert_debug_name(ident_id: IdentId, ident_name: &InlinableString) {
        let mut names = DEBUG_IDENT_ID_NAMES.lock().expect("Failed to acquire lock for Debug interning into DEBUG_IDENT_ID_NAMES, presumably because a thread panicked.");

        names.insert(ident_id.value, ident_name.to_string().into());
    }

    #[cfg(not(debug_assertions))]
    fn insert_debug_name(_ident_id: IdentId, _ident_name: &InlinableString) {
        // By design, this is a no-op in release builds!
    }

    pub fn get_id(&self, ident_name: &InlinableString) -> Option<&IdentId> {
        self.by_name.get(ident_name)
    }

    pub fn get_name(&self, id: IdentId) -> Option<&InlinableString> {
        self.by_id.get(id.value as usize)
    }
}
