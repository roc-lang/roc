use crate::can::ident::ModuleName;
use crate::collections::{default_hasher, MutMap};
use std::collections::HashMap;
use std::fmt;

pub const NUM_BUILTIN_MODULES: usize = 12;

#[cfg(debug_assertions)]
lazy_static! {
    /// This is used in Debug builds only, to let us have a Debug instance
    /// which displays not only the Module ID, but also the Module Name which
    /// corresponds to that ID.
    ///
    pub static ref DEBUG_MODULE_ID_NAMES: std::sync::Mutex<crate::collections::MutMap<u32, Box<str>>> =
        // This stores a u32 key instead of a ModuleId key so that if there's
        // a problem with ModuleId's Debug implementation, logging this for diagnostic
        // purposes won't recursively trigger ModuleId's Debug instance in the course of printing
        // this out.
        std::sync::Mutex::new(crate::collections::MutMap::default());
}

/// A globally unique ID that gets assigned to each module as it is loaded.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId {
    value: u32,
}

/// In Debug builds only, ModuleId has a name() method that lets
/// you look up its name in a global intern table. This table is
/// behind a mutex, so it is neither populated nor available in release builds.
impl ModuleId {
    // NOTE: Always add constants to the *end* of this list (with a unique integer),
    // and then also go to `impl Default for ModuleIds` and incorporate the new constant
    // into the *end* of its default module insertions. Insertion order matters for those!
    pub const STR: ModuleId = ModuleId { value: 0 };
    pub const BOOL: ModuleId = ModuleId { value: 1 };
    pub const INT: ModuleId = ModuleId { value: 2 };
    pub const FLOAT: ModuleId = ModuleId { value: 3 };
    pub const LIST: ModuleId = ModuleId { value: 4 };
    pub const MAP: ModuleId = ModuleId { value: 5 };
    pub const SET: ModuleId = ModuleId { value: 6 };
    pub const NUM: ModuleId = ModuleId { value: 7 };

    #[cfg(debug_assertions)]
    pub fn name(self) -> Box<str> {
        let names =
        DEBUG_MODULE_ID_NAMES
            .lock()
            .expect("Failed to acquire lock for Debug reading from DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.");

        match names.get(&self.value) {
            Some(str_ref) => str_ref.clone(),
            None => {
                panic!(
                    "Could not find a Debug name for module ID {} in {:?}",
                    self.value, names,
                );
            }
        }
    }
}

impl fmt::Debug for ModuleId {
    /// In debug builds, whenever we create a new ModuleId, we record is name in
    /// a global interning table so that Debug can look it up later. That table
    /// needs a global mutex, so we don't do this in release builds. This means
    /// the Debug impl in release builds only shows the number, not the name (which
    /// it does not have available, due to having never stored it in the mutexed intern table.)
    #[cfg(debug_assertions)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Originally, this printed both name and numeric ID, but the numeric ID
        // didn't seem to add anything useful. Feel free to temporarily re-add it
        // if it's helpful in debugging!
        write!(f, "{}", self.name())
    }

    /// In relese builds, all we have access to is the number, so only display that.
    #[cfg(not(debug_assertions))]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

/// Stores a mapping between ModuleId and ModuleName.
///
/// Each module name is stored twice, for faster lookups.
/// Since these are interned strings, this shouldn't result in many total allocations in practice.
#[derive(Debug)]
pub struct ModuleIds {
    by_name: MutMap<ModuleName, ModuleId>,
    /// Each ModuleId is an index into this Vec
    by_id: Vec<ModuleName>,
}

impl Default for ModuleIds {
    fn default() -> Self {
        // +1 because the user will be compiling at least 1 non-builtin module!
        let capacity = NUM_BUILTIN_MODULES + 1;

        let mut by_name = HashMap::with_capacity_and_hasher(capacity, default_hasher());
        let mut by_id = Vec::with_capacity(capacity);

        let mut insert_both = |id: ModuleId, name_str: &'static str| {
            let name: ModuleName = name_str.into();

            // It's very important that these are inserted in the correct order!
            debug_assert!(id.value as usize == by_id.len(), "When setting up default ModuleIds, module `{:?}` was inserted in the wrong order. It has a hardcoded ID of {:?} but was inserted at index {:?}", name_str, id.value, by_id.len());

            // Make sure we haven't already inserted an entry for this module name.
            debug_assert!(!by_name.contains_key(&name), "Duplicate default module! We already have an ID for module `{:?}` (namely {:?}), but we tried to insert it again with ID {:?}", name, by_name.get(&name).unwrap(), id.value);

            if cfg!(debug_assertions) {
                Self::insert_debug_name(id, &name);
            }

            by_name.insert(name.clone(), id);
            by_id.push(name);
        };

        // These MUST be inserted in the correct order:
        //
        // * The first ModuleId pased in must be 0
        // * Each subsequent ModuleId must be 1 greater than the previous one
        //
        // This is because these will be translated into indices into a Vec,
        // and each time this gets called, the name gets pushed onto the Vec.
        // So for these IDs to correspond to the correct names, they must be
        // inserted in this order!
        //
        // Everywherere else this invariant is enforced by the API,
        // but for these hardcoded modules we have to enforce it manually.
        insert_both(ModuleId::STR, ModuleName::STR);
        insert_both(ModuleId::BOOL, ModuleName::BOOL);
        insert_both(ModuleId::INT, ModuleName::INT);
        insert_both(ModuleId::FLOAT, ModuleName::FLOAT);
        insert_both(ModuleId::LIST, ModuleName::LIST);
        insert_both(ModuleId::MAP, ModuleName::MAP);
        insert_both(ModuleId::SET, ModuleName::SET);
        insert_both(ModuleId::NUM, ModuleName::NUM);

        ModuleIds { by_name, by_id }
    }
}

impl ModuleIds {
    pub fn get_or_insert_id(&mut self, module_name: &ModuleName) -> ModuleId {
        match self.by_name.get(module_name) {
            Some(id) => *id,
            None => {
                let by_id = &mut self.by_id;
                let module_id = ModuleId {
                    value: by_id.len() as u32,
                };

                by_id.push(module_name.clone());

                self.by_name.insert(module_name.clone(), module_id);

                if cfg!(debug_assertions) {
                    Self::insert_debug_name(module_id, &module_name);
                }

                module_id
            }
        }
    }

    #[cfg(debug_assertions)]
    fn insert_debug_name(module_id: ModuleId, module_name: &ModuleName) {
        let mut names = DEBUG_MODULE_ID_NAMES.lock().expect("Failed to acquire lock for Debug interning into DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.");

        names.insert(module_id.value, module_name.as_str().into());
    }

    #[cfg(not(debug_assertions))]
    fn insert_debug_name(_module_id: ModuleId, _module_name: &ModuleName) {
        // By design, this is a no-op in release builds!
    }

    pub fn get_id(&self, module_name: &ModuleName) -> Option<&ModuleId> {
        self.by_name.get(module_name)
    }

    pub fn get_name(&self, id: ModuleId) -> Option<&ModuleName> {
        self.by_id.get(id.value as usize)
    }
}
