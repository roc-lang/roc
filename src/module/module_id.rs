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
    pub static ref DEBUG_MODULE_ID_NAMES: std::sync::Mutex<crate::collections::MutMap<ModuleId, Box<str>>> =
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
#[cfg(debug_assertions)]
impl ModuleId {
    pub const STR: ModuleId = ModuleId { value: 0 };
    pub const BOOL: ModuleId = ModuleId { value: 1 };
    pub const INT: ModuleId = ModuleId { value: 2 };
    pub const FLOAT: ModuleId = ModuleId { value: 3 };
    pub const LIST: ModuleId = ModuleId { value: 4 };
    pub const MAP: ModuleId = ModuleId { value: 5 };
    pub const SET: ModuleId = ModuleId { value: 6 };
    pub const NUM: ModuleId = ModuleId { value: 7 };

    pub fn name(&self) -> Box<str> {
        DEBUG_MODULE_ID_NAMES
            .lock()
            .expect("Failed to acquire lock for Debug reading from DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.")
            .get(self)
            .unwrap_or_else(|| panic!("Could not find a Debug name for module ID {:?}", self))
            .clone()
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
        write!(f, "({:?}, {})", self.name(), self.value)
    }

    /// In relese builds, all we have access to is the number, so only display that.
    #[cfg(not(debug_assertions))]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

#[derive(Debug)]
pub struct ModuleIds {
    by_name: MutMap<ModuleName, ModuleId>,
    by_id: MutMap<ModuleId, ModuleName>,
}

impl Default for ModuleIds {
    fn default() -> Self {
        let capacity = NUM_BUILTIN_MODULES + 1; // You'll be compiling at least 1 module!
        let mut by_name = HashMap::with_capacity_and_hasher(capacity, default_hasher());
        let mut by_id = Vec::with_capacity(capacity);
        let mut names = Vec::with_capacity(capacity);

        let insert_both = |id, name_str: &'static str| {
            let name: ModuleName = name_str.into();
            let index = names.len();

            // It's very important that these are inserted in the correct order!
            debug_assert!(id == index as usize, "When setting up default ModuleIds, module `{:?}` was inserted in the wrong order. It wants to have ID {:?} but was inserted at index {:?}", name_str, _id, names.len());

            by_name.insert(name, index);
            by_id.insert(index, index);
            names.push(name_str.into());
        };

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
        match self.store.get(module_name) {
            Some(symbol) => ModuleId {
                value: *symbol.id(),
            },
            None => {
                let module_id = ModuleId {
                    value: *self.store.get_or_insert(module_name.clone()).unwrap().id(),
                };

                if cfg!(debug_assetions) {
                    Self::insert_debug_name(module_id, &module_name);
                }

                module_id
            }
        }
    }

    #[cfg(debug_assertions)]
    fn insert_debug_name(module_id: ModuleId, module_name: &ModuleName) {
        let mut names = DEBUG_MODULE_ID_NAMES.lock().expect("Failed to acquire lock for Debug interning into DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.");

        names.insert(module_id, module_name.as_str().into());
    }

    #[cfg(not(debug_assertions))]
    fn insert_debug_name(_module_id: ModuleId, _module_name: &ModuleName) {
        // By design, this is a no-op in release builds!
    }

    pub fn get_id(&self, module_name: &ModuleName) -> Option<ModuleId> {
        self.store.get(module_name).map(|symbol| ModuleId {
            value: *symbol.id(),
        })
    }

    pub fn get_name(&self, id: ModuleId) -> Option<&ModuleName> {
        self.store.get_symbol(&id.value).map(Symbol::data)
    }
}
