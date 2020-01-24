use crate::can::ident::ModuleName;
use crate::collections::{default_hasher, MutMap};
use inlinable_string::InlinableString;
use std::collections::HashMap;
use std::{fmt, u32};

pub const NUM_BUILTIN_MODULES: usize = 12;

// TODO: benchmark this as { ident_id: u32, module_id: u32 } and see if perf stays the same
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(u64);

/// In Debug builds only, Symbol has a name() method that lets
/// you look up its name in a global intern table. This table is
/// behind a mutex, so it is neither populated nor available in release builds.
impl Symbol {
    // Attr
    pub const ATTR_ATTR: Symbol = Symbol::new(ModuleId::ATTR, IdentId::ATTR_ATTR);

    // Str
    pub const STR_STR: Symbol = Symbol::new(ModuleId::STR, IdentId::STR_STR);

    // Num
    pub const NUM_ABS: Symbol = Symbol::new(ModuleId::NUM, IdentId::NUM_ABS);
    pub const NUM_NUM: Symbol = Symbol::new(ModuleId::NUM, IdentId::NUM_NUM);
    pub const NUM_INT: Symbol = Symbol::new(ModuleId::NUM, IdentId::INT_INT);
    pub const INT_INTEGER: Symbol = Symbol::new(ModuleId::NUM, IdentId::INT_INTEGER);
    pub const NUM_FLOAT: Symbol = Symbol::new(ModuleId::NUM, IdentId::FLOAT_FLOAT);
    pub const FLOAT_FLOATINGPOINT: Symbol =
        Symbol::new(ModuleId::NUM, IdentId::FLOAT_FLOATINGPOINT);

    // Bool
    pub const BOOL_NOT: Symbol = Symbol::new(ModuleId::BOOL, IdentId::BOOL_NOT);
    pub const BOOL_BOOL: Symbol = Symbol::new(ModuleId::BOOL, IdentId::BOOL_BOOL);

    // List
    pub const LIST_LIST: Symbol = Symbol::new(ModuleId::LIST, IdentId::LIST_LIST);

    pub const fn new(module_id: ModuleId, ident_id: IdentId) -> Symbol {
        // The bit layout of the u64 inside a Symbol is:
        //
        // |------ 32 bits -----|------ 32 bits -----|
        // |      ident_id      |      module_id     |
        // |--------------------|--------------------|
        //
        // module_id comes second because we need to query it more often,
        // and this way we can get it by truncating the u64 to u32,
        // whereas accessing the first slot requires a bit shift first.
        let bits = ((ident_id.0 as u64) << 32) | (module_id.0 as u64);

        Symbol(bits)
    }

    pub fn module_id(&self) -> ModuleId {
        ModuleId(self.0 as u32)
    }

    pub fn ident_id(&self) -> IdentId {
        IdentId((self.0 >> 32) as u32)
    }

    pub fn module_string<'a>(&self, interns: &'a Interns) -> &'a InlinableString {
        interns
            .module_ids
            .get_name(self.module_id())
            .unwrap_or_else(|| panic!("Could not find IdentIds for {:?}", self.module_id()))
    }

    pub fn ident_string<'a>(&self, interns: &'a Interns) -> &'a InlinableString {
        let ident_ids = interns
            .all_ident_ids
            .get(&self.module_id())
            .unwrap_or_else(|| panic!("Could not find IdentIds for {:?}", self.module_id()));

        ident_ids.get_name(self.ident_id()).unwrap_or_else(|| {
            panic!(
                "Could not find IdentIds for {:?} in module {:?}",
                self.ident_id(),
                self.module_id()
            )
        })
    }

    pub fn fully_qualified<'a>(&self, interns: &'a Interns, home: ModuleId) -> InlinableString {
        let module_id = self.module_id();

        if module_id == home {
            self.ident_string(interns).clone()
        } else {
            format!(
                "{}.{}",
                self.module_string(interns),
                self.ident_string(interns)
            )
            .into()
        }
    }

    pub fn emit(self) -> InlinableString {
        format!("${}", self.0).into()
    }
}

/// Rather than displaying as this:
///
/// Symbol("Foo.bar")
///
/// ...instead display as this:
///
/// 'Foo.bar'
impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}'", self.0)
    }
}

// TODO this is only here to prevent clippy from complaining about an unused
// #[macro_use] on lazy_statc in --release builds, because as of January 2020,
// we only use lazy_static in the debug configuration. If we ever start using
// lazy_static in release builds, this do-nothing macro invocation will be safe to delete!
//
// There's probably also a way to get clippy to stop complaining about the unused
// #[macro_use] but it didn't seem worth the effort since probably someday we'll
// end up using it in release builds anyway. Right? ...Right?
lazy_static! {}

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

pub struct Interns {
    pub module_ids: ModuleIds,
    pub all_ident_ids: MutMap<ModuleId, IdentIds>,
}

/// A globally unique ID that gets assigned to each module as it is loaded.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId(u32);

/// In Debug builds only, ModuleId has a name() method that lets
/// you look up its name in a global intern table. This table is
/// behind a mutex, so it is neither populated nor available in release builds.
impl ModuleId {
    // NOTE: Always add constants to the *end* of this list (with a unique integer),
    // and then also go to `impl Default for ModuleIds` and incorporate the new constant
    // into the *end* of its default module insertions. Insertion order matters for those!
    pub const STR: ModuleId = ModuleId(0);
    pub const BOOL: ModuleId = ModuleId(1);
    pub const INT: ModuleId = ModuleId(2);
    pub const FLOAT: ModuleId = ModuleId(3);
    pub const LIST: ModuleId = ModuleId(4);
    pub const MAP: ModuleId = ModuleId(5);
    pub const SET: ModuleId = ModuleId(6);
    pub const NUM: ModuleId = ModuleId(7);
    pub const ATTR: ModuleId = ModuleId(u32::MAX);

    #[cfg(debug_assertions)]
    pub fn name(self) -> Box<str> {
        let names =
        DEBUG_MODULE_ID_NAMES
            .lock()
            .expect("Failed to acquire lock for Debug reading from DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.");

        match names.get(&self.0) {
            Some(str_ref) => str_ref.clone(),
            None => {
                panic!(
                    "Could not find a Debug name for module ID {} in {:?}",
                    self.0, names,
                );
            }
        }
    }

    pub fn to_string<'a>(&self, interns: &'a Interns) -> &'a InlinableString {
        interns
            .module_ids
            .get_name(*self)
            .unwrap_or_else(|| panic!("Could not find ModuleIds for {:?}", self))
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
        self.0.fmt(f)
    }
}

/// Stores a mapping between ModuleId and InlinableString.
///
/// Each module name is stored twice, for faster lookups.
/// Since these are interned strings, this shouldn't result in many total allocations in practice.
#[derive(Debug, Clone)]
pub struct ModuleIds {
    by_name: MutMap<InlinableString, ModuleId>,
    /// Each ModuleId is an index into this Vec
    by_id: Vec<InlinableString>,
}

impl Default for ModuleIds {
    fn default() -> Self {
        // +1 because the user will be compiling at least 1 non-builtin module!
        let capacity = NUM_BUILTIN_MODULES + 1;

        let mut by_name = HashMap::with_capacity_and_hasher(capacity, default_hasher());
        let mut by_id = Vec::with_capacity(capacity);

        let mut insert_both = |id: ModuleId, name_str: &'static str| {
            let name: InlinableString = name_str.into();

            // It's very important that these are inserted in the correct order!
            debug_assert!(id.0 as usize == by_id.len(), "When setting up default ModuleIds, `{:?}` was inserted in the wrong order. It has a hardcoded ID of {:?} but was inserted at index {:?}", name_str, id.0, by_id.len());

            // Make sure we haven't already inserted an entry for this module name.
            debug_assert!(!by_name.contains_key(&name), "Duplicate default module! We already have an ID for module `{:?}` (namely {:?}), but we tried to insert it again with ID {:?}", name, by_name.get(&name).unwrap(), id.0);

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
    pub fn get_or_insert(&mut self, module_name: &InlinableString) -> ModuleId {
        match self.by_name.get(module_name) {
            Some(id) => *id,
            None => {
                let by_id = &mut self.by_id;
                let module_id = ModuleId(by_id.len() as u32);

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
    fn insert_debug_name(module_id: ModuleId, module_name: &InlinableString) {
        let mut names = DEBUG_MODULE_ID_NAMES.lock().expect("Failed to acquire lock for Debug interning into DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.");

        names.insert(module_id.0, module_name.to_string().into());
    }

    #[cfg(not(debug_assertions))]
    fn insert_debug_name(_module_id: ModuleId, _module_name: &InlinableString) {
        // By design, this is a no-op in release builds!
    }

    pub fn get_id(&self, module_name: &InlinableString) -> Option<&ModuleId> {
        self.by_name.get(module_name)
    }

    pub fn get_name(&self, id: ModuleId) -> Option<&InlinableString> {
        self.by_id.get(id.0 as usize)
    }
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
pub struct IdentId(u32);

impl fmt::Debug for IdentId {
    /// In debug builds, whenever we create a new IdentId, we record is name in
    /// a global interning table so that Debug can look it up later. That table
    /// needs a global mutex, so we don't do this in release builds. This means
    /// the Debug impl in release builds only shows the number, not the name (which
    /// it does not have available, due to having never stored it in the mutexed intern table.)
    #[cfg(debug_assertions)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:?}, {})", self.name(), self.0)
    }

    /// In relese builds, all we have access to is the number, so only display that.
    #[cfg(not(debug_assertions))]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// In Debug builds only, IdentId has a name() method that lets
/// you look up its name in a global intern table. This table is
/// behind a mutex, so it is neither populated nor available in release builds.
impl IdentId {
    // Attr
    pub const ATTR_ATTR: IdentId = IdentId(0);

    // Num
    pub const NUM_ABS: IdentId = IdentId(0);
    pub const NUM_NUM: IdentId = IdentId(1);

    // Int
    pub const INT_INT: IdentId = IdentId(2);
    pub const INT_INTEGER: IdentId = IdentId(3);

    // Float
    pub const FLOAT_FLOAT: IdentId = IdentId(4);
    pub const FLOAT_FLOATINGPOINT: IdentId = IdentId(5);

    // Bool
    pub const BOOL_BOOL: IdentId = IdentId(0);
    pub const BOOL_NOT: IdentId = IdentId(1);

    // Str
    pub const STR_STR: IdentId = IdentId(0);

    // List
    pub const LIST_LIST: IdentId = IdentId(0);

    #[cfg(debug_assertions)]
    pub fn name(self) -> Box<str> {
        let names =
        DEBUG_IDENT_ID_NAMES
            .lock()
            .expect("Failed to acquire lock for Debug reading from DEBUG_IDENT_ID_NAMES, presumably because a thread panicked.");

        match names.get(&self.0) {
            Some(str_ref) => str_ref.clone(),
            None => {
                panic!(
                    "Could not find a Debug name for ident ID {} in {:?}",
                    self.0, names,
                );
            }
        }
    }
}

/// Stores a mapping between IdentId and InlinableString.
///
/// Each module name is stored twice, for faster lookups.
/// Since these are interned strings, this shouldn't result in many total allocations in practice.
#[derive(Debug, Clone, Default)]
pub struct IdentIds {
    /// Only private tag names can be looked up by name.
    private_tag_names: MutMap<InlinableString, IdentId>,

    /// Each IdentId is an index into this Vec
    by_id: Vec<InlinableString>,

    next_generated_name: u32,
}

impl IdentIds {
    pub fn idents(&self) -> impl Iterator<Item = (IdentId, &InlinableString)> {
        self.by_id
            .iter()
            .enumerate()
            .map(|(index, ident)| (IdentId(index as u32), ident))
    }

    pub fn add(&mut self, ident_name: InlinableString) -> IdentId {
        let by_id = &mut self.by_id;
        let ident_id = IdentId(by_id.len() as u32);

        by_id.push(ident_name);

        if cfg!(debug_assertions) {
            Self::insert_debug_name(ident_id, &ident_name);
        }

        ident_id
    }

    /// This is the same as ModuleId::get_or_insert, but with a different name
    /// because for idents this should only ever be used for private tags!
    ///
    /// All other module-scoped idents should only ever be added once, because
    /// otherwise they will either be shadowing or reusing the Ident from
    /// something in a sibling scope - both of which can cause bugs.
    ///
    /// Thus, only ever call this for private tags!
    pub fn private_tag(&mut self, private_tag_name: &InlinableString) -> IdentId {
        match self.private_tag_names.get(private_tag_name) {
            Some(id) => *id,
            None => {
                let by_id = &mut self.by_id;
                let ident_id = IdentId(by_id.len() as u32);

                by_id.push(private_tag_name.clone());

                self.private_tag_names
                    .insert(private_tag_name.clone(), ident_id);

                if cfg!(debug_assertions) {
                    Self::insert_debug_name(ident_id, &private_tag_name);
                }

                ident_id
            }
        }
    }

    /// Generates a unique, new name that's just a strigified integer
    /// (e.g. "1" or "5"), using an internal counter. Since valid Roc variable
    /// names cannot begin with a number, this has no chance of colliding
    /// with actual user-defined variables.
    ///
    /// This is used, for example, during canonicalization of an Expr::Closure
    /// to generate a unique symbol to refer to that closure.
    pub fn gen_unique(&mut self) -> IdentId {
        // TODO convert this directly from u32 into InlinableString,
        // without allocating an extra string along the way like this.
        let ident = self.next_generated_name.to_string().into();

        self.next_generated_name += 1;

        self.add(ident)
    }

    #[cfg(debug_assertions)]
    fn insert_debug_name(ident_id: IdentId, ident_name: &InlinableString) {
        let mut names = DEBUG_IDENT_ID_NAMES.lock().expect("Failed to acquire lock for Debug interning into DEBUG_IDENT_ID_NAMES, presumably because a thread panicked.");

        names.insert(ident_id.0, ident_name.to_string().into());
    }

    #[cfg(not(debug_assertions))]
    fn insert_debug_name(_ident_id: IdentId, _ident_name: &InlinableString) {
        // By design, this is a no-op in release builds!
    }

    pub fn get_id(&self, ident_name: &InlinableString) -> Option<&IdentId> {
        self.private_tag_names.get(ident_name)
    }

    pub fn get_name(&self, id: IdentId) -> Option<&InlinableString> {
        self.by_id.get(id.0 as usize)
    }
}
