use crate::collections::{default_hasher, MutMap};
use inlinable_string::InlinableString;
use std::collections::HashMap;
use std::sync::Arc;
use std::{fmt, u32};

// TODO: benchmark this as { ident_id: u32, module_id: u32 } and see if perf stays the same
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(u64);

/// In Debug builds only, Symbol has a name() method that lets
/// you look up its name in a global intern table. This table is
/// behind a mutex, so it is neither populated nor available in release builds.
impl Symbol {
    // NOTE: the define_builtins! macro adds a bunch of constants to this impl,
    //
    // e.g. pub const NUM_NUM: Symbol = …

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

    pub fn module_id(self) -> ModuleId {
        ModuleId(self.0 as u32)
    }

    pub fn ident_id(self) -> IdentId {
        IdentId((self.0 >> 32) as u32)
    }

    pub fn module_string<'a>(&self, interns: &'a Interns) -> &'a InlinableString {
        interns
            .module_ids
            .get_name(self.module_id())
            .unwrap_or_else(|| {
                panic!(
                    "module_string could not find IdentIds for {:?}",
                    self.module_id()
                )
            })
    }

    pub fn ident_string(self, interns: &Interns) -> &InlinableString {
        let ident_ids = interns
            .all_ident_ids
            .get(&self.module_id())
            .unwrap_or_else(|| {
                panic!(
                    "ident_string could not find IdentIds for {:?}",
                    self.module_id()
                )
            });

        ident_ids.get_name(self.ident_id()).unwrap_or_else(|| {
            panic!(
                "Could not find IdentIds for {} in module {:?}",
                self.ident_id().0,
                self.module_id()
            )
        })
    }

    pub fn fully_qualified(self, interns: &Interns, home: ModuleId) -> InlinableString {
        let module_id = self.module_id();

        if module_id == home {
            self.ident_string(interns).clone()
        } else {
            // TODO do this without format! to avoid allocation for short strings
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
/// `Foo.bar`
impl fmt::Debug for Symbol {
    #[cfg(debug_assertions)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let module_id = self.module_id();
        let ident_id = self.ident_id();
        let names =
        DEBUG_IDENT_IDS_BY_MODULE_ID
            .lock()
            .expect("Failed to acquire lock for Debug reading from DEBUG_IDENT_IDS_BY_MODULE_ID, presumably because a thread panicked.");
        let ident_ids = &names.get(&module_id.0).unwrap_or_else(|| {
            panic!(
                "Could not find module {:?} in DEBUG_IDENT_IDS_BY_MODULE_ID",
                module_id
            )
        });
        let ident_str = ident_ids.get_name(ident_id).unwrap_or_else(|| {
            panic!(
                "Could not find IdentID {} in DEBUG_IDENT_IDS_BY_MODULE_ID for module ID {:?}",
                ident_id.0, module_id
            )
        });

        write!(f, "`{:?}.{}`", module_id, ident_str)
    }

    #[cfg(not(debug_assertions))]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let module_id = self.module_id();
        let ident_id = self.ident_id();

        write!(f, "`{:?}.{:?}`", module_id, ident_id)
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
    static ref DEBUG_MODULE_ID_NAMES: std::sync::Mutex<crate::collections::MutMap<u32, Box<str>>> =
        // This stores a u32 key instead of a ModuleId key so that if there's
        // a problem with ModuleId's Debug implementation, logging this for diagnostic
        // purposes won't recursively trigger ModuleId's Debug instance in the course of printing
        // this out.
        std::sync::Mutex::new(crate::collections::MutMap::default());
}

#[derive(Debug)]
pub struct Interns {
    pub module_ids: ModuleIds,
    pub all_ident_ids: MutMap<ModuleId, IdentIds>,
}

#[cfg(debug_assertions)]
lazy_static! {
    /// This is used in Debug builds only, to let us have a Debug instance
    /// which displays not only the Module ID, but also the Module Name which
    /// corresponds to that ID.
    static ref DEBUG_IDENT_IDS_BY_MODULE_ID: std::sync::Mutex<crate::collections::MutMap<u32, IdentIds>> =
        // This stores a u32 key instead of a ModuleId key so that if there's
        // a problem with ModuleId's Debug implementation, logging this for diagnostic
        // purposes won't recursively trigger ModuleId's Debug instance in the course of printing
        // this out.
        std::sync::Mutex::new(crate::collections::MutMap::default());
}

/// A globally unique ID that gets assigned to each module as it is loaded.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId(u32);

/// In Debug builds only, ModuleId has a name() method that lets
/// you look up its name in a global intern table. This table is
/// behind a mutex, so it is neither populated nor available in release builds.
impl ModuleId {
    // NOTE: the define_builtins! macro adds a bunch of constants to this impl,
    //
    // e.g. pub const NUM: ModuleId = …

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

    #[cfg(debug_assertions)]
    pub fn register_debug_idents(self, ident_ids: &IdentIds) {
        let mut all = DEBUG_IDENT_IDS_BY_MODULE_ID.lock().expect("Failed to acquire lock for Debug interning into DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.");

        all.insert(self.0, ident_ids.clone());
    }

    #[cfg(not(debug_assertions))]
    pub fn register_debug_idents(self, _ident_ids: &IdentIds) {
        // This is a no-op that should get DCE'd
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

/// An ID that is assigned to interned string identifiers within a module.
/// By turning these strings into numbers, post-canonicalization processes
/// like unification and optimization can run a lot faster.
///
/// This ID is unique within a given module, not globally - so to turn this back into
/// a string, you would need a ModuleId, an IdentId, and a Map<ModuleId, Map<IdentId, String>>.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct IdentId(u32);

/// Stores a mapping between IdentId and InlinableString.
///
/// Each module name is stored twice, for faster lookups.
/// Since these are interned strings, this shouldn't result in many total allocations in practice.
#[derive(Clone, Debug, Default)]
pub struct IdentIds {
    by_ident: MutMap<InlinableString, IdentId>,

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

        self.by_ident.insert(ident_name.clone(), ident_id);
        by_id.push(ident_name);

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
    pub fn get_or_insert(&mut self, name: &InlinableString) -> IdentId {
        match self.by_ident.get(name) {
            Some(id) => *id,
            None => {
                let by_id = &mut self.by_id;
                let ident_id = IdentId(by_id.len() as u32);

                by_id.push(name.clone());

                self.by_ident.insert(name.clone(), ident_id);

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

    pub fn get_id(&self, ident_name: &InlinableString) -> Option<&IdentId> {
        self.by_ident.get(ident_name)
    }

    pub fn get_name(&self, id: IdentId) -> Option<&InlinableString> {
        self.by_id.get(id.0 as usize)
    }
}

// BUILTINS

macro_rules! define_builtins {
    {
        $(
            $module_id:literal $module_const:ident: $module_name:literal => {
                $(
                    $ident_id:literal $ident_const:ident: $ident_name:literal
                )+
            }
        )+
        num_modules: $total:literal
    } => {
        impl IdentIds {
            pub fn exposed_builtins() -> MutMap<ModuleId, Arc<IdentIds>> {
                let mut exposed_idents_by_module = MutMap::default();

                $(
                    debug_assert!(!exposed_idents_by_module.contains_key(&ModuleId($module_id)), "Error setting up Builtins: when setting up module {} {:?} - the module ID {} is already present in the map. Check the map for duplicate module IDs!", $module_id, $module_name, $module_id);

                    let ident_ids = {
                            let by_id = vec! [
                                $(
                                    $ident_name.into(),
                                )+
                            ];
                            let mut by_ident = MutMap::default();

                            $(
                                debug_assert!(!by_ident.contains_key($ident_name.clone().into()), "Error setting up Builtins: when inserting {} …: {:?} into module {} …: {:?} - the Ident name {:?} is already present in the map. Check the map for duplicate ident names within the {:?} module!", $ident_id, $ident_name, $module_id, $module_name, $ident_name, $module_name);
                                debug_assert!(by_ident.len() == $ident_id, "Error setting up Builtins: when inserting {} …: {:?} into module {} …: {:?} - this entry was assigned an ID of {}, but based on insertion order, it should have had an ID of {} instead! To fix this, change it from {} …: {:?} to {} …: {:?} instead.", $ident_id, $ident_name, $module_id, $module_name, $ident_id, by_ident.len(), $ident_id, $ident_name, by_ident.len(), $ident_name);

                                by_ident.insert($ident_name.into(), IdentId($ident_id));
                            )+

                            IdentIds {
                                by_ident,
                                by_id,
                                next_generated_name: 0,
                            }
                        };

                    if cfg!(debug_assertions) {
                        let module_id = ModuleId($module_id);

                        ModuleIds::insert_debug_name(module_id, &$module_name.into());
                        module_id.register_debug_idents(&ident_ids);
                    }

                    exposed_idents_by_module.insert(
                        ModuleId($module_id),
                        Arc::new(ident_ids)
                    );
                )+

                debug_assert!(exposed_idents_by_module.len() == $total, "Error setting up Builtins: `total:` is set to the wrong amount. It was set to {} but {} modules were set up.", $total, exposed_idents_by_module.len());

                exposed_idents_by_module
            }
        }

        impl ModuleId {
            $(
                pub const $module_const: ModuleId = ModuleId($module_id);
            )+
        }

        impl Default for ModuleIds {
            fn default() -> Self {
                // +1 because the user will be compiling at least 1 non-builtin module!
                let capacity = $total + 1;

                let mut by_name = HashMap::with_capacity_and_hasher(capacity, default_hasher());
                let mut by_id = Vec::with_capacity(capacity);

                let mut insert_both = |id: ModuleId, name_str: &'static str| {
                    let name: InlinableString = name_str.into();

                    if cfg!(debug_assertions) {
                        Self::insert_debug_name(id, &name);
                    }

                    by_name.insert(name.clone(), id);
                    by_id.push(name);
                };

                $(
                    insert_both(ModuleId($module_id), $module_name);
                )+

                ModuleIds { by_name, by_id }
            }
        }

        impl Symbol {
            $(
                $(
                    pub const $ident_const: Symbol = Symbol::new(ModuleId($module_id), IdentId($ident_id));
                )+
            )+

        }
    };
}

define_builtins! {
    0 ATTR: "Attr" => {
        0 ATTR_ATTR: "Attr" // the Attr.Attr type alias, used in uniqueness types
    }
    1 NUM: "Num" => {
        0 NUM_NUM: "Num" // the Num.Num type alias
        1 NUM_ABS: "abs"
        2 NUM_ADD: "add"
        3 NUM_SUB: "sub"
        4 NUM_MUL: "mul"
    }
    2 INT: "Int" => {
        0 INT_INT: "Int" // the Int.Int type alias
        1 INT_INTEGER: "Integer" // Int : Num Integer
        2 INT_DIV: "div"
    }
    3 FLOAT: "Float" => {
        0 FLOAT_FLOAT: "Float" // the Float.Float type alias
        1 FLOAT_FLOATINGPOINT: "FloatingPoint" // Float : Num FloatingPoint
        2 FLOAT_DIV: "div"
    }
    4 BOOL: "Bool" => {
        0 BOOL_BOOL: "Bool" // the Bool.Bool type alias
        1 BOOL_AND: "and"
        2 BOOL_OR: "or"
    }
    5 STR: "Str" => {
        0 STR_STR: "Str" // the Str.Str type alias
        1 STR_ISEMPTY: "isEmpty"
    }
    6 LIST: "List" => {
        0 LIST_LIST: "List" // the List.List type alias
        1 LIST_ISEMPTY: "isEmpty"
    }

    num_modules: 7 // Keep this count up to date by hand! (Rust macros can't do arithmetic.)
}
