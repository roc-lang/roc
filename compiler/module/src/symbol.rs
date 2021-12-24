use crate::ident::{Ident, ModuleName};
use crate::module_err::{IdentIdNotFound, ModuleIdNotFound, ModuleResult};
use roc_collections::all::{default_hasher, MutMap, SendMap};
use roc_ident::IdentStr;
use roc_region::all::Region;
use snafu::OptionExt;
use std::collections::HashMap;
use std::{fmt, u32};

// TODO: benchmark this as { ident_id: u32, module_id: u32 } and see if perf stays the same
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(u64);

// When this is `true` (which it normally should be), Symbol's Debug::fmt implementation
// attempts to pretty print debug symbols using interns recorded using
// register_debug_idents calls (which should be made in debug mode).
// Set it to false if you want to see the raw ModuleId and IdentId ints,
// but please set it back to true before checking in the result!
#[cfg(debug_assertions)]
const PRETTY_PRINT_DEBUG_SYMBOLS: bool = true;

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

    pub fn is_builtin(self) -> bool {
        self.module_id().is_builtin()
    }

    pub fn module_string<'a>(&self, interns: &'a Interns) -> &'a ModuleName {
        interns
            .module_ids
            .get_name(self.module_id())
            .unwrap_or_else(|| {
                panic!(
                    "module_string could not find IdentIds for module {:?} in {:?}",
                    self.module_id(),
                    interns
                )
            })
    }

    pub fn as_str(self, interns: &Interns) -> &str {
        self.ident_str(interns).as_str()
    }

    pub fn ident_str(self, interns: &Interns) -> &IdentStr {
        let ident_ids = interns
            .all_ident_ids
            .get(&self.module_id())
            .unwrap_or_else(|| {
                panic!(
                    "ident_string could not find IdentIds for module {:?} in {:?}",
                    self.module_id(),
                    interns
                )
            });

        ident_ids
            .get_name(self.ident_id())
            .unwrap_or_else(|| {
                panic!(
                    "ident_string's IdentIds did not contain an entry for {} in module {:?}",
                    self.ident_id().0,
                    self.module_id()
                )
            })
            .into()
    }

    pub fn as_u64(self) -> u64 {
        self.0
    }

    pub fn fully_qualified(self, interns: &Interns, home: ModuleId) -> ModuleName {
        let module_id = self.module_id();

        if module_id == home {
            self.ident_str(interns).clone().into()
        } else {
            // TODO do this without format! to avoid allocation for short strings
            format!(
                "{}.{}",
                self.module_string(interns).as_str(),
                self.ident_str(interns)
            )
            .into()
        }
    }

    pub const fn to_ne_bytes(self) -> [u8; 8] {
        self.0.to_ne_bytes()
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
        if PRETTY_PRINT_DEBUG_SYMBOLS {
            let module_id = self.module_id();
            let ident_id = self.ident_id();

            match DEBUG_IDENT_IDS_BY_MODULE_ID.lock() {
                Ok(names) => match &names.get(&module_id.0) {
                    Some(ident_ids) => match ident_ids.get_name(ident_id) {
                        Some(ident_str) => write!(f, "`{:?}.{}`", module_id, ident_str),
                        None => fallback_debug_fmt(*self, f),
                    },
                    None => fallback_debug_fmt(*self, f),
                },
                Err(err) => {
                    // Print and return Err rather than panicking, because this
                    // might be used in a panic error message, and if we panick
                    // while we're already panicking it'll kill the process
                    // without printing any of the errors!
                    println!("DEBUG INFO: Failed to acquire lock for Debug reading from DEBUG_IDENT_IDS_BY_MODULE_ID, presumably because a thread panicked: {:?}", err);

                    fallback_debug_fmt(*self, f)
                }
            }
        } else {
            fallback_debug_fmt(*self, f)
        }
    }

    #[cfg(not(debug_assertions))]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fallback_debug_fmt(*self, f)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let module_id = self.module_id();
        let ident_id = self.ident_id();

        match ident_id {
            IdentId(value) => write!(f, "{:?}.{:?}", module_id, value),
        }
    }
}

impl From<Symbol> for u64 {
    fn from(symbol: Symbol) -> Self {
        symbol.0
    }
}

fn fallback_debug_fmt(symbol: Symbol, f: &mut fmt::Formatter) -> fmt::Result {
    let module_id = symbol.module_id();
    let ident_id = symbol.ident_id();

    write!(f, "`{:?}.{:?}`", module_id, ident_id)
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
    static ref DEBUG_MODULE_ID_NAMES: std::sync::Mutex<roc_collections::all::MutMap<u32, Box<str>>> =
        // This stores a u32 key instead of a ModuleId key so that if there's
        // a problem with ModuleId's Debug implementation, logging this for diagnostic
        // purposes won't recursively trigger ModuleId's Debug instance in the course of printing
        // this out.
        std::sync::Mutex::new(roc_collections::all::MutMap::default());
}

#[derive(Debug, Default)]
pub struct Interns {
    pub module_ids: ModuleIds,
    pub all_ident_ids: MutMap<ModuleId, IdentIds>,
}

impl Interns {
    pub fn module_id(&mut self, name: &ModuleName) -> ModuleId {
        self.module_ids.get_or_insert(name)
    }

    pub fn module_name(&self, module_id: ModuleId) -> &ModuleName {
        self.module_ids.get_name(module_id).unwrap_or_else(|| {
            panic!(
                "Unable to find interns entry for module_id {:?} in Interns {:?}",
                module_id, self
            )
        })
    }

    pub fn symbol(&self, module_id: ModuleId, ident: IdentStr) -> Symbol {
        let ident: Ident = ident.into();

        match self.all_ident_ids.get(&module_id) {
            Some(ident_ids) => match ident_ids.get_id(&ident) {
                Some(ident_id) => Symbol::new(module_id, *ident_id),
                None => {
                    panic!("Interns::symbol could not find ident entry for {:?} for module {:?} in Interns {:?}", ident, module_id, self);
                }
            },
            None => {
                panic!(
                    "Interns::symbol could not find entry for module {:?} in Interns {:?}",
                    module_id, self
                );
            }
        }
    }

    pub fn from_index(module_id: ModuleId, ident_id: u32) -> Symbol {
        Symbol::new(module_id, IdentId(ident_id))
    }
}

pub fn get_module_ident_ids<'a>(
    all_ident_ids: &'a MutMap<ModuleId, IdentIds>,
    module_id: &ModuleId,
) -> ModuleResult<&'a IdentIds> {
    all_ident_ids
        .get(module_id)
        .with_context(|| ModuleIdNotFound {
            module_id: format!("{:?}", module_id),
            all_ident_ids: format!("{:?}", all_ident_ids),
        })
}

pub fn get_module_ident_ids_mut<'a>(
    all_ident_ids: &'a mut MutMap<ModuleId, IdentIds>,
    module_id: &ModuleId,
) -> ModuleResult<&'a mut IdentIds> {
    all_ident_ids
        .get_mut(module_id)
        .with_context(|| ModuleIdNotFound {
            module_id: format!("{:?}", module_id),
            all_ident_ids: "I could not return all_ident_ids here because of borrowing issues.",
        })
}

#[cfg(debug_assertions)]
lazy_static! {
    /// This is used in Debug builds only, to let us have a Debug instance
    /// which displays not only the Module ID, but also the Module Name which
    /// corresponds to that ID.
    static ref DEBUG_IDENT_IDS_BY_MODULE_ID: std::sync::Mutex<roc_collections::all::MutMap<u32, IdentIds>> =
        // This stores a u32 key instead of a ModuleId key so that if there's
        // a problem with ModuleId's Debug implementation, logging this for diagnostic
        // purposes won't recursively trigger ModuleId's Debug instance in the course of printing
        // this out.
        std::sync::Mutex::new(roc_collections::all::MutMap::default());
}

/// A globally unique ID that gets assigned to each module as it is loaded.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId(u32);

impl ModuleId {
    // NOTE: the define_builtins! macro adds a bunch of constants to this impl,
    //
    // e.g. pub const NUM: ModuleId = …

    #[cfg(debug_assertions)]
    pub fn register_debug_idents(self, ident_ids: &IdentIds) {
        let mut all = DEBUG_IDENT_IDS_BY_MODULE_ID.lock().expect("Failed to acquire lock for Debug interning into DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.");

        all.insert(self.0, ident_ids.clone());
    }

    #[cfg(not(debug_assertions))]
    pub fn register_debug_idents(self, _ident_ids: &IdentIds) {
        // This is a no-op that should get DCE'd
    }

    pub fn to_ident_str(self, interns: &Interns) -> &ModuleName {
        interns
            .module_ids
            .get_name(self)
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
        let names =
            DEBUG_MODULE_ID_NAMES
                .lock()
                .expect("Failed to acquire lock for Debug reading from DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.");

        if PRETTY_PRINT_DEBUG_SYMBOLS {
            match names.get(&self.0) {
                Some(str_ref) => write!(f, "{}", str_ref.clone()),
                None => {
                    panic!(
                        "Could not find a Debug name for module ID {} in {:?}",
                        self.0, names,
                    );
                }
            }
        } else {
            write!(f, "{}", self.0)
        }
    }

    /// In release builds, all we have access to is the number, so only display that.
    #[cfg(not(debug_assertions))]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// pf.Task
/// 1. build mapping from short name to package
/// 2. when adding new modules from package we need to register them in some other map (this module id goes with short name) (shortname, module-name) -> moduleId
/// 3. pass this around to other modules getting headers parsed. when parsing interfaces we need to use this map to reference shortnames
/// 4. throw away short names. stash the module id in the can env under the resolved module name
/// 5. test:

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PackageQualified<'a, T> {
    Unqualified(T),
    Qualified(&'a str, T),
}

/// Package-qualified module name
pub type PQModuleName<'a> = PackageQualified<'a, ModuleName>;

impl<'a, T> PackageQualified<'a, T> {
    pub fn as_inner(&self) -> &T {
        match self {
            PackageQualified::Unqualified(name) => name,
            PackageQualified::Qualified(_, name) => name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PackageModuleIds<'a> {
    by_name: MutMap<PQModuleName<'a>, ModuleId>,
    by_id: Vec<PQModuleName<'a>>,
}

impl<'a> PackageModuleIds<'a> {
    pub fn get_or_insert(&mut self, module_name: &PQModuleName<'a>) -> ModuleId {
        match self.by_name.get(module_name) {
            Some(id) => *id,
            None => {
                let by_id = &mut self.by_id;
                let module_id = ModuleId(by_id.len() as u32);

                by_id.push(module_name.clone());

                self.by_name.insert(module_name.clone(), module_id);

                if cfg!(debug_assertions) {
                    Self::insert_debug_name(module_id, module_name);
                }

                module_id
            }
        }
    }

    pub fn into_module_ids(self) -> ModuleIds {
        let by_name: MutMap<ModuleName, ModuleId> = self
            .by_name
            .into_iter()
            .map(|(pqname, module_id)| (pqname.as_inner().clone(), module_id))
            .collect();

        let by_id: Vec<ModuleName> = self
            .by_id
            .into_iter()
            .map(|pqname| pqname.as_inner().clone())
            .collect();

        ModuleIds { by_name, by_id }
    }

    #[cfg(debug_assertions)]
    fn insert_debug_name(module_id: ModuleId, module_name: &PQModuleName) {
        let mut names = DEBUG_MODULE_ID_NAMES.lock().expect("Failed to acquire lock for Debug interning into DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.");

        names
            .entry(module_id.0)
            .or_insert_with(|| match module_name {
                PQModuleName::Unqualified(module) => module.as_str().into(),
                PQModuleName::Qualified(package, module) => {
                    let name = format!("{}.{}", package, module.as_str()).into();
                    name
                }
            });
    }

    #[cfg(not(debug_assertions))]
    fn insert_debug_name(_module_id: ModuleId, _module_name: &PQModuleName) {
        // By design, this is a no-op in release builds!
    }

    pub fn get_id(&self, module_name: &PQModuleName<'a>) -> Option<&ModuleId> {
        self.by_name.get(module_name)
    }

    pub fn get_name(&self, id: ModuleId) -> Option<&PQModuleName> {
        self.by_id.get(id.0 as usize)
    }

    pub fn available_modules(&self) -> impl Iterator<Item = &PQModuleName> {
        self.by_id.iter()
    }
}

/// Stores a mapping between ModuleId and InlinableString.
///
/// Each module name is stored twice, for faster lookups.
/// Since these are interned strings, this shouldn't result in many total allocations in practice.
#[derive(Debug, Clone)]
pub struct ModuleIds {
    by_name: MutMap<ModuleName, ModuleId>,
    /// Each ModuleId is an index into this Vec
    by_id: Vec<ModuleName>,
}

impl ModuleIds {
    pub fn get_or_insert(&mut self, module_name: &ModuleName) -> ModuleId {
        match self.by_name.get(module_name) {
            Some(id) => *id,
            None => {
                let by_id = &mut self.by_id;
                let module_id = ModuleId(by_id.len() as u32);

                by_id.push(module_name.clone());

                self.by_name.insert(module_name.clone(), module_id);

                if cfg!(debug_assertions) {
                    Self::insert_debug_name(module_id, module_name);
                }

                module_id
            }
        }
    }

    #[cfg(debug_assertions)]
    fn insert_debug_name(module_id: ModuleId, module_name: &ModuleName) {
        let mut names = DEBUG_MODULE_ID_NAMES.lock().expect("Failed to acquire lock for Debug interning into DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.");

        // TODO make sure modules are never added more than once!
        names
            .entry(module_id.0)
            .or_insert_with(|| module_name.as_str().to_string().into());
    }

    #[cfg(not(debug_assertions))]
    fn insert_debug_name(_module_id: ModuleId, _module_name: &ModuleName) {
        // By design, this is a no-op in release builds!
    }

    pub fn get_id(&self, module_name: &ModuleName) -> Option<&ModuleId> {
        self.by_name.get(module_name)
    }

    pub fn get_name(&self, id: ModuleId) -> Option<&ModuleName> {
        self.by_id.get(id.0 as usize)
    }

    pub fn available_modules(&self) -> impl Iterator<Item = &ModuleName> {
        self.by_id.iter()
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
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct IdentIds {
    by_ident: MutMap<Ident, IdentId>,

    /// Each IdentId is an index into this Vec
    by_id: Vec<Ident>,

    next_generated_name: u32,
}

impl IdentIds {
    pub fn idents(&self) -> impl Iterator<Item = (IdentId, &Ident)> {
        self.by_id
            .iter()
            .enumerate()
            .map(|(index, ident)| (IdentId(index as u32), ident))
    }

    pub fn add(&mut self, ident_name: Ident) -> IdentId {
        let by_id = &mut self.by_id;
        let ident_id = IdentId(by_id.len() as u32);

        self.by_ident.insert(ident_name.clone(), ident_id);
        by_id.push(ident_name);

        ident_id
    }

    pub fn get_or_insert(&mut self, name: &Ident) -> IdentId {
        use std::collections::hash_map::Entry;

        match self.by_ident.entry(name.clone()) {
            Entry::Occupied(occupied) => *occupied.get(),
            Entry::Vacant(vacant) => {
                let by_id = &mut self.by_id;
                let ident_id = IdentId(by_id.len() as u32);

                by_id.push(name.clone());

                vacant.insert(ident_id);

                ident_id
            }
        }
    }

    // necessary when the name of a value is changed in the editor
    pub fn update_key(
        &mut self,
        old_ident_name: &str,
        new_ident_name: &str,
    ) -> Result<IdentId, String> {
        let old_ident: Ident = old_ident_name.into();

        let ident_id_ref_opt = self.by_ident.get(&old_ident);

        match ident_id_ref_opt {
            Some(ident_id_ref) => {
                let ident_id = *ident_id_ref;

                self.by_ident.remove(&old_ident);
                self.by_ident.insert(new_ident_name.into(), ident_id);

                let by_id = &mut self.by_id;
                let key_index_opt = by_id.iter().position(|x| *x == old_ident);

                if let Some(key_index) = key_index_opt {
                    if let Some(vec_elt) = by_id.get_mut(key_index) {
                        *vec_elt = new_ident_name.into();
                    } else {
                        // we get the index from by_id
                        unreachable!()
                    }

                    Ok(ident_id)
                } else {
                    Err(
                        format!(
                            "Tried to find position of key {:?} in IdentIds.by_id but I could not find the key. IdentIds.by_id: {:?}",
                            old_ident_name,
                            self.by_id
                        )
                    )
                }
            }
            None => Err(format!(
                "Tried to update key in IdentIds ({:?}) but I could not find the key ({}).",
                self.by_ident, old_ident_name
            )),
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
        // TODO convert this directly from u32 into IdentStr,
        // without allocating an extra string along the way like this.
        let ident = self.next_generated_name.to_string().into();

        self.next_generated_name += 1;

        self.add(ident)
    }

    pub fn get_id(&self, ident_name: &Ident) -> Option<&IdentId> {
        self.by_ident.get(ident_name)
    }

    pub fn get_name(&self, id: IdentId) -> Option<&Ident> {
        self.by_id.get(id.0 as usize)
    }

    pub fn get_name_str_res(&self, ident_id: IdentId) -> ModuleResult<&str> {
        Ok(self
            .get_name(ident_id)
            .with_context(|| IdentIdNotFound {
                ident_id,
                ident_ids_str: format!("{:?}", self),
            })?
            .as_inline_str()
            .as_str())
    }
}

// BUILTINS

macro_rules! define_builtins {
    {
        $(
            $module_id:literal $module_const:ident: $module_name:literal => {
                $(
                    $ident_id:literal $ident_const:ident: $ident_name:literal $($imported:ident)?
                )+
            }
        )+
        num_modules: $total:literal
    } => {
        impl IdentIds {
            pub fn exposed_builtins(extra_capacity: usize) -> MutMap<ModuleId, IdentIds> {
                let mut exposed_idents_by_module = HashMap::with_capacity_and_hasher(extra_capacity + $total, default_hasher());

                $(
                    debug_assert!(!exposed_idents_by_module.contains_key(&ModuleId($module_id)), "Error setting up Builtins: when setting up module {} {:?} - the module ID {} is already present in the map. Check the map for duplicate module IDs!", $module_id, $module_name, $module_id);

                    let ident_ids = {
                            let by_id = vec! [
                                $(
                                    $ident_name.into(),
                                )+
                            ];
                            let mut by_ident = MutMap::with_capacity_and_hasher(by_id.len(), default_hasher());

                            $(
                                debug_assert!(by_ident.len() == $ident_id, "Error setting up Builtins: when inserting {} …: {:?} into module {} …: {:?} - this entry was assigned an ID of {}, but based on insertion order, it should have had an ID of {} instead! To fix this, change it from {} …: {:?} to {} …: {:?} instead.", $ident_id, $ident_name, $module_id, $module_name, $ident_id, by_ident.len(), $ident_id, $ident_name, by_ident.len(), $ident_name);

                                let exists = by_ident.insert($ident_name.into(), IdentId($ident_id));

                                if let Some(_) = exists {
                                    debug_assert!(false, "Error setting up Builtins: when inserting {} …: {:?} into module {} …: {:?} - the Ident name {:?} is already present in the map. Check the map for duplicate ident names within the {:?} module!", $ident_id, $ident_name, $module_id, $module_name, $ident_name, $module_name);
                                }
                            )+

                            IdentIds {
                                by_ident,
                                by_id,
                                next_generated_name: 0,
                            }
                        };

                    if cfg!(debug_assertions) {
                        let module_id = ModuleId($module_id);

                        let name = PQModuleName::Unqualified($module_name.into());
                        PackageModuleIds::insert_debug_name(module_id, &name);
                        module_id.register_debug_idents(&ident_ids);
                    }

                    exposed_idents_by_module.insert(
                        ModuleId($module_id),
                        ident_ids
                    );
                )+

                debug_assert!(exposed_idents_by_module.len() == $total, "Error setting up Builtins: `total:` is set to the wrong amount. It was set to {} but {} modules were set up.", $total, exposed_idents_by_module.len());

                exposed_idents_by_module
            }
        }

        impl ModuleId {
            pub fn is_builtin(&self) -> bool {
                // This is a builtin ModuleId iff it's below the
                // total number of builtin modules, since they
                // take up the first $total ModuleId numbers.
                self.0 < $total
            }

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
                    let name: ModuleName = name_str.into();

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

        impl<'a> Default for PackageModuleIds<'a> {
            fn default() -> Self {
                // +1 because the user will be compiling at least 1 non-builtin module!
                let capacity = $total + 1;

                let mut by_name = HashMap::with_capacity_and_hasher(capacity, default_hasher());
                let mut by_id = Vec::with_capacity(capacity);

                let mut insert_both = |id: ModuleId, name_str: &'static str| {
                    let raw_name: IdentStr = name_str.into();
                    let name = PQModuleName::Unqualified(raw_name.into());

                    if cfg!(debug_assertions) {
                        Self::insert_debug_name(id, &name);
                    }

                    by_name.insert(name.clone(), id);
                    by_id.push(name);
                };

                $(
                    insert_both(ModuleId($module_id), $module_name);
                )+

                PackageModuleIds { by_name, by_id }
            }
        }

        impl Symbol {
            $(
                $(
                    pub const $ident_const: Symbol = Symbol::new(ModuleId($module_id), IdentId($ident_id));
                )+
            )+

            /// The default idents that should be in scope,
            /// and what symbols they should resolve to.
            ///
            /// This is for type aliases like `Int` and `Str` and such.
            pub fn default_in_scope() -> SendMap<Ident, (Symbol, Region)> {
                let mut scope = SendMap::default();

                $(
                    $(
                        $(
                            // TODO is there a cleaner way to do this?
                            // The goal is to make sure that we only
                            // actually import things into scope if
                            // they are tagged as "imported" in define_builtins!
                            let $imported = true;

                            if $imported {
                                scope.insert($ident_name.into(), (Symbol::new(ModuleId($module_id), IdentId($ident_id)), Region::zero()));
                            }
                        )?
                    )+
                )+

                scope
            }
        }
    };
}

// NOTE: Some of these builtins have a # in their names.
// This is because they are for compiler use only, and should not cause
// namespace conflicts with userspace!
define_builtins! {
    0 ATTR: "#Attr" => {
        0 UNDERSCORE: "_" // the _ used in pattern matches. This is Symbol 0.
        1 ATTR_ATTR: "Attr" // the #Attr.Attr type alias, used in uniqueness types.
        2 ARG_1: "#arg1"
        3 ARG_2: "#arg2"
        4 ARG_3: "#arg3"
        5 ARG_4: "#arg4"
        6 ARG_5: "#arg5"
        7 ARG_6: "#arg6"
        8 ARG_7: "#arg7"
        9 ARG_8: "#arg8"
        10 INC: "#inc" // internal function that increments the refcount
        11 DEC: "#dec" // internal function that increments the refcount
        12 ARG_CLOSURE: "#arg_closure" // symbol used to store the closure record
        13 LIST_EQ: "#list_eq" // internal function that checks list equality

        14 GENERIC_HASH: "#generic_hash" // hash of arbitrary layouts
        15 GENERIC_HASH_REF: "#generic_hash_by_ref" // hash of arbitrary layouts, passed as an opaque pointer

        16 GENERIC_EQ_REF: "#generic_eq_by_ref" // equality of arbitrary layouts, passed as an opaque pointer
        17 GENERIC_RC_REF: "#generic_rc_by_ref" // refcount of arbitrary layouts, passed as an opaque pointer

        18 GENERIC_EQ: "#generic_eq" // internal function that checks generic equality

        // a user-defined function that we need to capture in a closure
        // see e.g. Set.walk
        19 USER_FUNCTION: "#user_function"

        // A caller (wrapper) that we pass to zig for it to be able to call Roc functions
        20 ZIG_FUNCTION_CALLER: "#zig_function_caller"

        // a caller (wrapper) for comparison
        21 GENERIC_COMPARE_REF: "#generic_compare_ref"

        // used to initialize parameters in borrow.rs
        22 EMPTY_PARAM: "#empty_param"

        // used by the dev backend to store the pointer to where to store large return types
        23 RET_POINTER: "#ret_pointer"

        // used in wasm dev backend to mark temporary values in the VM stack
        24 WASM_TMP: "#wasm_tmp"

        // the _ used in mono when a specialized symbol is deleted
        25 REMOVED_SPECIALIZATION: "#removed_specialization"

        // used in dev backend
        26 DEV_TMP: "#dev_tmp"
    }
    1 NUM: "Num" => {
        0 NUM_NUM: "Num" imported // the Num.Num type alias
        1 NUM_AT_NUM: "@Num" // the Num.@Num private tag
        2 NUM_I128: "I128" imported // the Num.I128 type alias
        3 NUM_U128: "U128" imported // the Num.U128 type alias
        4 NUM_I64: "I64" imported // the Num.I64 type alias
        5 NUM_U64: "U64" imported // the Num.U64 type alias
        6 NUM_I32: "I32" imported // the Num.I32 type alias
        7 NUM_U32: "U32" imported // the Num.U32 type alias
        8 NUM_I16: "I16" imported // the Num.I16 type alias
        9 NUM_U16: "U16" imported // the Num.U16 type alias
        10 NUM_I8: "I8" imported // the Num.I8 type alias
        11 NUM_U8: "U8" imported // the Num.U8 type alias
        12 NUM_INTEGER: "Integer" imported // Int : Num Integer
        13 NUM_AT_INTEGER: "@Integer" // the Int.@Integer private tag
        14 NUM_F64: "F64" imported // the Num.F64 type alias
        15 NUM_F32: "F32" imported // the Num.F32 type alias
        16 NUM_FLOATINGPOINT: "FloatingPoint" imported // Float : Num FloatingPoint
        17 NUM_AT_FLOATINGPOINT: "@FloatingPoint" // the Float.@FloatingPoint private tag
        18 NUM_MAX_INT: "maxInt"
        19 NUM_MIN_INT: "minInt"
        20 NUM_MAX_FLOAT: "maxFloat"
        21 NUM_MIN_FLOAT: "minFloat"
        22 NUM_ABS: "abs"
        23 NUM_NEG: "neg"
        24 NUM_ADD: "add"
        25 NUM_SUB: "sub"
        26 NUM_MUL: "mul"
        27 NUM_LT: "isLt"
        28 NUM_LTE: "isLte"
        29 NUM_GT: "isGt"
        30 NUM_GTE: "isGte"
        31 NUM_TO_FLOAT: "toFloat"
        32 NUM_SIN: "sin"
        33 NUM_COS: "cos"
        34 NUM_TAN: "tan"
        35 NUM_IS_ZERO: "isZero"
        36 NUM_IS_EVEN: "isEven"
        37 NUM_IS_ODD: "isOdd"
        38 NUM_IS_POSITIVE: "isPositive"
        39 NUM_IS_NEGATIVE: "isNegative"
        40 NUM_REM: "rem"
        41 NUM_DIV_FLOAT: "div"
        42 NUM_DIV_INT: "divFloor"
        43 NUM_MOD_INT: "modInt"
        44 NUM_MOD_FLOAT: "modFloat"
        45 NUM_SQRT: "sqrt"
        46 NUM_LOG: "log"
        47 NUM_ROUND: "round"
        48 NUM_COMPARE: "compare"
        49 NUM_POW: "pow"
        50 NUM_CEILING: "ceiling"
        51 NUM_POW_INT: "powInt"
        52 NUM_FLOOR: "floor"
        53 NUM_ADD_WRAP: "addWrap"
        54 NUM_ADD_CHECKED: "addChecked"
        55 NUM_ATAN: "atan"
        56 NUM_ACOS: "acos"
        57 NUM_ASIN: "asin"
        58 NUM_AT_SIGNED128: "@Signed128"
        59 NUM_SIGNED128: "Signed128" imported
        60 NUM_AT_SIGNED64: "@Signed64"
        61 NUM_SIGNED64: "Signed64" imported
        62 NUM_AT_SIGNED32: "@Signed32"
        63 NUM_SIGNED32: "Signed32" imported
        64 NUM_AT_SIGNED16: "@Signed16"
        65 NUM_SIGNED16: "Signed16" imported
        66 NUM_AT_SIGNED8: "@Signed8"
        67 NUM_SIGNED8: "Signed8" imported
        68 NUM_AT_UNSIGNED128: "@Unsigned128"
        69 NUM_UNSIGNED128: "Unsigned128" imported
        70 NUM_AT_UNSIGNED64: "@Unsigned64"
        71 NUM_UNSIGNED64: "Unsigned64" imported
        72 NUM_AT_UNSIGNED32: "@Unsigned32"
        73 NUM_UNSIGNED32: "Unsigned32" imported
        74 NUM_AT_UNSIGNED16: "@Unsigned16"
        75 NUM_UNSIGNED16: "Unsigned16" imported
        76 NUM_AT_UNSIGNED8: "@Unsigned8"
        77 NUM_UNSIGNED8: "Unsigned8" imported
        78 NUM_AT_BINARY64: "@Binary64"
        79 NUM_BINARY64: "Binary64" imported
        80 NUM_AT_BINARY32: "@Binary32"
        81 NUM_BINARY32: "Binary32" imported
        82 NUM_BITWISE_AND: "bitwiseAnd"
        83 NUM_BITWISE_XOR: "bitwiseXor"
        84 NUM_BITWISE_OR: "bitwiseOr"
        85 NUM_SHIFT_LEFT: "shiftLeftBy"
        86 NUM_SHIFT_RIGHT: "shiftRightBy"
        87 NUM_SHIFT_RIGHT_ZERO_FILL: "shiftRightZfBy"
        88 NUM_SUB_WRAP: "subWrap"
        89 NUM_SUB_CHECKED: "subChecked"
        90 NUM_MUL_WRAP: "mulWrap"
        91 NUM_MUL_CHECKED: "mulChecked"
        92 NUM_INT: "Int" imported
        93 NUM_FLOAT: "Float" imported
        94 NUM_AT_NATURAL: "@Natural"
        95 NUM_NATURAL: "Natural" imported
        96 NUM_NAT: "Nat" imported
        97 NUM_INT_CAST: "intCast"
        98 NUM_MAX_I128: "maxI128"
        99 NUM_IS_MULTIPLE_OF: "isMultipleOf"
        100 NUM_AT_DECIMAL: "@Decimal"
        101 NUM_DECIMAL: "Decimal" imported
        102 NUM_DEC: "Dec" imported // the Num.Dectype alias
        103 NUM_BYTES_TO_U16: "bytesToU16"
        104 NUM_BYTES_TO_U32: "bytesToU32"
        105 NUM_CAST_TO_NAT: "#castToNat"
        106 NUM_DIV_CEIL: "divCeil"
        107 NUM_TO_STR: "toStr"
    }
    2 BOOL: "Bool" => {
        0 BOOL_BOOL: "Bool" imported // the Bool.Bool type alias
        1 BOOL_FALSE: "False" imported // Bool.Bool = [ False, True ]
                                       // NB: not strictly needed; used for finding global tag names in error suggestions
        2 BOOL_TRUE: "True" imported // Bool.Bool = [ False, True ]
                                     // NB: not strictly needed; used for finding global tag names in error suggestions
        3 BOOL_AND: "and"
        4 BOOL_OR: "or"
        5 BOOL_NOT: "not"
        6 BOOL_XOR: "xor"
        7 BOOL_EQ: "isEq"
        8 BOOL_NEQ: "isNotEq"
    }
    3 STR: "Str" => {
        0 STR_STR: "Str" imported // the Str.Str type alias
        1 STR_AT_STR: "@Str" // the Str.@Str private tag
        2 STR_IS_EMPTY: "isEmpty"
        3 STR_APPEND: "append"
        4 STR_CONCAT: "concat"
        5 STR_JOIN_WITH: "joinWith"
        6 STR_SPLIT: "split"
        7 STR_COUNT_GRAPHEMES: "countGraphemes"
        8 STR_STARTS_WITH: "startsWith"
        9 STR_ENDS_WITH: "endsWith"
        10 STR_FROM_UTF8: "fromUtf8"
        11 STR_UT8_PROBLEM: "Utf8Problem" // the Utf8Problem type alias
        12 STR_UT8_BYTE_PROBLEM: "Utf8ByteProblem" // the Utf8ByteProblem type alias
        13 STR_TO_UTF8: "toUtf8"
        14 STR_STARTS_WITH_CODE_PT: "startsWithCodePt"
        15 STR_ALIAS_ANALYSIS_STATIC: "#aliasAnalysisStatic" // string with the static lifetime
        16 STR_FROM_UTF8_RANGE: "fromUtf8Range"
        17 STR_REPEAT: "repeat"
        18 STR_TRIM: "trim"
        19 STR_TRIM_LEFT: "trimLeft"
        20 STR_TRIM_RIGHT: "trimRight"
        21 STR_TO_DEC: "toDec"
        22 STR_TO_F64: "toF64"
        23 STR_TO_F32: "toF32"
        24 STR_TO_NAT: "toNat"
        25 STR_TO_U128: "toU128"
        26 STR_TO_I128: "toI128"
        27 STR_TO_U64: "toU64"
        28 STR_TO_I64: "toI64"
        29 STR_TO_U32: "toU32"
        30 STR_TO_I32: "toI32"
        31 STR_TO_U16: "toU16"
        32 STR_TO_I16: "toI16"
        33 STR_TO_U8: "toU8"
        34 STR_TO_I8: "toI8"

    }
    4 LIST: "List" => {
        0 LIST_LIST: "List" imported // the List.List type alias
        1 LIST_AT_LIST: "@List" // the List.@List private tag
        2 LIST_IS_EMPTY: "isEmpty"
        3 LIST_GET: "get"
        4 LIST_SET: "set"
        5 LIST_APPEND: "append"
        6 LIST_MAP: "map"
        7 LIST_LEN: "len"
        8 LIST_WALK_BACKWARDS: "walkBackwards"
        9 LIST_CONCAT: "concat"
        10 LIST_FIRST: "first"
        11 LIST_SINGLE: "single"
        12 LIST_REPEAT: "repeat"
        13 LIST_REVERSE: "reverse"
        14 LIST_PREPEND: "prepend"
        15 LIST_JOIN: "join"
        16 LIST_KEEP_IF: "keepIf"
        17 LIST_CONTAINS: "contains"
        18 LIST_SUM: "sum"
        19 LIST_WALK: "walk"
        20 LIST_LAST: "last"
        21 LIST_KEEP_OKS: "keepOks"
        22 LIST_KEEP_ERRS: "keepErrs"
        23 LIST_MAP_WITH_INDEX: "mapWithIndex"
        24 LIST_MAP2: "map2"
        25 LIST_MAP3: "map3"
        26 LIST_PRODUCT: "product"
        27 LIST_SUM_ADD: "#sumadd"
        28 LIST_PRODUCT_MUL: "#productmul"
        29 LIST_WALK_UNTIL: "walkUntil"
        30 LIST_RANGE: "range"
        31 LIST_SORT_WITH: "sortWith"
        32 LIST_DROP: "drop"
        33 LIST_SWAP: "swap"
        34 LIST_DROP_AT: "dropAt"
        35 LIST_DROP_LAST: "dropLast"
        36 LIST_MIN: "min"
        37 LIST_MIN_LT: "#minlt"
        38 LIST_MAX: "max"
        39 LIST_MAX_GT: "#maxGt"
        40 LIST_MAP4: "map4"
        41 LIST_DROP_FIRST: "dropFirst"
        42 LIST_JOIN_MAP: "joinMap"
        43 LIST_JOIN_MAP_CONCAT: "#joinMapConcat"
        44 LIST_ANY: "any"
        45 LIST_TAKE_FIRST: "takeFirst"
        46 LIST_TAKE_LAST: "takeLast"
        47 LIST_FIND: "find"
        48 LIST_FIND_RESULT: "#find_result" // symbol used in the definition of List.find
        49 LIST_SUBLIST: "sublist"
        50 LIST_INTERSPERSE: "intersperse"
        51 LIST_INTERSPERSE_CLOS: "#intersperseClos"
        52 LIST_SPLIT: "split"
        53 LIST_SPLIT_CLOS: "#splitClos"
        54 LIST_ALL: "all"
        55 LIST_DROP_IF: "dropIf"
        56 LIST_DROP_IF_PREDICATE: "#dropIfPred"
    }
    5 RESULT: "Result" => {
        0 RESULT_RESULT: "Result" imported // the Result.Result type alias
        1 RESULT_OK: "Ok" imported // Result.Result a e = [ Ok a, Err e ]
                                   // NB: not strictly needed; used for finding global tag names in error suggestions
        2 RESULT_ERR: "Err" imported // Result.Result a e = [ Ok a, Err e ]
                                     // NB: not strictly needed; used for finding global tag names in error suggestions
        3 RESULT_MAP: "map"
        4 RESULT_MAP_ERR: "mapErr"
        5 RESULT_WITH_DEFAULT: "withDefault"
        6 RESULT_AFTER: "after"
        7 RESULT_IS_OK: "isOk"
        8 RESULT_IS_ERR: "isErr"
    }
    6 DICT: "Dict" => {
        0 DICT_DICT: "Dict" imported // the Dict.Dict type alias
        1 DICT_AT_DICT: "@Dict" // the Dict.@Dict private tag
        2 DICT_EMPTY: "empty"
        3 DICT_SINGLE: "single"
        4 DICT_GET: "get"
        5 DICT_GET_RESULT: "#get_result" // symbol used in the definition of Dict.get
        6 DICT_WALK: "walk"
        7 DICT_INSERT: "insert"
        8 DICT_LEN: "len"

        9 DICT_REMOVE: "remove"
        10 DICT_CONTAINS: "contains"
        11 DICT_KEYS: "keys"
        12 DICT_VALUES: "values"

        13 DICT_UNION: "union"
        14 DICT_INTERSECTION: "intersection"
        15 DICT_DIFFERENCE: "difference"
    }
    7 SET: "Set" => {
        0 SET_SET: "Set" imported // the Set.Set type alias
        1 SET_AT_SET: "@Set" // the Set.@Set private tag
        2 SET_EMPTY: "empty"
        3 SET_SINGLE: "single"
        4 SET_LEN: "len"
        5 SET_INSERT: "insert"
        6 SET_REMOVE: "remove"
        7 SET_UNION: "union"
        8 SET_DIFFERENCE: "difference"
        9 SET_INTERSECTION: "intersection"
        10 SET_TO_LIST: "toList"
        11 SET_FROM_LIST: "fromList"
        12 SET_WALK: "walk"
        13 SET_WALK_USER_FUNCTION: "#walk_user_function"
        14 SET_CONTAINS: "contains"
    }

    num_modules: 8 // Keep this count up to date by hand! (TODO: see the mut_map! macro for how we could determine this count correctly in the macro)
}
