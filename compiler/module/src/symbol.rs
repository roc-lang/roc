use crate::ident::{Ident, ModuleName};
use crate::module_err::{IdentIdNotFound, ModuleIdNotFound, ModuleResult};
use roc_collections::{SmallStringInterner, VecMap};
use roc_ident::IdentStr;
use roc_region::all::Region;
use snafu::OptionExt;
use std::num::NonZeroU32;
use std::{fmt, u32};

// the packed(4) is needed for faster equality comparisons. With it, the structure is
// treated as a single u64, and comparison is one instruction
//
//  example::eq_sym64:
//          cmp     rdi, rsi
//          sete    al
//          ret
//
// while without it we get 2 extra instructions
//
//  example::eq_sym64:
//          xor     edi, edx
//          xor     esi, ecx
//          or      esi, edi
//          sete    al
//          ret
//
// #[repr(packed)] gives you #[repr(packed(1))], and then all your reads are unaligned
// so we set the alignment to (the natural) 4
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(packed(4))]
pub struct Symbol {
    ident_id: u32,
    module_id: NonZeroU32,
}

/// An Option<Symbol> will use the 0 that is not used by the NonZeroU32 module_id field to encode
/// the Nothing case. An Option<Symbol> hence takes no more space than a Symbol.
#[allow(dead_code)]
const SYMBOL_HAS_NICHE: () =
    assert!(std::mem::size_of::<Symbol>() == std::mem::size_of::<Option<Symbol>>());

// When this is `true` (which it normally should be), Symbol's Debug::fmt implementation
// attempts to pretty print debug symbols using interns recorded using
// register_debug_idents calls (which should be made in debug mode).
// Set it to false if you want to see the raw ModuleId and IdentId ints,
// but please set it back to true before checking in the result!
#[cfg(debug_assertions)]
const PRETTY_PRINT_DEBUG_SYMBOLS: bool = true;

pub const BUILTIN_ABILITIES: &[Symbol] = &[Symbol::ENCODE_ENCODING];

/// In Debug builds only, Symbol has a name() method that lets
/// you look up its name in a global intern table. This table is
/// behind a mutex, so it is neither populated nor available in release builds.
impl Symbol {
    // NOTE: the define_builtins! macro adds a bunch of constants to this impl,
    //
    // e.g. pub const NUM_NUM: Symbol = …

    pub const fn new(module_id: ModuleId, ident_id: IdentId) -> Symbol {
        // The bit layout of the inside of a Symbol is:
        //
        // |------ 32 bits -----|------ 32 bits -----|
        // |      ident_id      |      module_id     |
        // |--------------------|--------------------|
        //
        // module_id comes second because we need to query it more often,
        // and this way we can get it by truncating the u64 to u32,
        // whereas accessing the first slot requires a bit shift first.

        Self {
            module_id: module_id.0,
            ident_id: ident_id.0,
        }
    }

    pub const fn module_id(self) -> ModuleId {
        ModuleId(self.module_id)
    }

    pub const fn ident_id(self) -> IdentId {
        IdentId(self.ident_id)
    }

    pub const fn is_builtin(self) -> bool {
        self.module_id().is_builtin()
    }

    pub fn is_builtin_ability(self) -> bool {
        BUILTIN_ABILITIES.contains(&self)
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

        ident_ids.get_name(self.ident_id()).unwrap_or_else(|| {
            panic!(
                "ident_string's IdentIds did not contain an entry for {} in module {:?}",
                self.ident_id().0,
                self.module_id()
            )
        })
    }

    pub const fn as_u64(self) -> u64 {
        u64::from_ne_bytes(self.to_ne_bytes())
    }

    pub fn fully_qualified(self, interns: &Interns, home: ModuleId) -> ModuleName {
        let module_id = self.module_id();

        if module_id == home {
            ModuleName::from(self.as_str(interns))
        } else {
            // TODO do this without format! to avoid allocation for short strings
            format!(
                "{}.{}",
                self.module_string(interns).as_str(),
                self.as_str(interns)
            )
            .into()
        }
    }

    pub const fn to_ne_bytes(self) -> [u8; 8] {
        unsafe { std::mem::transmute(self) }
    }

    #[cfg(debug_assertions)]
    pub fn contains(self, needle: &str) -> bool {
        format!("{:?}", self).contains(needle)
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
                Ok(names) => match &names.get(&(module_id.to_zero_indexed() as u32)) {
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
        symbol.as_u64()
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
    static ref DEBUG_MODULE_ID_NAMES: std::sync::Mutex<roc_collections::SmallStringInterner> =
        // This stores a u32 key instead of a ModuleId key so that if there's
        // a problem with ModuleId's Debug implementation, logging this for diagnostic
        // purposes won't recursively trigger ModuleId's Debug instance in the course of printing
        // this out.
        std::sync::Mutex::new(roc_collections::SmallStringInterner::with_capacity(10));
}

#[derive(Debug, Default, Clone)]
pub struct Interns {
    pub module_ids: ModuleIds,
    pub all_ident_ids: IdentIdsByModule,
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
            Some(ident_ids) => match ident_ids.get_id(ident.as_str()) {
                Some(ident_id) => Symbol::new(module_id, ident_id),
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
    all_ident_ids: &'a IdentIdsByModule,
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
    all_ident_ids: &'a mut IdentIdsByModule,
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
    static ref DEBUG_IDENT_IDS_BY_MODULE_ID: std::sync::Mutex<roc_collections::VecMap<u32, IdentIds>> =
        // This stores a u32 key instead of a ModuleId key so that if there's
        // a problem with ModuleId's Debug implementation, logging this for diagnostic
        // purposes won't recursively trigger ModuleId's Debug instance in the course of printing
        // this out.
        std::sync::Mutex::new(roc_collections::VecMap::default());
}

/// A globally unique ID that gets assigned to each module as it is loaded.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId(NonZeroU32);

impl ModuleId {
    // NOTE: the define_builtins! macro adds a bunch of constants to this impl,
    //
    // e.g. pub const NUM: ModuleId = …

    const fn from_zero_indexed(mut id: usize) -> Self {
        id += 1;

        // only happens on overflow
        debug_assert!(id != 0);

        ModuleId(unsafe { NonZeroU32::new_unchecked(id as u32) })
    }

    const fn to_zero_indexed(self) -> usize {
        (self.0.get() - 1) as usize
    }

    #[cfg(debug_assertions)]
    pub fn register_debug_idents(self, ident_ids: &IdentIds) {
        let mut all = DEBUG_IDENT_IDS_BY_MODULE_ID.lock().expect("Failed to acquire lock for Debug interning into DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.");

        all.insert(self.to_zero_indexed() as u32, ident_ids.clone());
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
            match names.try_get(self.to_zero_indexed()) {
                Some(str_ref) => write!(f, "{}", str_ref),
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
    by_id: Vec<PQModuleName<'a>>,
}

impl<'a> PackageModuleIds<'a> {
    pub fn get_or_insert(&mut self, module_name: &PQModuleName<'a>) -> ModuleId {
        if let Some(module_id) = self.get_id(module_name) {
            return module_id;
        }

        // didn't find it, so we'll add it
        let module_id = ModuleId::from_zero_indexed(self.by_id.len());
        self.by_id.push(module_name.clone());
        if cfg!(debug_assertions) {
            Self::insert_debug_name(module_id, module_name);
        }

        module_id
    }

    pub fn into_module_ids(self) -> ModuleIds {
        let by_id: Vec<ModuleName> = self
            .by_id
            .into_iter()
            .map(|pqname| pqname.as_inner().clone())
            .collect();

        ModuleIds { by_id }
    }

    #[cfg(debug_assertions)]
    fn insert_debug_name(module_id: ModuleId, module_name: &PQModuleName) {
        let mut names = DEBUG_MODULE_ID_NAMES.lock().expect("Failed to acquire lock for Debug interning into DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.");

        if names.try_get(module_id.to_zero_indexed()).is_none() {
            match module_name {
                PQModuleName::Unqualified(module) => {
                    names.insert(module.as_str());
                }
                PQModuleName::Qualified(package, module) => {
                    names.insert(&format!("{}.{}", package, module.as_str()));
                }
            }
        }
    }

    #[cfg(not(debug_assertions))]
    fn insert_debug_name(_module_id: ModuleId, _module_name: &PQModuleName) {
        // By design, this is a no-op in release builds!
    }

    pub fn get_id(&self, module_name: &PQModuleName<'a>) -> Option<ModuleId> {
        for (index, name) in self.by_id.iter().enumerate() {
            if name == module_name {
                return Some(ModuleId::from_zero_indexed(index));
            }
        }

        None
    }

    pub fn get_name(&self, id: ModuleId) -> Option<&PQModuleName> {
        self.by_id.get(id.to_zero_indexed())
    }

    pub fn available_modules(&self) -> impl Iterator<Item = &PQModuleName> {
        self.by_id.iter()
    }
}

/// Stores a mapping between ModuleId and InlinableString.
#[derive(Debug, Clone)]
pub struct ModuleIds {
    /// Each ModuleId is an index into this Vec
    by_id: Vec<ModuleName>,
}

impl ModuleIds {
    pub fn get_or_insert(&mut self, module_name: &ModuleName) -> ModuleId {
        if let Some(module_id) = self.get_id(module_name) {
            return module_id;
        }

        // didn't find it, so we'll add it
        let module_id = ModuleId::from_zero_indexed(self.by_id.len());
        self.by_id.push(module_name.clone());
        if cfg!(debug_assertions) {
            Self::insert_debug_name(module_id, module_name);
        }

        module_id
    }

    #[cfg(debug_assertions)]
    fn insert_debug_name(module_id: ModuleId, module_name: &ModuleName) {
        let mut names = DEBUG_MODULE_ID_NAMES.lock().expect("Failed to acquire lock for Debug interning into DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.");

        // TODO make sure modules are never added more than once!
        if names.try_get(module_id.to_zero_indexed()).is_none() {
            names.insert(module_name.as_str());
        }
    }

    #[cfg(not(debug_assertions))]
    fn insert_debug_name(_module_id: ModuleId, _module_name: &ModuleName) {
        // By design, this is a no-op in release builds!
    }

    #[inline]
    pub fn get_id(&self, module_name: &ModuleName) -> Option<ModuleId> {
        for (index, name) in self.by_id.iter().enumerate() {
            if name == module_name {
                return Some(ModuleId::from_zero_indexed(index));
            }
        }

        None
    }

    pub fn get_name(&self, id: ModuleId) -> Option<&ModuleName> {
        self.by_id.get(id.to_zero_indexed())
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

impl IdentId {
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

/// Stores a mapping between Ident and IdentId.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct IdentIds {
    pub interner: SmallStringInterner,
}

impl IdentIds {
    pub fn ident_strs(&self) -> impl Iterator<Item = (IdentId, &str)> {
        self.interner
            .iter()
            .enumerate()
            .map(|(index, ident)| (IdentId(index as u32), ident))
    }

    pub fn add_str(&mut self, ident_name: &str) -> IdentId {
        IdentId(self.interner.insert(ident_name) as u32)
    }

    pub fn duplicate_ident(&mut self, ident_id: IdentId) -> IdentId {
        IdentId(self.interner.duplicate(ident_id.0 as usize) as u32)
    }

    pub fn get_or_insert(&mut self, name: &str) -> IdentId {
        match self.get_id(name) {
            Some(id) => id,
            None => self.add_str(name),
        }
    }

    // necessary when the name of a value is changed in the editor
    // TODO fix when same ident_name is present multiple times, see issue #2548
    pub fn update_key(&mut self, old_name: &str, new_name: &str) -> Result<IdentId, String> {
        match self.interner.find_and_update(old_name, new_name) {
            Some(index) => Ok(IdentId(index as u32)),
            None => Err(format!("The identifier {:?} is not in IdentIds", old_name)),
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
        IdentId(self.interner.insert_index_str() as u32)
    }

    #[inline(always)]
    pub fn get_id(&self, ident_name: &str) -> Option<IdentId> {
        self.interner
            .find_index(ident_name)
            .map(|i| IdentId(i as u32))
    }

    #[inline(always)]
    pub fn get_id_many<'a>(&'a self, ident_name: &'a str) -> impl Iterator<Item = IdentId> + 'a {
        self.interner
            .find_indices(ident_name)
            .map(|i| IdentId(i as u32))
    }

    pub fn get_name(&self, id: IdentId) -> Option<&str> {
        self.interner.try_get(id.0 as usize)
    }

    pub fn get_name_str_res(&self, ident_id: IdentId) -> ModuleResult<&str> {
        self.get_name(ident_id).with_context(|| IdentIdNotFound {
            ident_id,
            ident_ids_str: format!("{:?}", self),
        })
    }

    pub fn len(&self) -> usize {
        self.interner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.interner.is_empty()
    }
}

#[derive(Debug, Default, Clone)]
pub struct IdentIdsByModule(VecMap<ModuleId, IdentIds>);

impl IdentIdsByModule {
    pub fn get_or_insert(&mut self, module_id: ModuleId) -> &mut IdentIds {
        self.0.get_or_insert(module_id, IdentIds::default)
    }

    pub fn get_mut(&mut self, key: &ModuleId) -> Option<&mut IdentIds> {
        self.0.get_mut(key)
    }

    pub fn get(&self, key: &ModuleId) -> Option<&IdentIds> {
        self.0.get(key)
    }

    pub fn insert(&mut self, key: ModuleId, value: IdentIds) -> Option<IdentIds> {
        self.0.insert(key, value)
    }

    pub fn keys(&self) -> impl Iterator<Item = &ModuleId> {
        self.0.keys()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

// BUILTINS

const fn offset_helper<const N: usize>(mut array: [u32; N]) -> [u32; N] {
    let mut sum = 0u32;

    let mut i = 0;
    while i < N {
        // In rust 1.60 change to: (array[i], sum) = (sum, sum + array[i]);
        let temp = array[i];
        array[i] = sum;
        sum += temp;

        i += 1;
    }

    array
}

const fn byte_slice_equality(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }

    let mut i = 0;
    while i < a.len() {
        if a[i] != b[i] {
            return false;
        }

        i += 1;
    }

    true
}

const fn find_duplicates<const N: usize>(array: [&str; N]) -> Option<(usize, usize)> {
    let mut i = 0;
    while i < N {
        let needle = array[i];
        let mut j = i + 1;
        while j < N {
            if byte_slice_equality(needle.as_bytes(), array[j].as_bytes()) {
                return Some((i, j));
            }

            j += 1;
        }

        i += 1;
    }

    None
}

const fn check_indices<const N: usize>(array: [u32; N]) -> Option<(u32, usize)> {
    let mut i = 0;
    while i < N {
        if array[i] as usize != i {
            return Some((array[i], i));
        }

        i += 1;
    }

    None
}

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
            pub fn exposed_builtins(extra_capacity: usize) -> IdentIdsByModule {
                let mut exposed_idents_by_module = VecMap::with_capacity(extra_capacity + $total);

                $(
                    let module_id = ModuleId::$module_const;
                    debug_assert!(!exposed_idents_by_module.contains_key(&module_id), r"Error setting up Builtins: when setting up module {} {:?} - the module ID {} is already present in the map. Check the map for duplicate module IDs!", $module_id, $module_name, $module_id);

                    let ident_ids = {
                        const TOTAL : usize = [ $($ident_name),+ ].len();
                        const NAMES : [ &str; TOTAL] = [ $($ident_name),+ ];
                        const LENGTHS: [ u16; TOTAL] = [ $($ident_name.len() as u16),+ ];
                        const OFFSETS: [ u32; TOTAL] = offset_helper([ $($ident_name.len() as u32),+ ]);
                        const BUFFER: &str = concat!($($ident_name),+);

                        const LENGTH_CHECK: Option<(u32, usize)> = check_indices([ $($ident_id),+ ]);
                        const DUPLICATE_CHECK: Option<(usize, usize)> = find_duplicates(NAMES);

                        if cfg!(debug_assertions) {
                            match LENGTH_CHECK {
                                None => (),
                                Some((given, expected)) => panic!(
                                    "Symbol {} : {} should have index {} based on the insertion order, try {} : {} instead",
                                    given, NAMES[expected], expected, expected, NAMES[expected],
                                ),
                            }
                        };

                        if cfg!(debug_assertions) {
                            match DUPLICATE_CHECK {
                                None => (),
                                Some((first, second)) => panic!(
                                    "Symbol {} : {} is duplicated at position {}, try removing the duplicate",
                                    first, NAMES[first], second
                                ),
                            }
                        };

                        // Safety: all lengths are non-negative and smaller than 2^15
                        let interner = unsafe {
                            SmallStringInterner::from_parts (
                            BUFFER.as_bytes().to_vec(),
                            LENGTHS.to_vec(),
                            OFFSETS.to_vec(),
                        )};

                        IdentIds{ interner }
                    };

                    if cfg!(debug_assertions) {
                        let name = PQModuleName::Unqualified($module_name.into());
                        PackageModuleIds::insert_debug_name(module_id, &name);
                        module_id.register_debug_idents(&ident_ids);
                    }


                    exposed_idents_by_module.insert(
                        module_id,
                        ident_ids
                    );
                )+

                debug_assert!(exposed_idents_by_module.len() == $total, "Error setting up Builtins: `total:` is set to the wrong amount. It was set to {} but {} modules were set up.", $total, exposed_idents_by_module.len());

                IdentIdsByModule(exposed_idents_by_module)
            }
        }

        impl ModuleId {
            pub const fn is_builtin(self) -> bool {
                // This is a builtin ModuleId iff it's below the
                // total number of builtin modules, since they
                // take up the first $total ModuleId numbers.
                self.to_zero_indexed() < $total
            }

            $(
                pub const $module_const: ModuleId = ModuleId::from_zero_indexed($module_id);
            )+
        }

        impl Default for ModuleIds {
            fn default() -> Self {
                // +1 because the user will be compiling at least 1 non-builtin module!
                let capacity = $total + 1;

                let mut by_id = Vec::with_capacity(capacity);

                let mut insert_both = |id: ModuleId, name_str: &'static str| {
                    let name: ModuleName = name_str.into();

                    if cfg!(debug_assertions) {
                        Self::insert_debug_name(id, &name);
                    }

                    by_id.push(name);
                };

                $(
                    insert_both(ModuleId::$module_const, $module_name);
                )+

                ModuleIds {  by_id }
            }
        }

        impl<'a> Default for PackageModuleIds<'a> {
            fn default() -> Self {
                // +1 because the user will be compiling at least 1 non-builtin module!
                let capacity = $total + 1;

                let mut by_id = Vec::with_capacity(capacity);

                let mut insert_both = |id: ModuleId, name_str: &'static str| {
                    let raw_name: IdentStr = name_str.into();
                    let name = PQModuleName::Unqualified(raw_name.into());

                    if cfg!(debug_assertions) {
                        Self::insert_debug_name(id, &name);
                    }

                    by_id.push(name);
                };

                $(
                    insert_both(ModuleId::$module_const, $module_name);
                )+

                PackageModuleIds { by_id }
            }
        }

        impl Symbol {
            $(
                $(
                    pub const $ident_const: Symbol = Symbol::new(ModuleId::$module_const, IdentId($ident_id));
                )+
            )+

            /// The default idents that should be in scope,
            /// and what symbols they should resolve to.
            ///
            /// This is for type aliases like `Int` and `Str` and such.
            pub fn default_in_scope() -> VecMap<Ident, (Symbol, Region)> {
                let mut scope = VecMap::default();

                $(
                    $(
                        $(
                            // TODO is there a cleaner way to do this?
                            // The goal is to make sure that we only
                            // actually import things into scope if
                            // they are tagged as "imported" in define_builtins!
                            let $imported = true;

                            if $imported {
                                scope.insert($ident_name.into(), (Symbol::new(ModuleId::$module_const, IdentId($ident_id)), Region::zero()));
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
        27 DEV_TMP2: "#dev_tmp2"
        28 DEV_TMP3: "#dev_tmp3"
        29 DEV_TMP4: "#dev_tmp4"
        30 DEV_TMP5: "#dev_tmp5"
    }
    1 NUM: "Num" => {
        0 NUM_NUM: "Num"  // the Num.Num type alias
        1 NUM_I128: "I128"  // the Num.I128 type alias
        2 NUM_U128: "U128"  // the Num.U128 type alias
        3 NUM_I64: "I64"  // the Num.I64 type alias
        4 NUM_U64: "U64"  // the Num.U64 type alias
        5 NUM_I32: "I32"  // the Num.I32 type alias
        6 NUM_U32: "U32"  // the Num.U32 type alias
        7 NUM_I16: "I16"  // the Num.I16 type alias
        8 NUM_U16: "U16"  // the Num.U16 type alias
        9 NUM_I8: "I8"  // the Num.I8 type alias
        10 NUM_U8: "U8"  // the Num.U8 type alias
        11 NUM_INTEGER: "Integer" // Int : Num Integer
        12 NUM_F64: "F64"  // the Num.F64 type alias
        13 NUM_F32: "F32"  // the Num.F32 type alias
        14 NUM_FLOATINGPOINT: "FloatingPoint" // Float : Num FloatingPoint
        15 NUM_MAX_F32: "maxF32"
        16 NUM_MIN_F32: "minF32"
        17 NUM_ABS: "abs"
        18 NUM_NEG: "neg"
        19 NUM_ADD: "add"
        20 NUM_SUB: "sub"
        21 NUM_MUL: "mul"
        22 NUM_LT: "isLt"
        23 NUM_LTE: "isLte"
        24 NUM_GT: "isGt"
        25 NUM_GTE: "isGte"
        26 NUM_TO_FRAC: "toFrac"
        27 NUM_SIN: "sin"
        28 NUM_COS: "cos"
        29 NUM_TAN: "tan"
        30 NUM_IS_ZERO: "isZero"
        31 NUM_IS_EVEN: "isEven"
        32 NUM_IS_ODD: "isOdd"
        33 NUM_IS_POSITIVE: "isPositive"
        34 NUM_IS_NEGATIVE: "isNegative"
        35 NUM_REM: "rem"
        36 NUM_REM_CHECKED: "remChecked"
        37 NUM_DIV_FRAC: "div"
        38 NUM_DIV_FRAC_CHECKED: "divChecked"
        39 NUM_DIV_TRUNC: "divTrunc"
        40 NUM_DIV_TRUNC_CHECKED: "divTruncChecked"
        41 NUM_SQRT: "sqrt"
        42 NUM_SQRT_CHECKED: "sqrtChecked"
        43 NUM_LOG: "log"
        44 NUM_LOG_CHECKED: "logChecked"
        45 NUM_ROUND: "round"
        46 NUM_COMPARE: "compare"
        47 NUM_POW: "pow"
        48 NUM_CEILING: "ceiling"
        49 NUM_POW_INT: "powInt"
        50 NUM_FLOOR: "floor"
        51 NUM_ADD_WRAP: "addWrap"
        52 NUM_ADD_CHECKED: "addChecked"
        53 NUM_ADD_SATURATED: "addSaturated"
        54 NUM_ATAN: "atan"
        55 NUM_ACOS: "acos"
        56 NUM_ASIN: "asin"
        57 NUM_SIGNED128: "Signed128"
        58 NUM_SIGNED64: "Signed64"
        59 NUM_SIGNED32: "Signed32"
        60 NUM_SIGNED16: "Signed16"
        61 NUM_SIGNED8: "Signed8"
        62 NUM_UNSIGNED128: "Unsigned128"
        63 NUM_UNSIGNED64: "Unsigned64"
        64 NUM_UNSIGNED32: "Unsigned32"
        65 NUM_UNSIGNED16: "Unsigned16"
        66 NUM_UNSIGNED8: "Unsigned8"
        67 NUM_BINARY64: "Binary64"
        68 NUM_BINARY32: "Binary32"
        69 NUM_BITWISE_AND: "bitwiseAnd"
        70 NUM_BITWISE_XOR: "bitwiseXor"
        71 NUM_BITWISE_OR: "bitwiseOr"
        72 NUM_SHIFT_LEFT: "shiftLeftBy"
        73 NUM_SHIFT_RIGHT: "shiftRightBy"
        74 NUM_SHIFT_RIGHT_ZERO_FILL: "shiftRightZfBy"
        75 NUM_SUB_WRAP: "subWrap"
        76 NUM_SUB_CHECKED: "subChecked"
        77 NUM_SUB_SATURATED: "subSaturated"
        78 NUM_MUL_WRAP: "mulWrap"
        79 NUM_MUL_CHECKED: "mulChecked"
        80 NUM_INT: "Int"
        81 NUM_FRAC: "Frac"
        82 NUM_NATURAL: "Natural"
        83 NUM_NAT: "Nat"
        84 NUM_INT_CAST: "intCast"
        85 NUM_IS_MULTIPLE_OF: "isMultipleOf"
        86 NUM_DECIMAL: "Decimal"
        87 NUM_DEC: "Dec"  // the Num.Dectype alias
        88 NUM_BYTES_TO_U16: "bytesToU16"
        89 NUM_BYTES_TO_U32: "bytesToU32"
        90 NUM_CAST_TO_NAT: "#castToNat"
        91 NUM_DIV_CEIL: "divCeil"
        92 NUM_DIV_CEIL_CHECKED: "divCeilChecked"
        93 NUM_TO_STR: "toStr"
        94 NUM_MIN_I8: "minI8"
        95 NUM_MAX_I8: "maxI8"
        96 NUM_MIN_U8: "minU8"
        97 NUM_MAX_U8: "maxU8"
        98 NUM_MIN_I16: "minI16"
        99 NUM_MAX_I16: "maxI16"
        100 NUM_MIN_U16: "minU16"
        101 NUM_MAX_U16: "maxU16"
        102 NUM_MIN_I32: "minI32"
        103 NUM_MAX_I32: "maxI32"
        104 NUM_MIN_U32: "minU32"
        105 NUM_MAX_U32: "maxU32"
        106 NUM_MIN_I64: "minI64"
        107 NUM_MAX_I64: "maxI64"
        108 NUM_MIN_U64: "minU64"
        109 NUM_MAX_U64: "maxU64"
        110 NUM_MIN_I128: "minI128"
        111 NUM_MAX_I128: "maxI128"
        112 NUM_TO_I8: "toI8"
        113 NUM_TO_I8_CHECKED: "toI8Checked"
        114 NUM_TO_I16: "toI16"
        115 NUM_TO_I16_CHECKED: "toI16Checked"
        116 NUM_TO_I32: "toI32"
        117 NUM_TO_I32_CHECKED: "toI32Checked"
        118 NUM_TO_I64: "toI64"
        119 NUM_TO_I64_CHECKED: "toI64Checked"
        120 NUM_TO_I128: "toI128"
        121 NUM_TO_I128_CHECKED: "toI128Checked"
        122 NUM_TO_U8: "toU8"
        123 NUM_TO_U8_CHECKED: "toU8Checked"
        124 NUM_TO_U16: "toU16"
        125 NUM_TO_U16_CHECKED: "toU16Checked"
        126 NUM_TO_U32: "toU32"
        127 NUM_TO_U32_CHECKED: "toU32Checked"
        128 NUM_TO_U64: "toU64"
        129 NUM_TO_U64_CHECKED: "toU64Checked"
        130 NUM_TO_U128: "toU128"
        131 NUM_TO_U128_CHECKED: "toU128Checked"
        132 NUM_TO_NAT: "toNat"
        133 NUM_TO_NAT_CHECKED: "toNatChecked"
        134 NUM_TO_F32: "toF32"
        135 NUM_TO_F32_CHECKED: "toF32Checked"
        136 NUM_TO_F64: "toF64"
        137 NUM_TO_F64_CHECKED: "toF64Checked"
        138 NUM_MAX_F64: "maxF64"
        139 NUM_MIN_F64: "minF64"
    }
    2 BOOL: "Bool" => {
        0 BOOL_BOOL: "Bool" // the Bool.Bool type alias
        1 BOOL_FALSE: "False" imported // Bool.Bool = [False, True]
                                       // NB: not strictly needed; used for finding tag names in error suggestions
        2 BOOL_TRUE: "True" imported // Bool.Bool = [False, True]
                                     // NB: not strictly needed; used for finding tag names in error suggestions
        3 BOOL_AND: "and"
        4 BOOL_OR: "or"
        5 BOOL_NOT: "not"
        6 BOOL_XOR: "xor"
        7 BOOL_EQ: "isEq"
        8 BOOL_NEQ: "isNotEq"
    }
    3 STR: "Str" => {
        0 STR_STR: "Str" imported // the Str.Str type alias
        1 STR_IS_EMPTY: "isEmpty"
        2 STR_APPEND: "#append" // unused
        3 STR_CONCAT: "concat"
        4 STR_JOIN_WITH: "joinWith"
        5 STR_SPLIT: "split"
        6 STR_COUNT_GRAPHEMES: "countGraphemes"
        7 STR_STARTS_WITH: "startsWith"
        8 STR_ENDS_WITH: "endsWith"
        9 STR_FROM_UTF8: "fromUtf8"
        10 STR_UT8_PROBLEM: "Utf8Problem" // the Utf8Problem type alias
        11 STR_UT8_BYTE_PROBLEM: "Utf8ByteProblem" // the Utf8ByteProblem type alias
        12 STR_TO_UTF8: "toUtf8"
        13 STR_STARTS_WITH_CODE_PT: "startsWithCodePt"
        14 STR_ALIAS_ANALYSIS_STATIC: "#aliasAnalysisStatic" // string with the static lifetime
        15 STR_FROM_UTF8_RANGE: "fromUtf8Range"
        16 STR_REPEAT: "repeat"
        17 STR_TRIM: "trim"
        18 STR_TRIM_LEFT: "trimLeft"
        19 STR_TRIM_RIGHT: "trimRight"
        20 STR_TO_DEC: "toDec"
        21 STR_TO_F64: "toF64"
        22 STR_TO_F32: "toF32"
        23 STR_TO_NAT: "toNat"
        24 STR_TO_U128: "toU128"
        25 STR_TO_I128: "toI128"
        26 STR_TO_U64: "toU64"
        27 STR_TO_I64: "toI64"
        28 STR_TO_U32: "toU32"
        29 STR_TO_I32: "toI32"
        30 STR_TO_U16: "toU16"
        31 STR_TO_I16: "toI16"
        32 STR_TO_U8: "toU8"
        33 STR_TO_I8: "toI8"
    }
    4 LIST: "List" => {
        0 LIST_LIST: "List" imported // the List.List type alias
        1 LIST_IS_EMPTY: "isEmpty"
        2 LIST_GET: "get"
        3 LIST_SET: "set"
        4 LIST_APPEND: "append"
        5 LIST_MAP: "map"
        6 LIST_LEN: "len"
        7 LIST_WALK_BACKWARDS: "walkBackwards"
        8 LIST_CONCAT: "concat"
        9 LIST_FIRST: "first"
        10 LIST_SINGLE: "single"
        11 LIST_REPEAT: "repeat"
        12 LIST_REVERSE: "reverse"
        13 LIST_PREPEND: "prepend"
        14 LIST_JOIN: "join"
        15 LIST_KEEP_IF: "keepIf"
        16 LIST_CONTAINS: "contains"
        17 LIST_SUM: "sum"
        18 LIST_WALK: "walk"
        19 LIST_LAST: "last"
        20 LIST_KEEP_OKS: "keepOks"
        21 LIST_KEEP_ERRS: "keepErrs"
        22 LIST_MAP_WITH_INDEX: "mapWithIndex"
        23 LIST_MAP2: "map2"
        24 LIST_MAP3: "map3"
        25 LIST_PRODUCT: "product"
        26 LIST_WALK_UNTIL: "walkUntil"
        27 LIST_RANGE: "range"
        28 LIST_SORT_WITH: "sortWith"
        29 LIST_DROP: "drop"
        30 LIST_SWAP: "swap"
        31 LIST_DROP_AT: "dropAt"
        32 LIST_DROP_LAST: "dropLast"
        33 LIST_MIN: "min"
        34 LIST_MIN_LT: "#minlt"
        35 LIST_MAX: "max"
        36 LIST_MAX_GT: "#maxGt"
        37 LIST_MAP4: "map4"
        38 LIST_DROP_FIRST: "dropFirst"
        39 LIST_JOIN_MAP: "joinMap"
        40 LIST_JOIN_MAP_CONCAT: "#joinMapConcat"
        41 LIST_ANY: "any"
        42 LIST_TAKE_FIRST: "takeFirst"
        43 LIST_TAKE_LAST: "takeLast"
        44 LIST_FIND: "find"
        45 LIST_FIND_RESULT: "#find_result" // symbol used in the definition of List.find
        46 LIST_SUBLIST: "sublist"
        47 LIST_INTERSPERSE: "intersperse"
        48 LIST_INTERSPERSE_CLOS: "#intersperseClos"
        49 LIST_SPLIT: "split"
        50 LIST_SPLIT_CLOS: "#splitClos"
        51 LIST_ALL: "all"
        52 LIST_DROP_IF: "dropIf"
        53 LIST_DROP_IF_PREDICATE: "#dropIfPred"
        54 LIST_SORT_ASC: "sortAsc"
        55 LIST_SORT_DESC: "sortDesc"
        56 LIST_SORT_DESC_COMPARE: "#sortDescCompare"
        57 LIST_REPLACE: "replace"
        58 LIST_IS_UNIQUE: "#isUnique"
    }
    5 RESULT: "Result" => {
        0 RESULT_RESULT: "Result" // the Result.Result type alias
        1 RESULT_OK: "Ok" imported // Result.Result a e = [Ok a, Err e]
                                   // NB: not strictly needed; used for finding tag names in error suggestions
        2 RESULT_ERR: "Err" imported // Result.Result a e = [Ok a, Err e]
                                     // NB: not strictly needed; used for finding tag names in error suggestions
        3 RESULT_MAP: "map"
        4 RESULT_MAP_ERR: "mapErr"
        5 RESULT_WITH_DEFAULT: "withDefault"
        6 RESULT_AFTER: "after"
        7 RESULT_IS_OK: "isOk"
        8 RESULT_IS_ERR: "isErr"
    }
    6 DICT: "Dict" => {
        0 DICT_DICT: "Dict" imported // the Dict.Dict type alias
        1 DICT_EMPTY: "empty"
        2 DICT_SINGLE: "single"
        3 DICT_GET: "get"
        4 DICT_GET_RESULT: "#get_result" // symbol used in the definition of Dict.get
        5 DICT_WALK: "walk"
        6 DICT_INSERT: "insert"
        7 DICT_LEN: "len"

        8 DICT_REMOVE: "remove"
        9 DICT_CONTAINS: "contains"
        10 DICT_KEYS: "keys"
        11 DICT_VALUES: "values"

        12 DICT_UNION: "union"
        13 DICT_INTERSECTION: "intersection"
        14 DICT_DIFFERENCE: "difference"
    }
    7 SET: "Set" => {
        0 SET_SET: "Set" imported // the Set.Set type alias
        1 SET_EMPTY: "empty"
        2 SET_SINGLE: "single"
        3 SET_LEN: "len"
        4 SET_INSERT: "insert"
        5 SET_REMOVE: "remove"
        6 SET_UNION: "union"
        7 SET_DIFFERENCE: "difference"
        8 SET_INTERSECTION: "intersection"
        9 SET_TO_LIST: "toList"
        10 SET_FROM_LIST: "fromList"
        11 SET_WALK: "walk"
        12 SET_WALK_USER_FUNCTION: "#walk_user_function"
        13 SET_CONTAINS: "contains"
        14 SET_TO_DICT: "toDict"
    }
    8 BOX: "Box" => {
        0 BOX_BOX_TYPE: "Box" imported // the Box.Box opaque type
        1 BOX_BOX_FUNCTION: "box" // Box.box
        2 BOX_UNBOX: "unbox"
    }
    9 ENCODE: "Encode" => {
        0 ENCODE_ENCODER: "Encoder"
        1 ENCODE_ENCODING: "Encoding"
        2 ENCODE_TO_ENCODER: "toEncoder"
        3 ENCODE_ENCODERFORMATTING: "EncoderFormatting"
        4 ENCODE_U8: "u8"
        5 ENCODE_U16: "u16"
        6 ENCODE_U32: "u32"
        7 ENCODE_U64: "u64"
        8 ENCODE_U128: "u128"
        9 ENCODE_I8: "i8"
        10 ENCODE_I16: "i16"
        11 ENCODE_I32: "i32"
        12 ENCODE_I64: "i64"
        13 ENCODE_I128: "i128"
        14 ENCODE_F32: "f32"
        15 ENCODE_F64: "f64"
        16 ENCODE_DEC: "dec"
        17 ENCODE_BOOL: "bool"
        18 ENCODE_STRING: "string"
        19 ENCODE_LIST: "list"
        20 ENCODE_CUSTOM: "custom"
        21 ENCODE_APPEND_WITH: "appendWith"
        22 ENCODE_APPEND: "append"
        23 ENCODE_TO_BYTES: "toBytes"
    }
    10 JSON: "Json" => {
        0 JSON_JSON: "Json"
    }

    num_modules: 11 // Keep this count up to date by hand! (TODO: see the mut_map! macro for how we could determine this count correctly in the macro)
}
