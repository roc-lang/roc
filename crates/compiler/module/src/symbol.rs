use crate::ident::{Ident, IdentSuffix, Lowercase, ModuleName};
use crate::module_err::{ModuleError, ModuleResult};
use roc_collections::{SmallStringInterner, VecMap};
use roc_error_macros::internal_error;
use roc_ident::IdentStr;
use roc_region::all::Region;
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
#[cfg(any(debug_assertions, feature = "debug-symbols"))]
const PRETTY_PRINT_DEBUG_SYMBOLS: bool = true;

pub const DERIVABLE_ABILITIES: &[(Symbol, &[Symbol])] = &[
    (Symbol::ENCODE_ENCODING, &[Symbol::ENCODE_TO_ENCODER]),
    (Symbol::DECODE_DECODING, &[Symbol::DECODE_DECODER]),
    (Symbol::HASH_HASH_ABILITY, &[Symbol::HASH_HASH]),
    (Symbol::BOOL_EQ, &[Symbol::BOOL_IS_EQ]),
    (
        Symbol::INSPECT_INSPECT_ABILITY,
        &[Symbol::INSPECT_TO_INSPECTOR],
    ),
];

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
            ident_id: ident_id.raw(),
        }
    }

    pub const fn module_id(self) -> ModuleId {
        ModuleId(self.module_id)
    }

    pub const fn ident_id(self) -> IdentId {
        IdentId::from_raw(self.ident_id)
    }

    pub const fn is_builtin(self) -> bool {
        self.module_id().is_builtin()
    }

    pub const fn suffix(self) -> IdentSuffix {
        self.ident_id().suffix()
    }

    pub fn is_derivable_ability(self) -> bool {
        self.derivable_ability().is_some()
    }

    pub fn derivable_ability(self) -> Option<&'static (Symbol, &'static [Symbol])> {
        DERIVABLE_ABILITIES.iter().find(|(name, _)| *name == self)
    }

    /// A symbol that should never be exposed to userspace, but needs to be exposed
    /// to compiled modules for deriving abilities for structural types.
    pub fn is_exposed_for_builtin_derivers(&self) -> bool {
        matches!(
            self,
            // The `structuralEq` call used deriving structural equality, which will wrap the `Eq`
            // low-level implementation.
            &Self::BOOL_STRUCTURAL_EQ
        )
    }

    pub fn is_automatically_imported(self) -> bool {
        let module_id = self.module_id();

        module_id.is_automatically_imported()
            && Self::builtin_types_in_scope(module_id)
                .iter()
                .any(|(_, (s, _))| *s == self)
    }

    pub fn is_generated(self, interns: &Interns) -> bool {
        self.ident_ids(interns).is_generated_id(self.ident_id())
    }

    pub fn module_string<'a>(&self, interns: &'a Interns) -> &'a ModuleName {
        interns
            .module_ids
            .get_name(self.module_id())
            .unwrap_or_else(|| {
                internal_error!(
                    "module_string could not find IdentIds for module {:?} in {:?}",
                    self.module_id(),
                    interns
                )
            })
    }

    pub fn as_str(self, interns: &Interns) -> &str {
        self.ident_ids(interns)
            .get_name(self.ident_id())
            .unwrap_or_else(|| {
                internal_error!(
                    "ident_string's IdentIds did not contain an entry for {} in module {:?}",
                    self.ident_id().index(),
                    self.module_id()
                )
            })
    }

    pub fn as_unsuffixed_str(self, interns: &Interns) -> &str {
        self.as_str(interns).trim_end_matches('!')
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
        // repr(packed(4)) is repr(c), and with the fields as defined will not having padding.
        unsafe { std::mem::transmute(self) }
    }

    #[cfg(debug_assertions)]
    pub fn contains(self, needle: &str) -> bool {
        format!("{self:?}").contains(needle)
    }

    fn ident_ids(self, interns: &Interns) -> &IdentIds {
        interns
            .all_ident_ids
            .get(&self.module_id())
            .unwrap_or_else(|| {
                internal_error!(
                    "ident_string could not find IdentIds for module {:?} in {:?}",
                    self.module_id(),
                    interns
                )
            })
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
    #[cfg(any(debug_assertions, feature = "debug-symbols"))]
    #[allow(clippy::print_in_format_impl)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if PRETTY_PRINT_DEBUG_SYMBOLS {
            let module_id = self.module_id();
            let ident_id = self.ident_id();

            match DEBUG_IDENT_IDS_BY_MODULE_ID.lock() {
                Ok(names) => match &names.get(&(module_id.to_zero_indexed() as u32)) {
                    Some(ident_ids) => match ident_ids.get_name(ident_id) {
                        Some(ident_str) => write!(f, "`{module_id:?}.{ident_str}`"),
                        None => fallback_debug_fmt(*self, f),
                    },
                    None => fallback_debug_fmt(*self, f),
                },
                Err(err) => {
                    // Print and return Err rather than panicking, because this
                    // might be used in a panic error message, and if we panick
                    // while we're already panicking it'll kill the process
                    // without printing any of the errors!
                    use std::io::Write;

                    let mut stderr = std::io::stderr();
                    writeln!(stderr, "DEBUG INFO: Failed to acquire lock for Debug reading from DEBUG_IDENT_IDS_BY_MODULE_ID, presumably because a thread panicked: {err:?}").unwrap();

                    fallback_debug_fmt(*self, f)
                }
            }
        } else {
            fallback_debug_fmt(*self, f)
        }
    }

    #[cfg(not(any(debug_assertions, feature = "debug-symbols")))]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fallback_debug_fmt(*self, f)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let module_id = self.module_id();
        let ident_id = self.ident_id().index();

        write!(f, "{module_id:?}.{ident_id:?}")
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

    write!(f, "`{module_id:?}.{ident_id:?}`")
}

/// This is used in Debug builds only, to let us have a Debug instance
/// which displays not only the Module ID, but also the Module Name which
/// corresponds to that ID.
///
#[cfg(any(debug_assertions, feature = "debug-symbols"))]
static DEBUG_MODULE_ID_NAMES: std::sync::Mutex<roc_collections::SmallStringInterner> =
    std::sync::Mutex::new(roc_collections::SmallStringInterner::new());

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
            internal_error!(
                "Unable to find interns entry for module_id {:?} in Interns {:?}",
                module_id,
                self
            )
        })
    }

    pub fn symbol(&self, module_id: ModuleId, ident: IdentStr) -> Symbol {
        let ident: Ident = ident.into();

        match self.all_ident_ids.get(&module_id) {
            Some(ident_ids) => match ident_ids.get_id(ident.as_str()) {
                Some(ident_id) => Symbol::new(module_id, ident_id),
                None => {
                    internal_error!(
                        "Interns::symbol could not find ident entry for {:?} for module {:?}",
                        ident,
                        module_id
                    );
                }
            },
            None => {
                internal_error!(
                    "Interns::symbol could not find entry for module {:?} in Interns {:?}",
                    module_id,
                    self
                );
            }
        }
    }
}

pub fn get_module_ident_ids<'a>(
    all_ident_ids: &'a IdentIdsByModule,
    module_id: &ModuleId,
) -> ModuleResult<&'a IdentIds> {
    all_ident_ids
        .get(module_id)
        .ok_or_else(|| ModuleError::ModuleIdNotFound {
            module_id: format!("{module_id:?}"),
            all_ident_ids: format!("{all_ident_ids:?}"),
        })
}

pub fn get_module_ident_ids_mut<'a>(
    all_ident_ids: &'a mut IdentIdsByModule,
    module_id: &ModuleId,
) -> ModuleResult<&'a mut IdentIds> {
    all_ident_ids
        .get_mut(module_id)
        .ok_or_else(|| ModuleError::ModuleIdNotFound {
            module_id: format!("{module_id:?}"),
            all_ident_ids: "I could not return all_ident_ids here because of borrowing issues."
                .into(),
        })
}

/// This is used in Debug builds only, to let us have a Debug instance
/// which displays not only the Module ID, but also the Module Name which
/// corresponds to that ID.
#[cfg(any(debug_assertions, feature = "debug-symbols"))]
static DEBUG_IDENT_IDS_BY_MODULE_ID: std::sync::Mutex<roc_collections::VecMap<u32, IdentIds>> =
    // This stores a u32 key instead of a ModuleId key so that if there's
    // a problem with ModuleId's Debug implementation, logging this for diagnostic
    // purposes won't recursively trigger ModuleId's Debug instance in the course of printing
    // this out.
    std::sync::Mutex::new(roc_collections::VecMap::new());

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

    #[cfg(any(debug_assertions, feature = "debug-symbols"))]
    pub fn register_debug_idents(self, ident_ids: &IdentIds) {
        let mut all = DEBUG_IDENT_IDS_BY_MODULE_ID.lock().expect("Failed to acquire lock for Debug interning into DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.");

        all.insert(self.to_zero_indexed() as u32, ident_ids.clone());
    }

    #[cfg(not(any(debug_assertions, feature = "debug-symbols")))]
    pub fn register_debug_idents(self, _ident_ids: &IdentIds) {
        // This is a no-op that should get DCE'd
    }

    pub fn to_ident_str(self, interns: &Interns) -> &ModuleName {
        interns
            .module_ids
            .get_name(self)
            .unwrap_or_else(|| internal_error!("Could not find ModuleIds for {:?}", self))
    }

    pub fn is_automatically_imported(self) -> bool {
        self.is_builtin()
    }
}

impl fmt::Debug for ModuleId {
    /// In debug builds, whenever we create a new ModuleId, we record is name in
    /// a global interning table so that Debug can look it up later. That table
    /// needs a global mutex, so we don't do this in release builds. This means
    /// the Debug impl in release builds only shows the number, not the name (which
    /// it does not have available, due to having never stored it in the mutexed intern table.)
    #[cfg(any(debug_assertions, feature = "debug-symbols"))]
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
                Some(str_ref) => write!(f, "{str_ref}"),
                None => {
                    internal_error!(
                        "Could not find a Debug name for module ID {} in {:?}",
                        self.0,
                        names,
                    );
                }
            }
        } else {
            write!(f, "{}", self.0)
        }
    }

    /// In release builds, all we have access to is the number, so only display that.
    #[cfg(not(any(debug_assertions, feature = "debug-symbols")))]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

    pub fn unqualified(&self) -> Option<&T> {
        match self {
            PackageQualified::Unqualified(name) => Some(name),
            PackageQualified::Qualified(_, _) => None,
        }
    }

    pub fn package_shorthand(&self) -> Option<&'a str> {
        match self {
            PackageQualified::Unqualified(_) => None,
            PackageQualified::Qualified(package, _) => Some(package),
        }
    }

    pub fn map_module<B>(&self, f: impl FnOnce(&T) -> B) -> PackageQualified<'a, B> {
        match self {
            PackageQualified::Unqualified(name) => PackageQualified::Unqualified(f(name)),
            PackageQualified::Qualified(package, name) => {
                PackageQualified::Qualified(package, f(name))
            }
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
        if cfg!(any(debug_assertions, feature = "debug-symbols")) {
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

    #[cfg(any(debug_assertions, feature = "debug-symbols"))]
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

    #[cfg(not(any(debug_assertions, feature = "debug-symbols")))]
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

    /// Returns true iff two modules belong to the same package.
    /// Returns [None] if one module is unknown.
    pub fn package_eq(&self, left: ModuleId, right: ModuleId) -> Option<bool> {
        if left.is_builtin() ^ right.is_builtin() {
            return Some(false);
        }
        let result = match (self.get_name(left)?, self.get_name(right)?) {
            (PQModuleName::Unqualified(_), PQModuleName::Unqualified(_)) => true,
            (PQModuleName::Qualified(pkg1, _), PQModuleName::Qualified(pkg2, _)) => pkg1 == pkg2,
            _ => false,
        };
        Some(result)
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
        if cfg!(any(debug_assertions, feature = "debug-symbols")) {
            Self::insert_debug_name(module_id, module_name);
        }

        module_id
    }

    #[cfg(any(debug_assertions, feature = "debug-symbols"))]
    fn insert_debug_name(module_id: ModuleId, module_name: &ModuleName) {
        let mut names = DEBUG_MODULE_ID_NAMES.lock().expect("Failed to acquire lock for Debug interning into DEBUG_MODULE_ID_NAMES, presumably because a thread panicked.");

        // TODO make sure modules are never added more than once!
        if names.try_get(module_id.to_zero_indexed()).is_none() {
            names.insert(module_name.as_str());
        }
    }

    #[cfg(not(any(debug_assertions, feature = "debug-symbols")))]
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

    pub fn iter(&self) -> impl ExactSizeIterator<Item = (ModuleId, &ModuleName)> {
        self.by_id
            .iter()
            .enumerate()
            .map(|(index, name)| (ModuleId::from_zero_indexed(index), name))
    }
}

mod ident_id {
    use crate::ident::IdentSuffix;

    /// An ID that is assigned to interned string identifiers within a module.
    /// By turning these strings into numbers, post-canonicalization processes
    /// like unification and optimization can run a lot faster.
    ///
    /// This ID is unique within a given module, not globally - so to turn this back into
    /// a string, you would need a ModuleId, an IdentId, and a Map<ModuleId, Map<IdentId, String>>.
    #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
    pub struct IdentId(u32);

    const BANG_FLAG: u32 = 1u32 << 31;
    const UNSUFFIXED: u32 = !BANG_FLAG;

    impl IdentId {
        pub const fn index(self) -> usize {
            (self.0 & UNSUFFIXED) as usize
        }

        pub const fn suffix(self) -> IdentSuffix {
            if self.0 & BANG_FLAG > 0 {
                IdentSuffix::Bang
            } else {
                IdentSuffix::None
            }
        }

        pub(super) const fn raw(self) -> u32 {
            self.0
        }

        pub(super) const fn from_raw(raw: u32) -> Self {
            Self(raw)
        }

        pub(super) const fn from_index(index: usize, suffix: IdentSuffix) -> Self {
            assert!(index as u32 <= UNSUFFIXED, "IdentId index too large");

            match suffix {
                IdentSuffix::None => Self(index as u32),
                IdentSuffix::Bang => Self((index as u32) | BANG_FLAG),
            }
        }

        pub(super) const fn from_index_named(index: usize, name: &str) -> Self {
            Self::from_index(index, IdentSuffix::from_name(name))
        }
    }
}

pub use ident_id::IdentId;

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
            .map(|(index, ident)| (IdentId::from_index_named(index, ident), ident))
    }

    pub fn add_str(&mut self, ident_name: &str) -> IdentId {
        IdentId::from_index_named(self.interner.insert(ident_name), ident_name)
    }

    pub fn duplicate_ident(&mut self, ident_id: IdentId) -> IdentId {
        IdentId::from_index(self.interner.duplicate(ident_id.index()), ident_id.suffix())
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
            Some(index) => Ok(IdentId::from_index_named(index, new_name)),
            None => Err(format!("The identifier {old_name:?} is not in IdentIds")),
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
        IdentId::from_index(self.interner.insert_index_str(), IdentSuffix::None)
    }

    pub fn is_generated_id(&self, id: IdentId) -> bool {
        self.interner
            .try_get(id.index())
            .map_or(false, |str| str.starts_with(|c: char| c.is_ascii_digit()))
    }

    #[inline(always)]
    pub fn get_id(&self, ident_name: &str) -> Option<IdentId> {
        self.interner
            .find_index(ident_name)
            .map(|i| IdentId::from_index_named(i, ident_name))
    }

    #[inline(always)]
    pub fn get_id_many<'a>(&'a self, ident_name: &'a str) -> impl Iterator<Item = IdentId> + 'a {
        self.interner
            .find_indices(ident_name)
            .map(|i| IdentId::from_index_named(i, ident_name))
    }

    pub fn get_name(&self, id: IdentId) -> Option<&str> {
        self.interner.try_get(id.index())
    }

    pub fn get_name_str_res(&self, ident_id: IdentId) -> ModuleResult<&str> {
        self.get_name(ident_id)
            .ok_or_else(|| ModuleError::IdentIdNotFound {
                ident_id,
                ident_ids_str: format!("{self:?}"),
            })
    }

    pub fn len(&self) -> usize {
        self.interner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.interner.is_empty()
    }

    pub fn exposed_values(&self) -> Vec<Lowercase> {
        self.ident_strs()
            .filter(|(_, ident)| ident.starts_with(|c: char| c.is_lowercase()))
            .map(|(_, ident)| Lowercase::from(ident))
            .collect()
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
                    $(#[$ident_meta:meta])*
                    $ident_id:literal $ident_const:ident: $ident_name:literal
                    $(exposed_apply_type=$exposed_apply_type:literal)?
                    $(exposed_type=$exposed_type:literal)?
                    $(in_scope_for_hints=$in_scope_for_hints:literal)?
                )*
                $(unexposed $u_ident_id:literal $u_ident_const:ident: $u_ident_name:literal)*
            }
        )+
        num_modules: $total:literal
    } => {
        impl<'a> super::ident::QualifiedModuleName<'a> {
            pub fn is_builtin(&self) -> bool {
                self.opt_package.is_none() && ($($module_name == self.module.as_str() ||)+ false)
            }
        }

        impl IdentIds {
            pub fn exposed_builtins(extra_capacity: usize) -> IdentIdsByModule {
                let mut exposed_idents_by_module = VecMap::with_capacity(extra_capacity + $total);

                $(
                    let module_id = ModuleId::$module_const;
                    debug_assert!(!exposed_idents_by_module.contains_key(&module_id), r"Error setting up Builtins: when setting up module {} {:?} - the module ID {} is already present in the map. Check the map for duplicate module IDs!", $module_id, $module_name, $module_id);

                    let ident_ids = {
                        const TOTAL : usize = (&[ $($ident_name),* ] as &[&str]).len();
                        const NAMES : [ &str; TOTAL] = [ $($ident_name),* ];
                        const LENGTHS: [ u16; TOTAL] = [ $($ident_name.len() as u16),* ];
                        const OFFSETS: [ u32; TOTAL] = offset_helper([ $($ident_name.len() as u32),* ]);
                        const BUFFER: &str = concat!($($ident_name),*);

                        const LENGTH_CHECK: Option<(u32, usize)> = check_indices([ $($ident_id),* ]);
                        const DUPLICATE_CHECK: Option<(usize, usize)> = find_duplicates(NAMES);

                        if cfg!(debug_assertions) {
                            match LENGTH_CHECK {
                                None => (),
                                Some((given, expected)) => internal_error!(
                                    "Symbol {} : {} should have index {} based on the insertion order, try {} : {} instead",
                                    given, NAMES[expected], expected, expected, NAMES[expected],
                                ),
                            }
                        };

                        if cfg!(debug_assertions) {
                            match DUPLICATE_CHECK {
                                None => (),
                                Some((first, second)) => internal_error!(
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

                    if cfg!(any(debug_assertions, feature = "debug-symbols")) {
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

            pub const fn first_after_builtins() -> Self {
                ModuleId::from_zero_indexed($total)
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

                    if cfg!(any(debug_assertions, feature = "debug-symbols")) {
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

                    if cfg!(any(debug_assertions, feature = "debug-symbols")) {
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
                    $(#[$ident_meta])*
                    pub const $ident_const: Symbol = Symbol::new(ModuleId::$module_const, IdentId::from_index_named($ident_id, $ident_name));
                )*
                $(
                    pub const $u_ident_const: Symbol = Symbol::new(ModuleId::$module_const, IdentId::from_index_named($u_ident_id, $u_ident_name));
                )*
            )+

            /// The default `Apply` types that should be in scope,
            /// and what symbols they should resolve to.
            ///
            /// This is for type aliases that don't have a concrete Roc representation and as such
            /// we hide their implementation, like `Str` and `List`.
            pub fn apply_types_in_scope() -> VecMap<Ident, (Symbol, Region)> {
                let mut scope = VecMap::default();

                $(
                    $(
                        $(
                            // All types should be exposed, and all non-types
                            // should not be exposed. (Types are uppercase.)
                            //
                            // We only check this in debug builds so that in
                            // release builds, this condition is either `if true`
                            // or `if false` and will get optimized out.
                            debug_assert_eq!($exposed_apply_type, $ident_name.chars().next().unwrap().is_uppercase());
                            // Types should not be suffixed
                            debug_assert!(IdentSuffix::from_name($ident_name).is_none());

                            if $exposed_apply_type {
                                scope.insert($ident_name.into(), (Symbol::new(ModuleId::$module_const, IdentId::from_index($ident_id, IdentSuffix::None)), Region::zero()));
                            }
                        )?
                    )*
                )+

                scope
            }

            /// Types from a builtin module that should always be added to the default scope.
            #[track_caller]
            pub fn builtin_types_in_scope(module_id: ModuleId) -> &'static [(&'static str, (Symbol, Region))] {
                match module_id {
                    $(
                    ModuleId::$module_const => {
                        const LIST : &'static [(&'static str, (Symbol, Region))] = &[
                            $(
                                $(
                                    if $exposed_type {
                                        ($ident_name, (Symbol::new(ModuleId::$module_const, IdentId::from_raw($ident_id)), Region::zero()))
                                    } else {
                                        unreachable!()
                                    },
                                )?
                            )*
                        ];
                        LIST
                    }
                    )+
                    m => roc_error_macros::internal_error!("{:?} is not a builtin module!", m),
                }
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
        18 GENERIC_COPY_REF: "#generic_copy_by_ref" // copy of arbitrary layouts, passed as an opaque pointer

        19 GENERIC_EQ: "#generic_eq" // internal function that checks generic equality

        // a user-defined function that we need to capture in a closure
        // see e.g. Set.walk
        20 USER_FUNCTION: "#user_function"

        // A caller (wrapper) that we pass to zig for it to be able to call Roc functions
        21 ZIG_FUNCTION_CALLER: "#zig_function_caller"

        // a caller (wrapper) for comparison
        22 GENERIC_COMPARE_REF: "#generic_compare_ref"

        // used to initialize parameters in borrow.rs
        23 EMPTY_PARAM: "#empty_param"

        // used by the dev backend to store the pointer to where to store large return types
        24 RET_POINTER: "#ret_pointer"

        // used in wasm dev backend to mark temporary values in the VM stack
        25 WASM_TMP: "#wasm_tmp"

        // the _ used in mono when a specialized symbol is deleted
        26 REMOVED_SPECIALIZATION: "#removed_specialization"

        // used in dev backend
        27 DEV_TMP: "#dev_tmp"
        28 DEV_TMP2: "#dev_tmp2"
        29 DEV_TMP3: "#dev_tmp3"
        30 DEV_TMP4: "#dev_tmp4"
        31 DEV_TMP5: "#dev_tmp5"

        32 ATTR_INVALID: "#attr_invalid"

        33 CLONE: "#clone" // internal function that clones a value into a buffer
    }
    // Fake module for synthesizing and storing derived implementations
    1 DERIVED_SYNTH: "#Derived" => {
    }
    // Fake module from which derived implementations are code-generated
    2 DERIVED_GEN: "#Derived_gen" => {
    }
    3 NUM: "Num" => {
        0 NUM_NUM: "Num" exposed_type=true  // the Num.Num type alias
        1 NUM_I128: "I128" exposed_type=true  // the Num.I128 type alias
        2 NUM_U128: "U128" exposed_type=true  // the Num.U128 type alias
        3 NUM_I64: "I64" exposed_type=true  // the Num.I64 type alias
        4 NUM_U64: "U64" exposed_type=true  // the Num.U64 type alias
        5 NUM_I32: "I32" exposed_type=true  // the Num.I32 type alias
        6 NUM_U32: "U32" exposed_type=true  // the Num.U32 type alias
        7 NUM_I16: "I16" exposed_type=true  // the Num.I16 type alias
        8 NUM_U16: "U16" exposed_type=true  // the Num.U16 type alias
        9 NUM_I8: "I8" exposed_type=true  // the Num.I8 type alias
        10 NUM_U8: "U8" exposed_type=true  // the Num.U8 type alias
        11 NUM_INTEGER: "Integer" exposed_type=true // Int : Num Integer
        12 NUM_F64: "F64" exposed_type=true  // the Num.F64 type alias
        13 NUM_F32: "F32" exposed_type=true  // the Num.F32 type alias
        14 NUM_FLOATINGPOINT: "FloatingPoint" exposed_type=true // Float : Num FloatingPoint
        15 NUM_MAX_F32: "max_f32"
        16 NUM_MIN_F32: "min_f32"
        17 NUM_ABS: "abs"
        18 NUM_NEG: "neg"
        19 NUM_ADD: "add"
        20 NUM_SUB: "sub"
        21 NUM_MUL: "mul"
        22 NUM_LT: "is_lt"
        23 NUM_LTE: "is_lte"
        24 NUM_GT: "is_gt"
        25 NUM_GTE: "is_gte"
        26 NUM_TO_FRAC: "to_frac"
        27 NUM_SIN: "sin"
        28 NUM_COS: "cos"
        29 NUM_TAN: "tan"
        30 NUM_IS_ZERO: "is_zero"
        31 NUM_IS_EVEN: "is_even"
        32 NUM_IS_ODD: "is_odd"
        33 NUM_IS_POSITIVE: "is_positive"
        34 NUM_IS_NEGATIVE: "is_negative"
        35 NUM_REM: "rem"
        36 NUM_REM_CHECKED: "rem_checked"
        37 NUM_DIV_FRAC: "div"
        38 NUM_DIV_FRAC_CHECKED: "div_checked"
        39 NUM_DIV_TRUNC: "div_trunc"
        40 NUM_DIV_TRUNC_CHECKED: "div_trunc_checked"
        41 NUM_SQRT: "sqrt"
        42 NUM_SQRT_CHECKED: "sqrt_checked"
        43 NUM_LOG: "log"
        44 NUM_LOG_CHECKED: "log_checked"
        45 NUM_ROUND: "round"
        46 NUM_COMPARE: "compare"
        47 NUM_POW: "pow"
        48 NUM_CEILING: "ceiling"
        49 NUM_POW_INT: "pow_int"
        50 NUM_FLOOR: "floor"
        51 NUM_ADD_WRAP: "add_wrap"
        52 NUM_ADD_CHECKED: "add_checked"
        53 NUM_ADD_SATURATED: "add_saturated"
        54 NUM_ATAN: "atan"
        55 NUM_ACOS: "acos"
        56 NUM_ASIN: "asin"
        57 NUM_SIGNED128: "Signed128" exposed_type=true
        58 NUM_SIGNED64: "Signed64" exposed_type=true
        59 NUM_SIGNED32: "Signed32" exposed_type=true
        60 NUM_SIGNED16: "Signed16" exposed_type=true
        61 NUM_SIGNED8: "Signed8" exposed_type=true
        62 NUM_UNSIGNED128: "Unsigned128" exposed_type=true
        63 NUM_UNSIGNED64: "Unsigned64" exposed_type=true
        64 NUM_UNSIGNED32: "Unsigned32" exposed_type=true
        65 NUM_UNSIGNED16: "Unsigned16" exposed_type=true
        66 NUM_UNSIGNED8: "Unsigned8" exposed_type=true
        67 NUM_BINARY64: "Binary64" exposed_type=true
        68 NUM_BINARY32: "Binary32" exposed_type=true
        69 NUM_BITWISE_AND: "bitwise_and"
        70 NUM_BITWISE_XOR: "bitwise_xor"
        71 NUM_BITWISE_OR: "bitwise_or"
        72 NUM_SHIFT_LEFT: "shift_left_by"
        73 NUM_SHIFT_RIGHT: "shift_right_by"
        74 NUM_SHIFT_RIGHT_ZERO_FILL: "shift_right_zf_by"
        75 NUM_SUB_WRAP: "sub_wrap"
        76 NUM_SUB_CHECKED: "sub_checked"
        77 NUM_SUB_SATURATED: "sub_saturated"
        78 NUM_MUL_WRAP: "mul_wrap"
        79 NUM_MUL_CHECKED: "mul_checked"
        80 NUM_MUL_SATURATED: "mul_saturated"
        81 NUM_INT: "Int" exposed_type=true
        82 NUM_FRAC: "Frac" exposed_type=true
        83 NUM_E: "e"
        84 NUM_PI: "pi"
        85 NUM_TAU: "tau"
        86 NUM_IS_MULTIPLE_OF: "is_multiple_of"
        87 NUM_DECIMAL: "Decimal" exposed_type=true
        88 NUM_DEC: "Dec" exposed_type=true  // the Num.Dectype alias
        89 NUM_COUNT_ONE_BITS: "count_one_bits"
        90 NUM_ABS_DIFF: "abs_diff"
        91 NUM_IS_NAN: "is_nan"
        92 NUM_IS_INFINITE: "is_infinite"
        93 NUM_IS_FINITE: "is_finite"
        94 NUM_COUNT_LEADING_ZERO_BITS: "count_leading_zero_bits"
        95 NUM_COUNT_TRAILING_ZERO_BITS: "count_trailing_zero_bits"
        96 NUM_TO_STR: "to_str"
        97 NUM_MIN_I8: "min_i8"
        98 NUM_MAX_I8: "max_i8"
        99 NUM_MIN_U8: "min_u8"
        100 NUM_MAX_U8: "max_u8"
        101 NUM_MIN_I16: "min_i16"
        102 NUM_MAX_I16: "max_i16"
        103 NUM_MIN_U16: "min_u16"
        104 NUM_MAX_U16: "max_u16"
        105 NUM_MIN_I32: "min_i32"
        106 NUM_MAX_I32: "max_i32"
        107 NUM_MIN_U32: "min_u32"
        108 NUM_MAX_U32: "max_u32"
        109 NUM_MIN_I64: "min_i64"
        110 NUM_MAX_I64: "max_i64"
        111 NUM_MIN_U64: "min_u64"
        112 NUM_MAX_U64: "max_u64"
        113 NUM_MIN_I128: "min_i128"
        114 NUM_MAX_I128: "max_i128"
        115 NUM_MIN_U128: "min_u128"
        116 NUM_MAX_U128: "max_u128"
        117 NUM_TO_I8: "to_i8"
        118 NUM_TO_I8_CHECKED: "to_i8_checked"
        119 NUM_TO_I16: "to_i16"
        120 NUM_TO_I16_CHECKED: "to_i16_checked"
        121 NUM_TO_I32: "to_i32"
        122 NUM_TO_I32_CHECKED: "to_i32_checked"
        123 NUM_TO_I64: "to_i64"
        124 NUM_TO_I64_CHECKED: "to_i64_checked"
        125 NUM_TO_I128: "to_i128"
        126 NUM_TO_I128_CHECKED: "to_i128_checked"
        127 NUM_TO_U8: "to_u8"
        128 NUM_TO_U8_CHECKED: "to_u8_checked"
        129 NUM_TO_U16: "to_u16"
        130 NUM_TO_U16_CHECKED: "to_u16_checked"
        131 NUM_TO_U32: "to_u32"
        132 NUM_TO_U32_CHECKED: "to_u32_checked"
        133 NUM_TO_U64: "to_u64"
        134 NUM_TO_U64_CHECKED: "to_u64_checked"
        135 NUM_TO_U128: "to_u128"
        136 NUM_TO_U128_CHECKED: "to_u128_checked"
        137 NUM_DIV_CEIL: "div_ceil"
        138 NUM_DIV_CEIL_CHECKED: "div_ceil_checked"
        139 NUM_TO_F32: "to_f32"
        140 NUM_TO_F32_CHECKED: "to_f32_checked"
        141 NUM_TO_F64: "to_f64"
        142 NUM_TO_F64_CHECKED: "to_f64_checked"
        143 NUM_MAX_F64: "max_f64"
        144 NUM_MIN_F64: "min_f64"
        145 NUM_ADD_CHECKED_LOWLEVEL: "add_checked_lowlevel"
        146 NUM_SUB_CHECKED_LOWLEVEL: "sub_checked_lowlevel"
        147 NUM_MUL_CHECKED_LOWLEVEL: "mul_checked_lowlevel"
        148 NUM_MIN: "min"
        149 NUM_MAX: "max"
        150 NUM_BITWISE_NOT: "bitwise_not"
        151 NUM_INT_CAST: "int_cast"
        152 NUM_IS_APPROX_EQ: "is_approx_eq"
        153 NUM_BYTES_TO_U16_LOWLEVEL: "bytes_to_u16_owlevel"
        154 NUM_BYTES_TO_U32_LOWLEVEL: "bytes_to_u32_lowlevel"
        155 NUM_BYTES_TO_U64_LOWLEVEL: "bytes_to_u64_lowlevel"
        156 NUM_BYTES_TO_U128_LOWLEVEL: "bytes_to_u128_lowlevel"
        157 NUM_DIV_TRUNC_UNCHECKED: "div_trunc_unchecked" // traps on division by zero
        158 NUM_REM_UNCHECKED: "rem_unchecked" // traps on division by zero
        159 NUM_WITHOUT_DECIMAL_POINT: "without_decimal_point"
        160 NUM_WITH_DECIMAL_POINT: "with_decimal_point"
        161 NUM_F32_TO_PARTS: "f32_to_parts"
        162 NUM_F64_TO_PARTS: "f64_to_parts"
        163 NUM_F32_FROM_PARTS: "f32_from_parts"
        164 NUM_F64_FROM_PARTS: "f64_from_parts"
        165 NUM_NAN_F32: "nan_f32"
        166 NUM_NAN_F64: "nan_f64"
        167 NUM_INFINITY_F32: "infinity_f32"
        168 NUM_INFINITY_F64: "infinity_f64"
        169 NUM_FROM_BOOL: "from_bool"
        170 NUM_F32_TO_BITS: "f32_to_bits"
        171 NUM_F64_TO_BITS: "f64_to_bits"
        172 NUM_DEC_TO_BITS: "dec_to_bits"
        173 NUM_F32_FROM_BITS: "f32_from_bits"
        174 NUM_F64_FROM_BITS: "f64_from_bits"
        175 NUM_DEC_FROM_BITS: "dec_from_bits"
    }
    4 BOOL: "Bool" => {
        0 BOOL_BOOL: "Bool" exposed_type=true // the Bool.Bool type alias
        1 BOOL_FALSE: "false"
        2 BOOL_TRUE: "true"
        3 BOOL_NOT: "not"
        4 BOOL_XOR: "xor"
        5 BOOL_NEQ: "is_not_eq"
        6 BOOL_EQ: "Eq" exposed_type=true
        7 BOOL_IS_EQ: "is_eq"
        8 BOOL_IS_EQ_IMPL: "bool_is_eq"
        unexposed 9 BOOL_STRUCTURAL_EQ: "structural_eq"
        unexposed 10 BOOL_STRUCTURAL_NOT_EQ: "structural_not_eq"
    }
    5 STR: "Str" => {
        0 STR_STR: "Str" exposed_apply_type=true // the Str.Str type alias
        1 STR_IS_EMPTY: "is_empty"
        2 STR_APPEND: "#append" // unused
        3 STR_CONCAT: "concat"
        4 STR_JOIN_WITH: "join_with"
        5 STR_SPLIT_ON: "split_on"
        6 STR_WITH_PREFIX: "with_prefix"
        7 STR_STARTS_WITH: "starts_with"
        8 STR_ENDS_WITH: "ends_with"
        9 STR_FROM_UTF8: "from_utf8"
        10 STR_FROM_UTF8_LOSSY: "from_utf8_lossy"
        11 STR_UTF8_BYTE_PROBLEM: "Utf8Problem"
        12 STR_TO_UTF8: "to_utf8"
        13 STR_WALK_UTF8: "walk_utf8"
        14 STR_ALIAS_ANALYSIS_STATIC: "#aliasAnalysisStatic" // string with the static lifetime
        15 STR_FROM_UTF8_RANGE: "from_utf8_range"
        16 STR_REPEAT: "repeat"
        17 STR_TRIM: "trim"
        18 STR_TRIM_START: "trim_start"
        19 STR_TRIM_END: "trim_end"
        20 STR_WITH_CAPACITY: "with_capacity"
        21 STR_TO_F64: "to_f64"
        22 STR_TO_F32: "to_f32"
        23 STR_TO_DEC: "to_dec"
        24 STR_TO_U128: "to_u128"
        25 STR_TO_I128: "to_i128"
        26 STR_TO_U64: "to_u64"
        27 STR_TO_I64: "to_i64"
        28 STR_TO_U32: "to_u32"
        29 STR_TO_I32: "to_i32"
        30 STR_TO_U16: "to_u16"
        31 STR_TO_I16: "to_i16"
        32 STR_TO_U8: "to_u8"
        33 STR_TO_I8: "to_i8"
        34 STR_CONTAINS: "contains"
        35 STR_GET_UNSAFE: "get_unsafe"
        36 STR_COUNT_UTF8_BYTES: "count_utf8_bytes"
        37 STR_SUBSTRING_UNSAFE: "substring_unsafe"
        38 STR_SPLIT_FIRST: "split_first"
        39 STR_SPLIT_LAST: "split_last"
        40 STR_WALK_UTF8_WITH_INDEX: "walk_utf8_with_index"
        41 STR_RESERVE: "reserve"
        42 STR_TO_NUM: "str_to_num"
        43 STR_FROM_UTF8_LOWLEVEL: "from_utf8_lowlevel"
        44 STR_CAPACITY: "capacity"
        45 STR_REPLACE_EACH: "replace_each"
        46 STR_REPLACE_FIRST: "replace_first"
        47 STR_REPLACE_LAST: "replace_last"
        48 STR_RELEASE_EXCESS_CAPACITY: "release_excess_capacity"
        49 STR_DROP_PREFIX: "drop_prefix"
        50 STR_DROP_SUFFIX: "drop_suffix"
        51 STR_WITH_ASCII_LOWERCASED: "with_ascii_lowercased"
        52 STR_WITH_ASCII_UPPERCASED: "with_ascii_uppercased"
        53 STR_CASELESS_ASCII_EQUALS: "caseless_ascii_equals"
        54 STR_FROM_UTF16: "from_utf16"
        55 STR_FROM_UTF16_LOSSY: "from_utf16_lossy"
        56 STR_FROM_UTF32: "from_utf32"
        57 STR_FROM_UTF32_LOSSY: "from_utf32_lossy"
    }
    6 LIST: "List" => {
        0 LIST_LIST: "List" exposed_apply_type=true // the List.List type alias
        1 LIST_IS_EMPTY: "is_empty"
        2 LIST_GET: "get"
        3 LIST_SET: "set"
        4 LIST_APPEND: "append"
        5 LIST_MAP: "map"
        6 LIST_LEN_U64: "len"
        7 LIST_WALK_BACKWARDS: "walk_backwards"
        8 LIST_CONCAT: "concat"
        9 LIST_FIRST: "first"
        10 LIST_SINGLE: "single"
        11 LIST_REPEAT: "repeat"
        12 LIST_REVERSE: "reverse"
        13 LIST_PREPEND: "prepend"
        14 LIST_JOIN: "join"
        15 LIST_KEEP_IF: "keep_if"
        16 LIST_CONTAINS: "contains"
        17 LIST_SUM: "sum"
        18 LIST_WALK: "walk"
        19 LIST_LAST: "last"
        20 LIST_KEEP_OKS: "keep_oks"
        21 LIST_KEEP_ERRS: "keep_errs"
        22 LIST_MAP_WITH_INDEX: "map_with_index"
        23 LIST_MAP2: "map2"
        24 LIST_MAP3: "map3"
        25 LIST_PRODUCT: "product"
        26 LIST_WALK_UNTIL: "walk_until"
        27 LIST_RANGE: "range"
        28 LIST_SORT_WITH: "sort_with"
        29 LIST_CHUNKS_OF: "chunks_of"
        30 LIST_SWAP: "swap"
        31 LIST_DROP_AT: "drop_at"
        32 LIST_DROP_LAST: "drop_last"
        33 LIST_MIN: "min"
        34 LIST_MIN_LT: "#min_lt"
        35 LIST_MAX: "max"
        36 LIST_MAX_GT: "#max_gt"
        37 LIST_MAP4: "map4"
        38 LIST_DROP_FIRST: "drop_first"
        39 LIST_JOIN_MAP: "join_map"
        40 LIST_JOIN_MAP_CONCAT: "#join_map_concat"
        41 LIST_ANY: "any"
        42 LIST_TAKE_FIRST: "take_first"
        43 LIST_TAKE_LAST: "take_last"
        44 LIST_FIND_FIRST: "find_first"
        45 LIST_FIND_LAST: "find_last"
        46 LIST_FIND_FIRST_INDEX: "find_first_index"
        47 LIST_FIND_LAST_INDEX: "find_last_index"
        48 LIST_FIND_RESULT: "#find_result" // symbol used in the definition of List.findFirst
        49 LIST_SUBLIST: "sublist"
        50 LIST_INTERSPERSE: "intersperse"
        51 LIST_INTERSPERSE_CLOS: "#intersperse_clos"
        52 LIST_SPLIT_AT: "split_at"
        53 LIST_SPLIT_FIRST: "split_first"
        54 LIST_SPLIT_LAST: "split_last"
        55 LIST_SPLIT_CLOS: "#split_clos"
        56 LIST_ALL: "all"
        57 LIST_DROP_IF: "drop_if"
        58 LIST_DROP_IF_PREDICATE: "#drop_if_pred"
        59 LIST_SORT_ASC: "sort_asc"
        60 LIST_SORT_DESC: "sort_desc"
        61 LIST_SORT_DESC_COMPARE: "#sort_desc_compare"
        62 LIST_STARTS_WITH: "starts_with"
        63 LIST_ENDS_WITH: "ends_with"
        64 LIST_REPLACE: "replace"
        65 LIST_IS_UNIQUE: "#is_unique"
        66 LIST_GET_UNSAFE: "get_unsafe"
        67 LIST_REPLACE_UNSAFE: "replace_unsafe"
        68 LIST_WITH_CAPACITY: "with_capacity"
        69 LIST_UNREACHABLE: "unreachable"
        70 LIST_RESERVE: "reserve"
        71 LIST_APPEND_UNSAFE: "append_unsafe"
        72 LIST_SUBLIST_LOWLEVEL: "sublist_lowlevel"
        73 LIST_CAPACITY: "capacity"
        74 LIST_MAP_TRY: "map_try"
        75 LIST_WALK_TRY: "walk_try"
        76 LIST_WALK_BACKWARDS_UNTIL: "walk_backwards_until"
        77 LIST_COUNT_IF: "count_if"
        78 LIST_WALK_FROM: "walk_from"
        79 LIST_WALK_FROM_UNTIL: "walk_from_until"
        80 LIST_ITER_HELP: "iter_help"
        81 LIST_RELEASE_EXCESS_CAPACITY: "release_excess_capacity"
        82 LIST_UPDATE: "update"
        83 LIST_WALK_WITH_INDEX: "walk_with_index"
        84 LIST_APPEND_IF_OK: "append_if_ok"
        85 LIST_PREPEND_IF_OK: "prepend_if_ok"
        86 LIST_WALK_WITH_INDEX_UNTIL: "walk_with_index_until"
        87 LIST_CLONE: "clone"
        88 LIST_LEN_USIZE: "len_usize"
        89 LIST_CONCAT_UTF8: "concat_utf8"
        90 LIST_FOR_EACH_FX: "for_each!"
        91 LIST_FOR_EACH_TRY_FX: "for_each_try!"
        92 LIST_WALK_FX: "walk!"
        93 LIST_SPLIT_ON: "split_on"
        94 LIST_SPLIT_ON_LIST: "split_on_list"
        95 LIST_WALK_TRY_FX: "walk_try!"
        96 LIST_MAP_TRY_FX: "map_try!"
    }
    7 RESULT: "Result" => {
        0 RESULT_RESULT: "Result" exposed_type=true // the Result.Result type alias
        1 RESULT_IS_ERR: "is_err"
        2 RESULT_ON_ERR: "on_err"
        3 RESULT_MAP_OK: "map_ok"
        4 RESULT_MAP_ERR: "map_err"
        5 RESULT_WITH_DEFAULT: "with_default"
        6 RESULT_TRY: "try"
        7 RESULT_IS_OK: "is_ok"
        8 RESULT_MAP_BOTH: "map_both"
        9 RESULT_MAP_TWO: "map2"
        10 RESULT_ON_ERR_FX: "on_err!"
    }
    8 DICT: "Dict" => {
        0 DICT_DICT: "Dict" exposed_type=true // the Dict.Dict type alias
        1 DICT_EMPTY: "empty"
        2 DICT_SINGLE: "single"
        3 DICT_CLEAR: "clear"
        4 DICT_LEN: "len"
        5 DICT_GET: "get"
        6 DICT_GET_RESULT: "#get_result" // symbol used in the definition of Dict.get
        7 DICT_CONTAINS: "contains"
        8 DICT_INSERT: "insert"
        9 DICT_REMOVE: "remove"

        10 DICT_WALK: "walk"
        11 DICT_WALK_UNTIL: "walk_until"
        12 DICT_FROM_LIST: "from_list"
        13 DICT_TO_LIST: "to_list"
        14 DICT_KEYS: "keys"
        15 DICT_VALUES: "values"

        16 DICT_INSERT_ALL: "insert_all" // union
        17 DICT_KEEP_SHARED: "keep_shared" // intersection
        18 DICT_REMOVE_ALL: "remove_all" // difference

        19 DICT_WITH_CAPACITY: "with_capacity"
        20 DICT_CAPACITY: "capacity"
        21 DICT_UPDATE: "update"

        22 DICT_LIST_GET_UNSAFE: "list_get_unsafe"
        23 DICT_PSEUDO_SEED: "pseudo_seed"
        24 DICT_IS_EMPTY: "is_empty"
        25 DICT_MAP: "map"
        26 DICT_JOINMAP: "join_map"
        27 DICT_KEEP_IF: "keep_if"
        28 DICT_DROP_IF: "drop_if"
        29 DICT_RESERVE: "reserve"
        30 DICT_RELEASE_EXCESS_CAPACITY: "release_excess_capacity"
    }
    9 SET: "Set" => {
        0 SET_SET: "Set" exposed_type=true // the Set.Set type alias
        1 SET_EMPTY: "empty"
        2 SET_SINGLE: "single"
        3 SET_LEN: "len"
        4 SET_INSERT: "insert"
        5 SET_REMOVE: "remove"
        6 SET_UNION: "union"
        7 SET_DIFFERENCE: "difference"
        8 SET_INTERSECTION: "intersection"
        9 SET_TO_LIST: "to_list"
        10 SET_FROM_LIST: "from_list"
        11 SET_WALK: "walk"
        12 SET_WALK_UNTIL: "walk_until"
        13 SET_WALK_USER_FUNCTION: "#walk_user_function"
        14 SET_CONTAINS: "contains"
        15 SET_TO_DICT: "to_dict"
        16 SET_CAPACITY: "capacity"
        17 SET_IS_EMPTY: "is_empty"
        18 SET_MAP: "map"
        19 SET_JOIN_MAP: "join_map"
        20 SET_KEEP_IF: "keep_if"
        21 SET_DROP_IF: "drop_if"
        22 SET_WITH_CAPACITY: "with_capacity"
        23 SET_RESERVE: "reserve"
        24 SET_RELEASE_EXCESS_CAPACITY: "release_excess_capacity"
    }
    10 BOX: "Box" => {
        0 BOX_BOX_TYPE: "Box" exposed_apply_type=true // the Box.Box opaque type
        1 BOX_BOX_FUNCTION: "box" // Box.box
        2 BOX_UNBOX: "unbox"
    }
    11 ENCODE: "Encode" => {
        0 ENCODE_ENCODER: "Encoder" exposed_type=true
        1 ENCODE_ENCODING: "Encoding" exposed_type=true
        2 ENCODE_TO_ENCODER: "to_encoder"
        3 ENCODE_ENCODERFORMATTING: "EncoderFormatting" exposed_type=true
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
        20 ENCODE_RECORD: "record"
        21 ENCODE_TUPLE: "tuple"
        22 ENCODE_TAG: "tag"
        23 ENCODE_CUSTOM: "custom"
        24 ENCODE_APPEND_WITH: "append_with"
        25 ENCODE_APPEND: "append"
        26 ENCODE_TO_BYTES: "to_bytes"
    }
    12 DECODE: "Decode" => {
        0 DECODE_DECODE_ERROR: "DecodeError" exposed_type=true
        1 DECODE_DECODE_RESULT: "DecodeResult" exposed_type=true
        2 DECODE_DECODER_OPAQUE: "Decoder" exposed_type=true
        3 DECODE_DECODING: "Decoding" exposed_type=true
        4 DECODE_DECODER: "decoder"
        5 DECODE_DECODERFORMATTING: "DecoderFormatting" exposed_type=true
        6 DECODE_U8: "u8"
        7 DECODE_U16: "u16"
        8 DECODE_U32: "u32"
        9 DECODE_U64: "u64"
        10 DECODE_U128: "u128"
        11 DECODE_I8: "i8"
        12 DECODE_I16: "i16"
        13 DECODE_I32: "i32"
        14 DECODE_I64: "i64"
        15 DECODE_I128: "i128"
        16 DECODE_F32: "f32"
        17 DECODE_F64: "f64"
        18 DECODE_DEC: "dec"
        19 DECODE_BOOL: "bool"
        20 DECODE_STRING: "string"
        21 DECODE_LIST: "list"
        22 DECODE_RECORD: "record"
        23 DECODE_TUPLE: "tuple"
        24 DECODE_CUSTOM: "custom"
        25 DECODE_DECODE_WITH: "decode_with"
        26 DECODE_FROM_BYTES_PARTIAL: "from_bytes_partial"
        27 DECODE_FROM_BYTES: "from_bytes"
        28 DECODE_MAP_RESULT: "map_result"
    }
    13 HASH: "Hash" => {
        0 HASH_HASH_ABILITY: "Hash" exposed_type=true
        1 HASH_HASH: "hash"
        2 HASH_HASHER: "Hasher" exposed_type=true
        3  HASH_ADD_BYTES: "add_bytes"
        4  HASH_ADD_U8: "add_u8"
        5  HASH_ADD_U16: "add_u16"
        6  HASH_ADD_U32: "add_u32"
        7  HASH_ADD_U64: "add_u64"
        8  HASH_ADD_U128: "add_u128"
        9  HASH_HASH_BOOL: "hash_bool"
        10 HASH_HASH_I8: "hash_i8"
        11 HASH_HASH_I16: "hash_i16"
        12 HASH_HASH_I32: "hash_i32"
        13 HASH_HASH_I64: "hash_i64"
        14 HASH_HASH_I128: "hash_i128"
        15 HASH_HASH_UNORDERED: "hash_unordered"
        16 I128_OF_DEC: "i128_of_dec"
        17 HASH_HASH_DEC: "hash_dec"
        18 HASH_COMPLETE: "complete"
        19 HASH_HASH_STR_BYTES: "hash_str_bytes"
        20 HASH_HASH_LIST: "hash_list"
    }
    14 INSPECT: "Inspect" => {
        0 INSPECT_INSPECT_ABILITY: "Inspect" exposed_type=true
        1 INSPECT_INSPECTOR: "Inspector" exposed_type=true
        2 INSPECT_INSPECT_FORMATTER: "InspectFormatter" exposed_type=true
        3 INSPECT_ELEM_WALKER: "ElemWalker" exposed_type=true
        4 INSPECT_KEY_VAL_WALKER: "KeyValWalker" exposed_type=true
        5 INSPECT_INSPECT: "inspect"
        6 INSPECT_INIT: "init"
        7 INSPECT_LIST: "list"
        8 INSPECT_SET: "set"
        9 INSPECT_DICT: "dict"
        10 INSPECT_TAG: "tag"
        11 INSPECT_TUPLE: "tuple"
        12 INSPECT_RECORD: "record"
        13 INSPECT_BOOL: "bool"
        14 INSPECT_STR: "str"
        15 INSPECT_OPAQUE: "opaque"
        16 INSPECT_FUNCTION: "function"
        17 INSPECT_U8: "u8"
        18 INSPECT_I8: "i8"
        19 INSPECT_U16: "u16"
        20 INSPECT_I16: "i16"
        21 INSPECT_U32: "u32"
        22 INSPECT_I32: "i32"
        23 INSPECT_U64: "u64"
        24 INSPECT_I64: "i64"
        25 INSPECT_U128: "u128"
        26 INSPECT_I128: "i128"
        27 INSPECT_F32: "f32"
        28 INSPECT_F64: "f64"
        29 INSPECT_DEC: "dec"
        30 INSPECT_CUSTOM: "custom"
        31 INSPECT_APPLY: "apply"
        32 INSPECT_TO_INSPECTOR: "to_inspector"
        33 INSPECT_TO_STR: "to_str"
    }

    num_modules: 15 // Keep this count up to date by hand! (TODO: see the mut_map! macro for how we could determine this count correctly in the macro)
}
