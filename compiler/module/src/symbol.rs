use crate::ident::Ident;
use inlinable_string::InlinableString;
use roc_collections::all::{default_hasher, ImMap, MutMap};
use roc_region::all::Region;
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

    pub fn module_string<'a>(&self, interns: &'a Interns) -> &'a InlinableString {
        interns
            .module_ids
            .get_name(self.module_id())
            .unwrap_or_else(|| {
                panic!(
                    "module_string could not find IdentIds for {:?} in interns {:?}",
                    self.module_id(),
                    interns
                )
            })
    }

    pub fn ident_string(self, interns: &Interns) -> &InlinableString {
        let ident_ids = interns
            .all_ident_ids
            .get(&self.module_id())
            .unwrap_or_else(|| {
                panic!(
                    "ident_string could not find IdentIds for {:?} in interns {:?}",
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

#[derive(Debug)]
pub struct Interns {
    pub module_ids: ModuleIds,
    pub all_ident_ids: MutMap<ModuleId, IdentIds>,
}

impl Interns {
    pub fn symbol(&self, module_id: ModuleId, ident: InlinableString) -> Symbol {
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

            /// The default idents that should be in scope,
            /// and what symbols they should resolve to.
            ///
            /// This is for type aliases like `Int` and `Str` and such.
            pub fn default_in_scope() -> ImMap<Ident, (Symbol, Region)> {
                let mut scope = ImMap::default();

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

// NOTE: Some of these builtins have a # at the beginning of their names.
// This is because they are for compiler use only, and should not cause
// namespace conflicts with userspace!
define_builtins! {
    0 ATTR: "#Attr" => {
        0 UNDERSCORE: "_" // the _ used in pattern matches. This is Symbol 0.
        1 ATTR_ATTR: "Attr" // the #Attr.Attr type alias, used in uniqueness types.
    }
    1 NUM: "Num" => {
        0 NUM_NUM: "Num" imported // the Num.Num type alias
        1 NUM_AT_NUM: "@Num" // the Num.@Num private tag
        2 NUM_ABS: "abs"
        3 NUM_NEG: "neg"
        4 NUM_ADD: "add"
        5 NUM_SUB: "sub"
        6 NUM_MUL: "mul"
        7 NUM_LT: "isLt"
        8 NUM_LE: "isLte"
        9 NUM_GT: "isGt"
        10 NUM_GE: "isGte"
        11 NUM_TO_FLOAT: "toFloat"
    }
    2 INT: "Int" => {
        0 INT_INT: "Int" imported // the Int.Int type alias
        1 INT_INTEGER: "Integer" imported // Int : Num Integer
        2 INT_AT_INTEGER: "@Integer" // the Int.@Integer private tag
        3 INT_DIV: "div"
        4 INT_MOD: "mod"
        5 INT_HIGHEST: "highest"
        6 INT_LOWEST: "lowest"
        7 INT_ADD: "#add"
        8 INT_SUB: "#sub"
    }
    3 FLOAT: "Float" => {
        0 FLOAT_FLOAT: "Float" imported // the Float.Float type alias
        1 FLOAT_FLOATINGPOINT: "FloatingPoint" imported // Float : Num FloatingPoint
        2 FLOAT_AT_FLOATINGPOINT: "@FloatingPoint" // the Float.@FloatingPoint private tag
        3 FLOAT_DIV: "div"
        4 FLOAT_MOD: "mod"
        5 FLOAT_SQRT: "sqrt"
        6 FLOAT_HIGHEST: "highest"
        7 FLOAT_LOWEST: "lowest"
        8 FLOAT_ADD: "#add"
        9 FLOAT_SUB: "#sub"
    }
    4 BOOL: "Bool" => {
        0 BOOL_BOOL: "Bool" imported // the Bool.Bool type alias
        1 BOOL_AND: "and"
        2 BOOL_OR: "or"
        3 BOOL_NOT: "not"
        4 BOOL_XOR: "xor"
        5 BOOL_EQ: "isEq"
        6 BOOL_NEQ: "isNotEq"
    }
    5 STR: "Str" => {
        0 STR_STR: "Str" imported // the Str.Str type alias
        1 STR_AT_STR: "@Str" // the Str.@Str private tag
        2 STR_ISEMPTY: "isEmpty"
    }
    6 LIST: "List" => {
        0 LIST_LIST: "List" imported // the List.List type alias
        1 LIST_AT_LIST: "@List" // the List.@List private tag
        2 LIST_ISEMPTY: "isEmpty"
        3 LIST_GET: "get"
        4 LIST_SET: "set"
        5 LIST_SET_IN_PLACE: "#setInPlace"
        6 LIST_PUSH: "push"
        7 LIST_MAP: "map"
        8 LIST_LENGTH: "length"
        9 LIST_FOLDL: "foldl"
        10 LIST_FOLDR: "foldr"
        11 LIST_GET_UNSAFE: "getUnsafe" // TODO remove once we can code gen Result
    }
    7 RESULT: "Result" => {
        0 RESULT_RESULT: "Result" imported // the Result.Result type alias
        1 RESULT_MAP: "map"
    }
    8 MAP: "Map" => {
        0 MAP_MAP: "Map" imported // the Map.Map type alias
        1 MAP_AT_MAP: "@Map" // the Map.@Map private tag
        2 MAP_EMPTY: "empty"
        3 MAP_SINGLETON: "singleton"
        4 MAP_GET: "get"
        5 MAP_INSERT: "insert"
    }
    9 SET: "Set" => {
        0 SET_SET: "Set" imported // the Set.Set type alias
        1 SET_AT_SET: "@Set" // the Set.@Set private tag
        2 SET_EMPTY: "empty"
        3 SET_SINGLETON: "singleton"
        4 SET_UNION: "union"
        5 SET_FOLDL: "foldl"
        6 SET_INSERT: "insert"
        7 SET_REMOVE: "remove"
        8 SET_DIFF: "diff"
    }

    num_modules: 10 // Keep this count up to date by hand! (Rust macros can't do arithmetic.)
}
