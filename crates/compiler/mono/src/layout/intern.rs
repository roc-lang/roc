use std::{
    cell::RefCell,
    hash::{BuildHasher, Hasher},
    marker::PhantomData,
    sync::Arc,
};

use parking_lot::{Mutex, RwLock};
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::{default_hasher, BumpMap};
use roc_target::TargetInfo;

use super::{Builtin, Layout};

#[allow(unused)] // for now
pub struct InLayouts(PhantomData<()>);

macro_rules! cache_interned_layouts {
    ($($i:literal, $name:ident, $layout:expr)*; $total_constants:literal) => {
        impl InLayouts {
            $(
            #[allow(unused)] // for now
            pub const $name: InLayout<'static> = unsafe { InLayout::from_reserved_index($i) };
            )*
        }

        fn fill_reserved_layouts<'a>(interner: &mut STLayoutInterner<'a>) {
            assert!(interner.is_empty());
            $(
            interner.insert($layout);
            )*
        }

        const fn _are_constants_in_order_non_redundant() -> usize {
            let mut total_seen = 0;
            $(total_seen += ($i * 0) + 1;)*
            match 0usize {
                $($i => {})*
                _ => {}
            }
            total_seen
        }

        const _ASSERT_NON_REDUNDANT_CONSTANTS: () =
            assert!(_are_constants_in_order_non_redundant() == $total_constants);
    }
}

cache_interned_layouts! {
    0,  VOID, Layout::VOID
    1,  UNIT, Layout::UNIT
    2,  BOOL, Layout::Builtin(Builtin::Bool)
    3,  U8,   Layout::Builtin(Builtin::Int(IntWidth::U8))
    4,  U16,  Layout::Builtin(Builtin::Int(IntWidth::U16))
    5,  U32,  Layout::Builtin(Builtin::Int(IntWidth::U32))
    6,  U64,  Layout::Builtin(Builtin::Int(IntWidth::U64))
    7,  U128, Layout::Builtin(Builtin::Int(IntWidth::U128))
    8,  I8,   Layout::Builtin(Builtin::Int(IntWidth::I8))
    9,  I16,  Layout::Builtin(Builtin::Int(IntWidth::I16))
    10, I32,  Layout::Builtin(Builtin::Int(IntWidth::I32))
    11, I64,  Layout::Builtin(Builtin::Int(IntWidth::I64))
    12, I128, Layout::Builtin(Builtin::Int(IntWidth::I128))
    13, F32,  Layout::Builtin(Builtin::Float(FloatWidth::F32))
    14, F64,  Layout::Builtin(Builtin::Float(FloatWidth::F64))
    15, DEC,  Layout::Builtin(Builtin::Decimal)

    ; 16
}

impl InLayouts {
    #[allow(unused)] // for now
    pub const fn from_int_width(w: IntWidth) -> InLayout<'static> {
        match w {
            IntWidth::U8 => Self::U8,
            IntWidth::U16 => Self::U16,
            IntWidth::U32 => Self::U32,
            IntWidth::U64 => Self::U64,
            IntWidth::U128 => Self::U128,
            IntWidth::I8 => Self::I8,
            IntWidth::I16 => Self::I16,
            IntWidth::I32 => Self::I32,
            IntWidth::I64 => Self::I64,
            IntWidth::I128 => Self::I128,
        }
    }
    #[allow(unused)] // for now
    pub const fn from_float_width(w: FloatWidth) -> InLayout<'static> {
        match w {
            FloatWidth::F32 => Self::F32,
            FloatWidth::F64 => Self::F64,
        }
    }
}

pub trait LayoutInterner<'a>: Sized {
    /// Interns a value, returning its interned representation.
    /// If the value has been interned before, the old interned representation will be re-used.
    ///
    /// Note that the provided value must be allocated into an arena of your choosing, but which
    /// must live at least as long as the interner lives.
    // TODO: we should consider maintaining our own arena in the interner, to avoid redundant
    // allocations when values already have interned representations.
    fn insert(&mut self, value: Layout<'a>) -> InLayout<'a>;

    /// Retrieves a value from the interner.
    fn get(&self, key: InLayout<'a>) -> Layout<'a>;

    fn alignment_bytes(&self, target_info: TargetInfo, layout: InLayout<'a>) -> u32 {
        self.get(layout).alignment_bytes(self, target_info)
    }
}

/// An interned layout.
///
/// When possible, prefer comparing/hashing on the [InLayout] representation of a value, rather
/// than the value itself.
#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InLayout<'a>(usize, std::marker::PhantomData<&'a ()>);
impl<'a> Clone for InLayout<'a> {
    fn clone(&self) -> Self {
        Self(self.0, Default::default())
    }
}

impl<'a> Copy for InLayout<'a> {}

impl<'a> InLayout<'a> {
    /// # Safety
    ///
    /// The index is not guaranteed to exist. Use this only when creating an interner with constant
    /// indices, with the variant that `insert` returns a monotonically increasing index.
    ///
    /// For example:
    ///
    /// ```ignore(illustrative)
    /// let reserved_interned = InLayout::from_reserved_index(0);
    /// let interner = GlobalLayoutInterner::with_capacity(1);
    /// let inserted = interner.insert("something");
    /// assert_eq!(reserved_interned, inserted);
    /// ```
    const unsafe fn from_reserved_index(index: usize) -> Self {
        Self(index, PhantomData)
    }
}

/// A concurrent interner, suitable for usage between threads.
///
/// The interner does not currently maintain its own arena; you will have to supply
/// values-to-be-interned as allocated in an independent arena.
///
/// If you need a concurrent global interner, you'll likely want each thread to take a
/// [TLLayoutInterner] via [GlobalLayoutInterner::fork], for caching purposes.
///
/// Originally derived from https://gist.github.com/matklad/44ba1a5a6168bc0c26c995131c007907;
/// thank you, Aleksey!
#[derive(Debug)]
pub struct GlobalLayoutInterner<'a>(Arc<GlobalLayoutInternerInner<'a>>);

#[derive(Debug)]
struct GlobalLayoutInternerInner<'a> {
    map: Mutex<BumpMap<Layout<'a>, InLayout<'a>>>,
    vec: RwLock<Vec<Layout<'a>>>,
}

/// A derivative of a [GlobalLayoutInterner] interner that provides caching desirable for
/// thread-local workloads. The only way to get a [TLLayoutInterner] is via
/// [GlobalLayoutInterner::fork].
///
/// All values interned into a [TLLayoutInterner] are made available in its parent
/// [GlobalLayoutInterner], making this suitable for global sharing of interned values.
///
/// Originally derived from https://gist.github.com/matklad/44ba1a5a6168bc0c26c995131c007907;
/// thank you, Aleksey!
#[derive(Debug)]
pub struct TLLayoutInterner<'a> {
    parent: GlobalLayoutInterner<'a>,
    map: BumpMap<Layout<'a>, InLayout<'a>>,
    /// Cache of interned values from the parent for local access.
    vec: RefCell<Vec<Option<Layout<'a>>>>,
}

/// A single-threaded interner, with no concurrency properties.
///
/// The only way to construct such an interner is to collapse a shared [GlobalLayoutInterner] into
/// a [STLayoutInterner], via [GlobalLayoutInterner::unwrap].
#[derive(Debug)]
pub struct STLayoutInterner<'a> {
    map: BumpMap<Layout<'a>, InLayout<'a>>,
    vec: Vec<Layout<'a>>,
}

/// Generic hasher for a value, to be used by all interners.
///
/// This uses the [default_hasher], so interner maps should also rely on [default_hasher].
fn hash<V: std::hash::Hash>(val: V) -> u64 {
    let mut state = roc_collections::all::BuildHasher::default().build_hasher();
    val.hash(&mut state);
    state.finish()
}

impl<'a> GlobalLayoutInterner<'a> {
    /// Creates a new global interner with the given capacity.
    pub fn with_capacity(cap: usize) -> Self {
        STLayoutInterner::with_capacity(cap).into_global()
    }

    /// Creates a derivative [TLLayoutInterner] pointing back to this global interner.
    pub fn fork(&self) -> TLLayoutInterner<'a> {
        TLLayoutInterner {
            parent: Self(Arc::clone(&self.0)),
            map: Default::default(),
            vec: Default::default(),
        }
    }

    /// Collapses a shared [GlobalLayoutInterner] into a [STLayoutInterner].
    ///
    /// Returns an [Err] with `self` if there are outstanding references to the [GlobalLayoutInterner].
    pub fn unwrap(self) -> Result<STLayoutInterner<'a>, Self> {
        let GlobalLayoutInternerInner { map, vec } = match Arc::try_unwrap(self.0) {
            Ok(inner) => inner,
            Err(li) => return Err(Self(li)),
        };
        let map = Mutex::into_inner(map);
        let vec = RwLock::into_inner(vec);
        Ok(STLayoutInterner { map, vec })
    }

    /// Interns a value with a pre-computed hash.
    /// Prefer calling this when possible, especially from [TLLayoutInterner], to avoid
    /// re-computing hashes.
    fn insert_hashed(&self, value: Layout<'a>, hash: u64) -> InLayout<'a> {
        let mut map = self.0.map.lock();
        let (_, interned) = map
            .raw_entry_mut()
            .from_key_hashed_nocheck(hash, &value)
            .or_insert_with(|| {
                let mut vec = self.0.vec.write();
                let interned = InLayout(vec.len(), Default::default());
                vec.push(value);
                (value, interned)
            });
        *interned
    }

    fn get(&self, interned: InLayout<'a>) -> Layout<'a> {
        let InLayout(index, _) = interned;
        self.0.vec.read()[index]
    }

    pub fn is_empty(&self) -> bool {
        self.0.vec.read().is_empty()
    }
}

impl<'a> TLLayoutInterner<'a> {
    /// Records an interned value in thread-specific storage, for faster access on lookups.
    fn record(&self, key: Layout<'a>, interned: InLayout<'a>) {
        let mut vec = self.vec.borrow_mut();
        let len = vec.len().max(interned.0 + 1);
        vec.resize(len, None);
        vec[interned.0] = Some(key);
    }
}

impl<'a> LayoutInterner<'a> for TLLayoutInterner<'a> {
    fn insert(&mut self, value: Layout<'a>) -> InLayout<'a> {
        let global = &self.parent;
        let hash = hash(value);
        let (&mut value, &mut interned) = self
            .map
            .raw_entry_mut()
            .from_key_hashed_nocheck(hash, &value)
            .or_insert_with(|| {
                let interned = global.insert_hashed(value, hash);
                (value, interned)
            });
        self.record(value, interned);
        interned
    }

    fn get(&self, key: InLayout<'a>) -> Layout<'a> {
        if let Some(Some(value)) = self.vec.borrow().get(key.0) {
            return *value;
        }
        let value = self.parent.get(key);
        self.record(value, key);
        value
    }
}

impl<'a> STLayoutInterner<'a> {
    /// Creates a new single threaded interner with the given capacity.
    pub fn with_capacity(cap: usize) -> Self {
        let mut interner = Self {
            map: BumpMap::with_capacity_and_hasher(cap, default_hasher()),
            vec: Vec::with_capacity(cap),
        };
        fill_reserved_layouts(&mut interner);
        interner
    }

    /// Promotes the [STLayoutInterner] back to a [GlobalLayoutInterner].
    ///
    /// You should *only* use this if you need to go from a single-threaded to a concurrent context,
    /// or in a case where you explicitly need access to [TLLayoutInterner]s.
    pub fn into_global(self) -> GlobalLayoutInterner<'a> {
        let STLayoutInterner { map, vec } = self;
        GlobalLayoutInterner(Arc::new(GlobalLayoutInternerInner {
            map: Mutex::new(map),
            vec: RwLock::new(vec),
        }))
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }
}

impl<'a> LayoutInterner<'a> for STLayoutInterner<'a> {
    fn insert(&mut self, value: Layout<'a>) -> InLayout<'a> {
        let hash = hash(value);
        let (_, interned) = self
            .map
            .raw_entry_mut()
            .from_key_hashed_nocheck(hash, &value)
            .or_insert_with(|| {
                let interned = InLayout(self.vec.len(), Default::default());
                self.vec.push(value);
                (value, interned)
            });
        *interned
    }

    fn get(&self, key: InLayout<'a>) -> Layout<'a> {
        let InLayout(index, _) = key;
        self.vec[index]
    }
}
