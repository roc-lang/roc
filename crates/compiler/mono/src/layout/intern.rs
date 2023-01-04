use std::{
    cell::RefCell,
    hash::{BuildHasher, Hasher},
    marker::PhantomData,
    sync::Arc,
};

use parking_lot::{Mutex, RwLock};
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::{default_hasher, BumpMap};
use roc_module::symbol::Symbol;
use roc_target::TargetInfo;

use super::{Builtin, FieldOrderHash, LambdaSet, Layout, UnionLayout};

macro_rules! cache_interned_layouts {
    ($($i:literal, $name:ident, $layout:expr)*; $total_constants:literal) => {
        impl<'a> Layout<'a> {
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
    0,  VOID, Layout::VOID_NAKED
    1,  UNIT, Layout::UNIT_NAKED
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
    16, STR,  Layout::Builtin(Builtin::Str)
    17, RECURSIVE_PTR,  Layout::RecursivePointer

    ; 18
}

macro_rules! impl_to_from_int_width {
    ($($int_width:path => $layout:path,)*) => {
        impl<'a> Layout<'a> {
            pub const fn int_width(w: IntWidth) -> InLayout<'static> {
                match w {
                    $($int_width => $layout,)*
                }
            }
        }

        impl<'a> InLayout<'a> {
            /// # Panics
            ///
            /// Panics if the layout is not an integer
            pub fn to_int_width(&self) -> IntWidth {
                match self {
                    $(&$layout => $int_width,)*
                    _ => roc_error_macros::internal_error!("not an integer layout!")
                }
            }
        }
    };
}

impl_to_from_int_width! {
    IntWidth::U8 => Layout::U8,
    IntWidth::U16 => Layout::U16,
    IntWidth::U32 => Layout::U32,
    IntWidth::U64 => Layout::U64,
    IntWidth::U128 => Layout::U128,
    IntWidth::I8 => Layout::I8,
    IntWidth::I16 => Layout::I16,
    IntWidth::I32 => Layout::I32,
    IntWidth::I64 => Layout::I64,
    IntWidth::I128 => Layout::I128,
}

impl<'a> Layout<'a> {
    pub(super) const VOID_NAKED: Self = Layout::Union(UnionLayout::NonRecursive(&[]));
    pub(super) const UNIT_NAKED: Self = Layout::Struct {
        field_layouts: &[],
        field_order_hash: FieldOrderHash::ZERO_FIELD_HASH,
    };

    pub const fn float_width(w: FloatWidth) -> InLayout<'static> {
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

    /// Creates a [LambdaSet], including caching the [Layout::LambdaSet] representation of the
    /// lambda set onto itself.
    fn insert_lambda_set(
        &mut self,
        set: &'a &'a [(Symbol, &'a [InLayout<'a>])],
        representation: InLayout<'a>,
    ) -> LambdaSet<'a>;

    /// Retrieves a value from the interner.
    fn get(&self, key: InLayout<'a>) -> Layout<'a>;

    //
    // Convenience methods

    fn target_info(&self) -> TargetInfo;

    fn alignment_bytes(&self, layout: InLayout<'a>) -> u32 {
        self.get(layout).alignment_bytes(self, self.target_info())
    }

    fn allocation_alignment_bytes(&self, layout: InLayout<'a>) -> u32 {
        self.get(layout)
            .allocation_alignment_bytes(self, self.target_info())
    }

    fn stack_size(&self, layout: InLayout<'a>) -> u32 {
        self.get(layout).stack_size(self, self.target_info())
    }

    fn stack_size_and_alignment(&self, layout: InLayout<'a>) -> (u32, u32) {
        self.get(layout)
            .stack_size_and_alignment(self, self.target_info())
    }

    fn stack_size_without_alignment(&self, layout: InLayout<'a>) -> u32 {
        self.get(layout)
            .stack_size_without_alignment(self, self.target_info())
    }

    fn contains_refcounted(&self, layout: InLayout<'a>) -> bool {
        self.get(layout).contains_refcounted(self)
    }

    fn is_refcounted(&self, layout: InLayout<'a>) -> bool {
        self.get(layout).is_refcounted()
    }

    fn is_passed_by_reference(&self, layout: InLayout<'a>) -> bool {
        self.get(layout)
            .is_passed_by_reference(self, self.target_info())
    }

    fn runtime_representation(&self, layout: InLayout<'a>) -> Layout<'a> {
        self.get(layout).runtime_representation(self)
    }

    fn runtime_representation_in(&self, layout: InLayout<'a>) -> InLayout<'a> {
        Layout::runtime_representation_in(layout, self)
    }

    fn safe_to_memcpy(&self, layout: InLayout<'a>) -> bool {
        self.get(layout).safe_to_memcpy(self)
    }

    fn to_doc<'b, D, A>(
        &self,
        layout: InLayout<'a>,
        alloc: &'b D,
        parens: crate::ir::Parens,
    ) -> ven_pretty::DocBuilder<'b, D, A>
    where
        D: ven_pretty::DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        self.get(layout).to_doc(alloc, self, parens)
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
    normalized_lambda_set_map: Mutex<BumpMap<LambdaSet<'a>, LambdaSet<'a>>>,
    vec: RwLock<Vec<Layout<'a>>>,
    target_info: TargetInfo,
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
    normalized_lambda_set_map: BumpMap<LambdaSet<'a>, LambdaSet<'a>>,
    /// Cache of interned values from the parent for local access.
    vec: RefCell<Vec<Option<Layout<'a>>>>,
    target_info: TargetInfo,
}

/// A single-threaded interner, with no concurrency properties.
///
/// The only way to construct such an interner is to collapse a shared [GlobalLayoutInterner] into
/// a [STLayoutInterner], via [GlobalLayoutInterner::unwrap].
#[derive(Debug)]
pub struct STLayoutInterner<'a> {
    map: BumpMap<Layout<'a>, InLayout<'a>>,
    normalized_lambda_set_map: BumpMap<LambdaSet<'a>, LambdaSet<'a>>,
    vec: Vec<Layout<'a>>,
    target_info: TargetInfo,
}

/// Generic hasher for a value, to be used by all interners.
///
/// This uses the [default_hasher], so interner maps should also rely on [default_hasher].
fn hash<V: std::hash::Hash>(val: V) -> u64 {
    let mut state = roc_collections::all::BuildHasher::default().build_hasher();
    val.hash(&mut state);
    state.finish()
}

#[inline(always)]
fn make_normalized_lamdba_set<'a>(
    set: &'a &'a [(Symbol, &'a [InLayout<'a>])],
    representation: InLayout<'a>,
) -> LambdaSet<'a> {
    LambdaSet {
        set,
        representation,
        full_layout: Layout::VOID,
    }
}

impl<'a> GlobalLayoutInterner<'a> {
    /// Creates a new global interner with the given capacity.
    pub fn with_capacity(cap: usize, target_info: TargetInfo) -> Self {
        STLayoutInterner::with_capacity(cap, target_info).into_global()
    }

    /// Creates a derivative [TLLayoutInterner] pointing back to this global interner.
    pub fn fork(&self) -> TLLayoutInterner<'a> {
        TLLayoutInterner {
            parent: Self(Arc::clone(&self.0)),
            map: Default::default(),
            normalized_lambda_set_map: Default::default(),
            vec: Default::default(),
            target_info: self.0.target_info,
        }
    }

    /// Collapses a shared [GlobalLayoutInterner] into a [STLayoutInterner].
    ///
    /// Returns an [Err] with `self` if there are outstanding references to the [GlobalLayoutInterner].
    pub fn unwrap(self) -> Result<STLayoutInterner<'a>, Self> {
        let GlobalLayoutInternerInner {
            map,
            normalized_lambda_set_map,
            vec,
            target_info,
        } = match Arc::try_unwrap(self.0) {
            Ok(inner) => inner,
            Err(li) => return Err(Self(li)),
        };
        let map = Mutex::into_inner(map);
        let normalized_lambda_set_map = Mutex::into_inner(normalized_lambda_set_map);
        let vec = RwLock::into_inner(vec);
        Ok(STLayoutInterner {
            map,
            normalized_lambda_set_map,
            vec,
            target_info,
        })
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

    fn get_or_insert_hashed_normalized_lambda_set(
        &self,
        normalized: LambdaSet<'a>,
        normalized_hash: u64,
    ) -> WrittenGlobalLambdaSet<'a> {
        let mut normalized_lambda_set_map = self.0.normalized_lambda_set_map.lock();
        let (_, full_lambda_set) = normalized_lambda_set_map
            .raw_entry_mut()
            .from_key_hashed_nocheck(normalized_hash, &normalized)
            .or_insert_with(|| {
                // We don't already have an entry for the lambda set, which means it must be new to
                // the world. Reserve a slot, insert the lambda set, and that should fill the slot
                // in.
                let mut vec = self.0.vec.write();
                let slot = unsafe { InLayout::from_reserved_index(vec.len()) };
                let lambda_set = LambdaSet {
                    full_layout: slot,
                    ..normalized
                };
                vec.push(Layout::LambdaSet(lambda_set));
                (normalized, lambda_set)
            });
        let full_layout = self.0.vec.read()[full_lambda_set.full_layout.0];
        WrittenGlobalLambdaSet {
            full_lambda_set: *full_lambda_set,
            full_layout,
        }
    }

    fn get(&self, interned: InLayout<'a>) -> Layout<'a> {
        let InLayout(index, _) = interned;
        self.0.vec.read()[index]
    }

    pub fn is_empty(&self) -> bool {
        self.0.vec.read().is_empty()
    }
}

struct WrittenGlobalLambdaSet<'a> {
    full_lambda_set: LambdaSet<'a>,
    full_layout: Layout<'a>,
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

    fn insert_lambda_set(
        &mut self,
        set: &'a &'a [(Symbol, &'a [InLayout<'a>])],
        representation: InLayout<'a>,
    ) -> LambdaSet<'a> {
        // The tricky bit of inserting a lambda set is we need to fill in the `full_layout` only
        // after the lambda set is inserted, but we don't want to allocate a new interned slot if
        // the same lambda set layout has already been inserted with a different `full_layout`
        // slot.
        //
        // So,
        //   - check if the "normalized" lambda set (with a void full_layout slot) maps to an
        //     inserted lambda set in
        //     - in our thread-local cache, or globally
        //   - if so, use that one immediately
        //   - otherwise, allocate a new (global) slot, intern the lambda set, and then fill the slot in
        let global = &self.parent;
        let normalized = make_normalized_lamdba_set(set, representation);
        let normalized_hash = hash(normalized);
        let mut new_interned_layout = None;
        let (_, &mut full_lambda_set) = self
            .normalized_lambda_set_map
            .raw_entry_mut()
            .from_key_hashed_nocheck(normalized_hash, &normalized)
            .or_insert_with(|| {
                let WrittenGlobalLambdaSet {
                    full_lambda_set,
                    full_layout,
                } = global.get_or_insert_hashed_normalized_lambda_set(normalized, normalized_hash);

                // The Layout(lambda_set) isn't present in our thread; make sure it is for future
                // reference.
                new_interned_layout = Some((full_layout, full_lambda_set.full_layout));

                (normalized, full_lambda_set)
            });

        if let Some((new_layout, new_interned)) = new_interned_layout {
            // Write the interned lambda set layout into our thread-local cache.
            self.record(new_layout, new_interned);
        }

        full_lambda_set
    }

    fn get(&self, key: InLayout<'a>) -> Layout<'a> {
        if let Some(Some(value)) = self.vec.borrow().get(key.0) {
            return *value;
        }
        let value = self.parent.get(key);
        self.record(value, key);
        value
    }

    fn target_info(&self) -> TargetInfo {
        self.target_info
    }
}

impl<'a> STLayoutInterner<'a> {
    /// Creates a new single threaded interner with the given capacity.
    pub fn with_capacity(cap: usize, target_info: TargetInfo) -> Self {
        let mut interner = Self {
            map: BumpMap::with_capacity_and_hasher(cap, default_hasher()),
            normalized_lambda_set_map: BumpMap::with_capacity_and_hasher(cap, default_hasher()),
            vec: Vec::with_capacity(cap),
            target_info,
        };
        fill_reserved_layouts(&mut interner);
        interner
    }

    /// Promotes the [STLayoutInterner] back to a [GlobalLayoutInterner].
    ///
    /// You should *only* use this if you need to go from a single-threaded to a concurrent context,
    /// or in a case where you explicitly need access to [TLLayoutInterner]s.
    pub fn into_global(self) -> GlobalLayoutInterner<'a> {
        let STLayoutInterner {
            map,
            normalized_lambda_set_map,
            vec,
            target_info,
        } = self;
        GlobalLayoutInterner(Arc::new(GlobalLayoutInternerInner {
            map: Mutex::new(map),
            normalized_lambda_set_map: Mutex::new(normalized_lambda_set_map),
            vec: RwLock::new(vec),
            target_info,
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

    fn insert_lambda_set(
        &mut self,
        set: &'a &'a [(Symbol, &'a [InLayout<'a>])],
        representation: InLayout<'a>,
    ) -> LambdaSet<'a> {
        // IDEA:
        //   - check if the "normalized" lambda set (with a void full_layout slot) maps to an
        //     inserted lambda set
        //   - if so, use that one immediately
        //   - otherwise, allocate a new slot, intern the lambda set, and then fill the slot in
        let normalized_lambda_set = make_normalized_lamdba_set(set, representation);
        if let Some(lambda_set) = self.normalized_lambda_set_map.get(&normalized_lambda_set) {
            return *lambda_set;
        }

        // This lambda set must be new to the interner, reserve a slot and fill it in.
        let slot = unsafe { InLayout::from_reserved_index(self.vec.len()) };
        let lambda_set = LambdaSet {
            set,
            representation,
            full_layout: slot,
        };
        let filled_slot = self.insert(Layout::LambdaSet(lambda_set));
        assert_eq!(slot, filled_slot);

        self.normalized_lambda_set_map
            .insert(normalized_lambda_set, lambda_set);

        lambda_set
    }

    fn get(&self, key: InLayout<'a>) -> Layout<'a> {
        let InLayout(index, _) = key;
        self.vec[index]
    }

    fn target_info(&self) -> TargetInfo {
        self.target_info
    }
}
