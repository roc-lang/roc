use std::{cell::RefCell, hash::BuildHasher, marker::PhantomData, sync::Arc};

use bumpalo::Bump;
use parking_lot::{Mutex, RwLock};
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::{default_hasher, BumpMap};
use roc_module::symbol::Symbol;
use roc_target::Target;

use crate::layout::LayoutRepr;

use super::{LambdaSet, Layout, LayoutWrapper, SeenRecPtrs, SemanticRepr, UnionLayout};

macro_rules! cache_interned_layouts {
    ($($i:literal, $name:ident, $vis:vis, $layout:expr)*; $total_constants:literal) => {
        impl<'a> Layout<'a> {
            $(
            #[allow(unused)] // for now
            $vis const $name: InLayout<'static> = unsafe { InLayout::from_index($i) };
            )*
        }

        fn fill_reserved_layouts(interner: &mut STLayoutInterner<'_>) {
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

macro_rules! nosema {
    ($r:expr) => {
        Layout {
            repr: $r.direct(),
            semantic: SemanticRepr::NONE,
        }
    };
}

cache_interned_layouts! {
    0,  VOID, pub, Layout::VOID_NAKED
    1,  UNIT, pub, Layout::UNIT_NAKED
    2,  BOOL, pub, nosema!(LayoutRepr::BOOL)
    3,  U8,   pub, nosema!(LayoutRepr::U8)
    4,  U16,  pub, nosema!(LayoutRepr::U16)
    5,  U32,  pub, nosema!(LayoutRepr::U32)
    6,  U64,  pub, nosema!(LayoutRepr::U64)
    7,  U128, pub, nosema!(LayoutRepr::U128)
    8,  I8,   pub, nosema!(LayoutRepr::I8)
    9,  I16,  pub, nosema!(LayoutRepr::I16)
    10, I32,  pub, nosema!(LayoutRepr::I32)
    11, I64,  pub, nosema!(LayoutRepr::I64)
    12, I128, pub, nosema!(LayoutRepr::I128)
    13, F32,  pub, nosema!(LayoutRepr::F32)
    14, F64,  pub, nosema!(LayoutRepr::F64)
    15, DEC,  pub, nosema!(LayoutRepr::DEC)
    16, STR,  pub, nosema!(LayoutRepr::STR)
    17, OPAQUE_PTR,  pub, nosema!(LayoutRepr::OPAQUE_PTR)
    18, ERASED, pub, nosema!(LayoutRepr::ERASED)
    19, NAKED_RECURSIVE_PTR,  pub(super), nosema!(LayoutRepr::RecursivePointer(Layout::VOID))
    20, STR_PTR, pub, nosema!(LayoutRepr::Ptr(Layout::STR))
    21, LIST_U8, pub, nosema!(LayoutRepr::Builtin(crate::layout::Builtin::List(Layout::U8)))

    ; 22
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

            pub fn try_to_int_width(&self) -> Option<IntWidth> {
                match self {
                    $(&$layout => Some($int_width),)*
                    _ => None,
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
    pub(super) const VOID_NAKED: Self = Layout {
        repr: LayoutRepr::Union(UnionLayout::NonRecursive(&[])).direct(),
        semantic: SemanticRepr::NONE,
    };
    pub(super) const UNIT_NAKED: Self = Layout {
        repr: LayoutRepr::Struct(&[]).direct(),
        semantic: SemanticRepr::EMPTY_RECORD,
    };

    pub const fn float_width(w: FloatWidth) -> InLayout<'static> {
        match w {
            FloatWidth::F32 => Self::F32,
            FloatWidth::F64 => Self::F64,
        }
    }
}

/// Whether a recursive lambda set being inserted into an interner needs fixing-up of naked
/// recursion pointers in the capture set.
/// Applicable only if
///   - the lambda set is indeed recursive, and
///   - its capture set contain naked pointer references
pub struct NeedsRecursionPointerFixup(pub bool);

pub trait LayoutInterner<'a>: Sized {
    /// Interns a value, returning its interned representation.
    /// If the value has been interned before, the old interned representation will be re-used.
    ///
    /// Note that the provided value must be allocated into an arena of your choosing, but which
    /// must live at least as long as the interner lives.
    // TODO: we should consider maintaining our own arena in the interner, to avoid redundant
    // allocations when values already have interned representations.
    fn insert(&mut self, value: Layout<'a>) -> InLayout<'a>;

    /// Interns a value with no semantic representation, returning its interned representation.
    /// If the value has been interned before, the old interned representation will be re-used.
    fn insert_direct_no_semantic(&mut self, repr: LayoutRepr<'a>) -> InLayout<'a> {
        self.insert(Layout::no_semantic(repr.direct()))
    }

    /// Creates a [LambdaSet], including caching the [LayoutRepr::LambdaSet] representation of the
    /// lambda set onto itself.
    fn insert_lambda_set(
        &mut self,
        arena: &'a Bump,
        args: &'a &'a [InLayout<'a>],
        ret: InLayout<'a>,
        set: &'a &'a [(Symbol, &'a [InLayout<'a>])],
        needs_recursive_fixup: NeedsRecursionPointerFixup,
        representation: InLayout<'a>,
    ) -> LambdaSet<'a>;

    /// Inserts a recursive layout into the interner.
    /// Takes a normalized recursive layout with the recursion pointer set to [Layout::VOID].
    /// Will update the RecursivePointer as appropriate during insertion.
    fn insert_recursive(&mut self, arena: &'a Bump, normalized_layout: Layout<'a>) -> InLayout<'a>;

    /// Retrieves a value from the interner.
    fn get(&self, key: InLayout<'a>) -> Layout<'a>;

    //
    // Convenience methods

    fn get_repr(&self, mut key: InLayout<'a>) -> LayoutRepr<'a> {
        loop {
            match self.get(key).repr {
                LayoutWrapper::Direct(repr) => return repr,
                LayoutWrapper::Newtype(inner) => key = inner,
            }
        }
    }

    fn get_semantic(&self, key: InLayout<'a>) -> SemanticRepr<'a> {
        self.get(key).semantic
    }

    fn eq_repr(&self, a: InLayout<'a>, b: InLayout<'a>) -> bool {
        self.get_repr(a) == self.get_repr(b)
    }

    fn target(&self) -> Target;

    fn alignment_bytes(&self, layout: InLayout<'a>) -> u32 {
        self.get_repr(layout).alignment_bytes(self)
    }

    fn allocation_alignment_bytes(&self, layout: InLayout<'a>) -> u32 {
        self.get_repr(layout).allocation_alignment_bytes(self)
    }

    fn stack_size(&self, layout: InLayout<'a>) -> u32 {
        self.get_repr(layout).stack_size(self)
    }

    fn stack_size_and_alignment(&self, layout: InLayout<'a>) -> (u32, u32) {
        self.get_repr(layout).stack_size_and_alignment(self)
    }

    fn stack_size_without_alignment(&self, layout: InLayout<'a>) -> u32 {
        self.get_repr(layout).stack_size_without_alignment(self)
    }

    fn contains_refcounted(&self, layout: InLayout<'a>) -> bool {
        self.get_repr(layout).contains_refcounted(self)
    }

    fn is_refcounted(&self, layout: InLayout<'a>) -> bool {
        self.get_repr(layout).is_refcounted(self)
    }

    fn is_nullable(&self, layout: InLayout<'a>) -> bool {
        self.get_repr(layout).is_nullable()
    }

    fn is_passed_by_reference(&self, layout: InLayout<'a>) -> bool {
        self.get_repr(layout).is_passed_by_reference(self)
    }

    fn runtime_representation(&self, layout: InLayout<'a>) -> LayoutRepr<'a> {
        self.get_repr(self.runtime_representation_in(layout))
    }

    fn runtime_representation_in(&self, layout: InLayout<'a>) -> InLayout<'a> {
        Layout::runtime_representation_in(layout, self)
    }

    fn has_varying_stack_size(&self, layout: InLayout<'a>, arena: &'a Bump) -> bool {
        self.get_repr(layout).has_varying_stack_size(self, arena)
    }

    fn chase_recursive(&self, mut layout: InLayout<'a>) -> LayoutRepr<'a> {
        loop {
            let lay = self.get_repr(layout);
            match lay {
                LayoutRepr::RecursivePointer(l) => layout = l,
                _ => return lay,
            }
        }
    }

    fn chase_recursive_in(&self, mut layout: InLayout<'a>) -> InLayout<'a> {
        loop {
            match self.get_repr(layout) {
                LayoutRepr::RecursivePointer(l) => layout = l,
                _ => return layout,
            }
        }
    }

    fn safe_to_memcpy(&self, layout: InLayout<'a>) -> bool {
        self.get_repr(layout).safe_to_memcpy(self)
    }

    /// Checks if two layouts are equivalent up to isomorphism.
    ///
    /// This is only to be used when layouts need to be compared across statements and depths,
    /// for example
    ///   - when looking up a layout index in a lambda set
    ///   - in the [checker][crate::debug::check_procs], where `x = UnionAtIndex(f, 0)` may have
    ///     that the recorded layout of `x` is at a different depth than that determined when we
    ///     index the recorded layout of `f` at 0. Hence the two layouts may have different
    ///     interned representations, even if they are in fact isomorphic.
    fn equiv(&self, l1: InLayout<'a>, l2: InLayout<'a>) -> bool {
        std::thread_local! {
            static SCRATCHPAD: RefCell<Option<Vec<(InLayout<'static>, InLayout<'static>)>>> = RefCell::new(Some(Vec::with_capacity(64)));
        }

        SCRATCHPAD.with(|f| {
            // SAFETY: the promotion to lifetime 'a only lasts during equivalence-checking; the
            // scratchpad stack is cleared after every use.
            let mut stack: Vec<(InLayout<'a>, InLayout<'a>)> =
                unsafe { std::mem::transmute(f.take().unwrap()) };

            let answer = equiv::equivalent(&mut stack, self, l1, l2);
            stack.clear();

            let stack: Vec<(InLayout<'static>, InLayout<'static>)> =
                unsafe { std::mem::transmute(stack) };
            f.replace(Some(stack));
            answer
        })
    }

    fn to_doc<'b, D, A>(
        &self,
        layout: InLayout<'a>,
        alloc: &'b D,
        seen_rec: &mut SeenRecPtrs<'a>,
        parens: crate::ir::Parens,
    ) -> ven_pretty::DocBuilder<'b, D, A>
    where
        D: ven_pretty::DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use LayoutRepr::*;

        match self.get_repr(layout) {
            Builtin(builtin) => builtin.to_doc(alloc, self, seen_rec, parens),
            Struct(field_layouts) => {
                let fields_doc = field_layouts
                    .iter()
                    .map(|x| self.to_doc(*x, alloc, seen_rec, parens));

                alloc
                    .text("{")
                    .append(alloc.intersperse(fields_doc, ", "))
                    .append(alloc.text("}"))
            }
            Union(union_layout) => {
                let is_recursive = !matches!(union_layout, UnionLayout::NonRecursive(..));
                if is_recursive {
                    seen_rec.insert(layout);
                }
                let doc = union_layout.to_doc(alloc, self, seen_rec, parens);
                if is_recursive {
                    seen_rec.remove(&layout);
                }
                doc
            }
            LambdaSet(lambda_set) => {
                self.to_doc(lambda_set.runtime_representation(), alloc, seen_rec, parens)
            }
            RecursivePointer(rec_layout) => {
                if seen_rec.contains(&rec_layout) {
                    alloc.text("*self")
                } else {
                    self.to_doc(rec_layout, alloc, seen_rec, parens)
                }
            }
            Ptr(inner) => alloc
                .text("Ptr(")
                .append(self.to_doc(inner, alloc, seen_rec, parens))
                .append(")"),
            FunctionPointer(fp) => fp.to_doc(alloc, self, seen_rec, parens),
            Erased(e) => e.to_doc(alloc),
        }
    }

    fn to_doc_top<'b, D, A>(
        &self,
        layout: InLayout<'a>,
        alloc: &'b D,
    ) -> ven_pretty::DocBuilder<'b, D, A>
    where
        D: ven_pretty::DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        self.to_doc(
            layout,
            alloc,
            &mut Default::default(),
            crate::ir::Parens::NotNeeded,
        )
    }

    /// Pretty-print a representation of the layout.
    fn dbg(&self, layout: InLayout<'a>) -> String {
        let alloc: ven_pretty::Arena<()> = ven_pretty::Arena::new();
        let doc = self.to_doc_top(layout, &alloc);
        doc.1.pretty(80).to_string()
    }

    /// Yields a debug representation of a layout, traversing its entire nested structure and
    /// debug-printing all intermediate interned layouts.
    ///
    /// By default, a [Layout] is composed inductively by [interned layout][InLayout]s.
    /// This makes debugging a layout more than one level challenging, as you may run into further
    /// opaque interned layouts that need unwrapping.
    ///
    /// [`dbg_deep`][LayoutInterner::dbg_deep] works around this by returning a value whose debug
    /// representation chases through all nested interned layouts as you would otherwise have to do
    /// manually.
    ///
    /// ## Example
    ///
    /// ```ignore(illustrative)
    /// fn is_rec_ptr<'a>(interner: &impl LayoutInterner<'a>, layout: InLayout<'a>) -> bool {
    ///     if matches!(interner.get(layout), LayoutRepr::RecursivePointer(..)) {
    ///         return true;
    ///     }
    ///
    ///     let deep_dbg = interner.dbg_deep(layout);
    ///     roc_tracing::info!("not a recursive pointer, actually a {deep_dbg:?}");
    ///     return false;
    /// }
    /// ```
    fn dbg_deep<'r>(&'r self, layout: InLayout<'a>) -> dbg_deep::Dbg<'a, 'r, Self> {
        dbg_deep::Dbg(self, layout)
    }

    fn dbg_deep_iter<'r>(
        &'r self,
        layouts: &'a [InLayout<'a>],
    ) -> dbg_deep::DbgFields<'a, 'r, Self> {
        dbg_deep::DbgFields(self, layouts)
    }

    /// Similar to `Self::dbg_deep`, but does not display the interned name of symbols. This keeps
    /// the output consistent in a multi-threaded (test) run
    fn dbg_stable<'r>(&'r self, layout: InLayout<'a>) -> dbg_stable::Dbg<'a, 'r, Self> {
        dbg_stable::Dbg(self, layout)
    }

    fn dbg_stable_iter<'r>(
        &'r self,
        layouts: &'a [InLayout<'a>],
    ) -> dbg_stable::DbgFields<'a, 'r, Self> {
        dbg_stable::DbgFields(self, layouts)
    }
}

/// An interned layout.
///
/// When possible, prefer comparing/hashing on the [InLayout] representation of a value, rather
/// than the value itself.
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InLayout<'a>(usize, std::marker::PhantomData<&'a ()>);
impl<'a> Clone for InLayout<'a> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a> Copy for InLayout<'a> {}

impl std::fmt::Debug for InLayout<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Layout::VOID => f.write_str("InLayout(VOID)"),
            Layout::UNIT => f.write_str("InLayout(UNIT)"),
            Layout::BOOL => f.write_str("InLayout(BOOL)"),
            Layout::U8 => f.write_str("InLayout(U8)"),
            Layout::U16 => f.write_str("InLayout(U16)"),
            Layout::U32 => f.write_str("InLayout(U32)"),
            Layout::U64 => f.write_str("InLayout(U64)"),
            Layout::U128 => f.write_str("InLayout(U128)"),
            Layout::I8 => f.write_str("InLayout(I8)"),
            Layout::I16 => f.write_str("InLayout(I16)"),
            Layout::I32 => f.write_str("InLayout(I32)"),
            Layout::I64 => f.write_str("InLayout(I64)"),
            Layout::I128 => f.write_str("InLayout(I128)"),
            Layout::F32 => f.write_str("InLayout(F32)"),
            Layout::F64 => f.write_str("InLayout(F64)"),
            Layout::DEC => f.write_str("InLayout(DEC)"),
            Layout::STR => f.write_str("InLayout(STR)"),
            Layout::OPAQUE_PTR => f.write_str("InLayout(OPAQUE_PTR)"),
            Layout::NAKED_RECURSIVE_PTR => f.write_str("InLayout(NAKED_RECURSIVE_PTR)"),
            Layout::STR_PTR => f.write_str("InLayout(STR_PTR)"),
            Layout::LIST_U8 => f.write_str("InLayout(LIST_U8)"),
            _ => f.debug_tuple("InLayout").field(&self.0).finish(),
        }
    }
}

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
    pub(crate) const unsafe fn from_index(index: usize) -> Self {
        Self(index, PhantomData)
    }

    pub(crate) const fn newtype(self) -> LayoutWrapper<'a> {
        LayoutWrapper::Newtype(self)
    }

    pub fn index(&self) -> usize {
        self.0
    }

    pub fn try_int_width(self) -> Option<IntWidth> {
        match self {
            Layout::U8 => Some(IntWidth::U8),
            Layout::U16 => Some(IntWidth::U16),
            Layout::U32 => Some(IntWidth::U32),
            Layout::U64 => Some(IntWidth::U64),
            Layout::U128 => Some(IntWidth::U128),
            Layout::I8 => Some(IntWidth::I8),
            Layout::I16 => Some(IntWidth::I16),
            Layout::I32 => Some(IntWidth::I32),
            Layout::I64 => Some(IntWidth::I64),
            Layout::I128 => Some(IntWidth::I128),
            _ => None,
        }
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
    target: Target,
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
    target: Target,
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
    target: Target,
}

/// Interner constructed with an exclusive lock over [GlobalLayoutInterner]
struct LockedGlobalInterner<'a, 'r> {
    map: &'r mut BumpMap<Layout<'a>, InLayout<'a>>,
    normalized_lambda_set_map: &'r mut BumpMap<LambdaSet<'a>, LambdaSet<'a>>,
    vec: &'r mut Vec<Layout<'a>>,
    target: Target,
}

/// Generic hasher for a value, to be used by all interners.
///
/// This uses the [default_hasher], so interner maps should also rely on [default_hasher].
fn hash<V: std::hash::Hash>(val: V) -> u64 {
    let hasher = roc_collections::all::BuildHasher::default();
    hasher.hash_one(&val)
}

#[inline(always)]
fn make_normalized_lamdba_set<'a>(
    args: &'a &'a [InLayout<'a>],
    ret: InLayout<'a>,
    set: &'a &'a [(Symbol, &'a [InLayout<'a>])],
    representation: InLayout<'a>,
) -> LambdaSet<'a> {
    LambdaSet {
        args,
        ret,
        set,
        representation,
        full_layout: Layout::VOID,
    }
}

impl<'a> GlobalLayoutInterner<'a> {
    /// Creates a new global interner with the given capacity.
    pub fn with_capacity(cap: usize, target: Target) -> Self {
        STLayoutInterner::with_capacity(cap, target).into_global()
    }

    /// Creates a derivative [TLLayoutInterner] pointing back to this global interner.
    pub fn fork(&self) -> TLLayoutInterner<'a> {
        TLLayoutInterner {
            parent: Self(Arc::clone(&self.0)),
            map: Default::default(),
            normalized_lambda_set_map: Default::default(),
            vec: Default::default(),
            target: self.0.target,
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
            target,
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
            target,
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
        arena: &'a Bump,
        normalized: LambdaSet<'a>,
        needs_recursive_fixup: NeedsRecursionPointerFixup,
        normalized_hash: u64,
    ) -> WrittenGlobalLambdaSet<'a> {
        let mut normalized_lambda_set_map = self.0.normalized_lambda_set_map.lock();
        if let Some((_, &full_lambda_set)) = normalized_lambda_set_map
            .raw_entry()
            .from_key_hashed_nocheck(normalized_hash, &normalized)
        {
            let full_layout = self.0.vec.read()[full_lambda_set.full_layout.0];
            return WrittenGlobalLambdaSet {
                full_lambda_set,
                full_layout,
            };
        }

        // We don't already have an entry for the lambda set, which means it must be new to
        // the world. Reserve a slot, insert the lambda set, and that should fill the slot
        // in.
        let mut map = self.0.map.lock();
        let mut vec = self.0.vec.write();

        let slot = unsafe { InLayout::from_index(vec.len()) };
        vec.push(Layout::VOID_NAKED);

        let set = if needs_recursive_fixup.0 {
            let mut interner = LockedGlobalInterner {
                map: &mut map,
                normalized_lambda_set_map: &mut normalized_lambda_set_map,
                vec: &mut vec,
                target: self.0.target,
            };
            reify::reify_lambda_set_captures(arena, &mut interner, slot, normalized.set)
        } else {
            normalized.set
        };

        let full_lambda_set = LambdaSet {
            full_layout: slot,
            set,
            ..normalized
        };
        let lambda_set_layout = Layout {
            repr: LayoutRepr::LambdaSet(full_lambda_set).direct(),
            semantic: SemanticRepr::NONE,
        };

        vec[slot.0] = lambda_set_layout;

        // TODO: Is it helpful to persist the hash and give it back to the thread-local
        // interner?
        let _old = map.insert(lambda_set_layout, slot);
        debug_assert!(_old.is_none());

        let _old_normalized = normalized_lambda_set_map.insert(normalized, full_lambda_set);
        debug_assert!(_old_normalized.is_none());

        let full_layout = vec[full_lambda_set.full_layout.0];
        WrittenGlobalLambdaSet {
            full_lambda_set,
            full_layout,
        }
    }

    fn get_or_insert_hashed_normalized_recursive(
        &self,
        arena: &'a Bump,
        normalized: Layout<'a>,
        normalized_hash: u64,
    ) -> WrittenGlobalRecursive<'a> {
        let mut map = self.0.map.lock();
        if let Some((_, &interned)) = map
            .raw_entry()
            .from_key_hashed_nocheck(normalized_hash, &normalized)
        {
            let full_layout = self.0.vec.read()[interned.0];
            return WrittenGlobalRecursive {
                interned_layout: interned,
                full_layout,
            };
        }

        let mut vec = self.0.vec.write();
        let mut normalized_lambda_set_map = self.0.normalized_lambda_set_map.lock();

        let slot = unsafe { InLayout::from_index(vec.len()) };
        vec.push(Layout::VOID_NAKED);

        let mut interner = LockedGlobalInterner {
            map: &mut map,
            normalized_lambda_set_map: &mut normalized_lambda_set_map,
            vec: &mut vec,
            target: self.0.target,
        };
        let full_layout = reify::reify_recursive_layout(arena, &mut interner, slot, normalized);

        vec[slot.0] = full_layout;

        let _old = map.insert(normalized, slot);
        debug_assert!(_old.is_none());

        let _old_full_layout = map.insert(full_layout, slot);
        debug_assert!(_old_full_layout.is_none());

        WrittenGlobalRecursive {
            interned_layout: slot,
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

struct WrittenGlobalRecursive<'a> {
    interned_layout: InLayout<'a>,
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
        arena: &'a Bump,
        args: &'a &'a [InLayout<'a>],
        ret: InLayout<'a>,
        set: &'a &'a [(Symbol, &'a [InLayout<'a>])],
        needs_recursive_fixup: NeedsRecursionPointerFixup,
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
        let normalized = make_normalized_lamdba_set(args, ret, set, representation);
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
                } = global.get_or_insert_hashed_normalized_lambda_set(
                    arena,
                    normalized,
                    needs_recursive_fixup,
                    normalized_hash,
                );

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

    fn insert_recursive(&mut self, arena: &'a Bump, normalized_layout: Layout<'a>) -> InLayout<'a> {
        // - Check if the normalized layout already has an interned slot. If it does we're done, since no
        //   recursive layout would ever have have VOID as the recursion pointer.
        // - If not, allocate a slot and compute the recursive layout with the recursion pointer
        //   resolving to the new slot.
        // - Point the resolved and normalized layout to the new slot.
        let global = &self.parent;
        let normalized_hash = hash(normalized_layout);
        let mut new_interned_full_layout = None;
        let (&mut _, &mut interned) = self
            .map
            .raw_entry_mut()
            .from_key_hashed_nocheck(normalized_hash, &normalized_layout)
            .or_insert_with(|| {
                let WrittenGlobalRecursive {
                    interned_layout,
                    full_layout,
                } = global.get_or_insert_hashed_normalized_recursive(
                    arena,
                    normalized_layout,
                    normalized_hash,
                );

                // The new filled-in layout isn't present in our thread; make sure it is for future
                // reference.
                new_interned_full_layout = Some(full_layout);

                (normalized_layout, interned_layout)
            });
        if let Some(full_layout) = new_interned_full_layout {
            self.record(full_layout, interned);
        }
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

    fn target(&self) -> Target {
        self.target
    }
}

impl<'a> STLayoutInterner<'a> {
    /// Creates a new single threaded interner with the given capacity.
    pub fn with_capacity(cap: usize, target: Target) -> Self {
        let mut interner = Self {
            map: BumpMap::with_capacity_and_hasher(cap, default_hasher()),
            normalized_lambda_set_map: BumpMap::with_capacity_and_hasher(cap, default_hasher()),
            vec: Vec::with_capacity(cap),
            target,
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
            target,
        } = self;
        GlobalLayoutInterner(Arc::new(GlobalLayoutInternerInner {
            map: Mutex::new(map),
            normalized_lambda_set_map: Mutex::new(normalized_lambda_set_map),
            vec: RwLock::new(vec),
            target,
        }))
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }
}

macro_rules! st_impl {
    ($($lt:lifetime)? $interner:ident) => {
        impl<'a$(, $lt)?> LayoutInterner<'a> for $interner<'a$(, $lt)?> {
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
                arena: &'a Bump,
                args: &'a &'a [InLayout<'a>],
                ret: InLayout<'a>,
                set: &'a &'a [(Symbol, &'a [InLayout<'a>])],
                needs_recursive_fixup: NeedsRecursionPointerFixup,
                representation: InLayout<'a>,
            ) -> LambdaSet<'a> {
                // IDEA:
                //   - check if the "normalized" lambda set (with a void full_layout slot) maps to an
                //     inserted lambda set
                //   - if so, use that one immediately
                //   - otherwise, allocate a new slot, intern the lambda set, and then fill the slot in
                let normalized_lambda_set =
                    make_normalized_lamdba_set(args, ret, set, representation);
                if let Some(lambda_set) = self.normalized_lambda_set_map.get(&normalized_lambda_set)
                {
                    return *lambda_set;
                }

                // This lambda set must be new to the interner, reserve a slot and fill it in.
                let slot = unsafe { InLayout::from_index(self.vec.len()) };
                self.vec.push(Layout::VOID_NAKED);

                let set = if needs_recursive_fixup.0 {
                    reify::reify_lambda_set_captures(arena, self, slot, set)
                } else {
                    set
                };

                let lambda_set = LambdaSet {
                    args,
                    ret,
                    set,
                    representation,
                    full_layout: slot,
                };
                let lay = Layout {
                    repr: LayoutRepr::LambdaSet(lambda_set).direct(),
                    semantic: SemanticRepr::NONE
                };
                self.vec[slot.0] = lay;

                let _old = self.map.insert(lay, slot);
                debug_assert!(_old.is_none());

                let _old = self.normalized_lambda_set_map
                    .insert(normalized_lambda_set, lambda_set);
                debug_assert!(_old.is_none());

                lambda_set
            }

            fn insert_recursive(
                &mut self,
                arena: &'a Bump,
                normalized_layout: Layout<'a>,
            ) -> InLayout<'a> {
                // IDEA:
                //   - check if the normalized layout (with a void recursion pointer) maps to an
                //     inserted lambda set
                //   - if so, use that one immediately
                //   - otherwise, allocate a new slot, update the recursive layout, and intern
                if let Some(in_layout) = self.map.get(&normalized_layout) {
                    return *in_layout;
                }

                // This recursive layout must be new to the interner, reserve a slot and fill it in.
                let slot = unsafe { InLayout::from_index(self.vec.len()) };
                self.vec.push(Layout::VOID_NAKED);
                let full_layout =
                    reify::reify_recursive_layout(arena, self, slot, normalized_layout);
                self.vec[slot.0] = full_layout;

                self.map.insert(normalized_layout, slot);
                self.map.insert(full_layout, slot);

                slot
            }

            fn get(&self, key: InLayout<'a>) -> Layout<'a> {
                let InLayout(index, _) = key;
                self.vec[index]
            }

            fn target(&self) -> Target{
                self.target
            }
        }
    };
}

st_impl!(STLayoutInterner);
st_impl!('r LockedGlobalInterner);

mod reify {
    use bumpalo::{collections::Vec, Bump};
    use roc_module::symbol::Symbol;

    use crate::layout::{
        Builtin, FunctionPointer, LambdaSet, Layout, LayoutRepr, LayoutWrapper, UnionLayout,
    };

    use super::{InLayout, LayoutInterner, NeedsRecursionPointerFixup};

    // TODO: if recursion becomes a problem we could make this iterative
    pub fn reify_recursive_layout<'a>(
        arena: &'a Bump,
        interner: &mut impl LayoutInterner<'a>,
        slot: InLayout<'a>,
        normalized_layout: Layout<'a>,
    ) -> Layout<'a> {
        let Layout { repr, semantic } = normalized_layout;
        let reified_repr = match repr {
            LayoutWrapper::Direct(repr) => {
                reify_recursive_layout_repr(arena, interner, slot, repr).direct()
            }
            LayoutWrapper::Newtype(inner) => reify_layout(arena, interner, slot, inner).newtype(),
        };

        Layout::new(reified_repr, semantic)
    }

    fn reify_recursive_layout_repr<'a>(
        arena: &'a Bump,
        interner: &mut impl LayoutInterner<'a>,
        slot: InLayout<'a>,
        repr: LayoutRepr<'a>,
    ) -> LayoutRepr<'a> {
        match repr {
            LayoutRepr::Builtin(builtin) => {
                LayoutRepr::Builtin(reify_builtin(arena, interner, slot, builtin))
            }
            LayoutRepr::Struct(field_layouts) => {
                LayoutRepr::Struct(reify_layout_slice(arena, interner, slot, field_layouts))
            }
            LayoutRepr::Ptr(lay) => LayoutRepr::Ptr(reify_layout(arena, interner, slot, lay)),
            LayoutRepr::Union(un) => LayoutRepr::Union(reify_union(arena, interner, slot, un)),
            LayoutRepr::LambdaSet(ls) => {
                LayoutRepr::LambdaSet(reify_lambda_set(arena, interner, slot, ls))
            }
            LayoutRepr::RecursivePointer(l) => {
                // If the layout is not void at its point then it has already been solved as
                // another recursive union's layout, do not change it.
                LayoutRepr::RecursivePointer(if l == Layout::VOID { slot } else { l })
            }
            LayoutRepr::FunctionPointer(FunctionPointer { args, ret }) => {
                LayoutRepr::FunctionPointer(FunctionPointer {
                    args: reify_layout_slice(arena, interner, slot, args),
                    ret: reify_layout(arena, interner, slot, ret),
                })
            }
            LayoutRepr::Erased(e) => LayoutRepr::Erased(e),
        }
    }

    fn reify_layout<'a>(
        arena: &'a Bump,
        interner: &mut impl LayoutInterner<'a>,
        slot: InLayout<'a>,
        layout: InLayout<'a>,
    ) -> InLayout<'a> {
        let layout = reify_recursive_layout(arena, interner, slot, interner.get(layout));
        interner.insert(layout)
    }

    fn reify_layout_slice<'a>(
        arena: &'a Bump,
        interner: &mut impl LayoutInterner<'a>,
        slot: InLayout<'a>,
        layouts: &[InLayout<'a>],
    ) -> &'a [InLayout<'a>] {
        let mut slice = Vec::with_capacity_in(layouts.len(), arena);
        for &layout in layouts {
            slice.push(reify_layout(arena, interner, slot, layout));
        }
        slice.into_bump_slice()
    }

    fn reify_layout_slice_slice<'a>(
        arena: &'a Bump,
        interner: &mut impl LayoutInterner<'a>,
        slot: InLayout<'a>,
        layouts: &[&[InLayout<'a>]],
    ) -> &'a [&'a [InLayout<'a>]] {
        let mut slice = Vec::with_capacity_in(layouts.len(), arena);
        for &layouts in layouts {
            slice.push(reify_layout_slice(arena, interner, slot, layouts));
        }
        slice.into_bump_slice()
    }

    fn reify_builtin<'a>(
        arena: &'a Bump,
        interner: &mut impl LayoutInterner<'a>,
        slot: InLayout<'a>,
        builtin: Builtin<'a>,
    ) -> Builtin<'a> {
        match builtin {
            Builtin::Int(_)
            | Builtin::Float(_)
            | Builtin::Bool
            | Builtin::Decimal
            | Builtin::Str => builtin,
            Builtin::List(elem) => Builtin::List(reify_layout(arena, interner, slot, elem)),
        }
    }

    fn reify_union<'a>(
        arena: &'a Bump,
        interner: &mut impl LayoutInterner<'a>,
        slot: InLayout<'a>,
        union: UnionLayout<'a>,
    ) -> UnionLayout<'a> {
        match union {
            UnionLayout::NonRecursive(tags) => {
                UnionLayout::NonRecursive(reify_layout_slice_slice(arena, interner, slot, tags))
            }
            UnionLayout::Recursive(tags) => {
                UnionLayout::Recursive(reify_layout_slice_slice(arena, interner, slot, tags))
            }
            UnionLayout::NonNullableUnwrapped(fields) => {
                UnionLayout::NonNullableUnwrapped(reify_layout_slice(arena, interner, slot, fields))
            }
            UnionLayout::NullableWrapped {
                nullable_id,
                other_tags,
            } => UnionLayout::NullableWrapped {
                nullable_id,
                other_tags: reify_layout_slice_slice(arena, interner, slot, other_tags),
            },
            UnionLayout::NullableUnwrapped {
                nullable_id,
                other_fields,
            } => UnionLayout::NullableUnwrapped {
                nullable_id,
                other_fields: reify_layout_slice(arena, interner, slot, other_fields),
            },
        }
    }

    fn reify_lambda_set<'a>(
        arena: &'a Bump,
        interner: &mut impl LayoutInterner<'a>,
        slot: InLayout<'a>,
        lambda_set: LambdaSet<'a>,
    ) -> LambdaSet<'a> {
        let LambdaSet {
            args,
            ret,
            set,
            representation,
            full_layout: _,
        } = lambda_set;

        let args = reify_layout_slice(arena, interner, slot, args);
        let ret = reify_layout(arena, interner, slot, ret);
        let set = {
            let mut new_set = Vec::with_capacity_in(set.len(), arena);
            for (lambda, captures) in set.iter() {
                new_set.push((*lambda, reify_layout_slice(arena, interner, slot, captures)));
            }
            new_set.into_bump_slice()
        };
        let representation = reify_layout(arena, interner, slot, representation);

        interner.insert_lambda_set(
            arena,
            arena.alloc(args),
            ret,
            arena.alloc(set),
            // All nested recursive pointers should been fixed up, since we just did that above.
            NeedsRecursionPointerFixup(false),
            representation,
        )
    }

    pub fn reify_lambda_set_captures<'a>(
        arena: &'a Bump,
        interner: &mut impl LayoutInterner<'a>,
        slot: InLayout<'a>,
        set: &[(Symbol, &'a [InLayout<'a>])],
    ) -> &'a &'a [(Symbol, &'a [InLayout<'a>])] {
        let mut reified_set = Vec::with_capacity_in(set.len(), arena);
        for (f, captures) in set.iter() {
            let reified_captures = reify_layout_slice(arena, interner, slot, captures);
            reified_set.push((*f, reified_captures));
        }
        arena.alloc(reified_set.into_bump_slice())
    }
}

mod equiv {
    use crate::layout::{self, LayoutRepr, UnionLayout};

    use super::{InLayout, LayoutInterner};

    pub fn equivalent<'a>(
        stack: &mut Vec<(InLayout<'a>, InLayout<'a>)>,
        interner: &impl LayoutInterner<'a>,
        l1: InLayout<'a>,
        l2: InLayout<'a>,
    ) -> bool {
        stack.push((l1, l2));

        macro_rules! equiv_fields {
            ($fields1:expr, $fields2:expr) => {{
                if $fields1.len() != $fields2.len() {
                    return false;
                }
                stack.extend($fields1.iter().copied().zip($fields2.iter().copied()));
            }};
        }

        macro_rules! equiv_unions {
            ($tags1:expr, $tags2:expr) => {{
                if $tags1.len() != $tags2.len() {
                    return false;
                }
                for (payloads1, payloads2) in $tags1.iter().zip($tags2) {
                    equiv_fields!(payloads1, payloads2)
                }
            }};
        }

        while let Some((l1, l2)) = stack.pop() {
            if l1 == l2 {
                continue;
            }
            use LayoutRepr::*;
            match (interner.get_repr(l1), interner.get_repr(l2)) {
                (RecursivePointer(rec), _) => stack.push((rec, l2)),
                (_, RecursivePointer(rec)) => stack.push((l1, rec)),
                (Builtin(b1), Builtin(b2)) => {
                    use crate::layout::Builtin::*;
                    match (b1, b2) {
                        (List(e1), List(e2)) => stack.push((e1, e2)),
                        (b1, b2) => {
                            if b1 != b2 {
                                return false;
                            }
                        }
                    }
                }
                (Struct(fl1), Struct(fl2)) => {
                    equiv_fields!(fl1, fl2)
                }
                (Ptr(b1), Ptr(b2)) => stack.push((b1, b2)),
                (Union(u1), Union(u2)) => {
                    use UnionLayout::*;
                    match (u1, u2) {
                        (NonRecursive(tags1), NonRecursive(tags2)) => equiv_unions!(tags1, tags2),
                        (Recursive(tags1), Recursive(tags2)) => equiv_unions!(tags1, tags2),
                        (NonNullableUnwrapped(fields1), NonNullableUnwrapped(fields2)) => {
                            equiv_fields!(fields1, fields2)
                        }
                        (
                            NullableWrapped {
                                nullable_id: null_id1,
                                other_tags: tags1,
                            },
                            NullableWrapped {
                                nullable_id: null_id2,
                                other_tags: tags2,
                            },
                        ) => {
                            if null_id1 != null_id2 {
                                return false;
                            }
                            equiv_unions!(tags1, tags2)
                        }
                        (
                            NullableUnwrapped {
                                nullable_id: null_id1,
                                other_fields: fields1,
                            },
                            NullableUnwrapped {
                                nullable_id: null_id2,
                                other_fields: fields2,
                            },
                        ) => {
                            if null_id1 != null_id2 {
                                return false;
                            }
                            equiv_fields!(fields1, fields2)
                        }
                        _ => return false,
                    }
                }
                (
                    LambdaSet(layout::LambdaSet {
                        args: args1,
                        ret: ret1,
                        set: set1,
                        representation: repr1,
                        full_layout: _,
                    }),
                    LambdaSet(layout::LambdaSet {
                        args: args2,
                        ret: ret2,
                        set: set2,
                        representation: repr2,
                        full_layout: _,
                    }),
                ) => {
                    for ((fn1, captures1), (fn2, captures2)) in (**set1).iter().zip(*set2) {
                        if fn1 != fn2 {
                            return false;
                        }
                        equiv_fields!(captures1, captures2);
                    }
                    equiv_fields!(args1, args2);
                    stack.push((ret1, ret2));
                    stack.push((repr1, repr2));
                }
                _ => return false,
            }
        }

        true
    }
}

pub mod dbg_deep {
    use roc_module::symbol::Symbol;

    use crate::layout::{Builtin, Erased, LambdaSet, LayoutRepr, UnionLayout};

    use super::{InLayout, LayoutInterner};

    pub struct Dbg<'a, 'r, I: LayoutInterner<'a>>(pub &'r I, pub InLayout<'a>);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for Dbg<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let repr = self.0.get_repr(self.1);
            let semantic = self.0.get_semantic(self.1);

            f.debug_struct("Layout")
                .field("repr", &DbgRepr(self.0, &repr))
                .field("semantic", &semantic)
                .finish()
        }
    }

    pub struct DbgFields<'a, 'r, I: LayoutInterner<'a>>(pub &'r I, pub &'a [InLayout<'a>]);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for DbgFields<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_list()
                .entries(self.1.iter().map(|l| Dbg(self.0, *l)))
                .finish()
        }
    }

    struct DbgRepr<'a, 'r, I: LayoutInterner<'a>>(&'r I, &'r LayoutRepr<'a>);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for DbgRepr<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.1 {
                LayoutRepr::Builtin(b) => f
                    .debug_tuple("Builtin")
                    .field(&DbgBuiltin(self.0, *b))
                    .finish(),
                LayoutRepr::Struct(field_layouts) => f
                    .debug_struct("Struct")
                    .field("fields", &DbgFields(self.0, field_layouts))
                    .finish(),
                LayoutRepr::Ptr(b) => f.debug_tuple("Ptr").field(&Dbg(self.0, *b)).finish(),
                LayoutRepr::Union(un) => f
                    .debug_tuple("Union")
                    .field(&DbgUnion(self.0, *un))
                    .finish(),
                LayoutRepr::LambdaSet(ls) => f
                    .debug_tuple("LambdaSet")
                    .field(&DbgLambdaSet(self.0, *ls))
                    .finish(),
                LayoutRepr::RecursivePointer(rp) => {
                    f.debug_tuple("RecursivePointer").field(&rp.0).finish()
                }
                LayoutRepr::FunctionPointer(fp) => f
                    .debug_struct("FunctionPointer")
                    .field("args", &fp.args)
                    .field("ret", &fp.ret)
                    .finish(),
                LayoutRepr::Erased(Erased) => f.debug_struct("?Erased").finish(),
            }
        }
    }

    struct DbgTags<'a, 'r, I: LayoutInterner<'a>>(&'r I, &'a [&'a [InLayout<'a>]]);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for DbgTags<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_list()
                .entries(self.1.iter().map(|l| DbgFields(self.0, l)))
                .finish()
        }
    }

    struct DbgBuiltin<'a, 'r, I: LayoutInterner<'a>>(&'r I, Builtin<'a>);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for DbgBuiltin<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.1 {
                Builtin::Int(w) => f.debug_tuple("Int").field(&w).finish(),
                Builtin::Float(w) => f.debug_tuple("Frac").field(&w).finish(),
                Builtin::Bool => f.debug_tuple("Bool").finish(),
                Builtin::Decimal => f.debug_tuple("Decimal").finish(),
                Builtin::Str => f.debug_tuple("Str").finish(),
                Builtin::List(e) => f.debug_tuple("List").field(&Dbg(self.0, e)).finish(),
            }
        }
    }

    struct DbgUnion<'a, 'r, I: LayoutInterner<'a>>(&'r I, UnionLayout<'a>);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for DbgUnion<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.1 {
                UnionLayout::NonRecursive(payloads) => f
                    .debug_tuple("NonRecursive")
                    .field(&DbgTags(self.0, payloads))
                    .finish(),
                UnionLayout::Recursive(payloads) => f
                    .debug_tuple("Recursive")
                    .field(&DbgTags(self.0, payloads))
                    .finish(),
                UnionLayout::NonNullableUnwrapped(fields) => f
                    .debug_tuple("NonNullableUnwrapped")
                    .field(&DbgFields(self.0, fields))
                    .finish(),
                UnionLayout::NullableWrapped {
                    nullable_id,
                    other_tags,
                } => f
                    .debug_struct("NullableWrapped")
                    .field("nullable_id", &nullable_id)
                    .field("other_tags", &DbgTags(self.0, other_tags))
                    .finish(),
                UnionLayout::NullableUnwrapped {
                    nullable_id,
                    other_fields,
                } => f
                    .debug_struct("NullableUnwrapped")
                    .field("nullable_id", &nullable_id)
                    .field("other_tags", &DbgFields(self.0, other_fields))
                    .finish(),
            }
        }
    }

    struct DbgLambdaSet<'a, 'r, I: LayoutInterner<'a>>(&'r I, LambdaSet<'a>);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for DbgLambdaSet<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let LambdaSet {
                args,
                ret,
                set,
                representation,
                full_layout,
            } = self.1;

            f.debug_struct("LambdaSet")
                .field("args", &DbgFields(self.0, args))
                .field("ret", &Dbg(self.0, ret))
                .field("set", &DbgCapturesSet(self.0, set))
                .field("representation", &Dbg(self.0, representation))
                .field("full_layout", &full_layout)
                .finish()
        }
    }

    struct DbgCapturesSet<'a, 'r, I: LayoutInterner<'a>>(&'r I, &'a [(Symbol, &'a [InLayout<'a>])]);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for DbgCapturesSet<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_list()
                .entries(
                    self.1
                        .iter()
                        .map(|(sym, captures)| (sym, DbgFields(self.0, captures))),
                )
                .finish()
        }
    }
}

/// Provides a stable debug output
///
/// The debug output defined in `dbg_deep` uses the `Symbol` `std::fmt::Debug` instance, which uses
/// interned string names to make the output easier to interpret. That is useful for manual
/// debugging, but the interned strings are not stable in a multi-threaded context (e.g. when
/// running `cargo test`). The output of this module is always stable.
pub mod dbg_stable {
    use roc_module::symbol::Symbol;

    use crate::layout::{Builtin, Erased, LambdaSet, LayoutRepr, SemanticRepr, UnionLayout};

    use super::{InLayout, LayoutInterner};

    pub struct Dbg<'a, 'r, I: LayoutInterner<'a>>(pub &'r I, pub InLayout<'a>);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for Dbg<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let repr = self.0.get_repr(self.1);
            let semantic = self.0.get_semantic(self.1);

            struct ConsistentSemanticRepr<'a>(SemanticRepr<'a>);

            impl std::fmt::Debug for ConsistentSemanticRepr<'_> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    self.0.fmt_consistent(f)
                }
            }

            f.debug_struct("Layout")
                .field("repr", &DbgRepr(self.0, &repr))
                .field("semantic", &ConsistentSemanticRepr(semantic))
                .finish()
        }
    }

    pub struct DbgFields<'a, 'r, I: LayoutInterner<'a>>(pub &'r I, pub &'a [InLayout<'a>]);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for DbgFields<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_list()
                .entries(self.1.iter().map(|l| Dbg(self.0, *l)))
                .finish()
        }
    }

    struct DbgRepr<'a, 'r, I: LayoutInterner<'a>>(&'r I, &'r LayoutRepr<'a>);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for DbgRepr<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.1 {
                LayoutRepr::Builtin(b) => f
                    .debug_tuple("Builtin")
                    .field(&DbgBuiltin(self.0, *b))
                    .finish(),
                LayoutRepr::Struct(field_layouts) => f
                    .debug_struct("Struct")
                    .field("fields", &DbgFields(self.0, field_layouts))
                    .finish(),
                LayoutRepr::Ptr(b) => f.debug_tuple("Ptr").field(&Dbg(self.0, *b)).finish(),
                LayoutRepr::Union(un) => f
                    .debug_tuple("Union")
                    .field(&DbgUnion(self.0, *un))
                    .finish(),
                LayoutRepr::LambdaSet(ls) => f
                    .debug_tuple("LambdaSet")
                    .field(&DbgLambdaSet(self.0, *ls))
                    .finish(),
                LayoutRepr::RecursivePointer(rp) => {
                    f.debug_tuple("RecursivePointer").field(&rp.0).finish()
                }
                LayoutRepr::FunctionPointer(fp) => f
                    .debug_struct("FunctionPointer")
                    .field("args", &fp.args)
                    .field("ret", &fp.ret)
                    .finish(),
                LayoutRepr::Erased(Erased) => f.debug_struct("?Erased").finish(),
            }
        }
    }

    struct DbgTags<'a, 'r, I: LayoutInterner<'a>>(&'r I, &'a [&'a [InLayout<'a>]]);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for DbgTags<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_list()
                .entries(self.1.iter().map(|l| DbgFields(self.0, l)))
                .finish()
        }
    }

    struct DbgBuiltin<'a, 'r, I: LayoutInterner<'a>>(&'r I, Builtin<'a>);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for DbgBuiltin<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.1 {
                Builtin::Int(w) => f.debug_tuple("Int").field(&w).finish(),
                Builtin::Float(w) => f.debug_tuple("Frac").field(&w).finish(),
                Builtin::Bool => f.debug_tuple("Bool").finish(),
                Builtin::Decimal => f.debug_tuple("Decimal").finish(),
                Builtin::Str => f.debug_tuple("Str").finish(),
                Builtin::List(e) => f.debug_tuple("List").field(&Dbg(self.0, e)).finish(),
            }
        }
    }

    struct DbgUnion<'a, 'r, I: LayoutInterner<'a>>(&'r I, UnionLayout<'a>);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for DbgUnion<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.1 {
                UnionLayout::NonRecursive(payloads) => f
                    .debug_tuple("NonRecursive")
                    .field(&DbgTags(self.0, payloads))
                    .finish(),
                UnionLayout::Recursive(payloads) => f
                    .debug_tuple("Recursive")
                    .field(&DbgTags(self.0, payloads))
                    .finish(),
                UnionLayout::NonNullableUnwrapped(fields) => f
                    .debug_tuple("NonNullableUnwrapped")
                    .field(&DbgFields(self.0, fields))
                    .finish(),
                UnionLayout::NullableWrapped {
                    nullable_id,
                    other_tags,
                } => f
                    .debug_struct("NullableWrapped")
                    .field("nullable_id", &nullable_id)
                    .field("other_tags", &DbgTags(self.0, other_tags))
                    .finish(),
                UnionLayout::NullableUnwrapped {
                    nullable_id,
                    other_fields,
                } => f
                    .debug_struct("NullableUnwrapped")
                    .field("nullable_id", &nullable_id)
                    .field("other_tags", &DbgFields(self.0, other_fields))
                    .finish(),
            }
        }
    }

    struct DbgLambdaSet<'a, 'r, I: LayoutInterner<'a>>(&'r I, LambdaSet<'a>);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for DbgLambdaSet<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let LambdaSet {
                args,
                ret,
                set,
                representation,
                full_layout,
            } = self.1;

            f.debug_struct("LambdaSet")
                .field("args", &DbgFields(self.0, args))
                .field("ret", &Dbg(self.0, ret))
                .field("set", &DbgCapturesSet(self.0, set))
                .field("representation", &Dbg(self.0, representation))
                .field("full_layout", &full_layout)
                .finish()
        }
    }

    struct DbgCapturesSet<'a, 'r, I: LayoutInterner<'a>>(&'r I, &'a [(Symbol, &'a [InLayout<'a>])]);

    impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for DbgCapturesSet<'a, 'r, I> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_list()
                .entries(
                    self.1
                        .iter()
                        .map(|(sym, captures)| (sym.as_u64(), DbgFields(self.0, captures))),
                )
                .finish()
        }
    }
}

#[cfg(test)]
mod insert_lambda_set {
    use bumpalo::Bump;
    use roc_module::symbol::Symbol;
    use roc_target::Target;

    use crate::layout::{LambdaSet, Layout, LayoutRepr, SemanticRepr};

    use super::{GlobalLayoutInterner, InLayout, LayoutInterner, NeedsRecursionPointerFixup};

    const TARGET: Target = Target::LinuxX64;
    const TEST_SET: &&[(Symbol, &[InLayout])] =
        &(&[(Symbol::ATTR_ATTR, &[Layout::UNIT] as &[_])] as &[_]);
    const TEST_ARGS: &&[InLayout] = &(&[Layout::UNIT] as &[_]);
    const TEST_RET: InLayout = Layout::UNIT;

    const FIXUP: NeedsRecursionPointerFixup = NeedsRecursionPointerFixup(true);

    #[test]
    fn two_threads_write() {
        for _ in 0..100 {
            let mut arenas: Vec<_> = std::iter::repeat_with(Bump::new).take(10).collect();
            let global = GlobalLayoutInterner::with_capacity(2, TARGET);
            let set = TEST_SET;
            let repr = Layout::UNIT;
            std::thread::scope(|s| {
                let mut handles = Vec::with_capacity(10);
                for arena in arenas.iter_mut() {
                    let mut interner = global.fork();
                    handles.push(s.spawn(move || {
                        interner.insert_lambda_set(arena, TEST_ARGS, TEST_RET, set, FIXUP, repr)
                    }))
                }
                let ins: Vec<LambdaSet> = handles.into_iter().map(|t| t.join().unwrap()).collect();
                let interned = ins[0];
                assert!(ins.iter().all(|in2| interned == *in2));
            });
        }
    }

    #[test]
    fn insert_then_reintern() {
        let arena = &Bump::new();
        let global = GlobalLayoutInterner::with_capacity(2, TARGET);
        let mut interner = global.fork();

        let lambda_set =
            interner.insert_lambda_set(arena, TEST_ARGS, TEST_RET, TEST_SET, FIXUP, Layout::UNIT);
        let lambda_set_layout_in = interner.insert(Layout {
            repr: LayoutRepr::LambdaSet(lambda_set).direct(),
            semantic: SemanticRepr::NONE,
        });
        assert_eq!(lambda_set.full_layout, lambda_set_layout_in);
    }

    #[test]
    fn write_global_then_single_threaded() {
        let arena = &Bump::new();
        let global = GlobalLayoutInterner::with_capacity(2, TARGET);
        let set = TEST_SET;
        let repr = Layout::UNIT;

        let in1 = {
            let mut interner = global.fork();
            interner.insert_lambda_set(arena, TEST_ARGS, TEST_RET, set, FIXUP, repr)
        };

        let in2 = {
            let mut st_interner = global.unwrap().unwrap();
            st_interner.insert_lambda_set(arena, TEST_ARGS, TEST_RET, set, FIXUP, repr)
        };

        assert_eq!(in1, in2);
    }

    #[test]
    fn write_single_threaded_then_global() {
        let arena = &Bump::new();
        let global = GlobalLayoutInterner::with_capacity(2, TARGET);
        let mut st_interner = global.unwrap().unwrap();

        let set = TEST_SET;
        let repr = Layout::UNIT;

        let in1 = st_interner.insert_lambda_set(arena, TEST_ARGS, TEST_RET, set, FIXUP, repr);

        let global = st_interner.into_global();
        let mut interner = global.fork();

        let in2 = interner.insert_lambda_set(arena, TEST_ARGS, TEST_RET, set, FIXUP, repr);

        assert_eq!(in1, in2);
    }
}

#[cfg(test)]
mod insert_recursive_layout {
    use bumpalo::Bump;
    use roc_target::Target;

    use crate::layout::{Builtin, InLayout, Layout, LayoutRepr, SemanticRepr, UnionLayout};

    use super::{GlobalLayoutInterner, LayoutInterner};

    const TARGET: Target = Target::LinuxX64;

    fn make_layout<'a>(arena: &'a Bump, interner: &mut impl LayoutInterner<'a>) -> Layout<'a> {
        let list_rec = Layout {
            repr: LayoutRepr::Builtin(Builtin::List(Layout::NAKED_RECURSIVE_PTR)).direct(),
            semantic: SemanticRepr::NONE,
        };
        let repr = LayoutRepr::Union(UnionLayout::Recursive(&*arena.alloc([
            &*arena.alloc([interner.insert(list_rec)]),
            &*arena.alloc_slice_fill_iter([interner.insert_direct_no_semantic(
                LayoutRepr::struct_(&*arena.alloc([Layout::NAKED_RECURSIVE_PTR])),
            )]),
        ])))
        .direct();
        Layout {
            repr,
            semantic: SemanticRepr::NONE,
        }
    }

    fn get_rec_ptr_index<'a>(interner: &impl LayoutInterner<'a>, layout: InLayout<'a>) -> usize {
        match interner.chase_recursive(layout) {
            LayoutRepr::Union(UnionLayout::Recursive(&[&[l1], &[l2]])) => {
                match (interner.get_repr(l1), interner.get_repr(l2)) {
                    (LayoutRepr::Builtin(Builtin::List(l1)), LayoutRepr::Struct(&[l2])) => {
                        match (interner.get_repr(l1), interner.get_repr(l2)) {
                            (
                                LayoutRepr::RecursivePointer(i1),
                                LayoutRepr::RecursivePointer(i2),
                            ) => {
                                assert_eq!(i1, i2);
                                assert_ne!(i1, Layout::VOID);
                                i1.0
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn write_two_threads() {
        let arena = &Bump::new();
        let global = GlobalLayoutInterner::with_capacity(2, TARGET);
        let layout = {
            let mut interner = global.fork();
            make_layout(arena, &mut interner)
        };

        let in1 = {
            let mut interner = global.fork();
            interner.insert_recursive(arena, layout)
        };

        let in2 = {
            let mut interner = global.fork();
            interner.insert_recursive(arena, layout)
        };

        assert_eq!(in1, in2);
    }

    #[test]
    fn write_twice_thread_local_single_thread() {
        let arena = &Bump::new();
        let global = GlobalLayoutInterner::with_capacity(2, TARGET);
        let mut interner = global.fork();
        let layout = make_layout(arena, &mut interner);

        let in1 = interner.insert_recursive(arena, layout);
        let rec1 = get_rec_ptr_index(&interner, in1);
        let in2 = interner.insert_recursive(arena, layout);
        let rec2 = get_rec_ptr_index(&interner, in2);

        assert_eq!(in1, in2);
        assert_eq!(rec1, rec2);
    }

    #[test]
    fn write_twice_single_thread() {
        let arena = &Bump::new();
        let global = GlobalLayoutInterner::with_capacity(2, TARGET);
        let mut interner = GlobalLayoutInterner::unwrap(global).unwrap();
        let layout = make_layout(arena, &mut interner);

        let in1 = interner.insert_recursive(arena, layout);
        let rec1 = get_rec_ptr_index(&interner, in1);
        let in2 = interner.insert_recursive(arena, layout);
        let rec2 = get_rec_ptr_index(&interner, in2);

        assert_eq!(in1, in2);
        assert_eq!(rec1, rec2);
    }

    #[test]
    fn many_threads_read_write() {
        for _ in 0..100 {
            let mut arenas: Vec<_> = std::iter::repeat_with(Bump::new).take(10).collect();
            let global = GlobalLayoutInterner::with_capacity(2, TARGET);
            std::thread::scope(|s| {
                let mut handles = Vec::with_capacity(10);
                for arena in arenas.iter_mut() {
                    let mut interner = global.fork();
                    let handle = s.spawn(move || {
                        let layout = make_layout(arena, &mut interner);
                        let in_layout = interner.insert_recursive(arena, layout);
                        (in_layout, get_rec_ptr_index(&interner, in_layout))
                    });
                    handles.push(handle);
                }
                let ins: Vec<(InLayout, usize)> =
                    handles.into_iter().map(|t| t.join().unwrap()).collect();
                let interned = ins[0];
                assert!(ins.iter().all(|in2| interned == *in2));
            });
        }
    }

    #[test]
    fn insert_then_reintern() {
        let arena = &Bump::new();
        let global = GlobalLayoutInterner::with_capacity(2, TARGET);
        let mut interner = global.fork();

        let layout = make_layout(arena, &mut interner);
        let interned_layout = interner.insert_recursive(arena, layout);
        let full_layout = interner.get(interned_layout);
        assert_ne!(layout, full_layout);
        assert_eq!(interner.insert(full_layout), interned_layout);
    }

    #[test]
    fn write_global_then_single_threaded() {
        let arena = &Bump::new();
        let global = GlobalLayoutInterner::with_capacity(2, TARGET);
        let layout = {
            let mut interner = global.fork();
            make_layout(arena, &mut interner)
        };

        let in1: InLayout = {
            let mut interner = global.fork();
            interner.insert_recursive(arena, layout)
        };

        let in2 = {
            let mut st_interner = global.unwrap().unwrap();
            st_interner.insert_recursive(arena, layout)
        };

        assert_eq!(in1, in2);
    }

    #[test]
    fn write_single_threaded_then_global() {
        let arena = &Bump::new();
        let global = GlobalLayoutInterner::with_capacity(2, TARGET);
        let mut st_interner = global.unwrap().unwrap();

        let layout = make_layout(arena, &mut st_interner);

        let in1 = st_interner.insert_recursive(arena, layout);

        let global = st_interner.into_global();
        let mut interner = global.fork();

        let in2 = interner.insert_recursive(arena, layout);

        assert_eq!(in1, in2);
    }
}
