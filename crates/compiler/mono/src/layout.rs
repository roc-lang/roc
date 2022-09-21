use crate::ir::Parens;
use bitvec::vec::BitVec;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::all::{default_hasher, FnvMap, MutMap};
use roc_error_macros::{internal_error, todo_abilities};
use roc_intern::{Interned, Interner, SingleThreadedInterner, ThreadLocalInterner};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{Interns, Symbol};
use roc_problem::can::RuntimeError;
use roc_target::{PtrWidth, TargetInfo};
use roc_types::num::NumericRange;
use roc_types::subs::{
    self, Content, FlatType, GetSubsSlice, Label, OptVariable, RecordFields, Subs, UnionTags,
    UnsortedUnionLabels, Variable,
};
use roc_types::types::{gather_fields_unsorted_iter, RecordField, RecordFieldsError};
use std::cmp::Ordering;
use std::collections::hash_map::{DefaultHasher, Entry};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use ven_pretty::{DocAllocator, DocBuilder};

// if your changes cause this number to go down, great!
// please change it to the lower number.
// if it went up, maybe check that the change is really required
roc_error_macros::assert_sizeof_aarch64!(Builtin, 2 * 8);
roc_error_macros::assert_sizeof_aarch64!(Layout, 4 * 8);
roc_error_macros::assert_sizeof_aarch64!(UnionLayout, 3 * 8);
roc_error_macros::assert_sizeof_aarch64!(LambdaSet, 3 * 8);

roc_error_macros::assert_sizeof_wasm!(Builtin, 2 * 4);
roc_error_macros::assert_sizeof_wasm!(Layout, 6 * 4);
roc_error_macros::assert_sizeof_wasm!(UnionLayout, 3 * 4);
roc_error_macros::assert_sizeof_wasm!(LambdaSet, 3 * 4);

roc_error_macros::assert_sizeof_default!(Builtin, 2 * 8);
roc_error_macros::assert_sizeof_default!(Layout, 4 * 8);
roc_error_macros::assert_sizeof_default!(UnionLayout, 3 * 8);
roc_error_macros::assert_sizeof_default!(LambdaSet, 3 * 8);

type LayoutResult<'a> = Result<Layout<'a>, LayoutProblem>;
type RawFunctionLayoutResult<'a> = Result<RawFunctionLayout<'a>, LayoutProblem>;

#[derive(Debug, Clone, Copy)]
struct CacheMeta {
    /// Does this cache entry include a recursive structure? If so, what's the recursion variable
    /// of that structure?
    has_recursive_structure: OptVariable,
}

impl CacheMeta {
    #[inline(always)]
    fn to_criteria(self) -> CacheCriteria {
        let CacheMeta {
            has_recursive_structure,
        } = self;
        CacheCriteria {
            has_naked_recursion_pointer: false,
            has_recursive_structure,
        }
    }
}

/// A single layer of the layout cache.
/// Snapshots are implemented by operating on new layers, and rollbacks by dropping the latest
/// layer.
#[derive(Debug)]
struct CacheLayer<Result>(FnvMap<Variable, (Result, CacheMeta)>);

impl<Result> Default for CacheLayer<Result> {
    fn default() -> Self {
        Self(Default::default())
    }
}

#[cfg(debug_assertions)]
#[derive(Debug, Default, Clone, Copy)]
pub struct CacheStatistics {
    pub hits: u64,
    pub misses: u64,
    /// How many times we could not cache a calculated layout
    pub non_insertable: u64,
    /// How many time we could not reuse a cached layout
    pub non_reusable: u64,
    /// How many times an entry was added to the cache
    pub insertions: u64,
}

macro_rules! inc_stat {
    ($stats:expr, $field:ident) => {
        #[cfg(debug_assertions)]
        {
            $stats.$field += 1;
        }
    };
}

pub type LayoutInterner<'a> = ThreadLocalInterner<'a, Layout<'a>>;
pub type STLayoutInterner<'a> = SingleThreadedInterner<'a, Layout<'a>>;

/// Layout cache to avoid recomputing [Layout] from a [Variable] multiple times.
#[derive(Debug)]
pub struct LayoutCache<'a> {
    pub target_info: TargetInfo,
    cache: std::vec::Vec<CacheLayer<LayoutResult<'a>>>,
    raw_function_cache: std::vec::Vec<CacheLayer<RawFunctionLayoutResult<'a>>>,

    pub interner: LayoutInterner<'a>,

    /// Statistics on the usage of the layout cache.
    #[cfg(debug_assertions)]
    stats: CacheStatistics,
    #[cfg(debug_assertions)]
    raw_function_stats: CacheStatistics,
}

impl<'a> LayoutCache<'a> {
    pub fn new(interner: LayoutInterner<'a>, target_info: TargetInfo) -> Self {
        let mut cache = std::vec::Vec::with_capacity(4);
        cache.push(CacheLayer::default());
        let mut raw_cache = std::vec::Vec::with_capacity(4);
        raw_cache.push(CacheLayer::default());
        Self {
            target_info,
            cache,
            raw_function_cache: raw_cache,

            interner,

            #[cfg(debug_assertions)]
            stats: CacheStatistics::default(),
            #[cfg(debug_assertions)]
            raw_function_stats: CacheStatistics::default(),
        }
    }

    pub fn from_var(
        &mut self,
        arena: &'a Bump,
        var: Variable,
        subs: &Subs,
    ) -> Result<Layout<'a>, LayoutProblem> {
        // Store things according to the root Variable, to avoid duplicate work.
        let var = subs.get_root_key_without_compacting(var);

        let mut env = Env {
            arena,
            subs,
            seen: Vec::new_in(arena),
            target_info: self.target_info,
            cache: self,
        };

        // [Layout::from_var] should query the cache!
        let Cacheable(value, criteria) = Layout::from_var(&mut env, var);
        debug_assert!(
            criteria.is_cacheable(),
            "{:?} not cacheable as top-level",
            value
        );
        value
    }

    pub fn raw_from_var(
        &mut self,
        arena: &'a Bump,
        var: Variable,
        subs: &Subs,
    ) -> Result<RawFunctionLayout<'a>, LayoutProblem> {
        // Store things according to the root Variable, to avoid duplicate work.
        let var = subs.get_root_key_without_compacting(var);

        let mut env = Env {
            arena,
            subs,
            seen: Vec::new_in(arena),
            target_info: self.target_info,
            cache: self,
        };

        // [Layout::from_var] should query the cache!
        let Cacheable(value, criteria) = RawFunctionLayout::from_var(&mut env, var);
        debug_assert!(
            criteria.is_cacheable(),
            "{:?} not cacheable as top-level",
            value
        );
        value
    }

    #[inline(always)]
    fn get_help<Result: Copy>(
        cache: &[CacheLayer<Result>],
        subs: &Subs,
        var: Variable,
    ) -> Option<(Result, CacheMeta)> {
        let root = subs.get_root_key_without_compacting(var);

        for layer in cache.iter().rev() {
            // TODO: it's possible that after unification, roots in earlier cache layers changed...
            // how often does that happen?
            if let Some(result) = layer.0.get(&root) {
                return Some(*result);
            }
        }
        None
    }

    #[inline(always)]
    fn insert_help<Result: std::fmt::Debug + Copy>(
        cache: &mut [CacheLayer<Result>],
        subs: &Subs,
        var: Variable,
        result: Result,
        cache_metadata: CacheMeta,
    ) {
        let root = subs.get_root_key_without_compacting(var);
        let layer = cache
            .last_mut()
            .expect("cache must have at least one layer");
        let opt_old_result = layer.0.insert(root, (result, cache_metadata));
        if let Some(old_result) = opt_old_result {
            // Can happen when we need to re-calculate a recursive layout
            roc_tracing::debug!(
                ?old_result,
                new_result=?result,
                ?var,
                "overwritting layout cache"
            );
        }
    }

    #[inline(always)]
    fn get(&self, subs: &Subs, var: Variable) -> Option<(LayoutResult<'a>, CacheMeta)> {
        Self::get_help(&self.cache, subs, var)
    }

    #[inline(always)]
    fn get_raw_function(
        &self,
        subs: &Subs,
        var: Variable,
    ) -> Option<(RawFunctionLayoutResult<'a>, CacheMeta)> {
        Self::get_help(&self.raw_function_cache, subs, var)
    }

    #[inline(always)]
    fn insert(
        &mut self,
        subs: &Subs,
        var: Variable,
        result: LayoutResult<'a>,
        cache_metadata: CacheMeta,
    ) {
        Self::insert_help(&mut self.cache, subs, var, result, cache_metadata)
    }

    #[inline(always)]
    fn insert_raw_function(
        &mut self,
        subs: &Subs,
        var: Variable,
        result: RawFunctionLayoutResult<'a>,
        cache_metadata: CacheMeta,
    ) {
        Self::insert_help(
            &mut self.raw_function_cache,
            subs,
            var,
            result,
            cache_metadata,
        )
    }

    #[inline(always)]
    pub fn snapshot(&mut self) -> CacheSnapshot {
        debug_assert_eq!(self.raw_function_cache.len(), self.cache.len());
        self.cache.push(Default::default());
        self.raw_function_cache.push(Default::default());
        CacheSnapshot {
            layer: self.cache.len(),
        }
    }

    #[inline(always)]
    pub fn rollback_to(&mut self, snapshot: CacheSnapshot) {
        let CacheSnapshot { layer } = snapshot;

        debug_assert_eq!(self.cache.len(), layer);
        debug_assert_eq!(self.raw_function_cache.len(), layer);

        self.cache.pop();
        self.raw_function_cache.pop();
    }

    /// Invalidates the list of given root variables.
    /// Usually called after unification, when merged variables with changed contents need to be
    /// invalidated.
    pub fn invalidate(&mut self, vars: impl IntoIterator<Item = Variable>) {
        for var in vars.into_iter() {
            for layer in self.cache.iter_mut().rev() {
                layer.0.remove(&var);
                roc_tracing::debug!(?var, "invalidating cached layout");
            }
            for layer in self.raw_function_cache.iter_mut().rev() {
                layer.0.remove(&var);
                roc_tracing::debug!(?var, "invalidating cached layout");
            }
        }
    }

    #[cfg(debug_assertions)]
    pub fn statistics(&self) -> (CacheStatistics, CacheStatistics) {
        (self.stats, self.raw_function_stats)
    }
}

pub struct CacheSnapshot {
    /// Index of the pushed layer
    layer: usize,
}

#[derive(Clone, Copy, Debug)]
struct CacheCriteria {
    /// Whether there is a naked recursion pointer in this layout, that doesn't pass through a
    /// recursive structure.
    has_naked_recursion_pointer: bool,
    /// Whether this layout contains a recursive structure. If `Some`, contains the variable of the
    /// recursion variable of that structure.
    has_recursive_structure: OptVariable,
}

const CACHEABLE: CacheCriteria = CacheCriteria {
    has_naked_recursion_pointer: false,
    has_recursive_structure: OptVariable::NONE,
};

const NAKED_RECURSION_PTR: CacheCriteria = CacheCriteria {
    has_naked_recursion_pointer: true,
    has_recursive_structure: OptVariable::NONE,
};

impl CacheCriteria {
    #[inline(always)]
    fn is_cacheable(&self) -> bool {
        // Can't cache if there a naked recursion pointer that isn't covered by a recursive layout.
        !self.has_naked_recursion_pointer
    }

    /// Makes `self` cacheable iff self and other are cacheable.
    #[inline(always)]
    fn and(&mut self, other: Self) {
        self.has_naked_recursion_pointer =
            self.has_naked_recursion_pointer || other.has_naked_recursion_pointer;
        // TODO: can these ever conflict?
        self.has_recursive_structure = self
            .has_recursive_structure
            .or(other.has_recursive_structure);
    }

    #[inline(always)]
    fn pass_through_recursive_union(&mut self, recursion_var: Variable) {
        self.has_naked_recursion_pointer = false;
        self.has_recursive_structure = OptVariable::some(recursion_var);
    }

    #[inline(always)]
    fn cache_metadata(&self) -> CacheMeta {
        CacheMeta {
            has_recursive_structure: self.has_recursive_structure,
        }
    }
}

#[derive(Debug)]
pub(crate) struct Cacheable<T>(T, CacheCriteria);

impl<T> Cacheable<T> {
    #[inline(always)]
    fn map<U>(self, f: impl FnOnce(T) -> U) -> Cacheable<U> {
        Cacheable(f(self.0), self.1)
    }

    #[inline(always)]
    fn decompose(self, and_with: &mut CacheCriteria) -> T {
        let Self(value, criteria) = self;
        and_with.and(criteria);
        value
    }

    #[inline(always)]
    pub fn value(self) -> T {
        self.0
    }
}

impl<T, E> Cacheable<Result<T, E>> {
    #[inline(always)]
    fn then<U>(self, f: impl FnOnce(T) -> U) -> Cacheable<Result<U, E>> {
        let Cacheable(result, criteria) = self;
        match result {
            Ok(t) => Cacheable(Ok(f(t)), criteria),
            Err(e) => Cacheable(Err(e), criteria),
        }
    }
}

#[inline(always)]
fn cacheable<T>(v: T) -> Cacheable<T> {
    Cacheable(v, CACHEABLE)
}

/// Decomposes a cached layout.
/// If the layout is an error, the problem is immediately returned with the cache policy (this is
/// like `?`).
/// If the layout is not an error, the cache policy is `and`ed with `total_criteria`, and the layout
/// is passed back.
macro_rules! cached {
    ($expr:expr, $total_criteria:expr) => {
        match $expr {
            Cacheable(Ok(v), criteria) => {
                $total_criteria.and(criteria);
                v
            }
            Cacheable(Err(v), criteria) => return Cacheable(Err(v), criteria),
        }
    };
}

pub type TagIdIntType = u16;
pub const MAX_ENUM_SIZE: usize = (std::mem::size_of::<TagIdIntType>() * 8) as usize;
const GENERATE_NULLABLE: bool = true;

#[derive(Debug, Clone, Copy)]
pub enum LayoutProblem {
    UnresolvedTypeVar(Variable),
    Erroneous,
}

impl From<LayoutProblem> for RuntimeError {
    fn from(lp: LayoutProblem) -> Self {
        match lp {
            LayoutProblem::UnresolvedTypeVar(_) => RuntimeError::UnresolvedTypeVar,
            LayoutProblem::Erroneous => RuntimeError::ErroneousType,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RawFunctionLayout<'a> {
    Function(&'a [Layout<'a>], LambdaSet<'a>, &'a Layout<'a>),
    ZeroArgumentThunk(Layout<'a>),
}

impl<'a> RawFunctionLayout<'a> {
    pub fn is_zero_argument_thunk(&self) -> bool {
        matches!(self, RawFunctionLayout::ZeroArgumentThunk(_))
    }

    fn new_help<'b>(
        env: &mut Env<'a, 'b>,
        var: Variable,
        content: Content,
    ) -> Cacheable<RawFunctionLayoutResult<'a>> {
        use roc_types::subs::Content::*;
        match content {
            FlexVar(_) | RigidVar(_) => cacheable(Err(LayoutProblem::UnresolvedTypeVar(var))),
            FlexAbleVar(_, _) | RigidAbleVar(_, _) => todo_abilities!("Not reachable yet"),
            RecursionVar { structure, .. } => {
                let structure_content = env.subs.get_content_without_compacting(structure);
                Self::new_help(env, structure, *structure_content)
            }
            LambdaSet(lset) => Self::layout_from_lambda_set(env, lset),
            Structure(flat_type) => Self::layout_from_flat_type(env, flat_type),
            RangedNumber(..) => Layout::new_help(env, var, content).then(Self::ZeroArgumentThunk),

            // Ints
            Alias(Symbol::NUM_I128, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::i128())))
            }
            Alias(Symbol::NUM_I64, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::i64())))
            }
            Alias(Symbol::NUM_I32, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::i32())))
            }
            Alias(Symbol::NUM_I16, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::i16())))
            }
            Alias(Symbol::NUM_I8, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::i8())))
            }

            // I think unsigned and signed use the same layout
            Alias(Symbol::NUM_U128, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::u128())))
            }
            Alias(Symbol::NUM_U64, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::u64())))
            }
            Alias(Symbol::NUM_U32, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::u32())))
            }
            Alias(Symbol::NUM_U16, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::u16())))
            }
            Alias(Symbol::NUM_U8, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::u8())))
            }

            // Floats
            Alias(Symbol::NUM_F64, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::f64())))
            }
            Alias(Symbol::NUM_F32, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::f32())))
            }

            // Nat
            Alias(Symbol::NUM_NAT, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::usize(env.target_info))))
            }

            Alias(symbol, _, _, _) if symbol.is_builtin() => {
                Layout::new_help(env, var, content).then(Self::ZeroArgumentThunk)
            }

            Alias(_, _, var, _) => Self::from_var(env, var),
            Error => cacheable(Err(LayoutProblem::Erroneous)),
        }
    }

    fn layout_from_lambda_set(
        _env: &mut Env<'a, '_>,
        _lset: subs::LambdaSet,
    ) -> Cacheable<RawFunctionLayoutResult<'a>> {
        unreachable!()
        // Lambda set is just a tag union from the layout's perspective.
        // Self::layout_from_flat_type(env, lset.as_tag_union())
    }

    fn layout_from_flat_type(
        env: &mut Env<'a, '_>,
        flat_type: FlatType,
    ) -> Cacheable<RawFunctionLayoutResult<'a>> {
        use roc_types::subs::FlatType::*;

        let arena = env.arena;

        match flat_type {
            Func(args, closure_var, ret_var) => {
                let mut fn_args = Vec::with_capacity_in(args.len(), arena);

                let mut cache_criteria = CACHEABLE;

                for index in args.into_iter() {
                    let arg_var = env.subs[index];
                    let layout = cached!(Layout::from_var(env, arg_var), cache_criteria);
                    fn_args.push(layout);
                }

                let ret = cached!(Layout::from_var(env, ret_var), cache_criteria);

                let fn_args = fn_args.into_bump_slice();
                let ret = arena.alloc(ret);

                let lambda_set = cached!(
                    LambdaSet::from_var(
                        env.cache,
                        env.arena,
                        env.subs,
                        closure_var,
                        env.target_info,
                    ),
                    cache_criteria
                );

                Cacheable(Ok(Self::Function(fn_args, lambda_set, ret)), cache_criteria)
            }
            TagUnion(tags, ext) if tags.is_newtype_wrapper(env.subs) => {
                debug_assert!(ext_var_is_empty_tag_union(env.subs, ext));
                let slice_index = tags.variables().into_iter().next().unwrap();
                let slice = env.subs[slice_index];
                let var_index = slice.into_iter().next().unwrap();
                let var = env.subs[var_index];

                Self::from_var(env, var)
            }
            Record(fields, ext) if fields.len() == 1 => {
                debug_assert!(ext_var_is_empty_record(env.subs, ext));

                let var_index = fields.iter_variables().next().unwrap();
                let var = env.subs[var_index];

                Self::from_var(env, var)
            }
            _ => {
                let mut criteria = CACHEABLE;
                let layout = cached!(layout_from_flat_type(env, flat_type), criteria);
                Cacheable(Ok(Self::ZeroArgumentThunk(layout)), criteria)
            }
        }
    }

    /// Returns Err(()) if given an error, or Ok(Layout) if given a non-erroneous Structure.
    /// Panics if given a FlexVar or RigidVar, since those should have been
    /// monomorphized away already!
    fn from_var(env: &mut Env<'a, '_>, var: Variable) -> Cacheable<RawFunctionLayoutResult<'a>> {
        env.cached_raw_function_or(var, |env| {
            if env.is_seen(var) {
                unreachable!("The initial variable of a signature cannot be seen already")
            } else {
                let content = env.subs.get_content_without_compacting(var);
                Self::new_help(env, var, *content)
            }
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FieldOrderHash(u64);

impl FieldOrderHash {
    // NB: This should really be a proper "zero" hash via `DefaultHasher::new().finish()`, but Rust
    // stdlib hashers are not (yet) compile-time-computable.
    const ZERO_FIELD_HASH: Self = Self(0);
    const IRRELEVANT_NON_ZERO_FIELD_HASH: Self = Self(1);

    pub fn from_ordered_fields(fields: &[&Lowercase]) -> Self {
        if fields.is_empty() {
            // HACK: we must make sure this is always equivalent to a `ZERO_FIELD_HASH`.
            return Self::ZERO_FIELD_HASH;
        }

        let mut hasher = DefaultHasher::new();
        fields.iter().for_each(|field| field.hash(&mut hasher));
        Self(hasher.finish())
    }
}

/// Types for code gen must be monomorphic. No type variables allowed!
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Layout<'a> {
    Builtin(Builtin<'a>),
    Struct {
        /// Two different struct types can have the same layout, for example
        ///   { a: U8,  b: I64 }
        ///   { a: I64, b: U8 }
        /// both have the layout {I64, U8}. Not distinguishing the order of record fields can cause
        /// us problems during monomorphization when we specialize the same type in different ways,
        /// so keep a hash of the record order for disambiguation. This still of course may result
        /// in collisions, but it's unlikely.
        ///
        /// See also https://github.com/roc-lang/roc/issues/2535.
        field_order_hash: FieldOrderHash,
        field_layouts: &'a [Layout<'a>],
    },
    Boxed(&'a Layout<'a>),
    Union(UnionLayout<'a>),
    LambdaSet(LambdaSet<'a>),
    RecursivePointer,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UnionLayout<'a> {
    /// A non-recursive tag union
    /// e.g. `Result a e : [Ok a, Err e]`
    NonRecursive(&'a [&'a [Layout<'a>]]),
    /// A recursive tag union (general case)
    /// e.g. `Expr : [Sym Str, Add Expr Expr]`
    Recursive(&'a [&'a [Layout<'a>]]),
    /// A recursive tag union with just one constructor
    /// Optimization: No need to store a tag ID (the payload is "unwrapped")
    /// e.g. `RoseTree a : [Tree a (List (RoseTree a))]`
    NonNullableUnwrapped(&'a [Layout<'a>]),
    /// A recursive tag union that has an empty variant
    /// Optimization: Represent the empty variant as null pointer => no memory usage & fast comparison
    /// It has more than one other variant, so they need tag IDs (payloads are "wrapped")
    /// e.g. `FingerTree a : [Empty, Single a, More (Some a) (FingerTree (Tuple a)) (Some a)]`
    /// see also: https://youtu.be/ip92VMpf_-A?t=164
    ///
    /// nullable_id refers to the index of the tag that is represented at runtime as NULL.
    /// For example, in `FingerTree a : [Empty, Single a, More (Some a) (FingerTree (Tuple a)) (Some a)]`,
    /// the ids would be Empty = 0, More = 1, Single = 2, because that's how those tags are
    /// ordered alphabetically. Since the Empty tag will be represented at runtime as NULL,
    /// and since Empty's tag id is 0, here nullable_id would be 0.
    NullableWrapped {
        nullable_id: u16,
        other_tags: &'a [&'a [Layout<'a>]],
    },
    /// A recursive tag union with only two variants, where one is empty.
    /// Optimizations: Use null for the empty variant AND don't store a tag ID for the other variant.
    /// e.g. `ConsList a : [Nil, Cons a (ConsList a)]`
    ///
    /// nullable_id is a bool because it's only ever 0 or 1, but (as with the NullableWrapped
    /// variant), it reprsents the index of the tag that will be represented at runtime as NULL.
    ///
    /// So for example, in `ConsList a : [Nil, Cons a (ConsList a)]`, Nil is tag id 1 and
    /// Cons is tag id 0 because Nil comes alphabetically after Cons. Here, Nil will be
    /// represented as NULL at runtime, so nullable_id is 1 - which is to say, `true`, because
    /// `(1 as bool)` is `true`.
    NullableUnwrapped {
        nullable_id: bool,
        other_fields: &'a [Layout<'a>],
    },
}

impl<'a> UnionLayout<'a> {
    pub fn to_doc<'b, D, A, I>(
        self,
        alloc: &'b D,
        interner: &I,
        _parens: Parens,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
        I: Interner<'a, Layout<'a>>,
    {
        use UnionLayout::*;

        match self {
            NonRecursive(tags) => {
                let tags_doc = tags.iter().map(|fields| {
                    alloc.text("C ").append(
                        alloc.intersperse(
                            fields
                                .iter()
                                .map(|x| x.to_doc(alloc, interner, Parens::InTypeParam)),
                            " ",
                        ),
                    )
                });

                alloc
                    .text("[")
                    .append(alloc.intersperse(tags_doc, ", "))
                    .append(alloc.text("]"))
            }
            Recursive(tags) => {
                let tags_doc = tags.iter().map(|fields| {
                    alloc.text("C ").append(
                        alloc.intersperse(
                            fields
                                .iter()
                                .map(|x| x.to_doc(alloc, interner, Parens::InTypeParam)),
                            " ",
                        ),
                    )
                });
                alloc
                    .text("[<r>")
                    .append(alloc.intersperse(tags_doc, ", "))
                    .append(alloc.text("]"))
            }
            NonNullableUnwrapped(fields) => {
                let fields_doc = alloc.text("C ").append(
                    alloc.intersperse(
                        fields
                            .iter()
                            .map(|x| x.to_doc(alloc, interner, Parens::InTypeParam)),
                        " ",
                    ),
                );
                alloc
                    .text("[<rnnu>")
                    .append(fields_doc)
                    .append(alloc.text("]"))
            }
            NullableUnwrapped {
                nullable_id,
                other_fields,
            } => {
                let fields_doc = alloc.text("C ").append(
                    alloc.intersperse(
                        other_fields
                            .iter()
                            .map(|x| x.to_doc(alloc, interner, Parens::InTypeParam)),
                        " ",
                    ),
                );
                let tags_doc = if nullable_id {
                    alloc.concat(vec![alloc.text("<null>, "), fields_doc])
                } else {
                    alloc.concat(vec![fields_doc, alloc.text(", <null>")])
                };
                alloc
                    .text("[<rnu>")
                    .append(tags_doc)
                    .append(alloc.text("]"))
            }
            _ => alloc.text("TODO"),
        }
    }

    pub fn layout_at(self, tag_id: TagIdIntType, index: usize) -> Layout<'a> {
        let result = match self {
            UnionLayout::NonRecursive(tag_layouts) => {
                let field_layouts = tag_layouts[tag_id as usize];

                // this cannot be recursive; return immediately
                return field_layouts[index];
            }
            UnionLayout::Recursive(tag_layouts) => {
                let field_layouts = tag_layouts[tag_id as usize];

                field_layouts[index]
            }
            UnionLayout::NonNullableUnwrapped(field_layouts) => field_layouts[index],
            UnionLayout::NullableWrapped {
                nullable_id,
                other_tags,
            } => {
                debug_assert_ne!(nullable_id, tag_id);

                let tag_index = if tag_id < nullable_id {
                    tag_id
                } else {
                    tag_id - 1
                };

                let field_layouts = other_tags[tag_index as usize];
                field_layouts[index]
            }

            UnionLayout::NullableUnwrapped {
                nullable_id,
                other_fields,
            } => {
                debug_assert_ne!(nullable_id, tag_id != 0);

                other_fields[index as usize]
            }
        };

        if let Layout::RecursivePointer = result {
            Layout::Union(self)
        } else {
            result
        }
    }

    pub fn number_of_tags(&'a self) -> usize {
        match self {
            UnionLayout::NonRecursive(tags) | UnionLayout::Recursive(tags) => tags.len(),

            UnionLayout::NullableWrapped { other_tags, .. } => other_tags.len() + 1,
            UnionLayout::NonNullableUnwrapped(_) => 1,
            UnionLayout::NullableUnwrapped { .. } => 2,
        }
    }

    pub fn discriminant(&self) -> Discriminant {
        match self {
            UnionLayout::NonRecursive(tags) => Discriminant::from_number_of_tags(tags.len()),
            UnionLayout::Recursive(tags) => Discriminant::from_number_of_tags(tags.len()),

            UnionLayout::NullableWrapped { other_tags, .. } => {
                Discriminant::from_number_of_tags(other_tags.len() + 1)
            }
            UnionLayout::NonNullableUnwrapped(_) => Discriminant::from_number_of_tags(2),
            UnionLayout::NullableUnwrapped { .. } => Discriminant::from_number_of_tags(1),
        }
    }

    pub fn tag_id_layout(&self) -> Layout<'a> {
        // TODO is it beneficial to return a more specific layout?
        // e.g. Layout::bool() and Layout::VOID
        match self.discriminant() {
            Discriminant::U0 => Layout::u8(),
            Discriminant::U1 => Layout::u8(),
            Discriminant::U8 => Layout::u8(),
            Discriminant::U16 => Layout::u16(),
        }
    }

    fn stores_tag_id_in_pointer_bits(tags: &[&[Layout<'a>]], target_info: TargetInfo) -> bool {
        tags.len() < target_info.ptr_width() as usize
    }

    pub const POINTER_MASK_32BIT: usize = 0b0000_0111;
    pub const POINTER_MASK_64BIT: usize = 0b0000_0011;

    pub fn tag_id_pointer_bits_and_mask(target_info: TargetInfo) -> (usize, usize) {
        match target_info.ptr_width() {
            PtrWidth::Bytes8 => (3, Self::POINTER_MASK_64BIT),
            PtrWidth::Bytes4 => (2, Self::POINTER_MASK_32BIT),
        }
    }

    // i.e. it is not implicit and not stored in the pointer bits
    pub fn stores_tag_id_as_data(&self, target_info: TargetInfo) -> bool {
        match self {
            UnionLayout::NonRecursive(_) => true,
            UnionLayout::Recursive(tags)
            | UnionLayout::NullableWrapped {
                other_tags: tags, ..
            } => !Self::stores_tag_id_in_pointer_bits(tags, target_info),
            UnionLayout::NonNullableUnwrapped(_) | UnionLayout::NullableUnwrapped { .. } => false,
        }
    }

    pub fn stores_tag_id_in_pointer(&self, target_info: TargetInfo) -> bool {
        match self {
            UnionLayout::NonRecursive(_) => false,
            UnionLayout::Recursive(tags)
            | UnionLayout::NullableWrapped {
                other_tags: tags, ..
            } => Self::stores_tag_id_in_pointer_bits(tags, target_info),
            UnionLayout::NonNullableUnwrapped(_) | UnionLayout::NullableUnwrapped { .. } => false,
        }
    }

    pub fn tag_is_null(&self, tag_id: TagIdIntType) -> bool {
        match self {
            UnionLayout::NonRecursive(_)
            | UnionLayout::NonNullableUnwrapped(_)
            | UnionLayout::Recursive(_) => false,
            UnionLayout::NullableWrapped { nullable_id, .. } => *nullable_id == tag_id,
            UnionLayout::NullableUnwrapped { nullable_id, .. } => *nullable_id == (tag_id != 0),
        }
    }

    pub fn is_nullable(&self) -> bool {
        match self {
            UnionLayout::NonRecursive(_)
            | UnionLayout::Recursive(_)
            | UnionLayout::NonNullableUnwrapped { .. } => false,
            UnionLayout::NullableWrapped { .. } | UnionLayout::NullableUnwrapped { .. } => true,
        }
    }

    fn tags_alignment_bytes<I>(
        interner: &I,
        tags: &[&'a [Layout<'a>]],
        target_info: TargetInfo,
    ) -> u32
    where
        I: Interner<'a, Layout<'a>>,
    {
        tags.iter()
            .map(|field_layouts| {
                Layout::struct_no_name_order(field_layouts).alignment_bytes(interner, target_info)
            })
            .max()
            .unwrap_or(0)
    }

    pub fn allocation_alignment_bytes<I>(&self, interner: &I, target_info: TargetInfo) -> u32
    where
        I: Interner<'a, Layout<'a>>,
    {
        let allocation = match self {
            UnionLayout::NonRecursive(tags) => {
                Self::tags_alignment_bytes(interner, tags, target_info)
            }
            UnionLayout::Recursive(tags) => Self::tags_alignment_bytes(interner, tags, target_info),
            UnionLayout::NonNullableUnwrapped(field_layouts) => {
                Layout::struct_no_name_order(field_layouts).alignment_bytes(interner, target_info)
            }
            UnionLayout::NullableWrapped { other_tags, .. } => {
                Self::tags_alignment_bytes(interner, other_tags, target_info)
            }
            UnionLayout::NullableUnwrapped { other_fields, .. } => {
                Layout::struct_no_name_order(other_fields).alignment_bytes(interner, target_info)
            }
        };

        // because we store a refcount, the alignment must be at least the size of a pointer
        allocation.max(target_info.ptr_width() as u32)
    }

    /// Size of the data in memory, whether it's stack or heap (for non-null tag ids)
    pub fn data_size_and_alignment<I>(&self, interner: &I, target_info: TargetInfo) -> (u32, u32)
    where
        I: Interner<'a, Layout<'a>>,
    {
        let (data_width, data_align) =
            self.data_size_and_alignment_help_match(interner, target_info);

        if self.stores_tag_id_as_data(target_info) {
            use Discriminant::*;
            match self.discriminant() {
                U0 => (round_up_to_alignment(data_width, data_align), data_align),
                U1 | U8 => (
                    round_up_to_alignment(data_width + 1, data_align),
                    data_align,
                ),
                U16 => {
                    // first, round up the data so the tag id is well-aligned;
                    // then add the tag id width, and make sure the whole extends
                    // to the next alignment multiple
                    let tag_align = data_align.max(2);
                    let tag_width =
                        round_up_to_alignment(round_up_to_alignment(data_width, 2) + 2, tag_align);

                    (tag_width, tag_align)
                }
            }
        } else {
            (data_width, data_align)
        }
    }

    /// Size of the data before the tag_id, if it exists.
    /// Returns None if the tag_id is not stored as data in the layout.
    pub fn data_size_without_tag_id<I>(&self, interner: &I, target_info: TargetInfo) -> Option<u32>
    where
        I: Interner<'a, Layout<'a>>,
    {
        if !self.stores_tag_id_as_data(target_info) {
            return None;
        };

        Some(
            self.data_size_and_alignment_help_match(interner, target_info)
                .0,
        )
    }

    fn data_size_and_alignment_help_match<I>(
        &self,
        interner: &I,
        target_info: TargetInfo,
    ) -> (u32, u32)
    where
        I: Interner<'a, Layout<'a>>,
    {
        match self {
            Self::NonRecursive(tags) => {
                Layout::stack_size_and_alignment_slices(interner, tags, target_info)
            }
            Self::Recursive(tags) => {
                Layout::stack_size_and_alignment_slices(interner, tags, target_info)
            }
            Self::NonNullableUnwrapped(fields) => {
                Layout::stack_size_and_alignment_slices(interner, &[fields], target_info)
            }
            Self::NullableWrapped { other_tags, .. } => {
                Layout::stack_size_and_alignment_slices(interner, other_tags, target_info)
            }
            Self::NullableUnwrapped { other_fields, .. } => {
                Layout::stack_size_and_alignment_slices(interner, &[other_fields], target_info)
            }
        }
    }

    pub fn tag_id_offset<I>(&self, interner: &I, target_info: TargetInfo) -> Option<u32>
    where
        I: Interner<'a, Layout<'a>>,
    {
        match self {
            UnionLayout::NonRecursive(tags)
            | UnionLayout::Recursive(tags)
            | UnionLayout::NullableWrapped {
                other_tags: tags, ..
            } => Some(Self::tag_id_offset_help(interner, tags, target_info)),
            UnionLayout::NonNullableUnwrapped(_) | UnionLayout::NullableUnwrapped { .. } => None,
        }
    }

    fn tag_id_offset_help<I>(
        interner: &I,
        layouts: &[&[Layout<'a>]],
        target_info: TargetInfo,
    ) -> u32
    where
        I: Interner<'a, Layout<'a>>,
    {
        let (data_width, data_align) =
            Layout::stack_size_and_alignment_slices(interner, layouts, target_info);

        round_up_to_alignment(data_width, data_align)
    }

    /// Very important to use this when doing a memcpy!
    fn stack_size_without_alignment<I>(&self, interner: &I, target_info: TargetInfo) -> u32
    where
        I: Interner<'a, Layout<'a>>,
    {
        match self {
            UnionLayout::NonRecursive(_) => {
                let (width, align) = self.data_size_and_alignment(interner, target_info);
                round_up_to_alignment(width, align)
            }
            UnionLayout::Recursive(_)
            | UnionLayout::NonNullableUnwrapped(_)
            | UnionLayout::NullableWrapped { .. }
            | UnionLayout::NullableUnwrapped { .. } => target_info.ptr_width() as u32,
        }
    }
}

pub enum Discriminant {
    U0,
    U1,
    U8,
    U16,
}

impl Discriminant {
    pub const fn from_number_of_tags(tags: usize) -> Self {
        match tags {
            0 => Discriminant::U0,
            1 => Discriminant::U0,
            2 => Discriminant::U1,
            3..=255 => Discriminant::U8,
            256..=65_535 => Discriminant::U16,
            _ => panic!("discriminant too large"),
        }
    }

    pub const fn stack_size(&self) -> u32 {
        match self {
            Discriminant::U0 => 0,
            Discriminant::U1 => 1,
            Discriminant::U8 => 1,
            Discriminant::U16 => 2,
        }
    }

    pub const fn alignment_bytes(&self) -> u32 {
        self.stack_size()
    }
}

/// Custom type so we can get the numeric representation of a symbol in tests (so `#UserApp.3`
/// instead of `UserApp.foo`). The pretty name is not reliable when running many tests
/// concurrently. The number does not change and will give a reliable output.
struct SetElement<'a> {
    symbol: Symbol,
    layout: &'a [Layout<'a>],
}

impl std::fmt::Debug for SetElement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol_string = crate::ir::symbol_to_doc_string(self.symbol);

        write!(f, "( {}, {:?})", symbol_string, self.layout)
    }
}

impl std::fmt::Debug for LambdaSet<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct Helper<'a> {
            set: &'a [(Symbol, &'a [Layout<'a>])],
        }

        impl std::fmt::Debug for Helper<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let entries = self.set.iter().map(|x| SetElement {
                    symbol: x.0,
                    layout: x.1,
                });

                f.debug_list().entries(entries).finish()
            }
        }

        f.debug_struct("LambdaSet")
            .field("set", &Helper { set: self.set })
            .field("representation", &self.representation)
            .finish()
    }
}

/// Sometimes we can end up with lambdas of the same name and different captures in the same
/// lambda set, like `fun` having lambda set `[[thunk U64, thunk U8]]` due to the following program:
///
/// ```roc
/// capture : _ -> ({} -> Str)
/// capture = \val ->
///     thunk = \{} -> Num.toStr val
///     thunk
///
/// fun = \x ->
///     when x is
///         True -> capture 123u64
///         False -> capture 18u8
/// ```
///
/// By recording the captures layouts this lambda expects in its identifier, we can distinguish
/// between such differences when constructing closure capture data.
///
/// See also https://github.com/roc-lang/roc/issues/3336.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct CapturesNiche<'a>(&'a [Layout<'a>]);

impl CapturesNiche<'_> {
    pub fn no_niche() -> Self {
        Self(&[])
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct LambdaName<'a> {
    name: Symbol,
    captures_niche: CapturesNiche<'a>,
}

impl<'a> LambdaName<'a> {
    #[inline(always)]
    pub fn name(&self) -> Symbol {
        self.name
    }

    #[inline(always)]
    pub fn captures_niche(&self) -> CapturesNiche<'a> {
        self.captures_niche
    }

    #[inline(always)]
    pub fn no_captures(&self) -> bool {
        self.captures_niche.0.is_empty()
    }

    #[inline(always)]
    pub fn no_niche(name: Symbol) -> Self {
        Self {
            name,
            captures_niche: CapturesNiche::no_niche(),
        }
    }

    #[inline(always)]
    pub fn replace_name(&self, name: Symbol) -> Self {
        Self {
            name,
            captures_niche: self.captures_niche,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LambdaSet<'a> {
    /// collection of function names and their closure arguments
    set: &'a [(Symbol, &'a [Layout<'a>])],
    /// how the closure will be represented at runtime
    representation: Interned<Layout<'a>>,
}

#[derive(Debug)]
pub enum EnumDispatch {
    Bool,
    U8,
}

/// representation of the closure *for a particular function*
#[derive(Debug)]
pub enum ClosureRepresentation<'a> {
    /// The closure is represented as a union. Includes the tag ID!
    /// Each variant is a different function, and its payloads are the captures.
    Union {
        alphabetic_order_fields: &'a [Layout<'a>],
        closure_name: Symbol,
        tag_id: TagIdIntType,
        union_layout: UnionLayout<'a>,
    },
    /// The closure is one function, whose captures are represented as a struct.
    /// The layouts are sorted alphabetically by the identifier that is captured.
    ///
    /// We MUST sort these according to their stack size before code gen!
    AlphabeticOrderStruct(&'a [Layout<'a>]),
    /// The closure is one function that captures a single identifier, whose value is unwrapped.
    UnwrappedCapture(Layout<'a>),
    /// The closure dispatches to multiple functions, but none of them capture anything, so this is
    /// a boolean or integer flag.
    EnumDispatch(EnumDispatch),
}

/// How the closure should be seen when determining a call-by-name.
#[derive(Debug)]
pub enum ClosureCallOptions<'a> {
    /// This is an empty lambda set, dispatching is an error
    Void,
    /// One of a few capturing functions can be called to
    Union(UnionLayout<'a>),
    /// The closure is one function, whose captures are represented as a struct.
    Struct {
        field_layouts: &'a [Layout<'a>],
        field_order_hash: FieldOrderHash,
    },
    /// The closure is one function that captures a single identifier, whose value is unwrapped.
    UnwrappedCapture(Layout<'a>),
    /// The closure dispatches to multiple possible functions, none of which capture.
    EnumDispatch(EnumDispatch),
}

impl<'a> LambdaSet<'a> {
    pub fn runtime_representation<I>(&self, interner: &I) -> Layout<'a>
    where
        I: Interner<'a, Layout<'a>>,
    {
        *interner.get(self.representation)
    }

    /// Does the lambda set contain the given symbol?
    pub fn contains(&self, symbol: Symbol) -> bool {
        self.set.iter().any(|(s, _)| *s == symbol)
    }

    pub fn is_represented<I>(&self, interner: &I) -> Option<Layout<'a>>
    where
        I: Interner<'a, Layout<'a>>,
    {
        if self.has_unwrapped_capture_repr() {
            let repr = interner.get(self.representation);
            Some(*repr)
        } else if self.has_enum_dispatch_repr() {
            None
        } else {
            let repr = interner.get(self.representation);
            match repr {
                Layout::Struct {
                    field_layouts: &[], ..
                } => None,
                repr => Some(*repr),
            }
        }
    }

    pub fn iter_set(&self) -> impl ExactSizeIterator<Item = LambdaName<'a>> {
        self.set.iter().map(|(name, captures_layouts)| LambdaName {
            name: *name,
            captures_niche: CapturesNiche(captures_layouts),
        })
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.set.len()
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.set.is_empty()
    }

    pub fn layout_for_member_with_lambda_name<I>(
        &self,
        interner: &I,
        lambda_name: LambdaName,
    ) -> ClosureRepresentation<'a>
    where
        I: Interner<'a, Layout<'a>>,
    {
        debug_assert!(self.contains(lambda_name.name));

        let comparator = |other_name: Symbol, other_captures_layouts: &[Layout]| {
            other_name == lambda_name.name
                // Make sure all captures are equal
                && other_captures_layouts
                    .iter()
                    .eq(lambda_name.captures_niche.0)
        };

        self.layout_for_member(interner, comparator)
    }

    /// Finds an alias name for a possible-multimorphic lambda variant in the lambda set.
    pub fn find_lambda_name<I>(
        &self,
        interner: &I,
        function_symbol: Symbol,
        captures_layouts: &[Layout],
    ) -> LambdaName<'a>
    where
        I: Interner<'a, Layout<'a>>,
    {
        debug_assert!(self.contains(function_symbol), "function symbol not in set");

        let comparator = |other_name: Symbol, other_captures_layouts: &[Layout]| {
            other_name == function_symbol
                && other_captures_layouts.iter().zip(captures_layouts).all(
                    |(other_layout, layout)| {
                        self.capture_layouts_eq(interner, other_layout, layout)
                    },
                )
        };

        let (name, layouts) = self
            .set
            .iter()
            .find(|(name, layouts)| comparator(*name, layouts))
            .unwrap_or_else(|| {
                internal_error!(
                    "no lambda set found for ({:?}, {:#?}): {:#?}",
                    function_symbol,
                    captures_layouts,
                    self
                )
            });

        LambdaName {
            name: *name,
            captures_niche: CapturesNiche(layouts),
        }
    }

    /// Checks if two captured layouts are equivalent under the current lambda set.
    /// Resolves recursive pointers to the layout of the lambda set.
    fn capture_layouts_eq<I>(&self, interner: &I, left: &Layout, right: &Layout) -> bool
    where
        I: Interner<'a, Layout<'a>>,
    {
        if left == right {
            return true;
        }

        let left = if left == &Layout::RecursivePointer {
            let runtime_repr = self.runtime_representation(interner);
            debug_assert!(matches!(
                runtime_repr,
                Layout::Union(UnionLayout::Recursive(_) | UnionLayout::NullableUnwrapped { .. })
            ));
            Layout::LambdaSet(*self)
        } else {
            *left
        };

        let right = if right == &Layout::RecursivePointer {
            let runtime_repr = self.runtime_representation(interner);
            debug_assert!(matches!(
                runtime_repr,
                Layout::Union(UnionLayout::Recursive(_) | UnionLayout::NullableUnwrapped { .. })
            ));
            Layout::LambdaSet(*self)
        } else {
            *right
        };

        left == right
    }

    fn layout_for_member<I, F>(&self, interner: &I, comparator: F) -> ClosureRepresentation<'a>
    where
        I: Interner<'a, Layout<'a>>,
        F: Fn(Symbol, &[Layout]) -> bool,
    {
        let repr = interner.get(self.representation);

        if self.has_unwrapped_capture_repr() {
            // Only one function, that captures one identifier.
            return ClosureRepresentation::UnwrappedCapture(*repr);
        }

        match repr {
            Layout::Union(union) => {
                // here we rely on the fact that a union in a closure would be stored in a one-element record.
                // a closure representation that is itself union must be a of the shape `Closure1 ... | Closure2 ...`
                match union {
                    UnionLayout::NonRecursive(_) => {
                        // get the fields from the set, where they are sorted in alphabetic order
                        // (and not yet sorted by their alignment)
                        let (index, (name, fields)) = self
                            .set
                            .iter()
                            .enumerate()
                            .find(|(_, (s, layouts))| comparator(*s, layouts))
                            .unwrap();

                        let closure_name = *name;

                        ClosureRepresentation::Union {
                            tag_id: index as TagIdIntType,
                            alphabetic_order_fields: fields,
                            closure_name,
                            union_layout: *union,
                        }
                    }
                    UnionLayout::Recursive(_) => {
                        let (index, (name, fields)) = self
                            .set
                            .iter()
                            .enumerate()
                            .find(|(_, (s, layouts))| comparator(*s, layouts))
                            .unwrap();

                        let closure_name = *name;

                        ClosureRepresentation::Union {
                            tag_id: index as TagIdIntType,
                            alphabetic_order_fields: fields,
                            closure_name,
                            union_layout: *union,
                        }
                    }
                    UnionLayout::NullableUnwrapped {
                        nullable_id: _,
                        other_fields: _,
                    } => {
                        let (index, (name, fields)) = self
                            .set
                            .iter()
                            .enumerate()
                            .find(|(_, (s, layouts))| comparator(*s, layouts))
                            .unwrap();

                        let closure_name = *name;

                        ClosureRepresentation::Union {
                            tag_id: index as TagIdIntType,
                            alphabetic_order_fields: fields,
                            closure_name,
                            union_layout: *union,
                        }
                    }
                    UnionLayout::NonNullableUnwrapped(_) => todo!("recursive closures"),
                    UnionLayout::NullableWrapped {
                        nullable_id: _,
                        other_tags: _,
                    } => todo!("recursive closures"),
                }
            }
            Layout::Struct { .. } => {
                debug_assert_eq!(self.set.len(), 1);

                // get the fields from the set, where they are sorted in alphabetic order
                // (and not yet sorted by their alignment)
                let (_, fields) = self
                    .set
                    .iter()
                    .find(|(s, layouts)| comparator(*s, layouts))
                    .unwrap();

                ClosureRepresentation::AlphabeticOrderStruct(fields)
            }
            layout => {
                debug_assert!(self.has_enum_dispatch_repr(),);
                let enum_repr = match layout {
                    Layout::Builtin(Builtin::Bool) => EnumDispatch::Bool,
                    Layout::Builtin(Builtin::Int(IntWidth::U8)) => EnumDispatch::U8,
                    other => internal_error!("Invalid layout for enum dispatch: {:?}", other),
                };
                ClosureRepresentation::EnumDispatch(enum_repr)
            }
        }
    }

    fn has_unwrapped_capture_repr(&self) -> bool {
        self.set.len() == 1 && self.set[0].1.len() == 1
    }

    fn has_enum_dispatch_repr(&self) -> bool {
        self.set.len() > 1 && self.set.iter().all(|(_, captures)| captures.is_empty())
    }

    pub fn call_by_name_options<I>(&self, interner: &I) -> ClosureCallOptions<'a>
    where
        I: Interner<'a, Layout<'a>>,
    {
        let repr = interner.get(self.representation);

        if self.has_unwrapped_capture_repr() {
            return ClosureCallOptions::UnwrappedCapture(*repr);
        }

        match repr {
            Layout::Union(union_layout) => {
                if repr == &Layout::VOID {
                    debug_assert!(self.set.is_empty());
                    return ClosureCallOptions::Void;
                }
                ClosureCallOptions::Union(*union_layout)
            }
            Layout::Struct {
                field_layouts,
                field_order_hash,
            } => {
                debug_assert_eq!(self.set.len(), 1);
                ClosureCallOptions::Struct {
                    field_layouts,
                    field_order_hash: *field_order_hash,
                }
            }
            layout => {
                debug_assert!(self.has_enum_dispatch_repr());
                let enum_repr = match layout {
                    Layout::Builtin(Builtin::Bool) => EnumDispatch::Bool,
                    Layout::Builtin(Builtin::Int(IntWidth::U8)) => EnumDispatch::U8,
                    other => internal_error!("Invalid layout for enum dispatch: {:?}", other),
                };
                ClosureCallOptions::EnumDispatch(enum_repr)
            }
        }
    }

    pub fn extend_argument_list<I>(
        &self,
        arena: &'a Bump,
        interner: &I,
        argument_layouts: &'a [Layout<'a>],
    ) -> &'a [Layout<'a>]
    where
        I: Interner<'a, Layout<'a>>,
    {
        match self.call_by_name_options(interner) {
            ClosureCallOptions::Void => argument_layouts,
            ClosureCallOptions::Struct {
                field_layouts: &[], ..
            } => {
                // this function does not have anything in its closure, and the lambda set is a
                // singleton, so we pass no extra argument
                argument_layouts
            }
            ClosureCallOptions::Struct { .. }
            | ClosureCallOptions::Union(_)
            | ClosureCallOptions::UnwrappedCapture(_) => {
                let mut arguments = Vec::with_capacity_in(argument_layouts.len() + 1, arena);
                arguments.extend(argument_layouts);
                arguments.push(Layout::LambdaSet(*self));

                arguments.into_bump_slice()
            }
            ClosureCallOptions::EnumDispatch(_) => {
                // No captures, don't pass this along
                argument_layouts
            }
        }
    }

    pub fn from_var_pub(
        cache: &mut LayoutCache<'a>,
        arena: &'a Bump,
        subs: &Subs,
        closure_var: Variable,
        target_info: TargetInfo,
    ) -> Result<Self, LayoutProblem> {
        Self::from_var(cache, arena, subs, closure_var, target_info).value()
    }

    fn from_var(
        cache: &mut LayoutCache<'a>,
        arena: &'a Bump,
        subs: &Subs,
        closure_var: Variable,
        target_info: TargetInfo,
    ) -> Cacheable<Result<Self, LayoutProblem>> {
        // Ideally we would pass `env` in directly, but that currently causes problems later on
        // (in alias analysis) with recursive pointers not appearing under recursive layouts. So,
        // we have to clear the `seen` cache before building a lambda set layout.
        //
        // I think more generally, we need to address https://github.com/roc-lang/roc/issues/2466,
        // which should also resolve the issue here.
        let mut env = Env::from_components(cache, subs, arena, target_info);

        let Cacheable(result, criteria) = env.cached_or(closure_var, |env| {
            let Cacheable(result, criteria) = Self::from_var_help(env, closure_var);
            let result = result.map(Layout::LambdaSet);
            Cacheable(result, criteria)
        });

        match result {
            Ok(Layout::LambdaSet(lambda_set)) => Cacheable(Ok(lambda_set), criteria),
            Err(err) => Cacheable(Err(err), criteria),
            Ok(layout) => internal_error!("other layout found for lambda set: {:?}", layout),
        }
    }

    fn from_var_help(
        env: &mut Env<'a, '_>,
        closure_var: Variable,
    ) -> Cacheable<Result<Self, LayoutProblem>> {
        roc_tracing::debug!(var = ?closure_var, size = ?lambda_set_size(env.subs, closure_var), "building lambda set layout");

        match resolve_lambda_set(env.subs, closure_var) {
            ResolvedLambdaSet::Set(mut lambdas, opt_recursion_var) => {
                // sort the tags; make sure ordering stays intact!
                lambdas.sort_by_key(|(sym, _)| *sym);

                let mut set: Vec<(Symbol, &[Layout])> =
                    Vec::with_capacity_in(lambdas.len(), env.arena);
                let mut set_with_variables: std::vec::Vec<(&Symbol, &[Variable])> =
                    std::vec::Vec::with_capacity(lambdas.len());

                let mut last_function_symbol = None;
                let mut lambdas_it = lambdas.iter().peekable();

                let mut has_duplicate_lambda_names = false;
                while let Some((function_symbol, variables)) = lambdas_it.next() {
                    let mut arguments = Vec::with_capacity_in(variables.len(), env.arena);

                    if let Some(rec_var) = opt_recursion_var.into_variable() {
                        env.insert_seen(rec_var);
                    }

                    for var in variables {
                        // We determine cacheability of the lambda set based on the runtime
                        // representation, so here the criteria doesn't matter.
                        let mut criteria = CACHEABLE;
                        let arg = cached!(Layout::from_var(env, *var), criteria);
                        arguments.push(arg);
                    }

                    let arguments = arguments.into_bump_slice();

                    let is_multimorphic = match (last_function_symbol, lambdas_it.peek()) {
                        (None, None) => false,
                        (Some(sym), None) | (None, Some((sym, _))) => function_symbol == sym,
                        (Some(sym1), Some((sym2, _))) => {
                            function_symbol == sym1 || function_symbol == sym2
                        }
                    };

                    has_duplicate_lambda_names = has_duplicate_lambda_names || is_multimorphic;

                    set.push((*function_symbol, arguments));
                    set_with_variables.push((function_symbol, variables.as_slice()));

                    last_function_symbol = Some(function_symbol);
                }

                let (set, set_with_variables) = if has_duplicate_lambda_names {
                    // If we have a lambda set with duplicate names, then we sort first by name,
                    // and break ties by sorting on the layout. We need to do this again since the
                    // first sort would not have sorted on the layout.

                    // TODO: be more efficient, we can compute the permutation once and then apply
                    // it to both vectors.
                    let mut joined = set
                        .into_iter()
                        .zip(set_with_variables.into_iter())
                        .collect::<std::vec::Vec<_>>();
                    joined.sort_by(|(lam_and_captures1, _), (lam_and_captures2, _)| {
                        lam_and_captures1.cmp(lam_and_captures2)
                    });
                    // Remove duplicate lambda captures layouts unification can't see as
                    // duplicates, for example [[Thunk {a: Str}, Thunk [A Str]]], each of which are
                    // newtypes over the lambda layout `Thunk Str`.
                    joined.dedup_by_key(|((name, captures), _)| (*name, *captures));

                    let (set, set_with_variables): (std::vec::Vec<_>, std::vec::Vec<_>) =
                        joined.into_iter().unzip();

                    let set = Vec::from_iter_in(set, env.arena);

                    (set, set_with_variables)
                } else {
                    (set, set_with_variables)
                };

                let Cacheable(representation, criteria) = Self::make_representation(
                    env,
                    set_with_variables,
                    opt_recursion_var.into_variable(),
                );
                let representation = env.cache.interner.insert(env.arena.alloc(representation));

                Cacheable(
                    Ok(LambdaSet {
                        set: set.into_bump_slice(),
                        representation,
                    }),
                    criteria,
                )
            }
            ResolvedLambdaSet::Unbound => {
                // The lambda set is unbound which means it must be unused. Just give it the empty lambda set.
                // See also https://github.com/roc-lang/roc/issues/3163.
                cacheable(Ok(LambdaSet {
                    set: &[],
                    representation: env.cache.interner.insert(env.arena.alloc(Layout::UNIT)),
                }))
            }
        }
    }

    fn make_representation(
        env: &mut Env<'a, '_>,
        set: std::vec::Vec<(&Symbol, &[Variable])>,
        opt_rec_var: Option<Variable>,
    ) -> Cacheable<Layout<'a>> {
        let union_labels = UnsortedUnionLabels { tags: set };

        match opt_rec_var {
            Some(rec_var) => {
                let Cacheable(result, criteria) =
                    layout_from_recursive_union(env, rec_var, &union_labels);
                let result = result.expect("unable to create lambda set representation");
                Cacheable(result, criteria)
            }

            None => layout_from_non_recursive_union(env, &union_labels),
        }
    }

    pub fn stack_size<I>(&self, interner: &I, target_info: TargetInfo) -> u32
    where
        I: Interner<'a, Layout<'a>>,
    {
        interner
            .get(self.representation)
            .stack_size(interner, target_info)
    }
    pub fn contains_refcounted<I>(&self, interner: &I) -> bool
    where
        I: Interner<'a, Layout<'a>>,
    {
        interner
            .get(self.representation)
            .contains_refcounted(interner)
    }
    pub fn safe_to_memcpy<I>(&self, interner: &I) -> bool
    where
        I: Interner<'a, Layout<'a>>,
    {
        interner.get(self.representation).safe_to_memcpy(interner)
    }

    pub fn alignment_bytes<I>(&self, interner: &I, target_info: TargetInfo) -> u32
    where
        I: Interner<'a, Layout<'a>>,
    {
        interner
            .get(self.representation)
            .alignment_bytes(interner, target_info)
    }
}

enum ResolvedLambdaSet {
    Set(
        std::vec::Vec<(Symbol, std::vec::Vec<Variable>)>,
        OptVariable,
    ),
    /// TODO: figure out if this can happen in a correct program, or is the result of a bug in our
    /// compiler. See https://github.com/roc-lang/roc/issues/3163.
    Unbound,
}

fn resolve_lambda_set(subs: &Subs, mut var: Variable) -> ResolvedLambdaSet {
    let mut set = vec![];
    loop {
        match subs.get_content_without_compacting(var) {
            Content::LambdaSet(subs::LambdaSet {
                solved,
                recursion_var,
                unspecialized,
                ambient_function: _,
            }) => {
                debug_assert!(
                    unspecialized.is_empty(),
                    "unspecialized lambda sets left over during resolution: {:?}, {:?}",
                    roc_types::subs::SubsFmtContent(subs.get_content_without_compacting(var), subs),
                    subs.uls_of_var
                );
                roc_types::pretty_print::push_union(subs, solved, &mut set);
                return ResolvedLambdaSet::Set(set, *recursion_var);
            }
            Content::RecursionVar { structure, .. } => {
                var = *structure;
            }
            Content::FlexVar(_) => return ResolvedLambdaSet::Unbound,

            c => internal_error!("called with a non-lambda set {:?}", c),
        }
    }
}

/// Determines the "size" of a lambda set. Size roughly calculates how many nested lambda sets are
/// captured in a lambda set.
/// Size is calculated in three dimensions:
///   - the depth of the longest chain of nested lambda sets, including type constructors besides
///     lambda sets.
///   - the depth of the longest chain of nested lambda sets, excluding type constructors besides
///     lambda sets.
///   - the total number of lambda sets
/// The returned tuple consists of these statistics in order. A lambda set with no nested lambda
/// set captures, but perhaps with other captures, would have a size of (1, 1, 1).
///
/// Follows recursion variables until they are seen twice.
/// Returns (0, 0, 0) if the provided variable is not a lambda set.
fn lambda_set_size(subs: &Subs, var: Variable) -> (usize, usize, usize) {
    // NOTE: we must be very careful not to recurse on the stack.
    let mut max_depth_any_ctor = 0;
    let mut max_depth_only_lset = 0;
    let mut total = 0;

    let mut seen_rec_vars = roc_collections::VecSet::default();

    // Run a DFS. I think in general deeply nested lambda sets wind up looking like multi-leaf
    // trees, so I think running the depth first saves space.
    let mut stack = std::vec::Vec::with_capacity(4);
    stack.push((var, 0, 0));
    while let Some((var, depth_any, depth_lset)) = stack.pop() {
        match subs.get_content_without_compacting(var) {
            // The interesting case
            Content::LambdaSet(roc_types::subs::LambdaSet {
                solved,
                recursion_var,
                unspecialized: _,
                ambient_function: _,
            }) => {
                total += 1;

                let new_depth_any = depth_any + 1;
                let new_depth_lset = depth_lset + 1;
                max_depth_any_ctor = std::cmp::max(max_depth_any_ctor, new_depth_any);
                max_depth_only_lset = std::cmp::max(max_depth_only_lset, new_depth_lset);

                if let Some(rec_var) = recursion_var.into_variable() {
                    seen_rec_vars.insert(rec_var);
                }
                for (_, captures) in solved.iter_from_subs(subs) {
                    for capture in captures {
                        stack.push((*capture, new_depth_any, new_depth_lset));
                    }
                }
            }
            // The boring ones
            Content::RecursionVar {
                structure,
                opt_name: _,
            } => {
                if !seen_rec_vars.contains(&var) {
                    stack.push((*structure, depth_any + 1, depth_lset))
                }
            }
            Content::Alias(_, _, real_var, _) => {
                // For layout purposes, only the real_var matters.
                stack.push((*real_var, depth_any + 1, depth_lset));
            }
            Content::Structure(flat_type) => match flat_type {
                FlatType::Apply(_, args) => {
                    for var in subs.get_subs_slice(*args) {
                        stack.push((*var, depth_any + 1, depth_lset));
                    }
                }
                FlatType::Func(args, lset, ret) => {
                    for var in subs.get_subs_slice(*args) {
                        stack.push((*var, depth_any + 1, depth_lset));
                    }
                    stack.push((*lset, depth_any + 1, depth_lset));
                    stack.push((*ret, depth_any + 1, depth_lset));
                }
                FlatType::Record(fields, ext) => {
                    for var_index in fields.iter_variables() {
                        let var = subs[var_index];
                        stack.push((var, depth_any + 1, depth_lset));
                    }
                    stack.push((*ext, depth_any + 1, depth_lset));
                }
                FlatType::FunctionOrTagUnion(_, _, ext) => {
                    stack.push((*ext, depth_any + 1, depth_lset));
                }
                FlatType::TagUnion(tags, ext) => {
                    for (_, payloads) in tags.iter_from_subs(subs) {
                        for payload in payloads {
                            stack.push((*payload, depth_any + 1, depth_lset));
                        }
                    }
                    stack.push((*ext, depth_any + 1, depth_lset));
                }
                FlatType::RecursiveTagUnion(rec_var, tags, ext) => {
                    seen_rec_vars.insert(*rec_var);
                    for (_, payloads) in tags.iter_from_subs(subs) {
                        for payload in payloads {
                            stack.push((*payload, depth_any + 1, depth_lset));
                        }
                    }
                    stack.push((*ext, depth_any + 1, depth_lset));
                }
                FlatType::Erroneous(_) | FlatType::EmptyRecord | FlatType::EmptyTagUnion => {}
            },
            Content::FlexVar(_)
            | Content::RigidVar(_)
            | Content::FlexAbleVar(_, _)
            | Content::RigidAbleVar(_, _)
            | Content::RangedNumber(_)
            | Content::Error => {}
        }
    }
    (max_depth_any_ctor, max_depth_only_lset, total)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Builtin<'a> {
    Int(IntWidth),
    Float(FloatWidth),
    Bool,
    Decimal,
    Str,
    List(&'a Layout<'a>),
}

pub struct Env<'a, 'b> {
    target_info: TargetInfo,
    arena: &'a Bump,
    seen: Vec<'a, Variable>,
    subs: &'b Subs,
    cache: &'b mut LayoutCache<'a>,
}

impl<'a, 'b> Env<'a, 'b> {
    pub fn from_components(
        cache: &'b mut LayoutCache<'a>,
        subs: &'b Subs,
        arena: &'a Bump,
        target_info: TargetInfo,
    ) -> Self {
        Self {
            cache,
            subs,
            seen: Vec::new_in(arena),
            arena,
            target_info,
        }
    }

    fn is_seen(&self, var: Variable) -> bool {
        let var = self.subs.get_root_key_without_compacting(var);

        self.seen.iter().rev().any(|x| x == &var)
    }

    fn insert_seen(&mut self, var: Variable) {
        let var = self.subs.get_root_key_without_compacting(var);

        self.seen.push(var);
    }

    fn remove_seen(&mut self, var: Variable) -> bool {
        let var = self.subs.get_root_key_without_compacting(var);

        if let Some(index) = self.seen.iter().rposition(|x| x == &var) {
            self.seen.remove(index);
            true
        } else {
            false
        }
    }

    #[inline(always)]
    fn can_reuse_cached(&self, var: Variable, cache_metadata: CacheMeta) -> bool {
        let CacheMeta {
            has_recursive_structure,
        } = cache_metadata;
        if let Some(recursive_structure) = has_recursive_structure.into_variable() {
            if self.is_seen(recursive_structure) {
                // If the cached entry references a recursive structure that we're in the process
                // of visiting currently, we can't use the cached entry, and instead must
                // recalculate the nested layout, because the nested recursive structure will
                // likely turn into a recursive pointer now.
                //
                // For example, suppose we are constructing the layout of
                //
                // [A, B (List r)] as r
                //
                // and we have already constructed and cached the layout of `List r`, which would
                // be
                //
                // List (Recursive [Unit, List RecursivePointer])
                //
                // If we use the cached entry of `List r`, we would end up with the layout
                //
                // Recursive [Unit, (List (Recursive [Unit, List RecursivePointer]))]
                //                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ cached layout for `List r`
                //
                // but this is not correct; the canonical layout of `[A, B (List r)] as r` is
                //
                // Recursive [Unit, (List RecursivePointer)]
                roc_tracing::debug!(?var, "not reusing cached recursive structure");
                return false;
            }
        }
        true
    }
}

macro_rules! cached_or_impl {
    ($self:ident, $var:ident, $compute_layout:ident, $get:ident, $insert:ident, $stats:ident) => {{
        if let Some((result, metadata)) = $self.cache.$get($self.subs, $var) {
            // cache HIT
            inc_stat!($self.cache.$stats, hits);

            if $self.can_reuse_cached($var, metadata) {
                // Happy path - the cached layout can be reused, return it immediately.
                return Cacheable(result, metadata.to_criteria());
            } else {
                // Although we have a cached layout, we cannot readily reuse it at this time. We'll
                // need to recompute the layout, as done below.
                inc_stat!($self.cache.$stats, non_reusable);
            }
        } else {
            // cache MISS - compute the layout
            inc_stat!($self.cache.$stats, misses);
        }

        let Cacheable(result, criteria) = $compute_layout($self);

        if criteria.is_cacheable() {
            // The computed layout is cacheable; insert it.
            $self
                .cache
                .$insert($self.subs, $var, result, criteria.cache_metadata());
            inc_stat!($self.cache.$stats, insertions);
        } else {
            // The computed layout is not cacheable. We'll return it with the criteria that made it
            // non-cacheable.
            inc_stat!($self.cache.$stats, non_insertable);
            roc_tracing::debug!(?result, ?$var, "not caching");
        }

        Cacheable(result, criteria)
    }};
}

impl<'a, 'b> Env<'a, 'b> {
    #[inline(always)]
    fn cached_or(
        &mut self,
        var: Variable,
        compute_layout: impl FnOnce(&mut Env<'a, 'b>) -> Cacheable<LayoutResult<'a>>,
    ) -> Cacheable<LayoutResult<'a>> {
        if self.is_seen(var) {
            // Always return recursion pointers directly, NEVER cache them as naked!
            return Cacheable(Ok(Layout::RecursivePointer), NAKED_RECURSION_PTR);
        }

        cached_or_impl!(self, var, compute_layout, get, insert, stats)
    }

    #[inline(always)]
    fn cached_raw_function_or(
        &mut self,
        var: Variable,
        compute_layout: impl FnOnce(&mut Env<'a, 'b>) -> Cacheable<RawFunctionLayoutResult<'a>>,
    ) -> Cacheable<RawFunctionLayoutResult<'a>> {
        cached_or_impl!(
            self,
            var,
            compute_layout,
            get_raw_function,
            insert_raw_function,
            raw_function_stats
        )
    }
}

pub const fn round_up_to_alignment(width: u32, alignment: u32) -> u32 {
    match alignment {
        0 => width,
        1 => width,
        _ => {
            if width % alignment > 0 {
                width + alignment - (width % alignment)
            } else {
                width
            }
        }
    }
}

#[inline(always)]
pub fn is_unresolved_var(subs: &Subs, var: Variable) -> bool {
    use Content::*;
    let content = subs.get_content_without_compacting(var);
    matches!(
        content,
        FlexVar(..) | RigidVar(..) | FlexAbleVar(..) | RigidAbleVar(..),
    )
}

#[inline(always)]
pub fn is_any_float_range(subs: &Subs, var: Variable) -> bool {
    use {Content::*, NumericRange::*};
    let content = subs.get_content_without_compacting(var);
    matches!(
        content,
        RangedNumber(NumAtLeastEitherSign(..) | NumAtLeastSigned(..)),
    )
}

impl<'a> Layout<'a> {
    pub const VOID: Self = Layout::Union(UnionLayout::NonRecursive(&[]));
    pub const UNIT: Self = Layout::Struct {
        field_layouts: &[],
        field_order_hash: FieldOrderHash::ZERO_FIELD_HASH,
    };

    fn new_help<'b>(
        env: &mut Env<'a, 'b>,
        _var: Variable,
        content: Content,
    ) -> Cacheable<LayoutResult<'a>> {
        use roc_types::subs::Content::*;
        match content {
            FlexVar(_) | RigidVar(_) => {
                roc_debug_flags::dbg_do!(roc_debug_flags::ROC_NO_UNBOUND_LAYOUT, {
                    return cacheable(Err(LayoutProblem::UnresolvedTypeVar(_var)));
                });

                // If we encounter an unbound type var (e.g. `*` or `a`)
                // then it's zero-sized; In the future we may drop this argument
                // completely, but for now we represent it with the empty tag union
                cacheable(Ok(Layout::VOID))
            }
            FlexAbleVar(_, _) | RigidAbleVar(_, _) => todo_abilities!("Not reachable yet"),
            RecursionVar { structure, .. } => {
                let structure_content = env.subs.get_content_without_compacting(structure);
                Self::new_help(env, structure, *structure_content)
            }
            LambdaSet(lset) => layout_from_lambda_set(env, lset),
            Structure(flat_type) => layout_from_flat_type(env, flat_type),

            Alias(symbol, _args, actual_var, _) => {
                if let Some(int_width) = IntWidth::try_from_symbol(symbol) {
                    return cacheable(Ok(Layout::Builtin(Builtin::Int(int_width))));
                }

                if let Some(float_width) = FloatWidth::try_from_symbol(symbol) {
                    return cacheable(Ok(Layout::Builtin(Builtin::Float(float_width))));
                }

                match symbol {
                    Symbol::NUM_DECIMAL => return cacheable(Ok(Layout::Builtin(Builtin::Decimal))),

                    Symbol::NUM_NAT | Symbol::NUM_NATURAL => {
                        return cacheable(Ok(Layout::usize(env.target_info)))
                    }

                    Symbol::NUM_NUM | Symbol::NUM_INT | Symbol::NUM_INTEGER
                        if is_unresolved_var(env.subs, actual_var) =>
                    {
                        // default to i64
                        return cacheable(Ok(Layout::i64()));
                    }

                    Symbol::NUM_FRAC | Symbol::NUM_FLOATINGPOINT
                        if is_unresolved_var(env.subs, actual_var)
                            || is_any_float_range(env.subs, actual_var) =>
                    {
                        // default to f64
                        return cacheable(Ok(Layout::f64()));
                    }

                    _ => Self::from_var(env, actual_var),
                }
            }

            RangedNumber(range) => Self::layout_from_ranged_number(env, range),

            Error => cacheable(Err(LayoutProblem::Erroneous)),
        }
    }

    fn layout_from_ranged_number(
        env: &mut Env<'a, '_>,
        range: NumericRange,
    ) -> Cacheable<LayoutResult<'a>> {
        use roc_types::num::IntLitWidth;

        // If we chose the default int layout then the real var might have been `Num *`, or
        // similar. In this case fix-up width if we need to. Choose I64 if the range says
        // that the number will fit, otherwise choose the next-largest number layout.
        //
        // We don't pass the range down because `RangedNumber`s are somewhat rare, they only
        // appear due to number literals, so no need to increase parameter list sizes.
        let num_layout = match range {
            NumericRange::IntAtLeastSigned(w) | NumericRange::NumAtLeastSigned(w) => {
                [IntLitWidth::I64, IntLitWidth::I128]
                    .iter()
                    .find(|candidate| candidate.is_superset(&w, true))
                    .expect("if number doesn't fit, should have been a type error")
            }
            NumericRange::IntAtLeastEitherSign(w) | NumericRange::NumAtLeastEitherSign(w) => [
                IntLitWidth::I64,
                IntLitWidth::U64,
                IntLitWidth::I128,
                IntLitWidth::U128,
            ]
            .iter()
            .find(|candidate| candidate.is_superset(&w, false))
            .expect("if number doesn't fit, should have been a type error"),
        };
        cacheable(Ok(Layout::int_literal_width_to_int(
            *num_layout,
            env.target_info,
        )))
    }

    /// Returns Err(()) if given an error, or Ok(Layout) if given a non-erroneous Structure.
    /// Panics if given a FlexVar or RigidVar, since those should have been
    /// monomorphized away already!
    fn from_var(env: &mut Env<'a, '_>, var: Variable) -> Cacheable<LayoutResult<'a>> {
        env.cached_or(var, |env| {
            let content = env.subs.get_content_without_compacting(var);
            Self::new_help(env, var, *content)
        })
    }

    pub fn safe_to_memcpy<I>(&self, interner: &I) -> bool
    where
        I: Interner<'a, Layout<'a>>,
    {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.safe_to_memcpy(),
            Struct { field_layouts, .. } => field_layouts
                .iter()
                .all(|field_layout| field_layout.safe_to_memcpy(interner)),
            Union(variant) => {
                use UnionLayout::*;

                match variant {
                    NonRecursive(tags) => tags.iter().all(|tag_layout| {
                        tag_layout
                            .iter()
                            .all(|field| field.safe_to_memcpy(interner))
                    }),
                    Recursive(_)
                    | NullableWrapped { .. }
                    | NullableUnwrapped { .. }
                    | NonNullableUnwrapped(_) => {
                        // a recursive union will always contain a pointer, and is thus not safe to memcpy
                        false
                    }
                }
            }
            LambdaSet(lambda_set) => lambda_set
                .runtime_representation(interner)
                .safe_to_memcpy(interner),
            Boxed(_) | RecursivePointer => {
                // We cannot memcpy pointers, because then we would have the same pointer in multiple places!
                false
            }
        }
    }

    pub fn is_dropped_because_empty(&self) -> bool {
        // For this calculation, we don't need an accurate
        // stack size, we just need to know whether it's zero,
        // so it's fine to use a pointer size of 1.
        false // TODO this should use is_zero_sized once doing so doesn't break things!
    }

    /// Like stack_size, but doesn't require target info because
    /// whether something is zero sized is not target-dependent.
    #[allow(dead_code)]
    fn is_zero_sized(&self) -> bool {
        match self {
            // There are no zero-sized builtins
            Layout::Builtin(_) => false,
            // Functions are never zero-sized
            Layout::LambdaSet(_) => false,
            // Empty structs, or structs with all zero-sized fields, are zero-sized
            Layout::Struct { field_layouts, .. } => field_layouts.iter().all(Self::is_zero_sized),
            // A Box that points to nothing should be unwrapped
            Layout::Boxed(content) => content.is_zero_sized(),
            Layout::Union(union_layout) => match union_layout {
                UnionLayout::NonRecursive(tags)
                | UnionLayout::Recursive(tags)
                | UnionLayout::NullableWrapped {
                    other_tags: tags, ..
                } => tags
                    .iter()
                    .all(|payloads| payloads.iter().all(Self::is_zero_sized)),
                UnionLayout::NonNullableUnwrapped(tags)
                | UnionLayout::NullableUnwrapped {
                    other_fields: tags, ..
                } => tags.iter().all(Self::is_zero_sized),
            },
            // Recursive pointers are considered zero-sized because
            // if you have a recursive data structure where everything
            // else but the recutsive pointer is zero-sized, then
            // the whole thing is unnecessary at runtime and should
            // be zero-sized.
            Layout::RecursivePointer => true,
        }
    }

    pub fn is_passed_by_reference<I>(&self, interner: &I, target_info: TargetInfo) -> bool
    where
        I: Interner<'a, Layout<'a>>,
    {
        match self {
            Layout::Builtin(builtin) => {
                use Builtin::*;

                match target_info.ptr_width() {
                    PtrWidth::Bytes4 => {
                        // more things fit into a register
                        false
                    }
                    PtrWidth::Bytes8 => {
                        // currently, only Str is passed by-reference internally
                        matches!(builtin, Str)
                    }
                }
            }
            Layout::Union(UnionLayout::NonRecursive(_)) => true,
            Layout::LambdaSet(lambda_set) => lambda_set
                .runtime_representation(interner)
                .is_passed_by_reference(interner, target_info),
            _ => false,
        }
    }

    pub fn stack_size<I>(&self, interner: &I, target_info: TargetInfo) -> u32
    where
        I: Interner<'a, Layout<'a>>,
    {
        let width = self.stack_size_without_alignment(interner, target_info);
        let alignment = self.alignment_bytes(interner, target_info);

        round_up_to_alignment(width, alignment)
    }

    pub fn stack_size_and_alignment<I>(&self, interner: &I, target_info: TargetInfo) -> (u32, u32)
    where
        I: Interner<'a, Layout<'a>>,
    {
        let width = self.stack_size_without_alignment(interner, target_info);
        let alignment = self.alignment_bytes(interner, target_info);

        let size = round_up_to_alignment(width, alignment);
        (size, alignment)
    }

    /// Very important to use this when doing a memcpy!
    pub fn stack_size_without_alignment<I>(&self, interner: &I, target_info: TargetInfo) -> u32
    where
        I: Interner<'a, Layout<'a>>,
    {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.stack_size(target_info),
            Struct { field_layouts, .. } => {
                let mut sum = 0;

                for field_layout in *field_layouts {
                    sum += field_layout.stack_size(interner, target_info);
                }

                sum
            }
            Union(variant) => variant.stack_size_without_alignment(interner, target_info),
            LambdaSet(lambda_set) => lambda_set
                .runtime_representation(interner)
                .stack_size_without_alignment(interner, target_info),
            RecursivePointer => target_info.ptr_width() as u32,
            Boxed(_) => target_info.ptr_width() as u32,
        }
    }

    pub fn alignment_bytes<I>(&self, interner: &I, target_info: TargetInfo) -> u32
    where
        I: Interner<'a, Layout<'a>>,
    {
        match self {
            Layout::Struct { field_layouts, .. } => field_layouts
                .iter()
                .map(|x| x.alignment_bytes(interner, target_info))
                .max()
                .unwrap_or(0),

            Layout::Union(variant) => {
                use UnionLayout::*;

                match variant {
                    NonRecursive(tags) => {
                        let max_alignment = tags
                            .iter()
                            .flat_map(|layouts| {
                                layouts
                                    .iter()
                                    .map(|layout| layout.alignment_bytes(interner, target_info))
                            })
                            .max();

                        let discriminant = variant.discriminant();
                        match max_alignment {
                            Some(align) => round_up_to_alignment(
                                align.max(discriminant.alignment_bytes()),
                                discriminant.alignment_bytes(),
                            ),
                            None => {
                                // none of the tags had any payload, but the tag id still contains information
                                discriminant.alignment_bytes()
                            }
                        }
                    }
                    Recursive(_)
                    | NullableWrapped { .. }
                    | NullableUnwrapped { .. }
                    | NonNullableUnwrapped(_) => target_info.ptr_width() as u32,
                }
            }
            Layout::LambdaSet(lambda_set) => lambda_set
                .runtime_representation(interner)
                .alignment_bytes(interner, target_info),
            Layout::Builtin(builtin) => builtin.alignment_bytes(target_info),
            Layout::RecursivePointer => target_info.ptr_width() as u32,
            Layout::Boxed(_) => target_info.ptr_width() as u32,
        }
    }

    pub fn allocation_alignment_bytes<I>(&self, interner: &I, target_info: TargetInfo) -> u32
    where
        I: Interner<'a, Layout<'a>>,
    {
        let ptr_width = target_info.ptr_width() as u32;

        match self {
            Layout::Builtin(builtin) => builtin.allocation_alignment_bytes(interner, target_info),
            Layout::Struct { .. } => self.alignment_bytes(interner, target_info).max(ptr_width),
            Layout::Union(union_layout) => {
                union_layout.allocation_alignment_bytes(interner, target_info)
            }
            Layout::LambdaSet(lambda_set) => lambda_set
                .runtime_representation(interner)
                .allocation_alignment_bytes(interner, target_info),
            Layout::RecursivePointer => unreachable!("should be looked up to get an actual layout"),
            Layout::Boxed(inner) => inner.allocation_alignment_bytes(interner, target_info),
        }
    }

    pub fn stack_size_and_alignment_slices<I>(
        interner: &I,
        slices: &[&[Self]],
        target_info: TargetInfo,
    ) -> (u32, u32)
    where
        I: Interner<'a, Layout<'a>>,
    {
        let mut data_align = 1;
        let mut data_width = 0;

        for tag in slices {
            let mut total = 0;
            for layout in tag.iter() {
                let (stack_size, alignment) =
                    layout.stack_size_and_alignment(interner, target_info);
                total += stack_size;
                data_align = data_align.max(alignment);
            }

            data_width = data_width.max(total);
        }

        data_width = round_up_to_alignment(data_width, data_align);

        (data_width, data_align)
    }

    pub fn is_refcounted(&self) -> bool {
        use self::Builtin::*;
        use Layout::*;

        match self {
            Union(UnionLayout::NonRecursive(_)) => false,

            Union(_) => true,

            RecursivePointer => true,

            Builtin(List(_)) | Builtin(Str) => true,

            _ => false,
        }
    }

    /// Even if a value (say, a record) is not itself reference counted,
    /// it may contains values/fields that are. Therefore when this record
    /// goes out of scope, the refcount on those values/fields must  be decremented.
    pub fn contains_refcounted<I>(&self, interner: &I) -> bool
    where
        I: Interner<'a, Layout<'a>>,
    {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.is_refcounted(),
            Struct { field_layouts, .. } => field_layouts
                .iter()
                .any(|f| f.contains_refcounted(interner)),
            Union(variant) => {
                use UnionLayout::*;

                match variant {
                    NonRecursive(fields) => fields
                        .iter()
                        .flat_map(|ls| ls.iter())
                        .any(|f| f.contains_refcounted(interner)),
                    Recursive(_)
                    | NullableWrapped { .. }
                    | NullableUnwrapped { .. }
                    | NonNullableUnwrapped(_) => true,
                }
            }
            LambdaSet(lambda_set) => lambda_set
                .runtime_representation(interner)
                .contains_refcounted(interner),
            RecursivePointer => true,
            Boxed(_) => true,
        }
    }

    pub fn to_doc<'b, D, A, I>(
        self,
        alloc: &'b D,
        interner: &I,
        parens: Parens,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
        I: Interner<'a, Layout<'a>>,
    {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.to_doc(alloc, interner, parens),
            Struct { field_layouts, .. } => {
                let fields_doc = field_layouts
                    .iter()
                    .map(|x| x.to_doc(alloc, interner, parens));

                alloc
                    .text("{")
                    .append(alloc.intersperse(fields_doc, ", "))
                    .append(alloc.text("}"))
            }
            Union(union_layout) => union_layout.to_doc(alloc, interner, parens),
            LambdaSet(lambda_set) => lambda_set
                .runtime_representation(interner)
                .to_doc(alloc, interner, parens),
            RecursivePointer => alloc.text("*self"),
            Boxed(inner) => alloc
                .text("Boxed(")
                .append(inner.to_doc(alloc, interner, parens))
                .append(")"),
        }
    }

    /// Used to build a `Layout::Struct` where the field name order is irrelevant.
    pub fn struct_no_name_order(field_layouts: &'a [Layout]) -> Self {
        if field_layouts.is_empty() {
            Self::UNIT
        } else {
            Self::Struct {
                field_layouts,
                field_order_hash: FieldOrderHash::IRRELEVANT_NON_ZERO_FIELD_HASH,
            }
        }
    }

    pub fn runtime_representation<I>(&self, interner: &I) -> Self
    where
        I: Interner<'a, Layout<'a>>,
    {
        match self {
            Layout::LambdaSet(lambda_set) => lambda_set.runtime_representation(interner),
            other => *other,
        }
    }
}

impl<'a> Layout<'a> {
    pub fn int_width(width: IntWidth) -> Layout<'a> {
        Layout::Builtin(Builtin::Int(width))
    }

    pub fn float_width(width: FloatWidth) -> Layout<'a> {
        Layout::Builtin(Builtin::Float(width))
    }

    pub fn f64() -> Layout<'a> {
        Layout::Builtin(Builtin::Float(FloatWidth::F64))
    }

    pub fn f32() -> Layout<'a> {
        Layout::Builtin(Builtin::Float(FloatWidth::F32))
    }

    pub fn usize(target_info: TargetInfo) -> Layout<'a> {
        match target_info.ptr_width() {
            roc_target::PtrWidth::Bytes4 => Self::u32(),
            roc_target::PtrWidth::Bytes8 => Self::u64(),
        }
    }

    pub fn isize(target_info: TargetInfo) -> Layout<'a> {
        match target_info.ptr_width() {
            roc_target::PtrWidth::Bytes4 => Self::i32(),
            roc_target::PtrWidth::Bytes8 => Self::i64(),
        }
    }

    pub fn bool() -> Layout<'a> {
        Layout::Builtin(Builtin::Bool)
    }

    pub const fn u8() -> Layout<'a> {
        Layout::Builtin(Builtin::Int(IntWidth::U8))
    }

    pub fn u16() -> Layout<'a> {
        Layout::Builtin(Builtin::Int(IntWidth::U16))
    }

    pub fn u32() -> Layout<'a> {
        Layout::Builtin(Builtin::Int(IntWidth::U32))
    }

    pub fn u64() -> Layout<'a> {
        Layout::Builtin(Builtin::Int(IntWidth::U64))
    }

    pub fn u128() -> Layout<'a> {
        Layout::Builtin(Builtin::Int(IntWidth::U128))
    }

    pub fn i8() -> Layout<'a> {
        Layout::Builtin(Builtin::Int(IntWidth::I8))
    }

    pub fn i16() -> Layout<'a> {
        Layout::Builtin(Builtin::Int(IntWidth::I16))
    }

    pub fn i32() -> Layout<'a> {
        Layout::Builtin(Builtin::Int(IntWidth::I32))
    }

    pub fn i64() -> Layout<'a> {
        Layout::Builtin(Builtin::Int(IntWidth::I64))
    }

    pub fn i128() -> Layout<'a> {
        Layout::Builtin(Builtin::Int(IntWidth::I128))
    }

    pub fn default_integer() -> Layout<'a> {
        Layout::i64()
    }

    pub fn default_float() -> Layout<'a> {
        Layout::f64()
    }

    pub fn int_literal_width_to_int(
        width: roc_types::num::IntLitWidth,
        target_info: TargetInfo,
    ) -> Layout<'a> {
        use roc_types::num::IntLitWidth::*;
        match width {
            U8 => Layout::u8(),
            U16 => Layout::u16(),
            U32 => Layout::u32(),
            U64 => Layout::u64(),
            U128 => Layout::u128(),
            I8 => Layout::i8(),
            I16 => Layout::i16(),
            I32 => Layout::i32(),
            I64 => Layout::i64(),
            I128 => Layout::i128(),
            Nat => Layout::usize(target_info),
            // f32 int literal bounded by +/- 2^24, so fit it into an i32
            F32 => Layout::i32(),
            // f64 int literal bounded by +/- 2^53, so fit it into an i32
            F64 => Layout::i64(),
            // dec int literal bounded by i128, so fit it into an i128
            Dec => Layout::i128(),
        }
    }
}

impl<'a> Builtin<'a> {
    const I1_SIZE: u32 = std::mem::size_of::<bool>() as u32;
    const DECIMAL_SIZE: u32 = std::mem::size_of::<i128>() as u32;

    /// Number of machine words in an empty one of these
    pub const STR_WORDS: u32 = 3;
    pub const LIST_WORDS: u32 = 3;

    /// Layout of collection wrapper for List, Str, Dict, and Set - a struct of (pointer, length, capacity).
    pub const WRAPPER_PTR: u32 = 0;
    pub const WRAPPER_LEN: u32 = 1;
    pub const WRAPPER_CAPACITY: u32 = 2;

    pub fn stack_size(&self, target_info: TargetInfo) -> u32 {
        use Builtin::*;

        let ptr_width = target_info.ptr_width() as u32;

        match self {
            Int(int) => int.stack_size(),
            Float(float) => float.stack_size(),
            Bool => Builtin::I1_SIZE,
            Decimal => Builtin::DECIMAL_SIZE,
            Str => Builtin::STR_WORDS * ptr_width,
            List(_) => Builtin::LIST_WORDS * ptr_width,
        }
    }

    pub fn alignment_bytes(&self, target_info: TargetInfo) -> u32 {
        use std::mem::align_of;
        use Builtin::*;

        let ptr_width = target_info.ptr_width() as u32;

        // for our data structures, what counts is the alignment of the `( ptr, len )` tuple, and
        // since both of those are one pointer size, the alignment of that structure is a pointer
        // size
        match self {
            Int(int_width) => int_width.alignment_bytes(target_info),
            Float(float_width) => float_width.alignment_bytes(target_info),
            Bool => align_of::<bool>() as u32,
            Decimal => IntWidth::I128.alignment_bytes(target_info),
            // we often treat these as i128 (64-bit systems)
            // or i64 (32-bit systems).
            //
            // In webassembly, For that to be safe
            // they must be aligned to allow such access
            List(_) => ptr_width,
            Str => ptr_width,
        }
    }

    pub fn safe_to_memcpy(&self) -> bool {
        use Builtin::*;

        match self {
            Int(_) | Float(_) | Bool | Decimal => true,

            Str | List(_) => false,
        }
    }

    // Question: does is_refcounted exactly correspond with the "safe to memcpy" property?
    pub fn is_refcounted(&self) -> bool {
        use Builtin::*;

        match self {
            Int(_) | Float(_) | Bool | Decimal => false,
            List(_) => true,
            Str => true,
        }
    }

    pub fn to_doc<'b, D, A, I>(
        self,
        alloc: &'b D,
        interner: &I,
        _parens: Parens,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
        I: Interner<'a, Layout<'a>>,
    {
        use Builtin::*;

        match self {
            Int(int_width) => {
                use IntWidth::*;

                match int_width {
                    I128 => alloc.text("I128"),
                    I64 => alloc.text("I64"),
                    I32 => alloc.text("I32"),
                    I16 => alloc.text("I16"),
                    I8 => alloc.text("I8"),
                    U128 => alloc.text("U128"),
                    U64 => alloc.text("U64"),
                    U32 => alloc.text("U32"),
                    U16 => alloc.text("U16"),
                    U8 => alloc.text("U8"),
                }
            }

            Float(float_width) => {
                use FloatWidth::*;

                match float_width {
                    F128 => alloc.text("Float128"),
                    F64 => alloc.text("Float64"),
                    F32 => alloc.text("Float32"),
                }
            }

            Bool => alloc.text("Int1"),
            Decimal => alloc.text("Decimal"),

            Str => alloc.text("Str"),
            List(layout) => {
                alloc
                    .text("List ")
                    .append(layout.to_doc(alloc, interner, Parens::InTypeParam))
            }
        }
    }

    pub fn allocation_alignment_bytes<I>(&self, interner: &I, target_info: TargetInfo) -> u32
    where
        I: Interner<'a, Layout<'a>>,
    {
        let ptr_width = target_info.ptr_width() as u32;

        let allocation = match self {
            Builtin::Str => ptr_width,
            Builtin::List(e) => e.alignment_bytes(interner, target_info).max(ptr_width),
            // The following are usually not heap-allocated, but they might be when inside a Box.
            Builtin::Int(int_width) => int_width.alignment_bytes(target_info).max(ptr_width),
            Builtin::Float(float_width) => float_width.alignment_bytes(target_info).max(ptr_width),
            Builtin::Bool => (core::mem::align_of::<bool>() as u32).max(ptr_width),
            Builtin::Decimal => IntWidth::I128.alignment_bytes(target_info).max(ptr_width),
        };

        allocation.max(ptr_width)
    }
}

fn layout_from_lambda_set<'a>(
    env: &mut Env<'a, '_>,
    lset: subs::LambdaSet,
) -> Cacheable<LayoutResult<'a>> {
    // Lambda set is just a tag union from the layout's perspective.
    let subs::LambdaSet {
        solved,
        recursion_var,
        unspecialized,
        ambient_function: _,
    } = lset;

    if !unspecialized.is_empty() {
        internal_error!(
            "unspecialized lambda sets remain during layout generation for {:?}",
            roc_types::subs::SubsFmtContent(&Content::LambdaSet(lset), env.subs)
        );
    }

    match recursion_var.into_variable() {
        None => {
            let labels = solved.unsorted_lambdas(env.subs);
            layout_from_non_recursive_union(env, &labels).map(Ok)
        }
        Some(rec_var) => {
            let labels = solved.unsorted_lambdas(env.subs);
            layout_from_recursive_union(env, rec_var, &labels)
        }
    }
}

fn layout_from_flat_type<'a>(
    env: &mut Env<'a, '_>,
    flat_type: FlatType,
) -> Cacheable<LayoutResult<'a>> {
    use roc_types::subs::FlatType::*;

    let arena = env.arena;
    let subs = env.subs;
    let target_info = env.target_info;

    match flat_type {
        Apply(symbol, args) => {
            let args = Vec::from_iter_in(args.into_iter().map(|index| subs[index]), arena);

            match symbol {
                // Ints
                Symbol::NUM_NAT => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::usize(env.target_info)))
                }

                Symbol::NUM_I128 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::i128()))
                }
                Symbol::NUM_I64 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::i64()))
                }
                Symbol::NUM_I32 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::i32()))
                }
                Symbol::NUM_I16 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::i16()))
                }
                Symbol::NUM_I8 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::i8()))
                }

                Symbol::NUM_U128 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::u128()))
                }
                Symbol::NUM_U64 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::u64()))
                }
                Symbol::NUM_U32 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::u32()))
                }
                Symbol::NUM_U16 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::u16()))
                }
                Symbol::NUM_U8 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::u8()))
                }

                // Floats
                Symbol::NUM_DEC => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::Builtin(Builtin::Decimal)))
                }
                Symbol::NUM_F64 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::f64()))
                }
                Symbol::NUM_F32 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::f32()))
                }

                Symbol::NUM_NUM => {
                    // Num.Num should only ever have 1 argument, e.g. Num.Num Int.Integer
                    debug_assert_eq!(args.len(), 1);

                    let var = args[0];
                    let content = subs.get_content_without_compacting(var);

                    layout_from_num_content(content, target_info)
                }

                Symbol::STR_STR => cacheable(Ok(Layout::Builtin(Builtin::Str))),
                Symbol::LIST_LIST => list_layout_from_elem(env, args[0]),
                Symbol::BOX_BOX_TYPE => {
                    // Num.Num should only ever have 1 argument, e.g. Num.Num Int.Integer
                    debug_assert_eq!(args.len(), 1);

                    let mut criteria = CACHEABLE;

                    let inner_var = args[0];
                    let inner_layout = cached!(Layout::from_var(env, inner_var), criteria);

                    Cacheable(Ok(Layout::Boxed(env.arena.alloc(inner_layout))), criteria)
                }
                _ => {
                    panic!(
                        "TODO layout_from_flat_type for Apply({:?}, {:?})",
                        symbol, args
                    );
                }
            }
        }
        Func(_, closure_var, _) => {
            if env.is_seen(closure_var) {
                Cacheable(Ok(Layout::RecursivePointer), NAKED_RECURSION_PTR)
            } else {
                let mut criteria = CACHEABLE;

                let lambda_set = cached!(
                    LambdaSet::from_var(
                        env.cache,
                        env.arena,
                        env.subs,
                        closure_var,
                        env.target_info,
                    ),
                    criteria
                );

                Cacheable(Ok(Layout::LambdaSet(lambda_set)), criteria)
            }
        }
        Record(fields, ext_var) => {
            let mut criteria = CACHEABLE;

            // extract any values from the ext_var
            let mut sortables = Vec::with_capacity_in(fields.len(), arena);
            let it = match fields.unsorted_iterator(subs, ext_var) {
                Ok(it) => it,
                Err(RecordFieldsError) => {
                    return Cacheable(Err(LayoutProblem::Erroneous), criteria)
                }
            };

            for (label, field) in it {
                match field {
                    RecordField::Required(field_var) | RecordField::Demanded(field_var) => {
                        sortables
                            .push((label, cached!(Layout::from_var(env, field_var), criteria)));
                    }
                    RecordField::Optional(_) | RecordField::RigidOptional(_) => {
                        // drop optional fields
                    }
                }
            }

            sortables.sort_by(|(label1, layout1), (label2, layout2)| {
                cmp_fields(
                    &env.cache.interner,
                    label1,
                    layout1,
                    label2,
                    layout2,
                    target_info,
                )
            });

            let ordered_field_names =
                Vec::from_iter_in(sortables.iter().map(|(label, _)| *label), arena);
            let field_order_hash =
                FieldOrderHash::from_ordered_fields(ordered_field_names.as_slice());

            let result = if sortables.len() == 1 {
                // If the record has only one field that isn't zero-sized,
                // unwrap it.
                Ok(sortables.pop().unwrap().1)
            } else {
                let layouts = Vec::from_iter_in(sortables.into_iter().map(|t| t.1), arena);

                Ok(Layout::Struct {
                    field_order_hash,
                    field_layouts: layouts.into_bump_slice(),
                })
            };

            Cacheable(result, criteria)
        }
        TagUnion(tags, ext_var) => {
            let (tags, ext_var) = tags.unsorted_tags_and_ext(subs, ext_var);

            debug_assert!(ext_var_is_empty_tag_union(subs, ext_var));

            layout_from_non_recursive_union(env, &tags).map(Ok)
        }
        FunctionOrTagUnion(tag_name, _, ext_var) => {
            debug_assert!(
                ext_var_is_empty_tag_union(subs, ext_var),
                "If ext_var wasn't empty, this wouldn't be a FunctionOrTagUnion!"
            );

            let union_tags = UnionTags::from_tag_name_index(tag_name);
            let (tags, _) = union_tags.unsorted_tags_and_ext(subs, ext_var);

            layout_from_non_recursive_union(env, &tags).map(Ok)
        }
        RecursiveTagUnion(rec_var, tags, ext_var) => {
            let (tags, ext_var) = tags.unsorted_tags_and_ext(subs, ext_var);

            debug_assert!(ext_var_is_empty_tag_union(subs, ext_var));

            layout_from_recursive_union(env, rec_var, &tags)
        }
        EmptyTagUnion => cacheable(Ok(Layout::VOID)),
        Erroneous(_) => cacheable(Err(LayoutProblem::Erroneous)),
        EmptyRecord => cacheable(Ok(Layout::UNIT)),
    }
}

pub type SortedField<'a> = (Lowercase, Variable, Result<Layout<'a>, Layout<'a>>);

pub fn sort_record_fields<'a>(
    env: &mut Env<'a, '_>,
    var: Variable,
) -> Result<Vec<'a, SortedField<'a>>, LayoutProblem> {
    let (it, _) = match gather_fields_unsorted_iter(env.subs, RecordFields::empty(), var) {
        Ok(it) => it,
        Err(_) => return Err(LayoutProblem::Erroneous),
    };

    let it = it
        .into_iter()
        .map(|(field, field_type)| (field.clone(), field_type));

    sort_record_fields_help(env, it)
}

fn sort_record_fields_help<'a>(
    env: &mut Env<'a, '_>,
    fields_map: impl Iterator<Item = (Lowercase, RecordField<Variable>)>,
) -> Result<Vec<'a, SortedField<'a>>, LayoutProblem> {
    let target_info = env.target_info;

    // Sort the fields by label
    let mut sorted_fields = Vec::with_capacity_in(fields_map.size_hint().0, env.arena);

    for (label, field) in fields_map {
        match field {
            RecordField::Demanded(v) | RecordField::Required(v) => {
                let Cacheable(layout, _) = Layout::from_var(env, v);
                sorted_fields.push((label, v, Ok(layout?)));
            }
            RecordField::Optional(v) | RecordField::RigidOptional(v) => {
                let Cacheable(layout, _) = Layout::from_var(env, v);
                sorted_fields.push((label, v, Err(layout?)));
            }
        };
    }

    sorted_fields.sort_by(
        |(label1, _, res_layout1), (label2, _, res_layout2)| match res_layout1 {
            Ok(layout1) | Err(layout1) => match res_layout2 {
                Ok(layout2) | Err(layout2) => cmp_fields(
                    &env.cache.interner,
                    label1,
                    layout1,
                    label2,
                    layout2,
                    target_info,
                ),
            },
        },
    );

    Ok(sorted_fields)
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TagOrClosure {
    Tag(TagName),
    Closure(Symbol),
}

impl TagOrClosure {
    pub fn expect_tag(self) -> TagName {
        match self {
            Self::Tag(t) => t,
            _ => internal_error!("not a tag"),
        }
    }
    pub fn expect_tag_ref(&self) -> &TagName {
        match self {
            Self::Tag(t) => t,
            _ => internal_error!("not a tag"),
        }
    }
}

impl From<TagName> for TagOrClosure {
    fn from(t: TagName) -> Self {
        Self::Tag(t)
    }
}

impl From<Symbol> for TagOrClosure {
    fn from(s: Symbol) -> Self {
        Self::Closure(s)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnionVariant<'a> {
    Never,
    Unit,
    BoolUnion {
        ttrue: TagOrClosure,
        ffalse: TagOrClosure,
    },
    ByteUnion(Vec<'a, TagOrClosure>),
    Newtype {
        tag_name: TagOrClosure,
        arguments: Vec<'a, Layout<'a>>,
    },
    NewtypeByVoid {
        data_tag_name: TagOrClosure,
        data_tag_id: TagIdIntType,
        data_tag_arguments: Vec<'a, Layout<'a>>,
    },
    Wrapped(WrappedVariant<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WrappedVariant<'a> {
    Recursive {
        sorted_tag_layouts: Vec<'a, (TagOrClosure, &'a [Layout<'a>])>,
    },
    NonRecursive {
        sorted_tag_layouts: Vec<'a, (TagOrClosure, &'a [Layout<'a>])>,
    },
    NullableWrapped {
        nullable_id: TagIdIntType,
        nullable_name: TagOrClosure,
        sorted_tag_layouts: Vec<'a, (TagOrClosure, &'a [Layout<'a>])>,
    },
    NonNullableUnwrapped {
        tag_name: TagOrClosure,
        fields: &'a [Layout<'a>],
    },
    NullableUnwrapped {
        nullable_id: bool,
        nullable_name: TagOrClosure,
        other_name: TagOrClosure,
        other_fields: &'a [Layout<'a>],
    },
}

impl<'a> WrappedVariant<'a> {
    pub fn tag_name_to_id(&self, tag_name: &TagName) -> (TagIdIntType, &'a [Layout<'a>]) {
        use WrappedVariant::*;

        match self {
            Recursive { sorted_tag_layouts } | NonRecursive { sorted_tag_layouts } => {
                let (tag_id, (_, argument_layouts)) = sorted_tag_layouts
                    .iter()
                    .enumerate()
                    .find(|(_, (key, _))| key.expect_tag_ref() == tag_name)
                    .expect("tag name is not in its own type");

                debug_assert!(tag_id < 256);
                (tag_id as TagIdIntType, *argument_layouts)
            }
            NullableWrapped {
                nullable_id,
                nullable_name,
                sorted_tag_layouts,
            } => {
                // assumption: the nullable_name is not included in sorted_tag_layouts

                if tag_name == nullable_name.expect_tag_ref() {
                    (*nullable_id as TagIdIntType, &[] as &[_])
                } else {
                    let (mut tag_id, (_, argument_layouts)) = sorted_tag_layouts
                        .iter()
                        .enumerate()
                        .find(|(_, (key, _))| key.expect_tag_ref() == tag_name)
                        .expect("tag name is not in its own type");

                    if tag_id >= *nullable_id as usize {
                        tag_id += 1;
                    }

                    debug_assert!(tag_id < 256);
                    (tag_id as TagIdIntType, *argument_layouts)
                }
            }
            NullableUnwrapped {
                nullable_id,
                nullable_name,
                other_name,
                other_fields,
            } => {
                if tag_name == nullable_name.expect_tag_ref() {
                    (*nullable_id as TagIdIntType, &[] as &[_])
                } else {
                    debug_assert_eq!(other_name.expect_tag_ref(), tag_name);

                    (!*nullable_id as TagIdIntType, *other_fields)
                }
            }
            NonNullableUnwrapped { fields, .. } => (0, fields),
        }
    }

    pub fn number_of_tags(&'a self) -> usize {
        use WrappedVariant::*;

        match self {
            Recursive { sorted_tag_layouts } | NonRecursive { sorted_tag_layouts } => {
                sorted_tag_layouts.len()
            }
            NullableWrapped {
                sorted_tag_layouts, ..
            } => {
                // assumption: the nullable_name is not included in sorted_tag_layouts

                sorted_tag_layouts.len() + 1
            }
            NullableUnwrapped { .. } => 2,
            NonNullableUnwrapped { .. } => 1,
        }
    }
}

pub fn union_sorted_tags<'a>(
    env: &mut Env<'a, '_>,
    var: Variable,
) -> Result<UnionVariant<'a>, LayoutProblem> {
    use roc_types::pretty_print::ChasedExt;
    use Content::*;

    let var = if let Content::RecursionVar { structure, .. } =
        env.subs.get_content_without_compacting(var)
    {
        *structure
    } else {
        var
    };

    let mut tags_vec = std::vec::Vec::new();
    let result = match roc_types::pretty_print::chase_ext_tag_union(env.subs, var, &mut tags_vec) {
        ChasedExt::Empty => {
            let opt_rec_var = get_recursion_var(env.subs, var);
            let Cacheable(result, _) = union_sorted_tags_help(env, tags_vec, opt_rec_var);
            result
        }
        ChasedExt::NonEmpty { content, .. } => {
            match content {
                FlexVar(_) | FlexAbleVar(..) | RigidVar(_) | RigidAbleVar(..) => {
                    // Admit type variables in the extension for now. This may come from things that never got
                    // monomorphized, like in
                    //   x : [A]*
                    //   x = A
                    //   x
                    // In such cases it's fine to drop the variable. We may be proven wrong in the future...
                    let opt_rec_var = get_recursion_var(env.subs, var);
                    let Cacheable(result, _) = union_sorted_tags_help(env, tags_vec, opt_rec_var);
                    result
                }
                RecursionVar { .. } => {
                    let opt_rec_var = get_recursion_var(env.subs, var);
                    let Cacheable(result, _) = union_sorted_tags_help(env, tags_vec, opt_rec_var);
                    result
                }

                Error => return Err(LayoutProblem::Erroneous),

                other => panic!("invalid content in tag union variable: {:?}", other),
            }
        }
    };

    Ok(result)
}

fn get_recursion_var(subs: &Subs, var: Variable) -> Option<Variable> {
    match subs.get_content_without_compacting(var) {
        Content::Structure(FlatType::RecursiveTagUnion(rec_var, _, _)) => Some(*rec_var),
        Content::Alias(_, _, actual, _) => get_recursion_var(subs, *actual),
        _ => None,
    }
}

fn is_recursive_tag_union(layout: &Layout) -> bool {
    matches!(
        layout,
        Layout::Union(
            UnionLayout::NullableUnwrapped { .. }
                | UnionLayout::Recursive(_)
                | UnionLayout::NullableWrapped { .. }
                | UnionLayout::NonNullableUnwrapped { .. },
        )
    )
}

fn union_sorted_non_recursive_tags_help<'a, L>(
    env: &mut Env<'a, '_>,
    tags_list: &[(&'_ L, &[Variable])],
) -> Cacheable<UnionVariant<'a>>
where
    L: Label + Ord + Clone + Into<TagOrClosure>,
{
    let mut cache_criteria = CACHEABLE;

    // sort up front; make sure the ordering stays intact!
    let mut tags_list = Vec::from_iter_in(tags_list.iter(), env.arena);
    tags_list.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));

    match tags_list.len() {
        0 => {
            // trying to instantiate a type with no values
            Cacheable(UnionVariant::Never, cache_criteria)
        }
        1 => {
            let &(tag_name, arguments) = tags_list.remove(0);
            let tag_name = tag_name.clone().into();

            // just one tag in the union (but with arguments) can be a struct
            let mut layouts = Vec::with_capacity_in(tags_list.len(), env.arena);

            for &var in arguments {
                let Cacheable(result, criteria) = Layout::from_var(env, var);
                cache_criteria.and(criteria);
                match result {
                    Ok(layout) => {
                        layouts.push(layout);
                    }
                    Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                        // If we encounter an unbound type var (e.g. `Ok *`)
                        // then it's zero-sized; In the future we may drop this argument
                        // completely, but for now we represent it with the empty tag union
                        layouts.push(Layout::VOID)
                    }
                    Err(LayoutProblem::Erroneous) => {
                        // An erroneous type var will code gen to a runtime
                        // error, so we don't need to store any data for it.
                    }
                }
            }

            layouts.sort_by(|layout1, layout2| {
                let size1 = layout1.alignment_bytes(&env.cache.interner, env.target_info);
                let size2 = layout2.alignment_bytes(&env.cache.interner, env.target_info);

                size2.cmp(&size1)
            });

            if layouts.is_empty() {
                Cacheable(UnionVariant::Unit, cache_criteria)
            } else {
                Cacheable(
                    UnionVariant::Newtype {
                        tag_name,
                        arguments: layouts,
                    },
                    cache_criteria,
                )
            }
        }
        num_tags => {
            // default path
            let mut answer: Vec<(TagOrClosure, &[Layout])> =
                Vec::with_capacity_in(tags_list.len(), env.arena);
            let mut has_any_arguments = false;

            let mut inhabited_tag_ids = BitVec::<usize>::repeat(true, num_tags);

            for &(tag_name, arguments) in tags_list.into_iter() {
                let mut arg_layouts = Vec::with_capacity_in(arguments.len() + 1, env.arena);

                for &var in arguments {
                    let Cacheable(result, criteria) = Layout::from_var(env, var);
                    cache_criteria.and(criteria);
                    match result {
                        Ok(layout) => {
                            has_any_arguments = true;

                            arg_layouts.push(layout);

                            if layout == Layout::VOID {
                                inhabited_tag_ids.set(answer.len(), false);
                            }
                        }
                        Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                            // If we encounter an unbound type var (e.g. `Ok *`)
                            // then it's zero-sized; In the future we may drop this argument
                            // completely, but for now we represent it with the empty tag union
                            arg_layouts.push(Layout::VOID);
                        }
                        Err(LayoutProblem::Erroneous) => {
                            // An erroneous type var will code gen to a runtime
                            // error, so we don't need to store any data for it.
                        }
                    }
                }

                arg_layouts.sort_by(|layout1, layout2| {
                    let size1 = layout1.alignment_bytes(&env.cache.interner, env.target_info);
                    let size2 = layout2.alignment_bytes(&env.cache.interner, env.target_info);

                    size2.cmp(&size1)
                });

                answer.push((tag_name.clone().into(), arg_layouts.into_bump_slice()));
            }

            if inhabited_tag_ids.count_ones() == 1 {
                let kept_tag_id = inhabited_tag_ids.first_one().unwrap();
                let kept = answer.get(kept_tag_id).unwrap();

                let variant = UnionVariant::NewtypeByVoid {
                    data_tag_name: kept.0.clone(),
                    data_tag_id: kept_tag_id as _,
                    data_tag_arguments: Vec::from_iter_in(kept.1.iter().copied(), env.arena),
                };
                return Cacheable(variant, cache_criteria);
            }

            match num_tags {
                2 if !has_any_arguments => {
                    // type can be stored in a boolean

                    // tags_vec is sorted, and answer is sorted the same way
                    let ttrue = answer.remove(1).0;
                    let ffalse = answer.remove(0).0;

                    Cacheable(UnionVariant::BoolUnion { ffalse, ttrue }, cache_criteria)
                }
                3..=MAX_ENUM_SIZE if !has_any_arguments => {
                    // type can be stored in a byte
                    // needs the sorted tag names to determine the tag_id
                    let mut tag_names = Vec::with_capacity_in(answer.len(), env.arena);

                    for (tag_name, _) in answer {
                        tag_names.push(tag_name);
                    }

                    Cacheable(UnionVariant::ByteUnion(tag_names), cache_criteria)
                }
                _ => {
                    let variant = WrappedVariant::NonRecursive {
                        sorted_tag_layouts: answer,
                    };

                    Cacheable(UnionVariant::Wrapped(variant), cache_criteria)
                }
            }
        }
    }
}

pub fn union_sorted_tags_pub<'a, L>(
    env: &mut Env<'a, '_>,
    tags_vec: std::vec::Vec<(L, std::vec::Vec<Variable>)>,
    opt_rec_var: Option<Variable>,
) -> UnionVariant<'a>
where
    L: Into<TagOrClosure> + Ord + Clone,
{
    union_sorted_tags_help(env, tags_vec, opt_rec_var).value()
}

fn union_sorted_tags_help<'a, L>(
    env: &mut Env<'a, '_>,
    mut tags_vec: std::vec::Vec<(L, std::vec::Vec<Variable>)>,
    opt_rec_var: Option<Variable>,
) -> Cacheable<UnionVariant<'a>>
where
    L: Into<TagOrClosure> + Ord + Clone,
{
    // sort up front; make sure the ordering stays intact!
    tags_vec.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));

    let mut cache_criteria = CACHEABLE;

    match tags_vec.len() {
        0 => {
            // trying to instantiate a type with no values
            Cacheable(UnionVariant::Never, cache_criteria)
        }
        1 => {
            let (tag_name, arguments) = tags_vec.remove(0);

            // just one tag in the union (but with arguments) can be a struct
            let mut layouts = Vec::with_capacity_in(tags_vec.len(), env.arena);

            for var in arguments {
                let Cacheable(result, criteria) = Layout::from_var(env, var);
                cache_criteria.and(criteria);
                match result {
                    Ok(layout) => {
                        layouts.push(layout);
                    }
                    Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                        // If we encounter an unbound type var (e.g. `Ok *`)
                        // then it's zero-sized; In the future we may drop this argument
                        // completely, but for now we represent it with the empty tag union
                        layouts.push(Layout::VOID)
                    }
                    Err(LayoutProblem::Erroneous) => {
                        // An erroneous type var will code gen to a runtime
                        // error, so we don't need to store any data for it.
                    }
                }
            }

            layouts.sort_by(|layout1, layout2| {
                let size1 = layout1.alignment_bytes(&env.cache.interner, env.target_info);
                let size2 = layout2.alignment_bytes(&env.cache.interner, env.target_info);

                size2.cmp(&size1)
            });

            if layouts.is_empty() {
                Cacheable(UnionVariant::Unit, cache_criteria)
            } else if let Some(rec_var) = opt_rec_var {
                let variant = UnionVariant::Wrapped(WrappedVariant::NonNullableUnwrapped {
                    tag_name: tag_name.into(),
                    fields: layouts.into_bump_slice(),
                });
                cache_criteria.pass_through_recursive_union(rec_var);
                Cacheable(variant, cache_criteria)
            } else {
                Cacheable(
                    UnionVariant::Newtype {
                        tag_name: tag_name.into(),
                        arguments: layouts,
                    },
                    cache_criteria,
                )
            }
        }
        num_tags => {
            // default path
            let mut answer = Vec::with_capacity_in(tags_vec.len(), env.arena);
            let mut has_any_arguments = false;

            let mut nullable = None;
            let mut inhabited_tag_ids = BitVec::<usize>::repeat(true, num_tags);

            // only recursive tag unions can be nullable
            let is_recursive = opt_rec_var.is_some();
            if is_recursive && GENERATE_NULLABLE {
                for (index, (name, variables)) in tags_vec.iter().enumerate() {
                    if variables.is_empty() {
                        nullable = Some((index as TagIdIntType, name.clone()));
                        break;
                    }
                }
            }

            for (index, (tag_name, arguments)) in tags_vec.into_iter().enumerate() {
                // reserve space for the tag discriminant
                if matches!(nullable, Some((i, _)) if i  as usize == index) {
                    debug_assert!(arguments.is_empty());
                    continue;
                }

                let mut arg_layouts = Vec::with_capacity_in(arguments.len() + 1, env.arena);

                for var in arguments {
                    let Cacheable(result, criteria) = Layout::from_var(env, var);
                    cache_criteria.and(criteria);
                    match result {
                        Ok(layout) => {
                            has_any_arguments = true;

                            // make sure to not unroll recursive types!
                            let self_recursion = opt_rec_var.is_some()
                                && env.subs.get_root_key_without_compacting(var)
                                    == env
                                        .subs
                                        .get_root_key_without_compacting(opt_rec_var.unwrap())
                                && is_recursive_tag_union(&layout);

                            if self_recursion {
                                arg_layouts.push(Layout::RecursivePointer);
                            } else {
                                arg_layouts.push(layout);
                            }

                            if layout == Layout::VOID {
                                inhabited_tag_ids.set(answer.len(), false);
                            }
                        }
                        Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                            // If we encounter an unbound type var (e.g. `Ok *`)
                            // then it's zero-sized; In the future we may drop this argument
                            // completely, but for now we represent it with the empty struct tag
                            // union
                            arg_layouts.push(Layout::VOID);
                        }
                        Err(LayoutProblem::Erroneous) => {
                            // An erroneous type var will code gen to a runtime
                            // error, so we don't need to store any data for it.
                        }
                    }
                }

                arg_layouts.sort_by(|layout1, layout2| {
                    let size1 = layout1.alignment_bytes(&env.cache.interner, env.target_info);
                    let size2 = layout2.alignment_bytes(&env.cache.interner, env.target_info);

                    size2.cmp(&size1)
                });

                answer.push((tag_name.into(), arg_layouts.into_bump_slice()));
            }

            if inhabited_tag_ids.count_ones() == 1 && !is_recursive {
                let kept_tag_id = inhabited_tag_ids.first_one().unwrap();
                let kept = answer.get(kept_tag_id).unwrap();

                let variant = UnionVariant::NewtypeByVoid {
                    data_tag_name: kept.0.clone(),
                    data_tag_id: kept_tag_id as _,
                    data_tag_arguments: Vec::from_iter_in(kept.1.iter().copied(), env.arena),
                };
                return Cacheable(variant, cache_criteria);
            }

            match num_tags {
                2 if !has_any_arguments => {
                    // type can be stored in a boolean

                    // tags_vec is sorted, and answer is sorted the same way
                    let ttrue = answer.remove(1).0;
                    let ffalse = answer.remove(0).0;

                    Cacheable(UnionVariant::BoolUnion { ffalse, ttrue }, cache_criteria)
                }
                3..=MAX_ENUM_SIZE if !has_any_arguments => {
                    // type can be stored in a byte
                    // needs the sorted tag names to determine the tag_id
                    let mut tag_names = Vec::with_capacity_in(answer.len(), env.arena);

                    for (tag_name, _) in answer {
                        tag_names.push(tag_name);
                    }

                    Cacheable(UnionVariant::ByteUnion(tag_names), cache_criteria)
                }
                _ => {
                    let variant = if let Some((nullable_id, nullable_name)) = nullable {
                        if answer.len() == 1 {
                            let (other_name, other_arguments) = answer.drain(..).next().unwrap();
                            let nullable_id = nullable_id != 0;

                            WrappedVariant::NullableUnwrapped {
                                nullable_id,
                                nullable_name: nullable_name.into(),
                                other_name,
                                other_fields: other_arguments,
                            }
                        } else {
                            WrappedVariant::NullableWrapped {
                                nullable_id,
                                nullable_name: nullable_name.into(),
                                sorted_tag_layouts: answer,
                            }
                        }
                    } else if is_recursive {
                        debug_assert!(answer.len() > 1);
                        WrappedVariant::Recursive {
                            sorted_tag_layouts: answer,
                        }
                    } else {
                        WrappedVariant::NonRecursive {
                            sorted_tag_layouts: answer,
                        }
                    };

                    if let Some(rec_var) = opt_rec_var {
                        cache_criteria.pass_through_recursive_union(rec_var);
                        debug_assert!(!matches!(variant, WrappedVariant::NonRecursive { .. }));
                    }

                    Cacheable(UnionVariant::Wrapped(variant), cache_criteria)
                }
            }
        }
    }
}

fn layout_from_newtype<'a, L: Label>(
    env: &mut Env<'a, '_>,
    tags: &UnsortedUnionLabels<L>,
) -> Cacheable<Layout<'a>> {
    debug_assert!(tags.is_newtype_wrapper(env.subs));

    let (_tag_name, var) = tags.get_newtype(env.subs);

    let Cacheable(result, criteria) = Layout::from_var(env, var);
    match result {
        Ok(layout) => Cacheable(layout, criteria),
        Err(LayoutProblem::UnresolvedTypeVar(_)) => {
            // If we encounter an unbound type var (e.g. `Ok *`)
            // then it's zero-sized; In the future we may drop this argument
            // completely, but for now we represent it with the empty tag union
            Cacheable(Layout::VOID, criteria)
        }
        Err(LayoutProblem::Erroneous) => {
            // An erroneous type var will code gen to a runtime
            // error, so we don't need to store any data for it.
            todo!()
        }
    }
}

fn layout_from_non_recursive_union<'a, L>(
    env: &mut Env<'a, '_>,
    tags: &UnsortedUnionLabels<L>,
) -> Cacheable<Layout<'a>>
where
    L: Label + Ord + Into<TagOrClosure>,
{
    use UnionVariant::*;

    if tags.is_newtype_wrapper(env.subs) {
        return layout_from_newtype(env, tags);
    }

    let tags_vec = &tags.tags;

    let mut criteria = CACHEABLE;

    let variant = union_sorted_non_recursive_tags_help(env, tags_vec).decompose(&mut criteria);

    let result = match variant {
        Never => Layout::VOID,
        Unit => Layout::UNIT,
        BoolUnion { .. } => Layout::bool(),
        ByteUnion(_) => Layout::u8(),
        Newtype {
            arguments: field_layouts,
            ..
        } => {
            let answer1 = if field_layouts.len() == 1 {
                field_layouts[0]
            } else {
                Layout::struct_no_name_order(field_layouts.into_bump_slice())
            };

            answer1
        }
        NewtypeByVoid {
            data_tag_arguments, ..
        } => {
            if data_tag_arguments.len() == 1 {
                data_tag_arguments[0]
            } else {
                Layout::struct_no_name_order(data_tag_arguments.into_bump_slice())
            }
        }
        Wrapped(variant) => {
            use WrappedVariant::*;

            match variant {
                NonRecursive {
                    sorted_tag_layouts: tags,
                } => {
                    let mut tag_layouts = Vec::with_capacity_in(tags.len(), env.arena);
                    tag_layouts.extend(tags.iter().map(|r| r.1));

                    Layout::Union(UnionLayout::NonRecursive(tag_layouts.into_bump_slice()))
                }

                Recursive { .. }
                | NullableWrapped { .. }
                | NullableUnwrapped { .. }
                | NonNullableUnwrapped { .. } => {
                    internal_error!("non-recursive tag union has recursive layout")
                }
            }
        }
    };

    Cacheable(result, criteria)
}

fn layout_from_recursive_union<'a, L>(
    env: &mut Env<'a, '_>,
    rec_var: Variable,
    tags: &UnsortedUnionLabels<L>,
) -> Cacheable<LayoutResult<'a>>
where
    L: Label + Ord + Into<TagOrClosure>,
{
    let arena = env.arena;
    let subs = env.subs;
    let target_info = env.target_info;

    let mut criteria = CACHEABLE;

    // some observations
    //
    // * recursive tag unions are always recursive
    // * therefore at least one tag has a pointer (non-zero sized) field
    // * they must (to be instantiated) have 2 or more tags
    //
    // That means none of the optimizations for enums or single tag tag unions apply

    let rec_var = subs.get_root_key_without_compacting(rec_var);
    let tags_vec = &tags.tags;
    let mut tag_layouts = Vec::with_capacity_in(tags_vec.len(), arena);

    let mut nullable = None;

    if GENERATE_NULLABLE {
        for (index, (_name, variables)) in tags_vec.iter().enumerate() {
            if variables.is_empty() {
                nullable = Some(index as TagIdIntType);
                break;
            }
        }
    }

    env.insert_seen(rec_var);
    for (index, &(_name, variables)) in tags_vec.iter().enumerate() {
        if matches!(nullable, Some(i) if i == index as TagIdIntType) {
            // don't add the nullable case
            continue;
        }

        let mut tag_layout = Vec::with_capacity_in(variables.len() + 1, arena);

        for &var in variables {
            // TODO does this cause problems with mutually recursive unions?
            if rec_var == subs.get_root_key_without_compacting(var) {
                tag_layout.push(Layout::RecursivePointer);
                continue;
            }

            let payload = cached!(Layout::from_var(env, var), criteria);
            tag_layout.push(payload);
        }

        tag_layout.sort_by(|layout1, layout2| {
            let size1 = layout1.alignment_bytes(&env.cache.interner, target_info);
            let size2 = layout2.alignment_bytes(&env.cache.interner, target_info);

            size2.cmp(&size1)
        });

        tag_layouts.push(tag_layout.into_bump_slice());
    }
    env.remove_seen(rec_var);

    let union_layout = if let Some(tag_id) = nullable {
        match tag_layouts.into_bump_slice() {
            [one] => {
                let nullable_id = tag_id != 0;

                UnionLayout::NullableUnwrapped {
                    nullable_id,
                    other_fields: one,
                }
            }
            many => UnionLayout::NullableWrapped {
                nullable_id: tag_id,
                other_tags: many,
            },
        }
    } else if tag_layouts.len() == 1 {
        // drop the tag id
        UnionLayout::NonNullableUnwrapped(tag_layouts.pop().unwrap())
    } else {
        UnionLayout::Recursive(tag_layouts.into_bump_slice())
    };
    criteria.pass_through_recursive_union(rec_var);

    Cacheable(Ok(Layout::Union(union_layout)), criteria)
}

#[cfg(debug_assertions)]
pub fn ext_var_is_empty_record(subs: &Subs, ext_var: Variable) -> bool {
    // the ext_var is empty
    let fields = match roc_types::types::gather_fields(subs, RecordFields::empty(), ext_var) {
        Ok(fields) => fields,
        Err(_) => return false,
    };

    fields.fields.is_empty()
}

#[cfg(not(debug_assertions))]
pub fn ext_var_is_empty_record(_subs: &Subs, _ext_var: Variable) -> bool {
    // This should only ever be used in debug_assert! macros
    unreachable!();
}

#[cfg(debug_assertions)]
pub fn ext_var_is_empty_tag_union(subs: &Subs, ext_var: Variable) -> bool {
    use roc_types::pretty_print::ChasedExt;
    use Content::*;

    // the ext_var is empty
    let mut ext_fields = std::vec::Vec::new();
    match roc_types::pretty_print::chase_ext_tag_union(subs, ext_var, &mut ext_fields) {
        ChasedExt::Empty => ext_fields.is_empty(),
        ChasedExt::NonEmpty { content, .. } => {
            match content {
                // Allow flex/rigid to decay away into nothing
                FlexVar(_) | FlexAbleVar(..) | RigidVar(_) | RigidAbleVar(..) => {
                    ext_fields.is_empty()
                }
                // So that we can continue compiling in the presence of errors
                Error => ext_fields.is_empty(),
                _ => panic!("invalid content in ext_var: {:?}", content),
            }
        }
    }
}

#[cfg(not(debug_assertions))]
pub fn ext_var_is_empty_tag_union(_: &Subs, _: Variable) -> bool {
    // This should only ever be used in debug_assert! macros
    unreachable!();
}

fn layout_from_num_content<'a>(
    content: &Content,
    target_info: TargetInfo,
) -> Cacheable<LayoutResult<'a>> {
    use roc_types::subs::Content::*;
    use roc_types::subs::FlatType::*;

    let result = match content {
        RecursionVar { .. } => panic!("recursion var in num"),
        FlexVar(_) | RigidVar(_) => {
            // If a Num makes it all the way through type checking with an unbound
            // type variable, then assume it's a 64-bit integer.
            //
            // (e.g. for (5 + 5) assume both 5s are 64-bit integers.)
            Ok(Layout::default_integer())
        }
        FlexAbleVar(_, _) | RigidAbleVar(_, _) => todo_abilities!("Not reachable yet"),
        Structure(Apply(symbol, args)) => match *symbol {
            // Ints
            Symbol::NUM_NAT => Ok(Layout::usize(target_info)),

            Symbol::NUM_INTEGER => Ok(Layout::i64()),
            Symbol::NUM_I128 => Ok(Layout::i128()),
            Symbol::NUM_I64 => Ok(Layout::i64()),
            Symbol::NUM_I32 => Ok(Layout::i32()),
            Symbol::NUM_I16 => Ok(Layout::i16()),
            Symbol::NUM_I8 => Ok(Layout::i8()),

            Symbol::NUM_U128 => Ok(Layout::u128()),
            Symbol::NUM_U64 => Ok(Layout::u64()),
            Symbol::NUM_U32 => Ok(Layout::u32()),
            Symbol::NUM_U16 => Ok(Layout::u16()),
            Symbol::NUM_U8 => Ok(Layout::u8()),

            // Floats
            Symbol::NUM_FLOATINGPOINT => Ok(Layout::f64()),
            Symbol::NUM_F64 => Ok(Layout::f64()),
            Symbol::NUM_F32 => Ok(Layout::f32()),

            // Dec
            Symbol::NUM_DEC => Ok(Layout::Builtin(Builtin::Decimal)),

            _ => {
                panic!(
                    "Invalid Num.Num type application: Apply({:?}, {:?})",
                    symbol, args
                );
            }
        },
        Alias(_, _, _, _) => {
            todo!("TODO recursively resolve type aliases in num_from_content");
        }
        Structure(_) | RangedNumber(..) | LambdaSet(_) => {
            panic!("Invalid Num.Num type application: {:?}", content);
        }
        Error => Err(LayoutProblem::Erroneous),
    };
    cacheable(result)
}

pub(crate) fn list_layout_from_elem<'a>(
    env: &mut Env<'a, '_>,
    element_var: Variable,
) -> Cacheable<LayoutResult<'a>> {
    let mut criteria = CACHEABLE;

    let is_variable = |content| matches!(content, &Content::FlexVar(_) | &Content::RigidVar(_));

    let element_content = env.subs.get_content_without_compacting(element_var);

    let element_layout = if is_variable(element_content) {
        // If this was still a (List *) then it must have been an empty list
        Layout::VOID
    } else {
        // NOTE: cannot re-use Content, because it may be recursive
        // then some state is not correctly kept, we have to go through from_var
        cached!(Layout::from_var(env, element_var), criteria)
    };

    Cacheable(
        Ok(Layout::Builtin(Builtin::List(
            env.arena.alloc(element_layout),
        ))),
        criteria,
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LayoutId(u32);

impl LayoutId {
    // Returns something like "#UserApp_foo_1" when given a symbol that interns to "foo"
    // and a LayoutId of 1.
    pub fn to_symbol_string(self, symbol: Symbol, interns: &Interns) -> String {
        let ident_string = symbol.as_str(interns);
        let module_string = interns.module_ids.get_name(symbol.module_id()).unwrap();
        format!("{}_{}_{}", module_string, ident_string, self.0)
    }

    // Returns something like "roc__foo_1_exposed" when given a symbol that interns to "foo"
    // and a LayoutId of 1.
    pub fn to_exposed_symbol_string(self, symbol: Symbol, interns: &Interns) -> String {
        let ident_string = symbol.as_str(interns);
        format!("roc__{}_{}_exposed", ident_string, self.0)
    }
}

struct IdsByLayout<'a> {
    by_id: MutMap<Layout<'a>, u32>,
    toplevels_by_id: MutMap<crate::ir::ProcLayout<'a>, u32>,
    next_id: u32,
}

impl<'a> IdsByLayout<'a> {
    #[inline(always)]
    fn insert_layout(&mut self, layout: Layout<'a>) -> LayoutId {
        match self.by_id.entry(layout) {
            Entry::Vacant(vacant) => {
                let answer = self.next_id;
                vacant.insert(answer);
                self.next_id += 1;

                LayoutId(answer)
            }
            Entry::Occupied(occupied) => LayoutId(*occupied.get()),
        }
    }

    #[inline(always)]
    fn singleton_layout(layout: Layout<'a>) -> (Self, LayoutId) {
        let mut by_id = HashMap::with_capacity_and_hasher(1, default_hasher());
        by_id.insert(layout, 1);

        let ids_by_layout = IdsByLayout {
            by_id,
            toplevels_by_id: Default::default(),
            next_id: 2,
        };

        (ids_by_layout, LayoutId(1))
    }

    #[inline(always)]
    fn insert_toplevel(&mut self, layout: crate::ir::ProcLayout<'a>) -> LayoutId {
        match self.toplevels_by_id.entry(layout) {
            Entry::Vacant(vacant) => {
                let answer = self.next_id;
                vacant.insert(answer);
                self.next_id += 1;

                LayoutId(answer)
            }
            Entry::Occupied(occupied) => LayoutId(*occupied.get()),
        }
    }

    #[inline(always)]
    fn singleton_toplevel(layout: crate::ir::ProcLayout<'a>) -> (Self, LayoutId) {
        let mut toplevels_by_id = HashMap::with_capacity_and_hasher(1, default_hasher());
        toplevels_by_id.insert(layout, 1);

        let ids_by_layout = IdsByLayout {
            by_id: Default::default(),
            toplevels_by_id,
            next_id: 2,
        };

        (ids_by_layout, LayoutId(1))
    }
}

#[derive(Default)]
pub struct LayoutIds<'a> {
    by_symbol: MutMap<Symbol, IdsByLayout<'a>>,
}

impl<'a> LayoutIds<'a> {
    /// Returns a LayoutId which is unique for the given symbol and layout.
    /// If given the same symbol and same layout, returns the same LayoutId.
    #[inline(always)]
    pub fn get<'b>(&mut self, symbol: Symbol, layout: &'b Layout<'a>) -> LayoutId {
        match self.by_symbol.entry(symbol) {
            Entry::Vacant(vacant) => {
                let (ids_by_layout, layout_id) = IdsByLayout::singleton_layout(*layout);

                vacant.insert(ids_by_layout);

                layout_id
            }
            Entry::Occupied(mut occupied_ids) => occupied_ids.get_mut().insert_layout(*layout),
        }
    }

    /// Returns a LayoutId which is unique for the given symbol and layout.
    /// If given the same symbol and same layout, returns the same LayoutId.
    #[inline(always)]
    pub fn get_toplevel<'b>(
        &mut self,
        symbol: Symbol,
        layout: &'b crate::ir::ProcLayout<'a>,
    ) -> LayoutId {
        match self.by_symbol.entry(symbol) {
            Entry::Vacant(vacant) => {
                let (ids_by_layout, layout_id) = IdsByLayout::singleton_toplevel(*layout);

                vacant.insert(ids_by_layout);

                layout_id
            }
            Entry::Occupied(mut occupied_ids) => occupied_ids.get_mut().insert_toplevel(*layout),
        }
    }
}

/// Compare two fields when sorting them for code gen.
/// This is called by both code gen and glue, so that
/// their field orderings agree.
#[inline(always)]
pub fn cmp_fields<'a, L: Ord, I>(
    interner: &I,
    label1: &L,
    layout1: &Layout<'a>,
    label2: &L,
    layout2: &Layout<'a>,
    target_info: TargetInfo,
) -> Ordering
where
    I: Interner<'a, Layout<'a>>,
{
    let size1 = layout1.alignment_bytes(interner, target_info);
    let size2 = layout2.alignment_bytes(interner, target_info);

    size2.cmp(&size1).then(label1.cmp(label2))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn width_and_alignment_union_empty_struct() {
        let mut interner = SingleThreadedInterner::with_capacity(4);

        let lambda_set = LambdaSet {
            set: &[(Symbol::LIST_MAP, &[])],
            representation: interner.insert(&Layout::UNIT),
        };

        let a = &[Layout::UNIT] as &[_];
        let b = &[Layout::LambdaSet(lambda_set)] as &[_];
        let tt = [a, b];

        let layout = Layout::Union(UnionLayout::NonRecursive(&tt));

        let target_info = TargetInfo::default_x86_64();
        assert_eq!(layout.stack_size(&interner, target_info), 1);
        assert_eq!(layout.alignment_bytes(&interner, target_info), 1);
    }

    #[test]
    fn memcpy_size_result_u32_unit() {
        let interner = SingleThreadedInterner::with_capacity(4);

        let ok_tag = &[Layout::Builtin(Builtin::Int(IntWidth::U32))];
        let err_tag = &[Layout::UNIT];
        let tags = [ok_tag as &[_], err_tag as &[_]];
        let union_layout = UnionLayout::NonRecursive(&tags as &[_]);
        let layout = Layout::Union(union_layout);

        let target_info = TargetInfo::default_x86_64();
        assert_eq!(
            layout.stack_size_without_alignment(&interner, target_info),
            8
        );
    }

    #[test]
    fn void_stack_size() {
        let interner = SingleThreadedInterner::with_capacity(4);
        let target_info = TargetInfo::default_x86_64();
        assert_eq!(Layout::VOID.stack_size(&interner, target_info), 0);
    }
}
