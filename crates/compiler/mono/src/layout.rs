use crate::ir::Parens;
use crate::layout::intern::NeedsRecursionPointerFixup;
use bitvec::vec::BitVec;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::all::{default_hasher, FnvMap, MutMap};
use roc_collections::{SmallVec, VecSet};
use roc_error_macros::{internal_error, todo_abilities};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{Interns, Symbol};
use roc_problem::can::RuntimeError;
use roc_target::{PtrWidth, Target};
use roc_types::num::NumericRange;
use roc_types::subs::{
    self, Content, FlatType, GetSubsSlice, OptVariable, RecordFields, Subs, TagExt, TupleElems,
    UnsortedUnionLabels, Variable, VariableSubsSlice,
};
use roc_types::types::{
    gather_fields_unsorted_iter, gather_tuple_elems_unsorted_iter, RecordField, RecordFieldsError,
    TupleElemsError,
};
use std::cmp::Ordering;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;
use ven_pretty::{DocAllocator, DocBuilder};

mod erased;
mod intern;
mod semantic;

pub use erased::Erased;
pub use intern::{
    GlobalLayoutInterner, InLayout, LayoutInterner, STLayoutInterner, TLLayoutInterner,
};
pub use semantic::SemanticRepr;

// if your changes cause this number to go down, great!
// please change it to the lower number.
// if it went up, maybe check that the change is really required
roc_error_macros::assert_sizeof_aarch64!(Builtin, 2 * 8);
roc_error_macros::assert_sizeof_aarch64!(Layout, 9 * 8);
roc_error_macros::assert_sizeof_aarch64!(UnionLayout, 3 * 8);
roc_error_macros::assert_sizeof_aarch64!(LambdaSet, 5 * 8);

roc_error_macros::assert_sizeof_wasm!(Builtin, 2 * 4);
roc_error_macros::assert_sizeof_wasm!(Layout, 9 * 4);
roc_error_macros::assert_sizeof_wasm!(UnionLayout, 3 * 4);
roc_error_macros::assert_sizeof_wasm!(LambdaSet, 5 * 4);

roc_error_macros::assert_sizeof_default!(Builtin, 2 * 8);
roc_error_macros::assert_sizeof_default!(Layout, 9 * 8);
roc_error_macros::assert_sizeof_default!(UnionLayout, 3 * 8);
roc_error_macros::assert_sizeof_default!(LambdaSet, 5 * 8);

type LayoutResult<'a> = Result<InLayout<'a>, LayoutProblem>;
type RawFunctionLayoutResult<'a> = Result<RawFunctionLayout<'a>, LayoutProblem>;

#[derive(Debug, Clone)]
struct CacheMeta {
    /// Does this cache entry include a recursive structure? If so, what's the recursion variable
    /// of that structure?
    recursive_structures: SmallVec<Variable, 2>,
}

impl CacheMeta {
    #[inline(always)]
    fn into_criteria(self) -> CacheCriteria {
        let CacheMeta {
            recursive_structures,
        } = self;
        CacheCriteria {
            has_naked_recursion_pointer: false,
            recursive_structures,
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

/// Layout cache to avoid recomputing [Layout] from a [Variable] multiple times.
#[derive(Debug)]
pub struct LayoutCache<'a> {
    pub target: Target,
    cache: std::vec::Vec<CacheLayer<LayoutResult<'a>>>,
    raw_function_cache: std::vec::Vec<CacheLayer<RawFunctionLayoutResult<'a>>>,

    pub interner: TLLayoutInterner<'a>,

    /// Statistics on the usage of the layout cache.
    #[cfg(debug_assertions)]
    stats: CacheStatistics,
    #[cfg(debug_assertions)]
    raw_function_stats: CacheStatistics,
}

impl<'a> LayoutCache<'a> {
    pub fn new(interner: TLLayoutInterner<'a>, target: Target) -> Self {
        let mut cache = std::vec::Vec::with_capacity(4);
        cache.push(Default::default());
        let mut raw_cache = std::vec::Vec::with_capacity(4);
        raw_cache.push(Default::default());
        Self {
            target,
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
    ) -> Result<InLayout<'a>, LayoutProblem> {
        // Store things according to the root Variable, to avoid duplicate work.
        let var = subs.get_root_key_without_compacting(var);

        let mut env = Env {
            arena,
            subs,
            seen: Vec::new_in(arena),
            cache: self,
        };

        // [Layout::from_var] should query the cache!
        let Cacheable(value, criteria) = Layout::from_var(&mut env, var);
        debug_assert!(
            criteria.is_cacheable(),
            "{value:?} not cacheable as top-level"
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
            cache: self,
        };

        // [Layout::from_var] should query the cache!
        let Cacheable(value, criteria) = RawFunctionLayout::from_var(&mut env, var);
        debug_assert!(
            criteria.is_cacheable(),
            "{value:?} not cacheable as top-level"
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
                return Some(result.clone());
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
    pub fn invalidate(&mut self, subs: &Subs, vars: impl IntoIterator<Item = Variable>) {
        // TODO(layout-cache): optimize me somehow
        for var in vars.into_iter() {
            let var = subs.get_root_key_without_compacting(var);
            for layer in self.cache.iter_mut().rev() {
                layer
                    .0
                    .retain(|k, _| !subs.equivalent_without_compacting(var, *k));
                roc_tracing::debug!(?var, "invalidating cached layout");
            }
            for layer in self.raw_function_cache.iter_mut().rev() {
                layer
                    .0
                    .retain(|k, _| !subs.equivalent_without_compacting(var, *k));
                roc_tracing::debug!(?var, "invalidating cached layout");
            }
        }
    }

    pub fn get_in(&self, interned: InLayout<'a>) -> Layout<'a> {
        self.interner.get(interned)
    }
    pub fn get_repr(&self, interned: InLayout<'a>) -> LayoutRepr<'a> {
        self.interner.get_repr(interned)
    }

    pub fn put_in(&mut self, layout: Layout<'a>) -> InLayout<'a> {
        self.interner.insert(layout)
    }
    pub(crate) fn put_in_direct_no_semantic(&mut self, repr: LayoutRepr<'a>) -> InLayout<'a> {
        self.interner.insert_direct_no_semantic(repr)
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

#[derive(Clone, Debug)]
struct CacheCriteria {
    /// Whether there is a naked recursion pointer in this layout, that doesn't pass through a
    /// recursive structure.
    has_naked_recursion_pointer: bool,
    /// Recursive structures this layout contains, if any.
    // Typically at most 1 recursive structure is contained, but there may be more.
    recursive_structures: SmallVec<Variable, 2>,
}

const CACHEABLE: CacheCriteria = CacheCriteria {
    has_naked_recursion_pointer: false,
    recursive_structures: SmallVec::new(),
};

const NAKED_RECURSION_PTR: CacheCriteria = CacheCriteria {
    has_naked_recursion_pointer: true,
    recursive_structures: SmallVec::new(),
};

impl CacheCriteria {
    #[inline(always)]
    fn is_cacheable(&self) -> bool {
        // Can't cache if there a naked recursion pointer that isn't covered by a recursive layout.
        !self.has_naked_recursion_pointer
    }

    /// Makes `self` cacheable iff self and other are cacheable.
    #[inline(always)]
    fn and(&mut self, other: Self, subs: &Subs) {
        self.has_naked_recursion_pointer =
            self.has_naked_recursion_pointer || other.has_naked_recursion_pointer;

        for &other_rec in other.recursive_structures.iter() {
            if self
                .recursive_structures
                .iter()
                .any(|rec| subs.equivalent_without_compacting(*rec, other_rec))
            {
                continue;
            }
            self.recursive_structures.push(other_rec);
        }
    }

    #[inline(always)]
    fn pass_through_recursive_union(&mut self, recursion_var: Variable) {
        self.has_naked_recursion_pointer = false;
        self.recursive_structures.push(recursion_var);
    }

    #[inline(always)]
    fn cache_metadata(&self) -> CacheMeta {
        CacheMeta {
            recursive_structures: self.recursive_structures.clone(),
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
    fn decompose(self, and_with: &mut CacheCriteria, subs: &Subs) -> T {
        let Self(value, criteria) = self;
        and_with.and(criteria, subs);
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
    ($expr:expr, $total_criteria:expr, $subs:expr) => {
        match $expr {
            Cacheable(Ok(v), criteria) => {
                $total_criteria.and(criteria, $subs);
                v
            }
            Cacheable(Err(v), criteria) => return Cacheable(Err(v), criteria),
        }
    };
}

pub type TagIdIntType = u16;
pub const MAX_ENUM_SIZE: usize = std::mem::size_of::<TagIdIntType>() * 8;
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
    Function(&'a [InLayout<'a>], LambdaSet<'a>, InLayout<'a>),
    ErasedFunction(&'a [InLayout<'a>], InLayout<'a>),
    ZeroArgumentThunk(InLayout<'a>),
}

impl<'a> RawFunctionLayout<'a> {
    pub fn is_zero_argument_thunk(&self) -> bool {
        matches!(self, RawFunctionLayout::ZeroArgumentThunk(_))
    }

    pub fn is_erased_function(&self) -> bool {
        matches!(self, RawFunctionLayout::ErasedFunction(_, _))
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
            LambdaSet(_) => {
                internal_error!("lambda set should only appear under a function, where it's handled independently.");
            }
            ErasedLambda => internal_error!("erased lambda type should only appear under a function, where it's handled independently"),
            Pure | Effectful => internal_error!("fx vars should only appear under a function"),
            Structure(flat_type) => Self::layout_from_flat_type(env, flat_type),
            RangedNumber(..) => Layout::new_help(env, var, content).then(Self::ZeroArgumentThunk),

            // Ints
            Alias(Symbol::NUM_I128, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::I128)))
            }
            Alias(Symbol::NUM_I64, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::I64)))
            }
            Alias(Symbol::NUM_I32, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::I32)))
            }
            Alias(Symbol::NUM_I16, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::I16)))
            }
            Alias(Symbol::NUM_I8, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::I8)))
            }

            // I think unsigned and signed use the same layout
            Alias(Symbol::NUM_U128, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::U128)))
            }
            Alias(Symbol::NUM_U64, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::U64)))
            }
            Alias(Symbol::NUM_U32, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::U32)))
            }
            Alias(Symbol::NUM_U16, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::U16)))
            }
            Alias(Symbol::NUM_U8, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::U8)))
            }

            // Floats
            Alias(Symbol::NUM_F64, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::F64)))
            }
            Alias(Symbol::NUM_F32, args, _, _) => {
                debug_assert!(args.is_empty());
                cacheable(Ok(Self::ZeroArgumentThunk(Layout::F32)))
            }

            Alias(Symbol::INSPECT_ELEM_WALKER | Symbol::INSPECT_KEY_VAL_WALKER, _, var, _) => Self::from_var(env, var),

            Alias(symbol, _, var, _) if symbol.is_builtin() => {
                Layout::new_help(env, var, content).then(Self::ZeroArgumentThunk)
            }

            Alias(_, _, var, _) => Self::from_var(env, var),
            Error => cacheable(Err(LayoutProblem::Erroneous)),
        }
    }

    fn layout_from_flat_type(
        env: &mut Env<'a, '_>,
        flat_type: FlatType,
    ) -> Cacheable<RawFunctionLayoutResult<'a>> {
        use roc_types::subs::FlatType::*;

        let arena = env.arena;

        match flat_type {
            Func(args, closure_var, ret_var, _fx_var) => {
                let mut fn_args = Vec::with_capacity_in(args.len(), arena);

                let mut cache_criteria = CACHEABLE;

                for index in args.into_iter() {
                    let arg_var = env.subs[index];
                    let layout = cached!(Layout::from_var(env, arg_var), cache_criteria, env.subs);
                    fn_args.push(layout);
                }

                let ret = cached!(Layout::from_var(env, ret_var), cache_criteria, env.subs);

                let fn_args = fn_args.into_bump_slice();

                let closure_data = build_function_closure_data(env, args, closure_var, ret_var);
                let closure_data = cached!(closure_data, cache_criteria, env.subs);

                let function_layout = match closure_data {
                    ClosureDataKind::LambdaSet(lambda_set) => {
                        Self::Function(fn_args, lambda_set, ret)
                    }
                    ClosureDataKind::Erased => Self::ErasedFunction(fn_args, ret),
                };

                Cacheable(Ok(function_layout), cache_criteria)
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
                let layout = cached!(layout_from_flat_type(env, flat_type), criteria, env.subs);
                Cacheable(Ok(Self::ZeroArgumentThunk(layout)), criteria)
            }
        }
    }

    /// Returns Err(()) if given an error, or Ok(Layout) if given a non-erroneous Structure.
    /// Panics if given a FlexVar or RigidVar, since those should have been
    /// monomorphized away already!
    pub(crate) fn from_var(
        env: &mut Env<'a, '_>,
        var: Variable,
    ) -> Cacheable<RawFunctionLayoutResult<'a>> {
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
pub struct Layout<'a> {
    repr: LayoutWrapper<'a>,
    semantic: SemanticRepr<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) enum LayoutWrapper<'a> {
    Direct(LayoutRepr<'a>),
    Newtype(InLayout<'a>),
}

/// Types for code gen must be monomorphic. No type variables allowed!
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum LayoutRepr<'a> {
    Builtin(Builtin<'a>),
    Struct(&'a [InLayout<'a>]),
    // A pointer (heap or stack) without any reference counting
    // Ptr is not user-facing. The compiler author must make sure that invariants are upheld
    Ptr(InLayout<'a>),
    Union(UnionLayout<'a>),
    LambdaSet(LambdaSet<'a>),
    RecursivePointer(InLayout<'a>),
    /// Only used in erased functions.
    FunctionPointer(FunctionPointer<'a>),
    /// The layout of an erasure.
    Erased(Erased),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FunctionPointer<'a> {
    pub args: &'a [InLayout<'a>],
    pub ret: InLayout<'a>,
}

impl<'a> FunctionPointer<'a> {
    pub fn to_doc<'b, D, A, I>(
        self,
        alloc: &'b D,
        interner: &I,
        seen_rec: &mut SeenRecPtrs<'a>,
        parens: Parens,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
        I: LayoutInterner<'a>,
    {
        let Self { args, ret } = self;

        let args = args
            .iter()
            .map(|arg| interner.to_doc(*arg, alloc, seen_rec, parens));
        let args = alloc.intersperse(args, alloc.text(", "));
        let ret = interner.to_doc(ret, alloc, seen_rec, parens);

        alloc
            .text("FunPtr((")
            .append(args)
            .append(alloc.text(") -> "))
            .append(ret)
            .append(")")
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UnionLayout<'a> {
    /// A non-recursive tag union
    /// e.g. `Result a e : [Ok a, Err e]`
    NonRecursive(&'a [&'a [InLayout<'a>]]),
    /// A recursive tag union (general case)
    /// e.g. `Expr : [Sym Str, Add Expr Expr]`
    Recursive(&'a [&'a [InLayout<'a>]]),
    /// A recursive tag union with just one constructor
    /// Optimization: No need to store a tag ID (the payload is "unwrapped")
    /// e.g. `RoseTree a : [Tree a (List (RoseTree a))]`
    NonNullableUnwrapped(&'a [InLayout<'a>]),
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
        other_tags: &'a [&'a [InLayout<'a>]],
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
        other_fields: &'a [InLayout<'a>],
    },
}

impl<'a> UnionLayout<'a> {
    pub fn to_doc<'b, D, A, I>(
        self,
        alloc: &'b D,
        interner: &I,
        seen_rec: &mut SeenRecPtrs<'a>,
        _parens: Parens,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
        I: LayoutInterner<'a>,
    {
        use UnionLayout::*;

        match self {
            NonRecursive(tags) => {
                let tags_doc = tags.iter().map(|fields| {
                    alloc.text("C ").append(
                        alloc.intersperse(
                            fields
                                .iter()
                                .map(|x| interner.to_doc(*x, alloc, seen_rec, Parens::InTypeParam)),
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
                                .map(|x| interner.to_doc(*x, alloc, seen_rec, Parens::InTypeParam)),
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
                            .map(|x| interner.to_doc(*x, alloc, seen_rec, Parens::InTypeParam)),
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
                            .map(|x| interner.to_doc(*x, alloc, seen_rec, Parens::InTypeParam)),
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
            NullableWrapped {
                nullable_id,
                other_tags,
            } => {
                let nullable_id = nullable_id as usize;
                let tags_docs =
                    (0..(other_tags.len() + 1)).map(|i| {
                        if i == nullable_id {
                            alloc.text("<null>")
                        } else {
                            let idx = if i > nullable_id { i - 1 } else { i };
                            alloc.text("C ").append(alloc.intersperse(
                                other_tags[idx].iter().map(|x| {
                                    interner.to_doc(*x, alloc, seen_rec, Parens::InTypeParam)
                                }),
                                " ",
                            ))
                        }
                    });
                let tags_docs = alloc.intersperse(tags_docs, alloc.text(", "));
                alloc
                    .text("[<rnw>")
                    .append(tags_docs)
                    .append(alloc.text("]"))
            }
        }
    }

    pub fn layout_at<I>(self, interner: &mut I, tag_id: TagIdIntType, index: usize) -> InLayout<'a>
    where
        I: LayoutInterner<'a>,
    {
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

                other_fields[index]
            }
        };

        // TODO(recursive-layouts): simplify after we have disjoint recursive pointers
        if let LayoutRepr::RecursivePointer(_) = interner.get_repr(result) {
            interner.insert_direct_no_semantic(LayoutRepr::Union(self))
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

    pub fn tag_id_layout(&self) -> InLayout<'static> {
        self.discriminant().layout()
    }

    fn stores_tag_id_in_pointer_bits(tags: &[&[InLayout<'a>]], target: Target) -> bool {
        tags.len() < target.ptr_width() as usize
    }

    pub const POINTER_MASK_32BIT: usize = 0b0000_0111;
    pub const POINTER_MASK_64BIT: usize = 0b0000_0011;

    pub fn tag_id_pointer_bits_and_mask(target: Target) -> (usize, usize) {
        match target.ptr_width() {
            PtrWidth::Bytes8 => (3, Self::POINTER_MASK_64BIT),
            PtrWidth::Bytes4 => (2, Self::POINTER_MASK_32BIT),
        }
    }

    // i.e. it is not implicit and not stored in the pointer bits
    pub fn stores_tag_id_as_data(&self, target: Target) -> bool {
        match self {
            UnionLayout::NonRecursive(_) => true,
            UnionLayout::Recursive(tags)
            | UnionLayout::NullableWrapped {
                other_tags: tags, ..
            } => !Self::stores_tag_id_in_pointer_bits(tags, target),
            UnionLayout::NonNullableUnwrapped(_) | UnionLayout::NullableUnwrapped { .. } => false,
        }
    }

    pub fn stores_tag_id_in_pointer(&self, target: Target) -> bool {
        match self {
            UnionLayout::NonRecursive(_) => false,
            UnionLayout::Recursive(tags)
            | UnionLayout::NullableWrapped {
                other_tags: tags, ..
            } => Self::stores_tag_id_in_pointer_bits(tags, target),
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

    fn tags_alignment_bytes<I>(interner: &I, tags: &[&'a [InLayout<'a>]]) -> u32
    where
        I: LayoutInterner<'a>,
    {
        tags.iter()
            .map(|field_layouts| LayoutRepr::struct_(field_layouts).alignment_bytes(interner))
            .max()
            .unwrap_or(1)
    }

    pub fn allocation_alignment_bytes<I>(&self, interner: &I) -> u32
    where
        I: LayoutInterner<'a>,
    {
        let allocation = match self {
            UnionLayout::NonRecursive(tags) => Self::tags_alignment_bytes(interner, tags),
            UnionLayout::Recursive(tags) => Self::tags_alignment_bytes(interner, tags),
            UnionLayout::NonNullableUnwrapped(field_layouts) => {
                LayoutRepr::struct_(field_layouts).alignment_bytes(interner)
            }
            UnionLayout::NullableWrapped { other_tags, .. } => {
                Self::tags_alignment_bytes(interner, other_tags)
            }
            UnionLayout::NullableUnwrapped { other_fields, .. } => {
                LayoutRepr::struct_(other_fields).alignment_bytes(interner)
            }
        };

        // because we store a refcount, the alignment must be at least the size of a pointer
        allocation.max(interner.target().ptr_width() as u32)
    }

    /// Size of the data in memory, whether it's stack or heap (for non-null tag ids)
    pub fn data_size_and_alignment<I>(&self, interner: &I) -> (u32, u32)
    where
        I: LayoutInterner<'a>,
    {
        let (data_width, data_align) = self.data_size_and_alignment_help_match(interner);

        if self.stores_tag_id_as_data(interner.target()) {
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
    pub fn data_size_without_tag_id<I>(&self, interner: &I) -> Option<u32>
    where
        I: LayoutInterner<'a>,
    {
        if !self.stores_tag_id_as_data(interner.target()) {
            return None;
        };

        Some(self.data_size_and_alignment_help_match(interner).0)
    }

    fn data_size_and_alignment_help_match<I>(&self, interner: &I) -> (u32, u32)
    where
        I: LayoutInterner<'a>,
    {
        match self {
            Self::NonRecursive(tags) => Layout::stack_size_and_alignment_slices(interner, tags),
            Self::Recursive(tags) => Layout::stack_size_and_alignment_slices(interner, tags),
            Self::NonNullableUnwrapped(fields) => {
                Layout::stack_size_and_alignment_slices(interner, &[fields])
            }
            Self::NullableWrapped { other_tags, .. } => {
                Layout::stack_size_and_alignment_slices(interner, other_tags)
            }
            Self::NullableUnwrapped { other_fields, .. } => {
                Layout::stack_size_and_alignment_slices(interner, &[other_fields])
            }
        }
    }

    pub fn tag_id_offset<I>(&self, interner: &I) -> Option<u32>
    where
        I: LayoutInterner<'a>,
    {
        match self {
            UnionLayout::NonRecursive(tags)
            | UnionLayout::Recursive(tags)
            | UnionLayout::NullableWrapped {
                other_tags: tags, ..
            } => Some(Self::tag_id_offset_help(interner, tags)),
            UnionLayout::NonNullableUnwrapped(_) | UnionLayout::NullableUnwrapped { .. } => None,
        }
    }

    fn tag_id_offset_help<I>(interner: &I, layouts: &[&[InLayout<'a>]]) -> u32
    where
        I: LayoutInterner<'a>,
    {
        let (data_width, data_align) = Layout::stack_size_and_alignment_slices(interner, layouts);

        round_up_to_alignment(data_width, data_align)
    }

    /// Very important to use this when doing a memcpy!
    fn stack_size_without_alignment<I>(&self, interner: &I) -> u32
    where
        I: LayoutInterner<'a>,
    {
        match self {
            UnionLayout::NonRecursive(_) => {
                let (width, align) = self.data_size_and_alignment(interner);
                round_up_to_alignment(width, align)
            }
            UnionLayout::Recursive(_)
            | UnionLayout::NonNullableUnwrapped(_)
            | UnionLayout::NullableWrapped { .. }
            | UnionLayout::NullableUnwrapped { .. } => interner.target().ptr_width() as u32,
        }
    }

    pub fn is_recursive(&self) -> bool {
        use UnionLayout::*;

        match self {
            NonRecursive(_) => false,
            Recursive(_)
            | NonNullableUnwrapped(_)
            | NullableWrapped { .. }
            | NullableUnwrapped { .. } => true,
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

    pub fn alignment_bytes(&self) -> u32 {
        self.stack_size().max(1)
    }

    pub const fn layout(&self) -> InLayout<'static> {
        // TODO is it beneficial to return a more specific layout?
        // e.g. Layout::bool() and Layout::VOID
        match self {
            Discriminant::U0 => Layout::U8,
            Discriminant::U1 => Layout::U8,
            Discriminant::U8 => Layout::U8,
            Discriminant::U16 => Layout::U16,
        }
    }
}

/// Custom type so we can get the numeric representation of a symbol in tests (so `#UserApp.3`
/// instead of `UserApp.foo`). The pretty name is not reliable when running many tests
/// concurrently. The number does not change and will give a reliable output.
struct SetElement<'a> {
    symbol: Symbol,
    layout: &'a [InLayout<'a>],
}

impl std::fmt::Debug for SetElement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol_string = crate::ir::symbol_to_doc_string(self.symbol, false);

        write!(f, "( {}, {:?})", symbol_string, self.layout)
    }
}

impl std::fmt::Debug for LambdaSet<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct Helper<'a> {
            set: &'a [(Symbol, &'a [InLayout<'a>])],
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
            .field("args", &self.args)
            .field("ret", &self.ret)
            .field("representation", &self.representation)
            .field("full_layout", &self.full_layout)
            .finish()
    }
}

/// See [Niche].
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum NichePriv<'a> {
    /// Distinguishes captures this proc takes, when it is a part of a lambda set that has multiple
    /// lambdas of the same name, but different captures.
    Captures(&'a [InLayout<'a>]),
}

/// Niches identify lambdas (including thunks) in ways that are not distinguishable solely by their
/// [runtime function layout][RawFunctionLayout].
///
/// Currently, there are two kinds of niches.
///
/// # Captures niches
///
/// Captures niches identify a procedure's set of captured symbols. This is relevant when a
/// procedure is part of a lambda set that has multiple lambdas of the procedure's name, but each
/// has a different set of captures.
///
/// The capture set is identified only in the body of a procedure and not in its runtime layout.
/// Any capturing lambda takes the whole lambda set as an argument, rather than just its captures.
/// A captures niche can be attached to a [lambda name][LambdaName] to uniquely identify lambdas
/// in these scenarios.
///
/// Procedure names with captures niches are typically produced by [find_lambda_name][LambdaSet::find_lambda_name].
/// Captures niches are irrelevant for thunks.
///
/// ## Example
///
/// `fun` has lambda set `[[forcer U64, forcer U8]]` in the following program:
///
/// ```roc
/// capture : _ -> ({} -> Str)
/// capture = \val ->
///     forcer = \{} -> Num.to_str val
///     forcer
///
/// fun = \x ->
///     when x is
///         True -> capture 123u64
///         False -> capture 18u8
/// ```
///
/// By recording the captures layouts each `forcer` expects, we can distinguish
/// between such differences when constructing the closure capture data that is
/// return value of `fun`.
///
/// See also https://github.com/roc-lang/roc/issues/3336.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Niche<'a>(NichePriv<'a>);

impl<'a> Niche<'a> {
    pub const NONE: Niche<'a> = Niche(NichePriv::Captures(&[]));

    pub fn to_doc<'b, D, A, I>(
        self,
        alloc: &'b D,
        interner: &I,
        seen_rec: &mut SeenRecPtrs<'a>,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
        I: LayoutInterner<'a>,
    {
        match self.0 {
            NichePriv::Captures(captures) => alloc.concat([
                alloc.reflow("(niche {"),
                alloc.intersperse(
                    captures
                        .iter()
                        .map(|c| interner.to_doc(*c, alloc, seen_rec, Parens::NotNeeded)),
                    alloc.reflow(", "),
                ),
                alloc.reflow("})"),
            ]),
        }
    }

    pub fn dbg_deep<'r, I: LayoutInterner<'a>>(
        &'r self,
        interner: &'r I,
    ) -> crate::layout::intern::dbg_deep::DbgFields<'a, 'r, I> {
        let NichePriv::Captures(caps) = &self.0;
        interner.dbg_deep_iter(caps)
    }

    pub fn dbg_stable<'r, I: LayoutInterner<'a>>(
        &'r self,
        interner: &'r I,
    ) -> crate::layout::intern::dbg_stable::DbgFields<'a, 'r, I> {
        let NichePriv::Captures(caps) = &self.0;
        interner.dbg_stable_iter(caps)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct LambdaName<'a> {
    name: Symbol,
    niche: Niche<'a>,
}

impl<'a> LambdaName<'a> {
    pub(crate) fn from_captures(symbol: Symbol, captures: &'a [InLayout<'a>]) -> Self {
        Self {
            name: symbol,
            niche: Niche(NichePriv::Captures(captures)),
        }
    }

    #[inline(always)]
    pub fn name(&self) -> Symbol {
        self.name
    }

    #[inline(always)]
    pub fn niche(&self) -> Niche<'a> {
        self.niche
    }

    #[inline(always)]
    pub(crate) fn no_captures(&self) -> bool {
        match self.niche.0 {
            NichePriv::Captures(captures) => captures.is_empty(),
        }
    }

    #[inline(always)]
    pub fn no_niche(name: Symbol) -> Self {
        Self {
            name,
            niche: Niche::NONE,
        }
    }

    #[inline(always)]
    pub(crate) fn replace_name(&self, name: Symbol) -> Self {
        Self { name, ..*self }
    }
}

/// Closure data for a function
#[derive(Debug, Clone, Copy)]
pub(crate) enum ClosureDataKind<'a> {
    /// The function is compiled with lambda sets.
    LambdaSet(LambdaSet<'a>),
    /// The function is compiled as type-erased.
    Erased,
}

impl<'a> ClosureDataKind<'a> {
    pub fn data_layout(&self) -> InLayout<'a> {
        match self {
            Self::LambdaSet(lambda_set) => lambda_set.full_layout,
            Self::Erased => Layout::ERASED,
        }
    }
}

fn build_function_closure_data<'a>(
    env: &mut Env<'a, '_>,
    args: VariableSubsSlice,
    closure_var: Variable,
    ret_var: Variable,
) -> Cacheable<Result<ClosureDataKind<'a>, LayoutProblem>> {
    match env.subs.get_content_without_compacting(closure_var) {
        Content::ErasedLambda => cacheable(Ok(ClosureDataKind::Erased)),
        _ => LambdaSet::from_var(env, args, closure_var, ret_var)
            .map(|result| result.map(ClosureDataKind::LambdaSet)),
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LambdaSet<'a> {
    pub(crate) args: &'a &'a [InLayout<'a>],
    pub(crate) ret: InLayout<'a>,
    /// collection of function names and their closure arguments
    // Double reference to cut from fat slice (16 bytes) to 8 bytes
    pub(crate) set: &'a &'a [(Symbol, &'a [InLayout<'a>])],
    /// how the closure will be represented at runtime
    pub(crate) representation: InLayout<'a>,
    /// The interned [Layout] representation of the lambda set, as `Layout::LambdaSet(self)`.
    pub(crate) full_layout: InLayout<'a>,
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
        alphabetic_order_fields: &'a [InLayout<'a>],
        closure_name: Symbol,
        tag_id: TagIdIntType,
        union_layout: UnionLayout<'a>,
    },
    /// The closure is one function, whose captures are represented as a struct.
    /// The layouts are sorted alphabetically by the identifier that is captured.
    ///
    /// We MUST sort these according to their stack size before code gen!
    AlphabeticOrderStruct(&'a [InLayout<'a>]),
    /// The closure is one function that captures a single identifier, whose value is unwrapped.
    UnwrappedCapture(InLayout<'a>),
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
    Struct(&'a [InLayout<'a>]),
    /// The closure is one function that captures a single identifier, whose value is unwrapped.
    UnwrappedCapture(InLayout<'a>),
    /// The closure dispatches to multiple possible functions, none of which capture.
    EnumDispatch(EnumDispatch),
}

impl<'a> LambdaSet<'a> {
    pub fn runtime_representation(&self) -> InLayout<'a> {
        self.representation
    }

    /// Does the lambda set contain the given symbol?
    pub fn contains(&self, symbol: Symbol) -> bool {
        self.set.iter().any(|(s, _)| *s == symbol)
    }

    pub fn is_represented<I>(&self, interner: &I) -> Option<InLayout<'a>>
    where
        I: LayoutInterner<'a>,
    {
        if self.has_unwrapped_capture_repr() {
            let repr = self.representation;
            Some(repr)
        } else if self.has_enum_dispatch_repr() {
            None
        } else {
            let repr = self.representation;
            match interner.get_repr(repr) {
                LayoutRepr::Struct(&[]) => None,
                _ => Some(repr),
            }
        }
    }

    pub fn iter_set(&self) -> impl ExactSizeIterator<Item = LambdaName<'a>> {
        self.set.iter().map(|(name, captures_layouts)| {
            let niche = match captures_layouts {
                [] => Niche::NONE,
                _ => Niche(NichePriv::Captures(captures_layouts)),
            };
            LambdaName { name: *name, niche }
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
        I: LayoutInterner<'a>,
    {
        debug_assert!(self.contains(lambda_name.name));

        let NichePriv::Captures(captures) = lambda_name.niche.0;

        let comparator = |other_name: Symbol, other_captures_layouts: &[InLayout]| {
            other_name == lambda_name.name
                // Make sure all captures are equal
                && other_captures_layouts
                    .iter()
                    .eq(captures)
        };

        self.layout_for_member(interner, comparator)
    }

    /// Finds an alias name for a possible-multimorphic lambda variant in the lambda set.
    pub fn find_lambda_name<I>(
        &self,
        interner: &I,
        function_symbol: Symbol,
        captures_layouts: &[InLayout<'a>],
    ) -> LambdaName<'a>
    where
        I: LayoutInterner<'a>,
    {
        debug_assert!(
            self.contains(function_symbol),
            "function symbol {function_symbol:?} not in set {self:?}"
        );

        let comparator = |other_name: Symbol, other_captures_layouts: &[InLayout<'a>]| {
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
            niche: Niche(NichePriv::Captures(layouts)),
        }
    }

    /// Checks if two captured layouts are equivalent under the current lambda set.
    /// Resolves recursive pointers to the layout of the lambda set.
    fn capture_layouts_eq<I>(&self, interner: &I, left: &InLayout<'a>, right: &InLayout<'a>) -> bool
    where
        I: LayoutInterner<'a>,
    {
        interner.equiv(*left, *right)
    }

    fn layout_for_member<I, F>(&self, interner: &I, comparator: F) -> ClosureRepresentation<'a>
    where
        I: LayoutInterner<'a>,
        F: Fn(Symbol, &[InLayout]) -> bool,
    {
        if self.has_unwrapped_capture_repr() {
            // Only one function, that captures one identifier.
            return ClosureRepresentation::UnwrappedCapture(self.representation);
        }

        let repr_layout = interner.chase_recursive(self.representation);

        match repr_layout {
            LayoutRepr::Union(union) => {
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
                            union_layout: union,
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
                            union_layout: union,
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
                            union_layout: union,
                        }
                    }
                    UnionLayout::NullableWrapped {
                        nullable_id: _,
                        other_tags: _,
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
                            union_layout: union,
                        }
                    }
                    UnionLayout::NonNullableUnwrapped(_) => internal_error!("I thought a non-nullable-unwrapped variant for a lambda set was impossible: how could such a lambda set be created without a base case?"),
                }
            }
            LayoutRepr::Struct { .. } => {
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
                debug_assert!(self.has_enum_dispatch_repr());
                let enum_repr = match layout {
                    LayoutRepr::Builtin(Builtin::Bool) => EnumDispatch::Bool,
                    LayoutRepr::Builtin(Builtin::Int(IntWidth::U8)) => EnumDispatch::U8,
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
        I: LayoutInterner<'a>,
    {
        if self.has_unwrapped_capture_repr() {
            return ClosureCallOptions::UnwrappedCapture(self.representation);
        }

        let repr_layout = interner.chase_recursive(self.representation);

        match repr_layout {
            LayoutRepr::Union(union_layout) => {
                if repr_layout == Layout::VOID_NAKED.repr(interner) {
                    debug_assert!(self.set.is_empty());
                    return ClosureCallOptions::Void;
                }
                ClosureCallOptions::Union(union_layout)
            }
            LayoutRepr::Struct(field_layouts) => {
                debug_assert_eq!(self.set.len(), 1);
                ClosureCallOptions::Struct(field_layouts)
            }
            layout => {
                debug_assert!(self.has_enum_dispatch_repr());
                let enum_repr = match layout {
                    LayoutRepr::Builtin(Builtin::Bool) => EnumDispatch::Bool,
                    LayoutRepr::Builtin(Builtin::Int(IntWidth::U8)) => EnumDispatch::U8,
                    other => internal_error!("Invalid layout for enum dispatch: {:?}", other),
                };
                ClosureCallOptions::EnumDispatch(enum_repr)
            }
        }
    }

    /// If `lambda_name` captures, extend the arguments to the lambda with the lambda set, from
    /// which the lambda should extract its captures from.
    ///
    /// If `lambda_name` doesn't capture, the arguments are unaffected.
    pub(crate) fn extend_argument_list_for_named(
        &self,
        arena: &'a Bump,
        lambda_name: LambdaName<'a>,
        argument_layouts: &'a [InLayout<'a>],
    ) -> &'a [InLayout<'a>] {
        let Niche(NichePriv::Captures(captures)) = lambda_name.niche;
        // TODO(https://github.com/roc-lang/roc/issues/4831): we should turn on this debug-assert;
        // however, currently it causes false-positives, because host-exposed functions that are
        // function pointers to platform-exposed functions are compiled as if they are proper
        // functions, despite not appearing in the lambda set.
        // We don't want to compile them as thunks, so we need to figure out a special-casing for
        // them.
        // To reproduce: test cli_tests
        //
        // debug_assert!(
        //     self.set
        //         .contains(&(lambda_name.name, lambda_name.captures_niche.0)),
        //     "{:?}",
        //     (self, lambda_name)
        // );

        // If we don't capture, there is nothing to extend.
        if captures.is_empty() {
            argument_layouts
        } else {
            let mut arguments = Vec::with_capacity_in(argument_layouts.len() + 1, arena);
            arguments.extend(argument_layouts);
            arguments.push(self.full_layout);

            arguments.into_bump_slice()
        }
    }

    pub fn from_var_pub(
        cache: &mut LayoutCache<'a>,
        arena: &'a Bump,
        subs: &Subs,
        args: VariableSubsSlice,
        closure_var: Variable,
        ret_var: Variable,
    ) -> Result<Self, LayoutProblem> {
        let mut env = Env::from_components(cache, subs, arena);
        Self::from_var(&mut env, args, closure_var, ret_var).value()
    }

    fn from_var(
        env: &mut Env<'a, '_>,
        args: VariableSubsSlice,
        closure_var: Variable,
        ret_var: Variable,
    ) -> Cacheable<Result<Self, LayoutProblem>> {
        let Cacheable(result, criteria) = env.cached_or(closure_var, |env| {
            let Cacheable(result, criteria) = Self::from_var_help(env, args, closure_var, ret_var);
            let result = result.map(|l| l.full_layout);
            Cacheable(result, criteria)
        });

        match result.map(|l| env.cache.interner.chase_recursive(l)) {
            Ok(LayoutRepr::LambdaSet(lambda_set)) => Cacheable(Ok(lambda_set), criteria),
            Err(err) => Cacheable(Err(err), criteria),
            Ok(layout) => internal_error!("other layout found for lambda set: {:?}", layout),
        }
    }

    fn from_var_help(
        env: &mut Env<'a, '_>,
        args: VariableSubsSlice,
        closure_var: Variable,
        ret_var: Variable,
    ) -> Cacheable<Result<Self, LayoutProblem>> {
        roc_tracing::debug!(var = ?closure_var, size = ?lambda_set_size(env.subs, closure_var), "building lambda set layout");

        let lambda_set = resolve_lambda_set(env.subs, closure_var);

        let mut cache_criteria = CACHEABLE;

        let mut fn_args = Vec::with_capacity_in(args.len(), env.arena);

        for index in args.into_iter() {
            let arg_var = env.subs[index];
            let layout = cached!(Layout::from_var(env, arg_var), cache_criteria, env.subs);
            fn_args.push(layout);
        }

        let ret = cached!(Layout::from_var(env, ret_var), cache_criteria, env.subs);

        let fn_args = env.arena.alloc(fn_args.into_bump_slice());

        match lambda_set {
            ResolvedLambdaSet::Set(mut lambdas, opt_recursion_var) => {
                // sort the tags; make sure ordering stays intact!
                lambdas.sort_by_key(|(sym, _)| *sym);

                let mut set: Vec<(Symbol, &[InLayout])> =
                    Vec::with_capacity_in(lambdas.len(), env.arena);
                let mut set_with_variables: std::vec::Vec<(&Symbol, &[Variable])> =
                    std::vec::Vec::with_capacity(lambdas.len());
                let mut set_captures_have_naked_rec_ptr = false;

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
                        let arg = cached!(Layout::from_var(env, *var), criteria, env.subs);
                        arguments.push(arg);
                        set_captures_have_naked_rec_ptr =
                            set_captures_have_naked_rec_ptr || criteria.has_naked_recursion_pointer;
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

                    if let Some(rec_var) = opt_recursion_var.into_variable() {
                        env.remove_seen(rec_var);
                    }
                }

                let (set, set_with_variables) = if has_duplicate_lambda_names {
                    // If we have a lambda set with duplicate names, then we sort first by name,
                    // and break ties by sorting on the layout. We need to do this again since the
                    // first sort would not have sorted on the layout.

                    // TODO: be more efficient, we can compute the permutation once and then apply
                    // it to both vectors.
                    let mut joined = set
                        .into_iter()
                        .zip(set_with_variables)
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
                cache_criteria.and(criteria, env.subs);

                let needs_recursive_fixup = NeedsRecursionPointerFixup(
                    opt_recursion_var.is_some() && set_captures_have_naked_rec_ptr,
                );

                let lambda_set = env.cache.interner.insert_lambda_set(
                    env.arena,
                    fn_args,
                    ret,
                    env.arena.alloc(set.into_bump_slice()),
                    needs_recursive_fixup,
                    representation,
                );

                Cacheable(Ok(lambda_set), cache_criteria)
            }
            ResolvedLambdaSet::Unbound => {
                // The lambda set is unbound which means it must be unused. Just give it the empty lambda set.
                // See also https://github.com/roc-lang/roc/issues/3163.
                let lambda_set = env.cache.interner.insert_lambda_set(
                    env.arena,
                    fn_args,
                    ret,
                    &(&[] as &[(Symbol, &[InLayout])]),
                    NeedsRecursionPointerFixup(false),
                    Layout::UNIT,
                );
                Cacheable(Ok(lambda_set), cache_criteria)
            }
        }
    }

    fn make_representation(
        env: &mut Env<'a, '_>,
        set: std::vec::Vec<(&Symbol, &[Variable])>,
        opt_rec_var: Option<Variable>,
    ) -> Cacheable<InLayout<'a>> {
        let union_labels = UnsortedUnionLabels { tags: set };

        // Even if a variant in the lambda set has uninhabitable captures (and is hence
        // unreachable as a function), we want to keep it in the representation. Failing to do so
        // risks dropping relevant specializations needed during monomorphization.
        let drop_uninhabited_variants = DropUninhabitedVariants(false);

        match opt_rec_var {
            Some(rec_var) => {
                let Cacheable(result, criteria) =
                    layout_from_recursive_union(env, rec_var, &union_labels);
                let result = result.expect("unable to create lambda set representation");
                Cacheable(result, criteria)
            }

            None => layout_from_non_recursive_union(env, &union_labels, drop_uninhabited_variants),
        }
    }

    pub fn stack_size<I>(&self, interner: &I) -> u32
    where
        I: LayoutInterner<'a>,
    {
        interner.get_repr(self.representation).stack_size(interner)
    }
    pub fn contains_refcounted<I>(&self, interner: &I) -> bool
    where
        I: LayoutInterner<'a>,
    {
        interner
            .get_repr(self.representation)
            .contains_refcounted(interner)
    }
    pub fn safe_to_memcpy<I>(&self, interner: &I) -> bool
    where
        I: LayoutInterner<'a>,
    {
        interner
            .get_repr(self.representation)
            .safe_to_memcpy(interner)
    }

    pub fn alignment_bytes<I>(&self, interner: &I) -> u32
    where
        I: LayoutInterner<'a>,
    {
        interner
            .get_repr(self.representation)
            .alignment_bytes(interner)
    }
}

enum ResolvedLambdaSet {
    Set(
        std::vec::Vec<(Symbol, std::vec::Vec<Variable>)>,
        OptVariable,
    ),
    /// The lambda set is empty, that means this function is never called.
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
                FlatType::Func(args, lset, ret, _fx_var) => {
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
                FlatType::Tuple(elems, ext) => {
                    for var_index in elems.iter_variables() {
                        let var = subs[var_index];
                        stack.push((var, depth_any + 1, depth_lset));
                    }
                    stack.push((*ext, depth_any + 1, depth_lset));
                }
                FlatType::FunctionOrTagUnion(_, _, ext) => {
                    stack.push((ext.var(), depth_any + 1, depth_lset));
                }
                FlatType::TagUnion(tags, ext) => {
                    for (_, payloads) in tags.iter_from_subs(subs) {
                        for payload in payloads {
                            stack.push((*payload, depth_any + 1, depth_lset));
                        }
                    }
                    stack.push((ext.var(), depth_any + 1, depth_lset));
                }
                FlatType::RecursiveTagUnion(rec_var, tags, ext) => {
                    seen_rec_vars.insert(*rec_var);
                    for (_, payloads) in tags.iter_from_subs(subs) {
                        for payload in payloads {
                            stack.push((*payload, depth_any + 1, depth_lset));
                        }
                    }
                    stack.push((ext.var(), depth_any + 1, depth_lset));
                }
                FlatType::EmptyRecord | FlatType::EmptyTagUnion | FlatType::EffectfulFunc => {}
            },
            Content::FlexVar(_)
            | Content::RigidVar(_)
            | Content::FlexAbleVar(_, _)
            | Content::RigidAbleVar(_, _)
            | Content::RangedNumber(_)
            | Content::Error
            | Content::ErasedLambda
            | Content::Pure
            | Content::Effectful => {}
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
    List(InLayout<'a>),
}

#[macro_export]
macro_rules! list_element_layout {
    ($interner:expr, $list_layout:expr) => {
        match $interner.get_repr($list_layout) {
            LayoutRepr::Builtin(Builtin::List(list_layout)) => list_layout,
            _ => internal_error!("invalid list layout"),
        }
    };
}

pub struct Env<'a, 'b> {
    pub(crate) arena: &'a Bump,
    seen: Vec<'a, Variable>,
    pub(crate) subs: &'b Subs,
    cache: &'b mut LayoutCache<'a>,
}

impl<'a, 'b> Env<'a, 'b> {
    pub fn from_components(
        cache: &'b mut LayoutCache<'a>,
        subs: &'b Subs,
        arena: &'a Bump,
    ) -> Self {
        Self {
            cache,
            subs,
            seen: Vec::new_in(arena),
            arena,
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
    fn can_reuse_cached(&self, var: Variable, cache_metadata: &CacheMeta) -> bool {
        let CacheMeta {
            recursive_structures,
        } = cache_metadata;
        for &recursive_structure in recursive_structures.iter() {
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

            if $self.can_reuse_cached($var, &metadata) {
                // Happy path - the cached layout can be reused, return it immediately.
                return Cacheable(result, metadata.into_criteria());
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
            // When this recursion pointer gets used in a recursive union, it will be filled to
            // looop back to the correct layout.
            // TODO(recursive-layouts): after the naked pointer is updated, we can cache `var` to
            // point to the updated layout.
            let rec_ptr = Layout::NAKED_RECURSIVE_PTR;
            return Cacheable(Ok(rec_ptr), NAKED_RECURSION_PTR);
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

pub fn round_up_to_alignment(width: u32, alignment: u32) -> u32 {
    match alignment {
        0 => panic!("Alignment invalid: Got 0, but alignment must be at least 1"),
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
    pub(crate) const fn new(repr: LayoutWrapper<'a>, semantic: SemanticRepr<'a>) -> Self {
        Self { repr, semantic }
    }

    pub(crate) const fn no_semantic(repr: LayoutWrapper<'a>) -> Self {
        Self {
            repr,
            semantic: SemanticRepr::NONE,
        }
    }

    pub(crate) fn repr<I>(&self, interner: &I) -> LayoutRepr<'a>
    where
        I: LayoutInterner<'a>,
    {
        let mut lay = *self;
        loop {
            match lay.repr {
                LayoutWrapper::Direct(repr) => return repr,
                LayoutWrapper::Newtype(real) => {
                    lay = interner.get(real);
                }
            }
        }
    }

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
            FlexAbleVar(_, _) | RigidAbleVar(_, _) => {
                roc_debug_flags::dbg_do!(roc_debug_flags::ROC_NO_UNBOUND_LAYOUT, {
                    todo_abilities!("Able var is unbound!");
                });

                // If we encounter an unbound type var (e.g. `*` or `a`)
                // then it's zero-sized; In the future we may drop this argument
                // completely, but for now we represent it with the empty tag union
                cacheable(Ok(Layout::VOID))
            }
            RecursionVar { structure, .. } => {
                let structure_content = env.subs.get_content_without_compacting(structure);
                Self::new_help(env, structure, *structure_content)
            }
            LambdaSet(_) => {
                internal_error!("lambda set should only appear under a function, where it's handled independently.");
            }
            ErasedLambda => {
                internal_error!("erased lambda type should only appear under a function, where it's handled independently.");
            }
            Pure | Effectful => {
                internal_error!("fx vars should only appear under a function, where they're handled independently.");
            }
            Structure(flat_type) => layout_from_flat_type(env, flat_type),

            Alias(symbol, _args, actual_var, _) => {
                if let Some(int_width) = IntWidth::try_from_symbol(symbol) {
                    return cacheable(Ok(Layout::int_width(int_width)));
                }

                if let Some(float_width) = FloatWidth::try_from_symbol(symbol) {
                    return cacheable(Ok(Layout::float_width(float_width)));
                }

                match symbol {
                    Symbol::NUM_DECIMAL => cacheable(Ok(Layout::DEC)),

                    Symbol::NUM_NUM | Symbol::NUM_INT | Symbol::NUM_INTEGER
                        if is_unresolved_var(env.subs, actual_var) =>
                    {
                        // default to i64
                        cacheable(Ok(Layout::default_integer()))
                    }

                    Symbol::NUM_FRAC | Symbol::NUM_FLOATINGPOINT
                        if is_unresolved_var(env.subs, actual_var)
                            || is_any_float_range(env.subs, actual_var) =>
                    {
                        // default to f64
                        cacheable(Ok(Layout::default_float()))
                    }

                    _ => Self::from_var(env, actual_var),
                }
            }

            RangedNumber(range) => Self::layout_from_ranged_number(range),

            Error => cacheable(Err(LayoutProblem::Erroneous)),
        }
    }

    pub const fn from_int_width(int_width: IntWidth) -> InLayout<'static> {
        match int_width {
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
    }

    fn layout_from_ranged_number(range: NumericRange) -> Cacheable<LayoutResult<'a>> {
        // We don't pass the range down because `RangedNumber`s are somewhat rare, they only
        // appear due to number literals, so no need to increase parameter list sizes.
        let num_layout = range.default_compilation_width();

        cacheable(Ok(Layout::int_literal_width_to_int(num_layout)))
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

    pub fn stack_size_and_alignment_slices<I>(
        interner: &I,
        slices: &[&[InLayout<'a>]],
    ) -> (u32, u32)
    where
        I: LayoutInterner<'a>,
    {
        let mut data_align = 1;
        let mut data_width = 0;

        for tag in slices {
            let mut total = 0;
            for layout in tag.iter() {
                let (stack_size, alignment) = interner
                    .get_repr(*layout)
                    .stack_size_and_alignment(interner);
                total += stack_size;
                data_align = data_align.max(alignment);
            }

            data_width = data_width.max(total);
        }

        data_width = round_up_to_alignment(data_width, data_align);

        (data_width, data_align)
    }

    pub fn runtime_representation<I>(&self, interner: &I) -> Self
    where
        I: LayoutInterner<'a>,
    {
        use LayoutRepr::*;
        match self.repr(interner) {
            LambdaSet(lambda_set) => interner.get(lambda_set.runtime_representation()),
            _ => *self,
        }
    }

    pub fn runtime_representation_in<I>(layout: InLayout<'a>, interner: &I) -> InLayout<'a>
    where
        I: LayoutInterner<'a>,
    {
        use LayoutRepr::*;
        match interner.get_repr(layout) {
            LambdaSet(lambda_set) => lambda_set.runtime_representation(),
            _ => layout,
        }
    }
}

impl<'a> LayoutRepr<'a> {
    pub const UNIT: Self = LayoutRepr::struct_(&[]);
    pub const BOOL: Self = LayoutRepr::Builtin(Builtin::Bool);
    pub const U8: Self = LayoutRepr::Builtin(Builtin::Int(IntWidth::U8));
    pub const U16: Self = LayoutRepr::Builtin(Builtin::Int(IntWidth::U16));
    pub const U32: Self = LayoutRepr::Builtin(Builtin::Int(IntWidth::U32));
    pub const U64: Self = LayoutRepr::Builtin(Builtin::Int(IntWidth::U64));
    pub const U128: Self = LayoutRepr::Builtin(Builtin::Int(IntWidth::U128));
    pub const I8: Self = LayoutRepr::Builtin(Builtin::Int(IntWidth::I8));
    pub const I16: Self = LayoutRepr::Builtin(Builtin::Int(IntWidth::I16));
    pub const I32: Self = LayoutRepr::Builtin(Builtin::Int(IntWidth::I32));
    pub const I64: Self = LayoutRepr::Builtin(Builtin::Int(IntWidth::I64));
    pub const I128: Self = LayoutRepr::Builtin(Builtin::Int(IntWidth::I128));
    pub const F32: Self = LayoutRepr::Builtin(Builtin::Float(FloatWidth::F32));
    pub const F64: Self = LayoutRepr::Builtin(Builtin::Float(FloatWidth::F64));
    pub const DEC: Self = LayoutRepr::Builtin(Builtin::Decimal);
    pub const STR: Self = LayoutRepr::Builtin(Builtin::Str);
    pub const OPAQUE_PTR: Self = LayoutRepr::Ptr(Layout::VOID);
    pub const ERASED: Self = LayoutRepr::Erased(Erased);

    pub const fn struct_(field_layouts: &'a [InLayout<'a>]) -> Self {
        Self::Struct(field_layouts)
    }

    pub(crate) const fn direct(self) -> LayoutWrapper<'a> {
        LayoutWrapper::Direct(self)
    }

    pub fn safe_to_memcpy<I>(&self, interner: &I) -> bool
    where
        I: LayoutInterner<'a>,
    {
        use LayoutRepr::*;

        match self {
            Builtin(builtin) => builtin.safe_to_memcpy(),
            Struct(field_layouts) => field_layouts
                .iter()
                .all(|field_layout| interner.get_repr(*field_layout).safe_to_memcpy(interner)),
            Union(variant) => {
                use UnionLayout::*;

                match variant {
                    NonRecursive(tags) => tags.iter().all(|tag_layout| {
                        tag_layout
                            .iter()
                            .all(|field| interner.get_repr(*field).safe_to_memcpy(interner))
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
            LambdaSet(lambda_set) => interner
                .get_repr(lambda_set.runtime_representation())
                .safe_to_memcpy(interner),
            Ptr(_) | RecursivePointer(_) => {
                // We cannot memcpy pointers, because then we would have the same pointer in multiple places!
                false
            }
            Erased(e) => e.safe_to_memcpy(),
            FunctionPointer(..) => true,
        }
    }

    pub fn is_dropped_because_empty(&self) -> bool {
        // For this calculation, we don't need an accurate
        // stack size, we just need to know whether it's zero,
        // so it's fine to use a pointer size of 1.
        false // TODO this should use is_zero_sized once doing so doesn't break things!
    }

    pub fn is_passed_by_reference<I>(&self, interner: &I) -> bool
    where
        I: LayoutInterner<'a>,
    {
        match self {
            LayoutRepr::Builtin(builtin) => {
                use Builtin::*;

                match interner.target().ptr_width() {
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
            LayoutRepr::Union(UnionLayout::NonRecursive(_)) => true,
            LayoutRepr::Struct(_) => {
                // TODO: write tests for this!
                self.stack_size(interner) as usize > interner.target().max_by_value_size()
            }

            LayoutRepr::LambdaSet(lambda_set) => interner
                .get_repr(lambda_set.runtime_representation())
                .is_passed_by_reference(interner),
            _ => false,
        }
    }

    pub fn stack_size<I>(&self, interner: &I) -> u32
    where
        I: LayoutInterner<'a>,
    {
        let width = self.stack_size_without_alignment(interner);
        let alignment = self.alignment_bytes(interner);

        round_up_to_alignment(width, alignment)
    }

    pub fn stack_size_and_alignment<I>(&self, interner: &I) -> (u32, u32)
    where
        I: LayoutInterner<'a>,
    {
        let width = self.stack_size_without_alignment(interner);
        let alignment = self.alignment_bytes(interner);

        let size = round_up_to_alignment(width, alignment);
        (size, alignment)
    }

    /// Very important to use this when doing a memcpy!
    pub fn stack_size_without_alignment<I>(&self, interner: &I) -> u32
    where
        I: LayoutInterner<'a>,
    {
        use LayoutRepr::*;

        match self {
            Builtin(builtin) => builtin.stack_size(interner.target()),
            Struct(field_layouts) => {
                let mut sum = 0;

                for field_layout in *field_layouts {
                    sum += interner.get_repr(*field_layout).stack_size(interner);
                }

                sum
            }
            Union(variant) => variant.stack_size_without_alignment(interner),
            LambdaSet(lambda_set) => interner
                .get_repr(lambda_set.runtime_representation())
                .stack_size_without_alignment(interner),
            RecursivePointer(_) | Ptr(_) | FunctionPointer(_) => {
                interner.target().ptr_width() as u32
            }
            Erased(e) => e.stack_size_without_alignment(interner.target()),
        }
    }

    pub fn alignment_bytes<I>(&self, interner: &I) -> u32
    where
        I: LayoutInterner<'a>,
    {
        use LayoutRepr::*;
        match self {
            Struct(field_layouts) => field_layouts
                .iter()
                .map(|x| interner.get_repr(*x).alignment_bytes(interner))
                .max()
                .unwrap_or(1),

            Union(variant) => {
                use UnionLayout::*;

                match variant {
                    NonRecursive(tags) => {
                        let max_alignment = tags
                            .iter()
                            .flat_map(|layouts| {
                                layouts.iter().map(|layout| {
                                    interner.get_repr(*layout).alignment_bytes(interner)
                                })
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
                    | NonNullableUnwrapped(_) => interner.target().ptr_width() as u32,
                }
            }
            LambdaSet(lambda_set) => interner
                .get_repr(lambda_set.runtime_representation())
                .alignment_bytes(interner),
            Builtin(builtin) => builtin.alignment_bytes(interner.target()),
            RecursivePointer(_) | Ptr(_) | FunctionPointer(_) => {
                interner.target().ptr_width() as u32
            }
            Erased(e) => e.alignment_bytes(interner.target()),
        }
    }

    pub fn allocation_alignment_bytes<I>(&self, interner: &I) -> u32
    where
        I: LayoutInterner<'a>,
    {
        let ptr_width = interner.target().ptr_width() as u32;

        use LayoutRepr::*;
        match self {
            Builtin(builtin) => builtin.allocation_alignment_bytes(interner),
            Struct { .. } => self.alignment_bytes(interner).max(ptr_width),
            Union(union_layout) => union_layout.allocation_alignment_bytes(interner),
            LambdaSet(lambda_set) => interner
                .get_repr(lambda_set.runtime_representation())
                .allocation_alignment_bytes(interner),
            RecursivePointer(_) => {
                unreachable!("should be looked up to get an actual layout")
            }
            Ptr(inner) => interner.get_repr(*inner).alignment_bytes(interner),
            FunctionPointer(_) => ptr_width,
            Erased(e) => e.allocation_alignment_bytes(interner.target()),
        }
    }

    pub fn is_refcounted<I>(&self, interner: &I) -> bool
    where
        I: LayoutInterner<'a>,
    {
        use self::Builtin::*;
        use LayoutRepr::*;

        match self {
            Union(UnionLayout::NonRecursive(_)) => false,

            Union(_) => true,

            RecursivePointer(_) => true,

            Builtin(List(_)) | Builtin(Str) => true,

            Erased(_) => true,

            LambdaSet(lambda_set) => interner.is_refcounted(lambda_set.runtime_representation()),

            _ => false,
        }
    }

    pub fn is_nullable(&self) -> bool {
        use LayoutRepr::*;

        match self {
            Union(union_layout) => match union_layout {
                UnionLayout::NonRecursive(_) => false,
                UnionLayout::Recursive(_) => false,
                UnionLayout::NonNullableUnwrapped(_) => false,
                UnionLayout::NullableWrapped { .. } => true,
                UnionLayout::NullableUnwrapped { .. } => true,
            },

            _ => false,
        }
    }

    /// Even if a value (say, a record) is not itself reference counted,
    /// it may contains values/fields that are. Therefore when this record
    /// goes out of scope, the refcount on those values/fields must  be decremented.
    pub fn contains_refcounted<I>(&self, interner: &I) -> bool
    where
        I: LayoutInterner<'a>,
    {
        use LayoutRepr::*;

        match self {
            Builtin(builtin) => builtin.is_refcounted(),
            Struct(field_layouts) => field_layouts
                .iter()
                .any(|f| interner.get_repr(*f).contains_refcounted(interner)),
            Union(variant) => {
                use UnionLayout::*;

                match variant {
                    NonRecursive(fields) => fields
                        .iter()
                        .flat_map(|ls| ls.iter())
                        .any(|f| interner.get_repr(*f).contains_refcounted(interner)),
                    Recursive(_)
                    | NullableWrapped { .. }
                    | NullableUnwrapped { .. }
                    | NonNullableUnwrapped(_) => true,
                }
            }
            LambdaSet(lambda_set) => interner
                .get_repr(lambda_set.runtime_representation())
                .contains_refcounted(interner),
            RecursivePointer(_) => true,
            Ptr(_) => {
                // we never consider pointers for refcounting. Ptr is not user-facing. The compiler
                // author must make sure that invariants are upheld
                false
            }
            FunctionPointer(_) => false,
            Erased(e) => e.is_refcounted(),
        }
    }

    pub fn has_varying_stack_size<I>(self, interner: &I, arena: &bumpalo::Bump) -> bool
    where
        I: LayoutInterner<'a>,
    {
        let mut stack: Vec<LayoutRepr> = bumpalo::collections::Vec::new_in(arena);

        stack.push(self);

        use LayoutRepr::*;
        while let Some(layout) = stack.pop() {
            match layout {
                Builtin(builtin) => {
                    use self::Builtin::*;
                    match builtin {
                    Int(_)
                        | Float(_)
                        | Bool
                        | Decimal
                        | Str
                        // If there's any layer of indirection (behind a pointer), then it doesn't vary!
                        | List(_) => { /* do nothing */ }
                }
                }
                // If there's any layer of indirection (behind a pointer), then it doesn't vary!
                Struct(field_layouts) => stack.extend(
                    field_layouts
                        .iter()
                        .map(|interned| interner.get_repr(*interned)),
                ),
                Union(tag_union) => match tag_union {
                    UnionLayout::NonRecursive(tags) | UnionLayout::Recursive(tags) => {
                        for tag in tags {
                            stack.extend(tag.iter().map(|interned| interner.get_repr(*interned)));
                        }
                    }
                    UnionLayout::NonNullableUnwrapped(fields) => {
                        stack.extend(fields.iter().map(|interned| interner.get_repr(*interned)));
                    }
                    UnionLayout::NullableWrapped { other_tags, .. } => {
                        for tag in other_tags {
                            stack.extend(tag.iter().map(|interned| interner.get_repr(*interned)));
                        }
                    }
                    UnionLayout::NullableUnwrapped { other_fields, .. } => {
                        stack.extend(
                            other_fields
                                .iter()
                                .map(|interned| interner.get_repr(*interned)),
                        );
                    }
                },
                LambdaSet(_) => return true,
                Ptr(_) => {
                    // If there's any layer of indirection (behind a pointer), then it doesn't vary!
                }
                RecursivePointer(_) => {
                    /* do nothing, we've already generated for this type through the Union(_) */
                }
                FunctionPointer(_) => {
                    // drop through
                }
                Erased(_) => {
                    // erasures are just pointers, so they do not vary
                }
            }
        }

        false
    }
}

pub type SeenRecPtrs<'a> = VecSet<InLayout<'a>>;

impl<'a> Layout<'a> {
    pub fn usize(target: Target) -> InLayout<'a> {
        match target.ptr_width() {
            roc_target::PtrWidth::Bytes4 => Layout::U32,
            roc_target::PtrWidth::Bytes8 => Layout::U64,
        }
    }

    pub fn isize(target: Target) -> InLayout<'a> {
        match target.ptr_width() {
            roc_target::PtrWidth::Bytes4 => Layout::I32,
            roc_target::PtrWidth::Bytes8 => Layout::I64,
        }
    }

    pub fn default_integer() -> InLayout<'a> {
        Layout::I64
    }

    pub fn default_float() -> InLayout<'a> {
        Layout::DEC
    }

    pub fn int_literal_width_to_int(width: roc_types::num::IntLitWidth) -> InLayout<'a> {
        use roc_types::num::IntLitWidth::*;
        match width {
            U8 => Layout::U8,
            U16 => Layout::U16,
            U32 => Layout::U32,
            U64 => Layout::U64,
            U128 => Layout::U128,
            I8 => Layout::I8,
            I16 => Layout::I16,
            I32 => Layout::I32,
            I64 => Layout::I64,
            I128 => Layout::I128,
            // f32 int literal bounded by +/- 2^24, so fit it into an i32
            F32 => Layout::F32,
            // f64 int literal bounded by +/- 2^53, so fit it into an i32
            F64 => Layout::F64,
            // dec int literal bounded by i128, so fit it into an i128
            Dec => Layout::DEC,
        }
    }

    pub fn is_recursive_tag_union<I>(self, interner: &I) -> bool
    where
        I: LayoutInterner<'a>,
    {
        matches!(
            self.repr(interner),
            LayoutRepr::Union(
                UnionLayout::NullableUnwrapped { .. }
                    | UnionLayout::Recursive(_)
                    | UnionLayout::NullableWrapped { .. }
                    | UnionLayout::NonNullableUnwrapped { .. },
            )
        )
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

    pub fn stack_size(&self, target: Target) -> u32 {
        use Builtin::*;

        let ptr_width = target.ptr_width() as u32;

        match self {
            Int(int) => int.stack_size(),
            Float(float) => float.stack_size(),
            Bool => Builtin::I1_SIZE,
            Decimal => Builtin::DECIMAL_SIZE,
            Str => Builtin::STR_WORDS * ptr_width,
            List(_) => Builtin::LIST_WORDS * ptr_width,
        }
    }

    pub fn alignment_bytes(&self, target: Target) -> u32 {
        use std::mem::align_of;
        use Builtin::*;

        let ptr_width = target.ptr_width() as u32;

        // for our data structures, what counts is the alignment of the `( ptr, len )` tuple, and
        // since both of those are one pointer size, the alignment of that structure is a pointer
        // size
        match self {
            Int(int_width) => int_width.alignment_bytes(target),
            Float(float_width) => float_width.alignment_bytes(target),
            Bool => align_of::<bool>() as u32,
            Decimal => IntWidth::I128.alignment_bytes(target),
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
        seen_rec: &mut SeenRecPtrs<'a>,
        _parens: Parens,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
        I: LayoutInterner<'a>,
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
                    F64 => alloc.text("Float64"),
                    F32 => alloc.text("Float32"),
                }
            }

            Bool => alloc.text("Int1"),
            Decimal => alloc.text("Decimal"),

            Str => alloc.text("Str"),
            List(layout) => alloc.text("List ").append(interner.to_doc(
                layout,
                alloc,
                seen_rec,
                Parens::InTypeParam,
            )),
        }
    }

    pub fn allocation_alignment_bytes<I>(&self, interner: &I) -> u32
    where
        I: LayoutInterner<'a>,
    {
        let target = interner.target();
        let ptr_width = target.ptr_width() as u32;

        let allocation = match self {
            Builtin::Str => ptr_width,
            Builtin::List(e) => {
                let e = interner.get_repr(*e);
                e.alignment_bytes(interner).max(ptr_width)
            }
            // The following are usually not heap-allocated, but they might be when inside a Box.
            Builtin::Int(int_width) => int_width.alignment_bytes(target).max(ptr_width),
            Builtin::Float(float_width) => float_width.alignment_bytes(target).max(ptr_width),
            Builtin::Bool => (core::mem::align_of::<bool>() as u32).max(ptr_width),
            Builtin::Decimal => IntWidth::I128.alignment_bytes(target).max(ptr_width),
        };

        allocation.max(ptr_width)
    }
}

fn layout_from_flat_type<'a>(
    env: &mut Env<'a, '_>,
    flat_type: FlatType,
) -> Cacheable<LayoutResult<'a>> {
    use roc_types::subs::FlatType::*;

    let arena = env.arena;
    let subs = env.subs;

    match flat_type {
        Apply(symbol, args) => {
            let args = Vec::from_iter_in(args.into_iter().map(|index| subs[index]), arena);

            match symbol {
                // Ints
                Symbol::NUM_I128 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::I128))
                }
                Symbol::NUM_I64 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::I64))
                }
                Symbol::NUM_I32 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::I32))
                }
                Symbol::NUM_I16 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::I16))
                }
                Symbol::NUM_I8 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::I8))
                }

                Symbol::NUM_U128 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::U128))
                }
                Symbol::NUM_U64 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::U64))
                }
                Symbol::NUM_U32 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::U32))
                }
                Symbol::NUM_U16 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::U16))
                }
                Symbol::NUM_U8 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::U8))
                }

                // Floats
                Symbol::NUM_DEC => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::DEC))
                }
                Symbol::NUM_F64 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::F64))
                }
                Symbol::NUM_F32 => {
                    debug_assert_eq!(args.len(), 0);
                    cacheable(Ok(Layout::F32))
                }

                Symbol::NUM_NUM => {
                    // Num.Num should only ever have 1 argument, e.g. Num.Num Int.Integer
                    debug_assert_eq!(args.len(), 1);

                    let var = args[0];
                    let content = subs.get_content_without_compacting(var);

                    layout_from_num_content(content)
                }

                Symbol::STR_STR => cacheable(Ok(Layout::STR)),
                Symbol::LIST_LIST => list_layout_from_elem(env, args[0]),
                Symbol::BOX_BOX_TYPE => {
                    // Num.Num should only ever have 1 argument, e.g. Num.Num Int.Integer
                    debug_assert_eq!(args.len(), 1);

                    let mut criteria = CACHEABLE;

                    let inner_var = args[0];
                    let inner_layout =
                        cached!(Layout::from_var(env, inner_var), criteria, env.subs);

                    let repr = LayoutRepr::Union(UnionLayout::NonNullableUnwrapped(
                        arena.alloc([inner_layout]),
                    ));

                    let boxed_layout = env.cache.put_in(Layout {
                        repr: repr.direct(),
                        semantic: SemanticRepr::NONE,
                    });

                    Cacheable(Ok(boxed_layout), criteria)
                }
                _ => {
                    panic!("TODO layout_from_flat_type for Apply({symbol:?}, {args:?})");
                }
            }
        }
        Func(args, closure_var, ret_var, _fx_var) => {
            if env.is_seen(closure_var) {
                // TODO(recursive-layouts): after the naked pointer is updated, we can cache `var` to
                // point to the updated layout.
                let rec_ptr = Layout::NAKED_RECURSIVE_PTR;
                Cacheable(Ok(rec_ptr), NAKED_RECURSION_PTR)
            } else {
                let mut criteria = CACHEABLE;

                let closure_data = build_function_closure_data(env, args, closure_var, ret_var);
                let closure_data = cached!(closure_data, criteria, env.subs);

                match closure_data {
                    ClosureDataKind::LambdaSet(lambda_set) => {
                        Cacheable(Ok(lambda_set.full_layout), criteria)
                    }
                    ClosureDataKind::Erased => Cacheable(Ok(Layout::ERASED), criteria),
                }
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
                    RecordField::Required(field_var)
                    | RecordField::Demanded(field_var)
                    | RecordField::RigidRequired(field_var) => {
                        let field_layout =
                            cached!(Layout::from_var(env, field_var), criteria, env.subs);
                        sortables.push((label, field_layout));
                    }
                    RecordField::Optional(_) | RecordField::RigidOptional(_) => {
                        // drop optional fields
                    }
                }
            }

            if sortables.is_empty() {
                Cacheable(Ok(Layout::UNIT), criteria)
            } else {
                sortables.sort_by(|(label1, layout1), (label2, layout2)| {
                    cmp_fields(&env.cache.interner, label1, *layout1, label2, *layout2)
                });

                let ordered_field_names = Vec::from_iter_in(
                    sortables
                        .iter()
                        .map(|(label, _)| &*arena.alloc_str(label.as_str())),
                    arena,
                )
                .into_bump_slice();
                let semantic = SemanticRepr::record(ordered_field_names);

                let repr = if sortables.len() == 1 {
                    // If the record has only one field that isn't zero-sized,
                    // unwrap it.
                    let inner_repr = sortables.pop().unwrap().1;
                    inner_repr.newtype()
                } else {
                    let layouts = Vec::from_iter_in(sortables.into_iter().map(|t| t.1), arena);
                    LayoutRepr::struct_(layouts.into_bump_slice()).direct()
                };

                let result = Ok(env.cache.put_in(Layout { repr, semantic }));

                Cacheable(result, criteria)
            }
        }
        Tuple(elems, ext_var) => {
            let mut criteria = CACHEABLE;

            // extract any values from the ext_var
            let mut sortables = Vec::with_capacity_in(elems.len(), arena);
            let it = match elems.unsorted_iterator(subs, ext_var) {
                Ok(it) => it,
                Err(TupleElemsError) => return Cacheable(Err(LayoutProblem::Erroneous), criteria),
            };

            for (index, elem) in it {
                let elem_layout = cached!(Layout::from_var(env, elem), criteria, env.subs);
                sortables.push((index, elem_layout));
            }

            sortables.sort_by(|(index1, layout1), (index2, layout2)| {
                cmp_fields(&env.cache.interner, index1, *layout1, index2, *layout2)
            });

            let result = if sortables.len() == 1 {
                // If the tuple has only one field that isn't zero-sized,
                // unwrap it.
                Ok(sortables.pop().unwrap().1)
            } else {
                let field_layouts =
                    Vec::from_iter_in(sortables.into_iter().map(|t| t.1), arena).into_bump_slice();
                let struct_layout = Layout {
                    repr: LayoutRepr::Struct(field_layouts).direct(),
                    semantic: SemanticRepr::tuple(field_layouts.len()),
                };

                Ok(env.cache.put_in(struct_layout))
            };

            Cacheable(result, criteria)
        }
        TagUnion(tags, ext_var) => {
            let (tags, ext_var) = tags.unsorted_tags_and_ext(subs, ext_var);

            debug_assert!(ext_var_is_empty_tag_union(subs, ext_var));

            layout_from_non_recursive_union(env, &tags, DropUninhabitedVariants(true)).map(Ok)
        }
        FunctionOrTagUnion(tag_names, _, ext_var) => {
            debug_assert!(
                ext_var_is_empty_tag_union(subs, ext_var),
                "If ext_var wasn't empty, this wouldn't be a FunctionOrTagUnion!"
            );

            let tag_names = subs.get_subs_slice(tag_names);
            let unsorted_tags = UnsortedUnionLabels {
                tags: tag_names.iter().map(|t| (t, &[] as &[Variable])).collect(),
            };

            layout_from_non_recursive_union(env, &unsorted_tags, DropUninhabitedVariants(true))
                .map(Ok)
        }
        RecursiveTagUnion(rec_var, tags, ext_var) => {
            let (tags, ext_var) = tags.unsorted_tags_and_ext(subs, ext_var);

            debug_assert!(ext_var_is_empty_tag_union(subs, ext_var));

            layout_from_recursive_union(env, rec_var, &tags)
        }
        EmptyTagUnion => cacheable(Ok(Layout::VOID)),
        EmptyRecord => cacheable(Ok(Layout::UNIT)),
        EffectfulFunc => {
            internal_error!("Cannot create a layout for an unconstrained EffectfulFunc")
        }
    }
}

pub type SortedTupleElem<'a> = (usize, Variable, InLayout<'a>);

pub fn sort_tuple_elems<'a>(
    env: &mut Env<'a, '_>,
    var: Variable,
) -> Result<Vec<'a, SortedTupleElem<'a>>, LayoutProblem> {
    let (it, _) = match gather_tuple_elems_unsorted_iter(env.subs, TupleElems::empty(), var) {
        Ok(it) => it,
        Err(_) => return Err(LayoutProblem::Erroneous),
    };

    sort_tuple_elems_help(env, it)
}

fn sort_tuple_elems_help<'a>(
    env: &mut Env<'a, '_>,
    elems_map: impl Iterator<Item = (usize, Variable)>,
) -> Result<Vec<'a, SortedTupleElem<'a>>, LayoutProblem> {
    let mut sorted_elems = Vec::with_capacity_in(elems_map.size_hint().0, env.arena);

    for (index, elem) in elems_map {
        let Cacheable(layout, _) = Layout::from_var(env, elem);
        let layout = layout?;
        sorted_elems.push((index, elem, layout));
    }

    sorted_elems.sort_by(|(index1, _, res_layout1), (index2, _, res_layout2)| {
        cmp_fields(
            &env.cache.interner,
            index1,
            *res_layout1,
            index2,
            *res_layout2,
        )
    });

    Ok(sorted_elems)
}

pub type SortedField<'a> = (Lowercase, Variable, Result<InLayout<'a>, InLayout<'a>>);

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
    // Sort the fields by label
    let mut sorted_fields = Vec::with_capacity_in(fields_map.size_hint().0, env.arena);

    for (label, field) in fields_map {
        match field {
            RecordField::Demanded(v) | RecordField::Required(v) | RecordField::RigidRequired(v) => {
                let Cacheable(layout, _) = Layout::from_var(env, v);
                let layout = layout?;
                sorted_fields.push((label, v, Ok(layout)));
            }
            RecordField::Optional(v) | RecordField::RigidOptional(v) => {
                let Cacheable(layout, _) = Layout::from_var(env, v);
                let layout = layout?;
                sorted_fields.push((label, v, Err(layout)));
            }
        };
    }

    sorted_fields.sort_by(
        |(label1, _, res_layout1), (label2, _, res_layout2)| match res_layout1 {
            Ok(layout1) | Err(layout1) => match res_layout2 {
                Ok(layout2) | Err(layout2) => {
                    cmp_fields(&env.cache.interner, label1, *layout1, label2, *layout2)
                }
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
        arguments: Vec<'a, InLayout<'a>>,
    },
    NewtypeByVoid {
        data_tag_name: TagOrClosure,
        data_tag_id: TagIdIntType,
        data_tag_arguments: Vec<'a, InLayout<'a>>,
    },
    Wrapped(WrappedVariant<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WrappedVariant<'a> {
    Recursive {
        sorted_tag_layouts: Vec<'a, (TagOrClosure, &'a [InLayout<'a>])>,
    },
    NonRecursive {
        sorted_tag_layouts: Vec<'a, (TagOrClosure, &'a [InLayout<'a>])>,
    },
    NullableWrapped {
        nullable_id: TagIdIntType,
        nullable_name: TagOrClosure,
        sorted_tag_layouts: Vec<'a, (TagOrClosure, &'a [InLayout<'a>])>,
    },
    NonNullableUnwrapped {
        tag_name: TagOrClosure,
        fields: &'a [InLayout<'a>],
    },
    NullableUnwrapped {
        nullable_id: bool,
        nullable_name: TagOrClosure,
        other_name: TagOrClosure,
        other_fields: &'a [InLayout<'a>],
    },
}

impl<'a> WrappedVariant<'a> {
    pub fn tag_name_to_id(&self, tag_name: &TagName) -> (TagIdIntType, &'a [InLayout<'a>]) {
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

    let drop_uninhabited_variants = DropUninhabitedVariants(true);

    let mut tags_vec = std::vec::Vec::new();
    let result = match roc_types::pretty_print::chase_ext_tag_union(env.subs, var, &mut tags_vec) {
        ChasedExt::Empty => {
            let opt_rec_var = get_recursion_var(env.subs, var);
            let Cacheable(result, _) =
                union_sorted_tags_help(env, tags_vec, opt_rec_var, drop_uninhabited_variants);
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
                    let Cacheable(result, _) = union_sorted_tags_help(
                        env,
                        tags_vec,
                        opt_rec_var,
                        drop_uninhabited_variants,
                    );
                    result
                }
                RecursionVar { .. } => {
                    let opt_rec_var = get_recursion_var(env.subs, var);
                    let Cacheable(result, _) = union_sorted_tags_help(
                        env,
                        tags_vec,
                        opt_rec_var,
                        drop_uninhabited_variants,
                    );
                    result
                }

                Error => return Err(LayoutProblem::Erroneous),

                other => panic!("invalid content in tag union variable: {other:?}"),
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

trait Label: subs::Label + Ord + Clone + Into<TagOrClosure> {
    fn semantic_repr<'a, 'r>(
        arena: &'a Bump,
        labels: impl ExactSizeIterator<Item = &'r Self>,
    ) -> SemanticRepr<'a>
    where
        Self: 'r;
}

impl Label for TagName {
    fn semantic_repr<'a, 'r>(
        arena: &'a Bump,
        labels: impl ExactSizeIterator<Item = &'r Self>,
    ) -> SemanticRepr<'a> {
        SemanticRepr::tag_union(
            arena.alloc_slice_fill_iter(labels.map(|x| &*arena.alloc_str(x.0.as_str()))),
        )
    }
}

impl Label for Symbol {
    fn semantic_repr<'a, 'r>(
        arena: &'a Bump,
        labels: impl ExactSizeIterator<Item = &'r Self>,
    ) -> SemanticRepr<'a> {
        SemanticRepr::lambdas(arena.alloc_slice_fill_iter(labels.copied()))
    }
}

struct DropUninhabitedVariants(bool);

fn union_sorted_non_recursive_tags_help<'a, L>(
    env: &mut Env<'a, '_>,
    tags_list: &mut Vec<'_, &'_ (&'_ L, &[Variable])>,
    drop_uninhabited_variants: DropUninhabitedVariants,
) -> Cacheable<UnionVariant<'a>>
where
    L: Label + Ord + Clone + Into<TagOrClosure>,
{
    let mut cache_criteria = CACHEABLE;

    // sort up front; make sure the ordering stays intact!
    tags_list.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));

    match tags_list.len() {
        0 => {
            // trying to instantiate a type with no values
            Cacheable(UnionVariant::Never, cache_criteria)
        }
        1 => {
            let &&(tag_name, arguments) = &tags_list[0];
            let tag_name = tag_name.clone().into();

            // just one tag in the union (but with arguments) can be a struct
            let mut layouts = Vec::with_capacity_in(tags_list.len(), env.arena);

            for &var in arguments {
                let Cacheable(result, criteria) = Layout::from_var(env, var);
                cache_criteria.and(criteria, env.subs);
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
                let size1 = env
                    .cache
                    .get_repr(*layout1)
                    .alignment_bytes(&env.cache.interner);
                let size2 = env
                    .cache
                    .get_repr(*layout2)
                    .alignment_bytes(&env.cache.interner);

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
            let mut answer: Vec<(TagOrClosure, &[InLayout])> =
                Vec::with_capacity_in(tags_list.len(), env.arena);
            let mut has_any_arguments = false;

            let mut inhabited_tag_ids = BitVec::<usize>::repeat(true, num_tags);

            for &&(tag_name, arguments) in tags_list.iter() {
                let mut arg_layouts = Vec::with_capacity_in(arguments.len() + 1, env.arena);

                for &var in arguments {
                    let Cacheable(result, criteria) = Layout::from_var(env, var);
                    cache_criteria.and(criteria, env.subs);
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
                            arg_layouts.push(Layout::VOID)
                        }
                        Err(LayoutProblem::Erroneous) => {
                            // An erroneous type var will code gen to a runtime
                            // error, so we don't need to store any data for it.
                        }
                    }
                }

                arg_layouts.sort_by(|layout1, layout2| {
                    let size1 = env
                        .cache
                        .get_repr(*layout1)
                        .alignment_bytes(&env.cache.interner);
                    let size2 = env
                        .cache
                        .get_repr(*layout2)
                        .alignment_bytes(&env.cache.interner);

                    size2.cmp(&size1)
                });

                answer.push((tag_name.clone().into(), arg_layouts.into_bump_slice()));
            }

            if inhabited_tag_ids.count_ones() == 1 && drop_uninhabited_variants.0 {
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
    union_sorted_tags_help(env, tags_vec, opt_rec_var, DropUninhabitedVariants(true)).value()
}

fn find_nullable_tag<'a, L, I>(tags: I) -> Option<(TagIdIntType, L)>
where
    I: Iterator<Item = (&'a L, &'a [Variable])>,
    L: Into<TagOrClosure> + Ord + Clone + 'a,
{
    let mut length = 0;
    let mut has_payload = 0;
    let mut nullable = None;

    for (index, (name, variables)) in tags.enumerate() {
        length += 1;

        if variables.is_empty() {
            nullable = nullable.or_else(|| Some((index as TagIdIntType, name.clone())));
        } else {
            has_payload += 1;
        }
    }

    let has_no_payload = length - has_payload;

    // in the scenario of `[ A Str, B, C, D ]`, rather than having one tag be nullable, we want
    // to store the tag id in the pointer. (we want NonNullableUnwrapped, not NullableWrapped)
    if (has_payload > 1 && has_no_payload > 0) || has_no_payload == 1 {
        nullable
    } else {
        None
    }
}

fn union_sorted_tags_help<'a, L>(
    env: &mut Env<'a, '_>,
    mut tags_vec: std::vec::Vec<(L, std::vec::Vec<Variable>)>,
    opt_rec_var: Option<Variable>,
    drop_uninhabited_variants: DropUninhabitedVariants,
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
                cache_criteria.and(criteria, env.subs);
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
                let size1 = env.cache.interner.alignment_bytes(*layout1);
                let size2 = env.cache.interner.alignment_bytes(*layout2);

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
                nullable = find_nullable_tag(tags_vec.iter().map(|(a, b)| (a, b.as_slice())));
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
                    cache_criteria.and(criteria, env.subs);
                    match result {
                        Ok(in_layout) => {
                            has_any_arguments = true;

                            let layout = env.cache.get_in(in_layout);

                            // make sure to not unroll recursive types!
                            let self_recursion = opt_rec_var.is_some()
                                && env.subs.get_root_key_without_compacting(var)
                                    == env
                                        .subs
                                        .get_root_key_without_compacting(opt_rec_var.unwrap())
                                && layout.is_recursive_tag_union(&env.cache.interner);

                            let arg_layout = if self_recursion {
                                Layout::NAKED_RECURSIVE_PTR
                            } else {
                                in_layout
                            };
                            arg_layouts.push(arg_layout);

                            if layout == Layout::VOID_NAKED {
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
                    let size1 = env
                        .cache
                        .get_repr(*layout1)
                        .alignment_bytes(&env.cache.interner);
                    let size2 = env
                        .cache
                        .get_repr(*layout2)
                        .alignment_bytes(&env.cache.interner);

                    size2.cmp(&size1)
                });

                answer.push((tag_name.into(), arg_layouts.into_bump_slice()));
            }

            if inhabited_tag_ids.count_ones() == 1 && !is_recursive && drop_uninhabited_variants.0 {
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
) -> Cacheable<InLayout<'a>> {
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
    drop_uninhabited_variants: DropUninhabitedVariants,
) -> Cacheable<InLayout<'a>>
where
    L: Label + Ord + Into<TagOrClosure>,
{
    use UnionVariant::*;

    if tags.is_newtype_wrapper(env.subs) {
        return layout_from_newtype(env, tags);
    }

    let mut tags_vec = Vec::from_iter_in(tags.tags.iter(), env.arena);

    let mut criteria = CACHEABLE;

    let variant =
        union_sorted_non_recursive_tags_help(env, &mut tags_vec, drop_uninhabited_variants)
            .decompose(&mut criteria, env.subs);

    let compute_semantic = || L::semantic_repr(env.arena, tags_vec.iter().map(|(l, _)| *l));

    let result = match variant {
        Never => Layout::VOID,
        Unit => env
            .cache
            .put_in(Layout::new(LayoutRepr::UNIT.direct(), compute_semantic())),
        BoolUnion { .. } => env
            .cache
            .put_in(Layout::new(LayoutRepr::BOOL.direct(), compute_semantic())),
        ByteUnion(_) => env
            .cache
            .put_in(Layout::new(LayoutRepr::U8.direct(), compute_semantic())),
        Newtype {
            arguments: field_layouts,
            ..
        } => {
            let answer1 = if field_layouts.len() == 1 {
                field_layouts[0]
            } else {
                env.cache
                    .put_in_direct_no_semantic(LayoutRepr::struct_(field_layouts.into_bump_slice()))
            };

            answer1
        }
        NewtypeByVoid {
            data_tag_arguments, ..
        } => {
            if data_tag_arguments.len() == 1 {
                data_tag_arguments[0]
            } else {
                env.cache.put_in_direct_no_semantic(LayoutRepr::struct_(
                    data_tag_arguments.into_bump_slice(),
                ))
            }
        }
        Wrapped(variant) => {
            use WrappedVariant::*;

            match variant {
                NonRecursive {
                    sorted_tag_layouts: tags,
                } => {
                    let mut layouts = Vec::with_capacity_in(tags.len(), env.arena);

                    let semantic = match tags.first() {
                        Some((tag_or_closure, _)) => match tag_or_closure {
                            TagOrClosure::Tag(_tag) => TagName::semantic_repr(
                                env.arena,
                                tags.iter()
                                    .map(|(tag_or_closure, layout)| match tag_or_closure {
                                        TagOrClosure::Tag(tag) => {
                                            layouts.push(*layout);
                                            tag
                                        }
                                        TagOrClosure::Closure(_symbol) => {
                                            unreachable!()
                                        }
                                    }),
                            ),
                            TagOrClosure::Closure(_symbol) => Symbol::semantic_repr(
                                env.arena,
                                tags.iter()
                                    .map(|(tag_or_closure, layout)| match tag_or_closure {
                                        TagOrClosure::Tag(_tag) => unreachable!(),
                                        TagOrClosure::Closure(symbol) => {
                                            layouts.push(*layout);
                                            symbol
                                        }
                                    }),
                            ),
                        },
                        None => {
                            // This would be a tag union with no tags, which should have hit a
                            // different case earlier.
                            unreachable!();
                        }
                    };

                    let layout = Layout {
                        repr: LayoutRepr::Union(UnionLayout::NonRecursive(
                            layouts.into_bump_slice(),
                        ))
                        .direct(),
                        semantic,
                    };
                    env.cache.put_in(layout)
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
        nullable = find_nullable_tag(tags_vec.iter().map(|(a, b)| (*a, *b)));
    }

    env.insert_seen(rec_var);
    for (index, &(_name, variables)) in tags_vec.iter().enumerate() {
        if matches!(nullable, Some((i, _)) if i == index as TagIdIntType) {
            // don't add the nullable case
            continue;
        }

        let mut tag_layout = Vec::with_capacity_in(variables.len() + 1, arena);

        for &var in variables {
            // TODO does this cause problems with mutually recursive unions?
            if rec_var == subs.get_root_key_without_compacting(var) {
                // The naked pointer will get fixed-up to loopback to the union below when we
                // intern the union.
                tag_layout.push(Layout::NAKED_RECURSIVE_PTR);
                criteria.and(NAKED_RECURSION_PTR, env.subs);
                continue;
            }

            let payload = cached!(Layout::from_var(env, var), criteria, env.subs);
            tag_layout.push(payload);
        }

        tag_layout.sort_by(|layout1, layout2| {
            // TODO(intern-layouts): provide alignment bytes on interner
            let size1 = env.cache.interner.alignment_bytes(*layout1);
            let size2 = env.cache.interner.alignment_bytes(*layout2);

            size2.cmp(&size1)
        });

        tag_layouts.push(tag_layout.into_bump_slice());
    }
    env.remove_seen(rec_var);

    let union_layout = if let Some((tag_id, _)) = nullable {
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

    let union_layout = if criteria.has_naked_recursion_pointer {
        env.cache.interner.insert_recursive(
            env.arena,
            Layout {
                repr: LayoutRepr::Union(union_layout).direct(),
                semantic: SemanticRepr::NONE,
            },
        )
    } else {
        // There are no naked recursion pointers, so we can insert the layout as-is.
        env.cache.interner.insert(Layout {
            repr: LayoutRepr::Union(union_layout).direct(),
            semantic: SemanticRepr::NONE,
        })
    };

    criteria.pass_through_recursive_union(rec_var);

    Cacheable(Ok(union_layout), criteria)
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
pub fn ext_var_is_empty_tag_union(subs: &Subs, tag_ext: TagExt) -> bool {
    use roc_types::pretty_print::ChasedExt;
    use Content::*;

    // the ext_var is empty
    let mut ext_fields = std::vec::Vec::new();
    match roc_types::pretty_print::chase_ext_tag_union(subs, tag_ext.var(), &mut ext_fields) {
        ChasedExt::Empty => ext_fields.is_empty(),
        ChasedExt::NonEmpty { content, .. } => {
            match content {
                // Allow flex/rigid to decay away into nothing
                FlexVar(_) | FlexAbleVar(..) | RigidVar(_) | RigidAbleVar(..) => {
                    ext_fields.is_empty()
                }
                // So that we can continue compiling in the presence of errors
                Error => ext_fields.is_empty(),
                _ => panic!("invalid content in ext_var: {content:?}"),
            }
        }
    }
}

#[cfg(not(debug_assertions))]
pub fn ext_var_is_empty_tag_union(_: &Subs, _: TagExt) -> bool {
    // This should only ever be used in debug_assert! macros
    unreachable!();
}

fn layout_from_num_content<'a>(content: &Content) -> Cacheable<LayoutResult<'a>> {
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
            Symbol::NUM_INTEGER => Ok(Layout::I64),
            Symbol::NUM_I128 => Ok(Layout::I128),
            Symbol::NUM_I64 => Ok(Layout::I64),
            Symbol::NUM_I32 => Ok(Layout::I32),
            Symbol::NUM_I16 => Ok(Layout::I16),
            Symbol::NUM_I8 => Ok(Layout::I8),

            Symbol::NUM_U128 => Ok(Layout::U128),
            Symbol::NUM_U64 => Ok(Layout::U64),
            Symbol::NUM_U32 => Ok(Layout::U32),
            Symbol::NUM_U16 => Ok(Layout::U16),
            Symbol::NUM_U8 => Ok(Layout::U8),

            // Floats
            Symbol::NUM_FLOATINGPOINT => Ok(Layout::F64),
            Symbol::NUM_F64 => Ok(Layout::F64),
            Symbol::NUM_F32 => Ok(Layout::F32),

            // Dec
            Symbol::NUM_DEC => Ok(Layout::DEC),

            _ => {
                panic!("Invalid Num.Num type application: Apply({symbol:?}, {args:?})");
            }
        },
        Alias(_, _, _, _) => {
            todo!("TODO recursively resolve type aliases in num_from_content");
        }
        Structure(_) | RangedNumber(..) | LambdaSet(_) | ErasedLambda | Pure | Effectful => {
            panic!("Invalid Num.Num type application: {content:?}");
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
        cached!(Layout::from_var(env, element_var), criteria, env.subs)
    };

    let list_layout = env.cache.put_in(Layout {
        repr: LayoutRepr::Builtin(Builtin::List(element_layout)).direct(),
        semantic: SemanticRepr::NONE,
    });

    Cacheable(Ok(list_layout), criteria)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LayoutId(u32);

impl LayoutId {
    // Returns something like "#UserApp_foo_1" when given a symbol that interns to "foo"
    // and a LayoutId of 1.
    pub fn to_symbol_string(self, symbol: Symbol, interns: &Interns) -> String {
        let ident_string = symbol.as_unsuffixed_str(interns);
        let module_string = interns.module_ids.get_name(symbol.module_id()).unwrap();
        format!("{}_{}_{}", module_string, ident_string, self.0)
    }

    // Returns something like "roc__foo_1_exposed" when given a symbol that interns to "foo"
    // and a LayoutId of 1.
    pub fn to_exposed_symbol_string(self, symbol: Symbol, interns: &Interns) -> String {
        let ident_string = symbol.as_unsuffixed_str(interns);
        format!("roc__{}_{}_exposed", ident_string, self.0)
    }

    pub fn to_exposed_generic_symbol_string(self, symbol: Symbol, interns: &Interns) -> String {
        let ident_string = symbol.as_unsuffixed_str(interns);
        format!("roc__{}_{}_exposed_generic", ident_string, self.0)
    }
}

struct IdsByLayout<'a> {
    by_id: MutMap<LayoutRepr<'a>, u32>,
    toplevels_by_id: MutMap<crate::ir::ProcLayout<'a>, u32>,
    next_id: u32,
}

impl<'a> IdsByLayout<'a> {
    #[inline(always)]
    fn insert_layout(&mut self, layout: LayoutRepr<'a>) -> LayoutId {
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
    fn singleton_layout(layout: LayoutRepr<'a>) -> (Self, LayoutId) {
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
    pub fn get<'b>(&mut self, symbol: Symbol, layout: &'b LayoutRepr<'a>) -> LayoutId {
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
    layout1: InLayout<'a>,
    label2: &L,
    layout2: InLayout<'a>,
) -> Ordering
where
    I: LayoutInterner<'a>,
{
    let size1 = interner.get_repr(layout1).alignment_bytes(interner);
    let size2 = interner.get_repr(layout2).alignment_bytes(interner);

    size2.cmp(&size1).then(label1.cmp(label2))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn width_and_alignment_union_empty_struct() {
        let mut interner = STLayoutInterner::with_capacity(4, Target::LinuxX64);

        let lambda_set = LambdaSet {
            args: &(&[] as &[InLayout]),
            ret: Layout::VOID,
            set: &(&[(Symbol::LIST_MAP, &[] as &[InLayout])] as &[(Symbol, &[InLayout])]),
            representation: Layout::UNIT,
            full_layout: Layout::VOID,
        };

        let a = &[Layout::UNIT] as &[_];
        let b = &[interner.insert(Layout {
            repr: LayoutRepr::LambdaSet(lambda_set).direct(),
            semantic: SemanticRepr::NONE,
        })] as &[_];
        let tt = [a, b];

        let repr = LayoutRepr::Union(UnionLayout::NonRecursive(&tt));

        assert_eq!(repr.stack_size(&interner), 1);
        assert_eq!(repr.alignment_bytes(&interner), 1);
    }

    #[test]
    fn memcpy_size_result_u32_unit() {
        let mut interner = STLayoutInterner::with_capacity(4, Target::LinuxX64);

        let ok_tag = &[interner.insert(Layout {
            repr: LayoutRepr::Builtin(Builtin::Int(IntWidth::U32)).direct(),
            semantic: SemanticRepr::NONE,
        })];
        let err_tag = &[Layout::UNIT];
        let tags = [ok_tag as &[_], err_tag as &[_]];
        let union_layout = UnionLayout::NonRecursive(&tags as &[_]);
        let repr = LayoutRepr::Union(union_layout);

        assert_eq!(repr.stack_size_without_alignment(&interner), 8);
    }

    #[test]
    fn void_stack_size() {
        let interner = STLayoutInterner::with_capacity(4, Target::LinuxX64);
        assert_eq!(Layout::VOID_NAKED.repr(&interner).stack_size(&interner), 0);
    }

    #[test]
    fn align_u128_in_tag_union() {
        let interner = STLayoutInterner::with_capacity(4, Target::LinuxX64);
        assert_eq!(interner.alignment_bytes(Layout::U128), 16);
    }
}
