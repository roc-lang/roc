use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::{default_hasher, MutMap, MutSet};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{Interns, Symbol};
use roc_types::subs::{Content, FlatType, Subs, Variable};
use roc_types::types::RecordField;
use std::collections::HashMap;

pub const MAX_ENUM_SIZE: usize = (std::mem::size_of::<u8>() * 8) as usize;

/// If a (Num *) gets translated to a Layout, this is the numeric type it defaults to.
const DEFAULT_NUM_BUILTIN: Builtin<'_> = Builtin::Int64;
pub const TAG_SIZE: Builtin<'_> = Builtin::Int64;

#[derive(Debug, Clone)]
pub enum LayoutProblem {
    UnresolvedTypeVar(Variable),
    Erroneous,
}

/// Types for code gen must be monomorphic. No type variables allowed!
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Layout<'a> {
    Builtin(Builtin<'a>),
    /// A layout that is empty (turns into the empty struct in LLVM IR
    /// but for our purposes, not zero-sized, so it does not get dropped from data structures
    /// this is important for closures that capture zero-sized values
    PhantomEmptyStruct,
    Struct(&'a [Layout<'a>]),
    Union(&'a [&'a [Layout<'a>]]),
    RecursiveUnion(&'a [&'a [Layout<'a>]]),
    RecursivePointer,
    /// A function. The types of its arguments, then the type of its return value.
    FunctionPointer(&'a [Layout<'a>], &'a Layout<'a>),
    Closure(&'a [Layout<'a>], ClosureLayout<'a>, &'a Layout<'a>),
    Pointer(&'a Layout<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClosureLayout<'a> {
    /// the layout that this specific closure captures
    /// uses a Vec instead of a MutMap because it's Hash
    /// the vec is likely to be small, so linear search is fine
    captured: &'a [(TagName, &'a [Layout<'a>])],

    layout: &'a Layout<'a>,
}

impl<'a> ClosureLayout<'a> {
    fn from_unit(arena: &'a Bump) -> Self {
        let layout = Layout::PhantomEmptyStruct;
        let layouts = arena.alloc(layout);
        ClosureLayout {
            captured: &[],
            layout: layouts,
        }
    }
    fn from_bool(arena: &'a Bump) -> Self {
        let layout = Layout::Builtin(Builtin::Int1);
        let layouts = arena.alloc(layout);
        ClosureLayout {
            captured: &[],
            layout: layouts,
        }
    }
    fn from_byte(arena: &'a Bump) -> Self {
        let layout = Layout::Builtin(Builtin::Int8);
        let layouts = arena.alloc(layout);
        ClosureLayout {
            captured: &[],
            layout: layouts,
        }
    }
    fn from_unwrapped(arena: &'a Bump, layouts: &'a [Layout<'a>]) -> Self {
        debug_assert!(!layouts.is_empty());

        // DO NOT unwrap 1-element records here!
        let layout = arena.alloc(Layout::Struct(layouts));

        ClosureLayout {
            captured: &[],
            layout,
        }
    }

    fn from_tag_union(arena: &'a Bump, tags: &'a [(TagName, &'a [Layout<'a>])]) -> Self {
        debug_assert!(tags.len() > 1);

        // if the closed-over value is actually a layout, it should be wrapped in a 1-element record
        debug_assert!(matches!(tags[0].0, TagName::Closure(_)));

        let mut tag_arguments = Vec::with_capacity_in(tags.len(), arena);

        for (_, tag_args) in tags.iter() {
            tag_arguments.push(&tag_args[0..]);
        }

        ClosureLayout {
            captured: tags,
            layout: arena.alloc(Layout::Union(tag_arguments.into_bump_slice())),
        }
    }

    pub fn get_wrapped(&self) -> crate::ir::Wrapped {
        use crate::ir::Wrapped;

        match self.layout {
            Layout::Struct(fields) if fields.len() == 1 => Wrapped::SingleElementRecord,
            Layout::Struct(_) => Wrapped::RecordOrSingleTagUnion,
            Layout::Union(_) => Wrapped::MultiTagUnion,
            _ => Wrapped::SingleElementRecord,
        }
    }

    pub fn from_var(
        arena: &'a Bump,
        subs: &Subs,
        closure_var: Variable,
    ) -> Result<Option<Self>, LayoutProblem> {
        let mut tags = std::vec::Vec::new();
        match roc_types::pretty_print::chase_ext_tag_union(subs, closure_var, &mut tags) {
            Ok(()) | Err((_, Content::FlexVar(_))) if !tags.is_empty() => {
                // special-case the `[ Closure1, Closure2, Closure3 ]` case, where none of
                // the tags have a payload
                let all_no_payload = tags.iter().all(|(_, arguments)| arguments.is_empty());

                if all_no_payload {
                    return Ok(None);
                }

                // otherwise, this is a closure with a payload
                let variant = union_sorted_tags_help(arena, tags, None, subs);

                use UnionVariant::*;
                match variant {
                    Never => Ok(None),
                    Unit => Ok(None),
                    UnitWithArguments => {
                        // the closure layout is zero-sized, but there is something in it (e.g.  `{}`)

                        let closure_layout = ClosureLayout::from_unit(arena);
                        Ok(Some(closure_layout))
                    }
                    BoolUnion { .. } => {
                        let closure_layout = ClosureLayout::from_bool(arena);

                        Ok(Some(closure_layout))
                    }
                    ByteUnion(_) => {
                        let closure_layout = ClosureLayout::from_byte(arena);

                        Ok(Some(closure_layout))
                    }
                    Unwrapped(layouts) => {
                        let closure_layout =
                            ClosureLayout::from_unwrapped(arena, layouts.into_bump_slice());

                        Ok(Some(closure_layout))
                    }
                    Wrapped(tags) => {
                        let closure_layout =
                            ClosureLayout::from_tag_union(arena, tags.into_bump_slice());

                        Ok(Some(closure_layout))
                    }
                }
            }

            Ok(()) | Err((_, Content::FlexVar(_))) => {
                // a max closure size of 0 means this is a standart top-level function
                Ok(None)
            }
            _ => panic!("called ClosureLayout.from_var on invalid input"),
        }
    }

    pub fn extend_function_layout(
        arena: &'a Bump,
        argument_layouts: &'a [Layout<'a>],
        closure_layout: Self,
        ret_layout: &'a Layout<'a>,
    ) -> Layout<'a> {
        let closure_data_layout = match closure_layout.layout {
            Layout::Struct(fields) if fields.len() == 1 => &fields[0],
            other => other,
        };

        // define the function pointer
        let function_ptr_layout = {
            let mut temp = Vec::from_iter_in(argument_layouts.iter().cloned(), arena);
            temp.push(closure_data_layout.clone());
            Layout::FunctionPointer(temp.into_bump_slice(), ret_layout)
        };

        function_ptr_layout
    }

    pub fn stack_size(&self, pointer_size: u32) -> u32 {
        self.layout.stack_size(pointer_size)
    }
    pub fn contains_refcounted(&self) -> bool {
        self.layout.contains_refcounted()
    }
    pub fn safe_to_memcpy(&self) -> bool {
        self.layout.safe_to_memcpy()
    }

    pub fn as_named_layout(&self, symbol: Symbol) -> Layout<'a> {
        let layouts = if self.captured.is_empty() {
            match self.layout {
                Layout::Struct(fields) if fields.len() == 1 => fields[0].clone(),
                other => other.clone(),
            }
        } else if let Some((_, tag_args)) = self
            .captured
            .iter()
            .find(|(tn, _)| *tn == TagName::Closure(symbol))
        {
            if tag_args.len() == 1 {
                tag_args[0].clone()
            } else {
                Layout::Struct(tag_args)
            }
        } else {
            unreachable!(
                "invariant broken, TagName::Closure({:?}) is not in {:?}",
                symbol, &self.captured
            );
        };

        layouts
    }

    pub fn as_block_of_memory_layout(&self) -> Layout<'a> {
        match self.layout {
            Layout::Struct(fields) if fields.len() == 1 => fields[0].clone(),
            other => other.clone(),
        }
    }

    pub fn internal_layout(&self) -> Layout<'a> {
        self.layout.clone()
    }

    pub fn build_closure_data(
        &self,
        original: Symbol,
        symbols: &'a [Symbol],
    ) -> Result<crate::ir::Expr<'a>, Symbol> {
        use crate::ir::Expr;

        match self.layout {
            Layout::Struct(fields) if fields.len() == 1 => Err(symbols[0]),
            Layout::Struct(fields) => {
                debug_assert!(fields.len() > 1);
                debug_assert_eq!(fields.len(), symbols.len());

                Ok(Expr::Struct(symbols))
            }
            Layout::Union(tags) => {
                // NOTE it's very important that this Union consists of Closure tags
                // and is not an unpacked 1-element record

                let tag_id = self
                    .captured
                    .iter()
                    .position(|(tn, _)| *tn == TagName::Closure(original))
                    .unwrap() as _;

                let expr = Expr::Tag {
                    tag_layout: Layout::Union(tags),
                    tag_name: TagName::Closure(original),
                    tag_id,
                    union_size: tags.len() as u8,
                    arguments: symbols,
                };

                Ok(expr)
            }
            Layout::PhantomEmptyStruct => {
                debug_assert_eq!(symbols.len(), 1);

                Ok(Expr::Struct(&[]))
            }

            _ => {
                debug_assert_eq!(
                    symbols.len(),
                    1,
                    "symbols {:?} for layout {:?}",
                    &symbols,
                    &self.layout
                );

                Err(symbols[0])
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
pub enum MemoryMode {
    Unique,
    Refcounted,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Builtin<'a> {
    Int128,
    Int64,
    Int32,
    Int16,
    Int8,
    Int1,
    Usize,
    Float128,
    Float64,
    Float32,
    Float16,
    Str,
    Dict(&'a Layout<'a>, &'a Layout<'a>),
    Set(&'a Layout<'a>),
    List(MemoryMode, &'a Layout<'a>),
    EmptyStr,
    EmptyList,
    EmptyDict,
    EmptySet,
}

pub struct Env<'a, 'b> {
    arena: &'a Bump,
    seen: MutSet<Variable>,
    subs: &'b Subs,
}

impl<'a, 'b> Env<'a, 'b> {
    fn is_seen(&self, var: Variable) -> bool {
        let var = self.subs.get_root_key_without_compacting(var);

        self.seen.contains(&var)
    }

    fn insert_seen(&mut self, var: Variable) -> bool {
        let var = self.subs.get_root_key_without_compacting(var);

        self.seen.insert(var)
    }
}

impl<'a> Layout<'a> {
    fn new_help<'b>(
        env: &mut Env<'a, 'b>,
        var: Variable,
        content: Content,
    ) -> Result<Self, LayoutProblem> {
        use roc_types::subs::Content::*;

        match content {
            FlexVar(_) | RigidVar(_) => Err(LayoutProblem::UnresolvedTypeVar(var)),
            RecursionVar { structure, .. } => {
                let structure_content = env.subs.get_without_compacting(structure).content;
                Self::new_help(env, structure, structure_content)
            }
            Structure(flat_type) => layout_from_flat_type(env, flat_type),

            // Ints
            Alias(Symbol::NUM_I128, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Int128))
            }
            Alias(Symbol::NUM_I64, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Int64))
            }
            Alias(Symbol::NUM_I32, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Int32))
            }
            Alias(Symbol::NUM_I16, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Int16))
            }
            Alias(Symbol::NUM_I8, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Int8))
            }

            // I think unsigned and signed use the same layout
            Alias(Symbol::NUM_U128, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Int128))
            }
            Alias(Symbol::NUM_U64, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Int64))
            }
            Alias(Symbol::NUM_U32, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Int32))
            }
            Alias(Symbol::NUM_U16, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Int16))
            }
            Alias(Symbol::NUM_U8, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Int8))
            }

            // Floats
            Alias(Symbol::NUM_F64, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Float64))
            }
            Alias(Symbol::NUM_F32, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Float32))
            }

            Alias(_, _, var) => Self::from_var(env, var),
            Error => Err(LayoutProblem::Erroneous),
        }
    }

    /// Returns Err(()) if given an error, or Ok(Layout) if given a non-erroneous Structure.
    /// Panics if given a FlexVar or RigidVar, since those should have been
    /// monomorphized away already!
    fn from_var(env: &mut Env<'a, '_>, var: Variable) -> Result<Self, LayoutProblem> {
        if env.is_seen(var) {
            Ok(Layout::RecursivePointer)
        } else {
            let content = env.subs.get_without_compacting(var).content;
            Self::new_help(env, var, content)
        }
    }

    pub fn safe_to_memcpy(&self) -> bool {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.safe_to_memcpy(),
            PhantomEmptyStruct => true,
            Struct(fields) => fields
                .iter()
                .all(|field_layout| field_layout.safe_to_memcpy()),
            Union(tags) => tags
                .iter()
                .all(|tag_layout| tag_layout.iter().all(|field| field.safe_to_memcpy())),
            RecursiveUnion(_) => {
                // a recursive union will always contain a pointer, and are thus not safe to memcpy
                false
            }
            FunctionPointer(_, _) => {
                // Function pointers are immutable and can always be safely copied
                true
            }
            Closure(_, closure_layout, _) => closure_layout.safe_to_memcpy(),
            Pointer(_) => {
                // We cannot memcpy pointers, because then we would have the same pointer in multiple places!
                false
            }
            RecursivePointer => {
                // We cannot memcpy pointers, because then we would have the same pointer in multiple places!
                false
            }
        }
    }

    pub fn is_dropped_because_empty(&self) -> bool {
        // For this calculation, we don't need an accurate
        // stack size, we just need to know whether it's zero,
        // so it's fine to use a pointer size of 1.
        if let Layout::PhantomEmptyStruct = self {
            false
        } else {
            // self.stack_size(1) == 0
            false
        }
    }

    pub fn stack_size(&self, pointer_size: u32) -> u32 {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.stack_size(pointer_size),
            PhantomEmptyStruct => 0,
            Struct(fields) => {
                let mut sum = 0;

                for field_layout in *fields {
                    sum += field_layout.stack_size(pointer_size);
                }

                sum
            }
            Union(fields) => fields
                .iter()
                .map(|tag_layout| {
                    tag_layout
                        .iter()
                        .map(|field| field.stack_size(pointer_size))
                        .sum()
                })
                .max()
                .unwrap_or_default(),
            RecursiveUnion(fields) => fields
                .iter()
                .map(|tag_layout| {
                    tag_layout
                        .iter()
                        .map(|field| field.stack_size(pointer_size))
                        .sum()
                })
                .max()
                .unwrap_or_default(),
            Closure(_, closure_layout, _) => pointer_size + closure_layout.stack_size(pointer_size),
            FunctionPointer(_, _) => pointer_size,
            RecursivePointer => pointer_size,
            Pointer(_) => pointer_size,
        }
    }

    pub fn alignment_bytes(&self, pointer_size: u32) -> u32 {
        match self {
            Layout::Struct(fields) => fields
                .iter()
                .map(|x| x.alignment_bytes(pointer_size))
                .max()
                .unwrap_or(0),
            Layout::Union(tags) | Layout::RecursiveUnion(tags) => tags
                .iter()
                .map(|x| x.iter())
                .flatten()
                .map(|x| x.alignment_bytes(pointer_size))
                .max()
                .unwrap_or(0),
            Layout::Builtin(builtin) => builtin.alignment_bytes(pointer_size),
            Layout::PhantomEmptyStruct => 0,
            Layout::RecursivePointer => pointer_size,
            Layout::FunctionPointer(_, _) => pointer_size,
            Layout::Pointer(_) => pointer_size,
            Layout::Closure(_, captured, _) => {
                pointer_size.max(captured.layout.alignment_bytes(pointer_size))
            }
        }
    }

    pub fn is_refcounted(&self) -> bool {
        matches!(self, Layout::Builtin(Builtin::List(MemoryMode::Refcounted, _)) | Layout::Builtin(Builtin::Str) | Layout::RecursiveUnion(_) | Layout::RecursivePointer)
    }

    /// Even if a value (say, a record) is not itself reference counted,
    /// it may contains values/fields that are. Therefore when this record
    /// goes out of scope, the refcount on those values/fields must  be decremented.
    pub fn contains_refcounted(&self) -> bool {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.is_refcounted(),
            PhantomEmptyStruct => false,
            Struct(fields) => fields.iter().any(|f| f.contains_refcounted()),
            Union(fields) => fields
                .iter()
                .map(|ls| ls.iter())
                .flatten()
                .any(|f| f.contains_refcounted()),
            RecursiveUnion(_) => true,
            Closure(_, closure_layout, _) => closure_layout.contains_refcounted(),
            FunctionPointer(_, _) | RecursivePointer | Pointer(_) => false,
        }
    }
}

/// Avoid recomputing Layout from Variable multiple times.
/// We use `ena` for easy snapshots and rollbacks of the cache.
/// During specialization, a type variable `a` can be specialized to different layouts,
/// e.g. `identity : a -> a` could be specialized to `Bool -> Bool` or `Str -> Str`.
/// Therefore in general it's invalid to store a map from variables to layouts
/// But if we're careful when to invalidate certain keys, we still get some benefit
#[derive(Default, Debug)]
pub struct LayoutCache<'a> {
    layouts: ven_ena::unify::UnificationTable<ven_ena::unify::InPlace<CachedVariable<'a>>>,
}

#[derive(Debug, Clone)]
pub enum CachedLayout<'a> {
    Cached(Layout<'a>),
    NotCached,
    Problem(LayoutProblem),
}

/// Must wrap so we can define a specific UnifyKey instance
/// PhantomData so we can store the 'a lifetime, which is needed to implement the UnifyKey trait,
/// specifically so we can use `type Value = CachedLayout<'a>`
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct CachedVariable<'a>(Variable, std::marker::PhantomData<&'a ()>);

impl<'a> CachedVariable<'a> {
    fn new(var: Variable) -> Self {
        CachedVariable(var, std::marker::PhantomData)
    }
}

// use ven_ena::unify::{InPlace, Snapshot, UnificationTable, UnifyKey};

impl<'a> ven_ena::unify::UnifyKey for CachedVariable<'a> {
    type Value = CachedLayout<'a>;

    fn index(&self) -> u32 {
        self.0.index()
    }

    fn from_index(index: u32) -> Self {
        CachedVariable(Variable::from_index(index), std::marker::PhantomData)
    }

    fn tag() -> &'static str {
        "CachedVariable"
    }
}

impl<'a> LayoutCache<'a> {
    /// Returns Err(()) if given an error, or Ok(Layout) if given a non-erroneous Structure.
    /// Panics if given a FlexVar or RigidVar, since those should have been
    /// monomorphized away already!
    pub fn from_var(
        &mut self,
        arena: &'a Bump,
        var: Variable,
        subs: &Subs,
    ) -> Result<Layout<'a>, LayoutProblem> {
        // Store things according to the root Variable, to avoid duplicate work.
        let var = subs.get_root_key_without_compacting(var);

        let cached_var = CachedVariable::new(var);

        self.expand_to_fit(cached_var);

        use CachedLayout::*;
        match self.layouts.probe_value(cached_var) {
            Cached(result) => Ok(result),
            Problem(problem) => Err(problem),
            NotCached => {
                let mut env = Env {
                    arena,
                    subs,
                    seen: MutSet::default(),
                };

                let result = Layout::from_var(&mut env, var);

                // Don't actually cache. The layout cache is very hard to get right in the presence
                // of specialization, it's turned of for now so an invalid cache is never the cause
                // of a problem
                if false {
                    let cached_layout = match &result {
                        Ok(layout) => Cached(layout.clone()),
                        Err(problem) => Problem(problem.clone()),
                    };

                    self.layouts
                        .update_value(cached_var, |existing| existing.value = cached_layout);
                }

                result
            }
        }
    }

    fn expand_to_fit(&mut self, var: CachedVariable<'a>) {
        use ven_ena::unify::UnifyKey;

        let required = (var.index() as isize) - (self.layouts.len() as isize) + 1;
        if required > 0 {
            self.layouts.reserve(required as usize);

            for _ in 0..required {
                self.layouts.new_key(CachedLayout::NotCached);
            }
        }
    }

    pub fn snapshot(
        &mut self,
    ) -> ven_ena::unify::Snapshot<ven_ena::unify::InPlace<CachedVariable<'a>>> {
        self.layouts.snapshot()
    }

    pub fn rollback_to(
        &mut self,
        snapshot: ven_ena::unify::Snapshot<ven_ena::unify::InPlace<CachedVariable<'a>>>,
    ) {
        self.layouts.rollback_to(snapshot)
    }
}

impl<'a> Builtin<'a> {
    const I128_SIZE: u32 = std::mem::size_of::<i128>() as u32;
    const I64_SIZE: u32 = std::mem::size_of::<i64>() as u32;
    const I32_SIZE: u32 = std::mem::size_of::<i32>() as u32;
    const I16_SIZE: u32 = std::mem::size_of::<i16>() as u32;
    const I8_SIZE: u32 = std::mem::size_of::<i8>() as u32;
    const I1_SIZE: u32 = std::mem::size_of::<bool>() as u32;
    const USIZE_SIZE: u32 = std::mem::size_of::<usize>() as u32;
    const F128_SIZE: u32 = 16;
    const F64_SIZE: u32 = std::mem::size_of::<f64>() as u32;
    const F32_SIZE: u32 = std::mem::size_of::<f32>() as u32;
    const F16_SIZE: u32 = 2;

    /// Number of machine words in an empty one of these
    pub const STR_WORDS: u32 = 2;
    pub const DICT_WORDS: u32 = 6;
    pub const SET_WORDS: u32 = Builtin::DICT_WORDS; // Set is an alias for Dict with {} for value
    pub const LIST_WORDS: u32 = 2;

    /// Layout of collection wrapper for List and Str - a struct of (pointer, length).
    ///
    /// We choose this layout (with pointer first) because it's how
    /// Rust slices are laid out, meaning we can cast to/from them for free.
    pub const WRAPPER_PTR: u32 = 0;
    pub const WRAPPER_LEN: u32 = 1;

    pub fn stack_size(&self, pointer_size: u32) -> u32 {
        use Builtin::*;

        match self {
            Int128 => Builtin::I128_SIZE,
            Int64 => Builtin::I64_SIZE,
            Int32 => Builtin::I32_SIZE,
            Int16 => Builtin::I16_SIZE,
            Int8 => Builtin::I8_SIZE,
            Int1 => Builtin::I1_SIZE,
            Usize => Builtin::USIZE_SIZE,
            Float128 => Builtin::F128_SIZE,
            Float64 => Builtin::F64_SIZE,
            Float32 => Builtin::F32_SIZE,
            Float16 => Builtin::F16_SIZE,
            Str | EmptyStr => Builtin::STR_WORDS * pointer_size,
            Dict(_, _) | EmptyDict => Builtin::DICT_WORDS * pointer_size,
            Set(_) | EmptySet => Builtin::SET_WORDS * pointer_size,
            List(_, _) | EmptyList => Builtin::LIST_WORDS * pointer_size,
        }
    }

    pub fn alignment_bytes(&self, pointer_size: u32) -> u32 {
        use std::mem::align_of;
        use Builtin::*;

        // for our data structures, what counts is the alignment of the `( ptr, len )` tuple, and
        // since both of those are one pointer size, the alignment of that structure is a pointer
        // size
        match self {
            Int128 => align_of::<i128>() as u32,
            Int64 => align_of::<i64>() as u32,
            Int32 => align_of::<i32>() as u32,
            Int16 => align_of::<i16>() as u32,
            Int8 => align_of::<i8>() as u32,
            Int1 => align_of::<bool>() as u32,
            Usize => align_of::<usize>() as u32,
            Float128 => align_of::<i128>() as u32,
            Float64 => align_of::<f64>() as u32,
            Float32 => align_of::<f32>() as u32,
            Float16 => align_of::<i16>() as u32,
            Str | EmptyStr => pointer_size,
            Dict(_, _) | EmptyDict => pointer_size,
            Set(_) | EmptySet => pointer_size,
            List(_, _) | EmptyList => pointer_size,
        }
    }

    pub fn safe_to_memcpy(&self) -> bool {
        use Builtin::*;

        match self {
            Int128 | Int64 | Int32 | Int16 | Int8 | Int1 | Usize | Float128 | Float64 | Float32
            | Float16 | EmptyStr | EmptyDict | EmptyList | EmptySet => true,
            Str | Dict(_, _) | Set(_) | List(_, _) => false,
        }
    }

    // Question: does is_refcounted exactly correspond with the "safe to memcpy" property?
    pub fn is_refcounted(&self) -> bool {
        use Builtin::*;

        match self {
            Int128 | Int64 | Int32 | Int16 | Int8 | Int1 | Usize | Float128 | Float64 | Float32
            | Float16 | EmptyStr | EmptyDict | EmptyList | EmptySet => false,
            List(mode, element_layout) => match mode {
                MemoryMode::Refcounted => true,
                MemoryMode::Unique => element_layout.contains_refcounted(),
            },

            Str | Dict(_, _) | Set(_) => true,
        }
    }
}

fn layout_from_flat_type<'a>(
    env: &mut Env<'a, '_>,
    flat_type: FlatType,
) -> Result<Layout<'a>, LayoutProblem> {
    use roc_types::subs::FlatType::*;

    let arena = env.arena;
    let subs = env.subs;

    match flat_type {
        Apply(symbol, args) => {
            match symbol {
                // Ints
                Symbol::NUM_NAT => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Usize))
                }

                Symbol::NUM_I128 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Int128))
                }
                Symbol::NUM_I64 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Int64))
                }
                Symbol::NUM_I32 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Int32))
                }
                Symbol::NUM_I16 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Int16))
                }
                Symbol::NUM_I8 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Int8))
                }

                Symbol::NUM_U128 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Int128))
                }
                Symbol::NUM_U64 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Int64))
                }
                Symbol::NUM_U32 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Int32))
                }
                Symbol::NUM_U16 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Int16))
                }
                Symbol::NUM_U8 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Int8))
                }

                // Floats
                Symbol::NUM_F64 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Float64))
                }
                Symbol::NUM_F32 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Float32))
                }

                Symbol::NUM_NUM | Symbol::NUM_AT_NUM => {
                    // Num.Num should only ever have 1 argument, e.g. Num.Num Int.Integer
                    debug_assert_eq!(args.len(), 1);

                    let var = args.first().unwrap();
                    let content = subs.get_without_compacting(*var).content;

                    layout_from_num_content(content)
                }

                Symbol::STR_STR => Ok(Layout::Builtin(Builtin::Str)),
                Symbol::LIST_LIST => list_layout_from_elem(env, args[0]),
                Symbol::ATTR_ATTR => {
                    debug_assert_eq!(args.len(), 2);

                    // The first argument is the uniqueness info;
                    // second is the base type
                    let wrapped_var = args[1];

                    // correct the memory mode of unique lists
                    match Layout::from_var(env, wrapped_var)? {
                        Layout::Builtin(Builtin::List(_ignored, elem_layout)) => {
                            let uniqueness_var = args[0];
                            let uniqueness_content =
                                subs.get_without_compacting(uniqueness_var).content;

                            let mode = if uniqueness_content.is_unique(subs) {
                                MemoryMode::Unique
                            } else {
                                MemoryMode::Refcounted
                            };

                            Ok(Layout::Builtin(Builtin::List(mode, elem_layout)))
                        }
                        other => Ok(other),
                    }
                }
                _ => {
                    panic!("TODO layout_from_flat_type for {:?}", Apply(symbol, args));
                }
            }
        }
        Func(args, closure_var, ret_var) => {
            let mut fn_args = Vec::with_capacity_in(args.len(), arena);

            for arg_var in args {
                fn_args.push(Layout::from_var(env, arg_var)?);
            }

            let ret = Layout::from_var(env, ret_var)?;

            let fn_args = fn_args.into_bump_slice();
            let ret = arena.alloc(ret);

            match ClosureLayout::from_var(env.arena, env.subs, closure_var)? {
                Some(closure_layout) => Ok(Layout::Closure(fn_args, closure_layout, ret)),
                None => Ok(Layout::FunctionPointer(fn_args, ret)),
            }
        }
        Record(fields, ext_var) => {
            // extract any values from the ext_var
            let mut fields_map = MutMap::default();
            fields_map.extend(fields);
            match roc_types::pretty_print::chase_ext_record(subs, ext_var, &mut fields_map) {
                Ok(()) | Err((_, Content::FlexVar(_))) => {}
                Err(_) => unreachable!("this would have been a type error"),
            }

            let sorted_fields = sort_record_fields_help(env, fields_map);

            // Determine the layouts of the fields, maintaining sort order
            let mut layouts = Vec::with_capacity_in(sorted_fields.len(), arena);

            for (_, _, res_layout) in sorted_fields {
                match res_layout {
                    Ok(layout) => {
                        // Drop any zero-sized fields like {}.
                        if !layout.is_dropped_because_empty() {
                            layouts.push(layout);
                        }
                    }
                    Err(_) => {
                        // optional field, ignore
                        continue;
                    }
                }
            }

            if layouts.len() == 1 {
                // If the record has only one field that isn't zero-sized,
                // unwrap it.
                Ok(layouts.pop().unwrap())
            } else {
                Ok(Layout::Struct(layouts.into_bump_slice()))
            }
        }
        TagUnion(tags, ext_var) => {
            debug_assert!(ext_var_is_empty_tag_union(subs, ext_var));

            Ok(layout_from_tag_union(arena, tags, subs))
        }
        RecursiveTagUnion(rec_var, tags, ext_var) => {
            debug_assert!(ext_var_is_empty_tag_union(subs, ext_var));

            // some observations
            //
            // * recursive tag unions are always recursive
            // * therefore at least one tag has a pointer (non-zero sized) field
            // * they must (to be instantiated) have 2 or more tags
            //
            // That means none of the optimizations for enums or single tag tag unions apply

            let rec_var = subs.get_root_key_without_compacting(rec_var);
            env.insert_seen(rec_var);
            let mut tag_layouts = Vec::with_capacity_in(tags.len(), arena);

            // VERY IMPORTANT: sort the tags
            let mut tags_vec: std::vec::Vec<_> = tags.into_iter().collect();
            tags_vec.sort();

            for (_name, variables) in tags_vec {
                let mut tag_layout = Vec::with_capacity_in(variables.len() + 1, arena);

                // store the discriminant
                tag_layout.push(Layout::Builtin(TAG_SIZE));

                for var in variables {
                    // TODO does this cause problems with mutually recursive unions?
                    if rec_var == subs.get_root_key_without_compacting(var) {
                        tag_layout.push(Layout::RecursivePointer);
                        continue;
                    }

                    tag_layout.push(Layout::from_var(env, var)?);
                }

                tag_layout.sort_by(|layout1, layout2| {
                    let ptr_bytes = 8;

                    let size1 = layout1.alignment_bytes(ptr_bytes);
                    let size2 = layout2.alignment_bytes(ptr_bytes);

                    size2.cmp(&size1)
                });

                tag_layouts.push(tag_layout.into_bump_slice());
            }

            Ok(Layout::RecursiveUnion(tag_layouts.into_bump_slice()))
        }
        EmptyTagUnion => {
            panic!("TODO make Layout for empty Tag Union");
        }
        Boolean(_) => {
            panic!("TODO make Layout for Boolean");
        }
        Erroneous(_) => Err(LayoutProblem::Erroneous),
        EmptyRecord => Ok(Layout::Struct(&[])),
    }
}

pub fn sort_record_fields<'a>(
    arena: &'a Bump,
    var: Variable,
    subs: &Subs,
) -> Vec<'a, (Lowercase, Variable, Result<Layout<'a>, Layout<'a>>)> {
    let mut fields_map = MutMap::default();

    let mut env = Env {
        arena,
        subs,
        seen: MutSet::default(),
    };

    match roc_types::pretty_print::chase_ext_record(subs, var, &mut fields_map) {
        Ok(()) | Err((_, Content::FlexVar(_))) => sort_record_fields_help(&mut env, fields_map),
        Err(other) => panic!("invalid content in record variable: {:?}", other),
    }
}

fn sort_record_fields_help<'a>(
    env: &mut Env<'a, '_>,
    fields_map: MutMap<Lowercase, RecordField<Variable>>,
) -> Vec<'a, (Lowercase, Variable, Result<Layout<'a>, Layout<'a>>)> {
    // Sort the fields by label
    let mut sorted_fields = Vec::with_capacity_in(fields_map.len(), env.arena);

    for (label, field) in fields_map {
        let var = match field {
            RecordField::Demanded(v) => v,
            RecordField::Required(v) => v,
            RecordField::Optional(v) => {
                let layout = Layout::from_var(env, v).expect("invalid layout from var");
                sorted_fields.push((label, v, Err(layout)));
                continue;
            }
        };

        let layout = Layout::from_var(env, var).expect("invalid layout from var");

        // Drop any zero-sized fields like {}
        if !layout.is_dropped_because_empty() {
            sorted_fields.push((label, var, Ok(layout)));
        }
    }

    sorted_fields.sort_by(
        |(label1, _, res_layout1), (label2, _, res_layout2)| match res_layout1 {
            Ok(layout1) | Err(layout1) => match res_layout2 {
                Ok(layout2) | Err(layout2) => {
                    let ptr_bytes = 8;

                    let size1 = layout1.alignment_bytes(ptr_bytes);
                    let size2 = layout2.alignment_bytes(ptr_bytes);

                    size2.cmp(&size1).then(label1.cmp(label2))
                }
            },
        },
    );

    sorted_fields
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnionVariant<'a> {
    Never,
    Unit,
    UnitWithArguments,
    BoolUnion { ttrue: TagName, ffalse: TagName },
    ByteUnion(Vec<'a, TagName>),
    Unwrapped(Vec<'a, Layout<'a>>),
    Wrapped(Vec<'a, (TagName, &'a [Layout<'a>])>),
}

pub fn union_sorted_tags<'a>(arena: &'a Bump, var: Variable, subs: &Subs) -> UnionVariant<'a> {
    let var =
        if let Content::RecursionVar { structure, .. } = subs.get_without_compacting(var).content {
            structure
        } else {
            var
        };

    let mut tags_vec = std::vec::Vec::new();
    let result = match roc_types::pretty_print::chase_ext_tag_union(subs, var, &mut tags_vec) {
        Ok(()) | Err((_, Content::FlexVar(_))) | Err((_, Content::RecursionVar { .. })) => {
            let opt_rec_var = get_recursion_var(subs, var);
            union_sorted_tags_help(arena, tags_vec, opt_rec_var, subs)
        }
        Err(other) => panic!("invalid content in tag union variable: {:?}", other),
    };

    result
}

fn get_recursion_var(subs: &Subs, var: Variable) -> Option<Variable> {
    match subs.get_without_compacting(var).content {
        Content::Structure(FlatType::RecursiveTagUnion(rec_var, _, _)) => Some(rec_var),
        Content::Structure(FlatType::Apply(Symbol::ATTR_ATTR, args)) => {
            get_recursion_var(subs, args[1])
        }
        Content::Alias(_, _, actual) => get_recursion_var(subs, actual),
        _ => None,
    }
}

pub fn union_sorted_tags_help<'a>(
    arena: &'a Bump,
    mut tags_vec: std::vec::Vec<(TagName, std::vec::Vec<Variable>)>,
    opt_rec_var: Option<Variable>,
    subs: &Subs,
) -> UnionVariant<'a> {
    // sort up front; make sure the ordering stays intact!
    tags_vec.sort();

    let mut env = Env {
        arena,
        subs,
        seen: MutSet::default(),
    };

    if let Some(rec_var) = opt_rec_var {
        env.insert_seen(rec_var);
    }

    match tags_vec.len() {
        0 => {
            // trying to instantiate a type with no values
            UnionVariant::Never
        }
        1 => {
            let (tag_name, arguments) = tags_vec.remove(0);

            // just one tag in the union (but with arguments) can be a struct
            let mut layouts = Vec::with_capacity_in(tags_vec.len(), arena);
            let mut contains_zero_sized = false;

            // special-case NUM_AT_NUM: if its argument is a FlexVar, make it Int
            match tag_name {
                TagName::Private(Symbol::NUM_AT_NUM) => {
                    layouts.push(unwrap_num_tag(subs, arguments[0]).expect("invalid num layout"));
                }
                _ => {
                    for var in arguments {
                        match Layout::from_var(&mut env, var) {
                            Ok(layout) => {
                                // Drop any zero-sized arguments like {}
                                if !layout.is_dropped_because_empty() {
                                    layouts.push(layout);
                                } else {
                                    contains_zero_sized = true;
                                }
                            }
                            Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                                // If we encounter an unbound type var (e.g. `Ok *`)
                                // then it's zero-sized; drop the argument.
                            }
                            Err(LayoutProblem::Erroneous) => {
                                // An erroneous type var will code gen to a runtime
                                // error, so we don't need to store any data for it.
                            }
                        }
                    }
                }
            }

            layouts.sort_by(|layout1, layout2| {
                let ptr_bytes = 8;

                let size1 = layout1.alignment_bytes(ptr_bytes);
                let size2 = layout2.alignment_bytes(ptr_bytes);

                size2.cmp(&size1)
            });

            if layouts.is_empty() {
                if contains_zero_sized {
                    UnionVariant::UnitWithArguments
                } else {
                    UnionVariant::Unit
                }
            } else {
                UnionVariant::Unwrapped(layouts)
            }
        }
        num_tags => {
            // default path
            let mut answer = Vec::with_capacity_in(tags_vec.len(), arena);
            let mut has_any_arguments = false;

            for (tag_name, arguments) in tags_vec {
                // reserve space for the tag discriminant
                let mut arg_layouts = Vec::with_capacity_in(arguments.len() + 1, arena);

                // add the tag discriminant (size currently always hardcoded to i64)
                arg_layouts.push(Layout::Builtin(TAG_SIZE));

                for var in arguments {
                    match Layout::from_var(&mut env, var) {
                        Ok(layout) => {
                            // Drop any zero-sized arguments like {}
                            if !layout.is_dropped_because_empty() {
                                has_any_arguments = true;

                                arg_layouts.push(layout);
                            }
                        }
                        Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                            // If we encounter an unbound type var (e.g. `Ok *`)
                            // then it's zero-sized; drop the argument.
                        }
                        Err(LayoutProblem::Erroneous) => {
                            // An erroneous type var will code gen to a runtime
                            // error, so we don't need to store any data for it.
                        }
                    }
                }

                arg_layouts.sort_by(|layout1, layout2| {
                    let ptr_bytes = 8;

                    let size1 = layout1.alignment_bytes(ptr_bytes);
                    let size2 = layout2.alignment_bytes(ptr_bytes);

                    size2.cmp(&size1)
                });

                answer.push((tag_name, arg_layouts.into_bump_slice()));
            }

            match num_tags {
                2 if !has_any_arguments => {
                    // type can be stored in a boolean

                    // tags_vec is sorted, and answer is sorted the same way
                    let ttrue = answer.remove(1).0;
                    let ffalse = answer.remove(0).0;

                    UnionVariant::BoolUnion { ffalse, ttrue }
                }
                3..=MAX_ENUM_SIZE if !has_any_arguments => {
                    // type can be stored in a byte
                    // needs the sorted tag names to determine the tag_id
                    let mut tag_names = Vec::with_capacity_in(answer.len(), arena);

                    for (tag_name, _) in answer {
                        tag_names.push(tag_name);
                    }

                    UnionVariant::ByteUnion(tag_names)
                }
                _ => UnionVariant::Wrapped(answer),
            }
        }
    }
}

pub fn layout_from_tag_union<'a>(
    arena: &'a Bump,
    tags: MutMap<TagName, std::vec::Vec<Variable>>,
    subs: &Subs,
) -> Layout<'a> {
    use UnionVariant::*;

    let tags_vec: std::vec::Vec<_> = tags.into_iter().collect();

    match tags_vec.get(0) {
        Some((tag_name, arguments)) if *tag_name == TagName::Private(Symbol::NUM_AT_NUM) => {
            debug_assert_eq!(arguments.len(), 1);

            let var = arguments.iter().next().unwrap();

            unwrap_num_tag(subs, *var).expect("invalid Num argument")
        }
        _ => {
            let opt_rec_var = None;
            let variant = union_sorted_tags_help(arena, tags_vec, opt_rec_var, subs);

            match variant {
                Never => Layout::Union(&[]),
                Unit | UnitWithArguments => Layout::Struct(&[]),
                BoolUnion { .. } => Layout::Builtin(Builtin::Int1),
                ByteUnion(_) => Layout::Builtin(Builtin::Int8),
                Unwrapped(mut field_layouts) => {
                    if field_layouts.len() == 1 {
                        field_layouts.pop().unwrap()
                    } else {
                        Layout::Struct(field_layouts.into_bump_slice())
                    }
                }
                Wrapped(tags) => {
                    let mut tag_layouts = Vec::with_capacity_in(tags.len(), arena);

                    for (_, tag_layout) in tags {
                        tag_layouts.push(tag_layout);
                    }
                    Layout::Union(tag_layouts.into_bump_slice())
                }
            }
        }
    }
}

#[cfg(debug_assertions)]
fn ext_var_is_empty_tag_union(subs: &Subs, ext_var: Variable) -> bool {
    // the ext_var is empty
    let mut ext_fields = std::vec::Vec::new();
    match roc_types::pretty_print::chase_ext_tag_union(subs, ext_var, &mut ext_fields) {
        Ok(()) | Err((_, Content::FlexVar(_))) => ext_fields.is_empty(),
        Err(content) => panic!("invalid content in ext_var: {:?}", content),
    }
}

#[cfg(not(debug_assertions))]
fn ext_var_is_empty_tag_union(_: &Subs, _: Variable) -> bool {
    // This should only ever be used in debug_assert! macros
    unreachable!();
}

fn layout_from_num_content<'a>(content: Content) -> Result<Layout<'a>, LayoutProblem> {
    use roc_types::subs::Content::*;
    use roc_types::subs::FlatType::*;

    match content {
        RecursionVar { .. } => panic!("recursion var in num"),
        FlexVar(_) | RigidVar(_) => {
            // If a Num makes it all the way through type checking with an unbound
            // type variable, then assume it's a 64-bit integer.
            //
            // (e.g. for (5 + 5) assume both 5s are 64-bit integers.)
            Ok(Layout::Builtin(DEFAULT_NUM_BUILTIN))
        }
        Structure(Apply(symbol, args)) => match symbol {
            // Ints
            Symbol::NUM_NAT => Ok(Layout::Builtin(Builtin::Usize)),

            Symbol::NUM_INTEGER => Ok(Layout::Builtin(Builtin::Int64)),
            Symbol::NUM_I128 => Ok(Layout::Builtin(Builtin::Int128)),
            Symbol::NUM_I64 => Ok(Layout::Builtin(Builtin::Int64)),
            Symbol::NUM_I32 => Ok(Layout::Builtin(Builtin::Int32)),
            Symbol::NUM_I16 => Ok(Layout::Builtin(Builtin::Int16)),
            Symbol::NUM_I8 => Ok(Layout::Builtin(Builtin::Int8)),

            Symbol::NUM_U128 => Ok(Layout::Builtin(Builtin::Int128)),
            Symbol::NUM_U64 => Ok(Layout::Builtin(Builtin::Int64)),
            Symbol::NUM_U32 => Ok(Layout::Builtin(Builtin::Int32)),
            Symbol::NUM_U16 => Ok(Layout::Builtin(Builtin::Int16)),
            Symbol::NUM_U8 => Ok(Layout::Builtin(Builtin::Int8)),

            // Floats
            Symbol::NUM_FLOATINGPOINT => Ok(Layout::Builtin(Builtin::Float64)),
            Symbol::NUM_F64 => Ok(Layout::Builtin(Builtin::Float64)),
            Symbol::NUM_F32 => Ok(Layout::Builtin(Builtin::Float32)),

            _ => {
                panic!(
                    "Invalid Num.Num type application: {:?}",
                    Apply(symbol, args)
                );
            }
        },
        Alias(_, _, _) => {
            todo!("TODO recursively resolve type aliases in num_from_content");
        }
        Structure(_) => {
            panic!("Invalid Num.Num type application: {:?}", content);
        }
        Error => Err(LayoutProblem::Erroneous),
    }
}

fn unwrap_num_tag<'a>(subs: &Subs, var: Variable) -> Result<Layout<'a>, LayoutProblem> {
    match subs.get_without_compacting(var).content {
        Content::Structure(flat_type) => match flat_type {
            FlatType::Apply(Symbol::ATTR_ATTR, args) => {
                debug_assert_eq!(args.len(), 2);

                let arg_var = args.get(1).unwrap();

                unwrap_num_tag(subs, *arg_var)
            }
            _ => {
                todo!("TODO handle Num.@Num flat_type {:?}", flat_type);
            }
        },
        Content::Alias(Symbol::NUM_INTEGER, args, _) => {
            debug_assert!(args.len() == 1);

            let (_, precision_var) = args[0];

            let precision = subs.get_without_compacting(precision_var).content;

            match precision {
                Content::Alias(symbol, args, _) => {
                    debug_assert!(args.is_empty());

                    let builtin = match symbol {
                        Symbol::NUM_SIGNED128 => Builtin::Int128,
                        Symbol::NUM_SIGNED64 => Builtin::Int64,
                        Symbol::NUM_SIGNED32 => Builtin::Int32,
                        Symbol::NUM_SIGNED16 => Builtin::Int16,
                        Symbol::NUM_SIGNED8 => Builtin::Int8,
                        Symbol::NUM_UNSIGNED128 => Builtin::Int128,
                        Symbol::NUM_UNSIGNED64 => Builtin::Int64,
                        Symbol::NUM_UNSIGNED32 => Builtin::Int32,
                        Symbol::NUM_UNSIGNED16 => Builtin::Int16,
                        Symbol::NUM_UNSIGNED8 => Builtin::Int8,
                        Symbol::NUM_NATURAL => Builtin::Usize,
                        _ => unreachable!("not a valid int variant: {:?} {:?}", symbol, args),
                    };

                    Ok(Layout::Builtin(builtin))
                }
                Content::FlexVar(_) | Content::RigidVar(_) => {
                    // default to i64
                    Ok(Layout::Builtin(Builtin::Int64))
                }
                _ => unreachable!("not a valid int variant: {:?}", precision),
            }
        }
        Content::Alias(Symbol::NUM_FLOATINGPOINT, args, _) => {
            debug_assert!(args.len() == 1);

            let (_, precision_var) = args[0];

            let precision = subs.get_without_compacting(precision_var).content;

            match precision {
                Content::Alias(Symbol::NUM_BINARY32, args, _) => {
                    debug_assert!(args.is_empty());

                    Ok(Layout::Builtin(Builtin::Float32))
                }
                Content::Alias(Symbol::NUM_BINARY64, args, _) => {
                    debug_assert!(args.is_empty());

                    Ok(Layout::Builtin(Builtin::Float64))
                }
                Content::FlexVar(_) | Content::RigidVar(_) => {
                    // default to f64
                    Ok(Layout::Builtin(Builtin::Float64))
                }
                _ => unreachable!("not a valid float variant: {:?}", precision),
            }
        }
        Content::FlexVar(_) | Content::RigidVar(_) => {
            // If this was still a (Num *) then default to compiling it to i64
            Ok(Layout::Builtin(DEFAULT_NUM_BUILTIN))
        }
        other => {
            todo!("TODO non structure Num.@Num flat_type {:?}", other);
        }
    }
}

pub fn list_layout_from_elem<'a>(
    env: &mut Env<'a, '_>,
    elem_var: Variable,
) -> Result<Layout<'a>, LayoutProblem> {
    match env.subs.get_without_compacting(elem_var).content {
        Content::Structure(FlatType::Apply(Symbol::ATTR_ATTR, args)) => {
            debug_assert_eq!(args.len(), 2);

            let var = *args.get(1).unwrap();

            list_layout_from_elem(env, var)
        }
        Content::FlexVar(_) | Content::RigidVar(_) => {
            // If this was still a (List *) then it must have been an empty list
            Ok(Layout::Builtin(Builtin::EmptyList))
        }
        content => {
            let elem_layout = Layout::new_help(env, elem_var, content)?;

            // This is a normal list.
            Ok(Layout::Builtin(Builtin::List(
                MemoryMode::Refcounted,
                env.arena.alloc(elem_layout),
            )))
        }
    }
}

pub fn mode_from_var(var: Variable, subs: &Subs) -> MemoryMode {
    match subs.get_without_compacting(var).content {
        Content::Structure(FlatType::Apply(Symbol::ATTR_ATTR, args)) => {
            debug_assert_eq!(args.len(), 2);

            let uvar = *args.get(0).unwrap();
            let content = subs.get_without_compacting(uvar).content;

            if content.is_unique(subs) {
                MemoryMode::Unique
            } else {
                MemoryMode::Refcounted
            }
        }
        _ => MemoryMode::Refcounted,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LayoutId(u32);

impl LayoutId {
    // Returns something like "foo#1" when given a symbol that interns to "foo"
    // and a LayoutId of 1.
    pub fn to_symbol_string(self, symbol: Symbol, interns: &Interns) -> String {
        let ident_string = symbol.ident_string(interns);
        let module_string = interns.module_ids.get_name(symbol.module_id()).unwrap();
        format!("{}_{}_{}", module_string, ident_string, self.0)
    }
}

struct IdsByLayout<'a> {
    by_id: MutMap<Layout<'a>, u32>,
    next_id: u32,
}

#[derive(Default)]
pub struct LayoutIds<'a> {
    by_symbol: MutMap<Symbol, IdsByLayout<'a>>,
}

impl<'a> LayoutIds<'a> {
    /// Returns a LayoutId which is unique for the given symbol and layout.
    /// If given the same symbol and same layout, returns the same LayoutId.
    pub fn get(&mut self, symbol: Symbol, layout: &Layout<'a>) -> LayoutId {
        // Note: this function does some weird stuff to satisfy the borrow checker.
        // There's probably a nicer way to write it that still works.
        let ids = self.by_symbol.entry(symbol).or_insert_with(|| IdsByLayout {
            by_id: HashMap::with_capacity_and_hasher(1, default_hasher()),
            next_id: 1,
        });

        // Get the id associated with this layout, or default to next_id.
        let answer = ids.by_id.get(layout).copied().unwrap_or(ids.next_id);

        // If we had to default to next_id, it must not have been found;
        // store the ID we're going to return and increment next_id.
        if answer == ids.next_id {
            ids.by_id.insert(layout.clone(), ids.next_id);

            ids.next_id += 1;
        }

        LayoutId(answer)
    }
}
