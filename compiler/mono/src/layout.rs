use crate::ir::Parens;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::{default_hasher, MutMap, MutSet};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{Interns, Symbol};
use roc_types::subs::{Content, FlatType, Subs, Variable};
use roc_types::types::RecordField;
use std::collections::HashMap;
use ven_pretty::{DocAllocator, DocBuilder};

pub const MAX_ENUM_SIZE: usize = (std::mem::size_of::<u8>() * 8) as usize;
const GENERATE_NULLABLE: bool = true;

/// If a (Num *) gets translated to a Layout, this is the numeric type it defaults to.
const DEFAULT_NUM_BUILTIN: Builtin<'_> = Builtin::Int64;

#[derive(Debug, Clone)]
pub enum LayoutProblem {
    UnresolvedTypeVar(Variable),
    Erroneous,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RawFunctionLayout<'a> {
    Function(&'a [Layout<'a>], LambdaSet<'a>, &'a Layout<'a>),
    ZeroArgumentThunk(Layout<'a>),
}

impl RawFunctionLayout<'_> {
    pub fn is_zero_argument_thunk(&self) -> bool {
        matches!(self, RawFunctionLayout::ZeroArgumentThunk(_))
    }
}

/// Types for code gen must be monomorphic. No type variables allowed!
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Layout<'a> {
    Builtin(Builtin<'a>),
    /// A layout that is empty (turns into the empty struct in LLVM IR
    /// but for our purposes, not zero-sized, so it does not get dropped from data structures
    /// this is important for closures that capture zero-sized values
    Struct(&'a [Layout<'a>]),
    Union(UnionLayout<'a>),
    RecursivePointer,

    Boxed(&'a Layout<'a>),

    /// A function. The types of its arguments, then the type of its return value.
    Closure(&'a [Layout<'a>], LambdaSet<'a>, &'a Layout<'a>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnionLayout<'a> {
    /// A non-recursive tag union
    /// e.g. `Result a e : [ Ok a, Err e ]`
    NonRecursive(&'a [&'a [Layout<'a>]]),
    /// A recursive tag union
    /// e.g. `Expr : [ Sym Str, Add Expr Expr ]`
    Recursive(&'a [&'a [Layout<'a>]]),
    /// A recursive tag union with just one constructor
    /// e.g. `RoseTree a : [ Tree a (List (RoseTree a)) ]`
    NonNullableUnwrapped(&'a [Layout<'a>]),
    /// A recursive tag union where the non-nullable variant(s) store the tag id
    /// e.g. `FingerTree a : [ Empty, Single a, More (Some a) (FingerTree (Tuple a)) (Some a) ]`
    /// see also: https://youtu.be/ip92VMpf_-A?t=164
    NullableWrapped {
        nullable_id: i64,
        other_tags: &'a [&'a [Layout<'a>]],
    },
    /// A recursive tag union where the non-nullable variant does NOT store the tag id
    /// e.g. `ConsList a : [ Nil, Cons a (ConsList a) ]`
    NullableUnwrapped {
        nullable_id: bool,
        other_fields: &'a [Layout<'a>],
    },
}

impl<'a> UnionLayout<'a> {
    pub fn to_doc<D, A>(self, alloc: &'a D, _parens: Parens) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use UnionLayout::*;

        match self {
            NonRecursive(tags) => {
                let tags_doc = tags.iter().map(|fields| {
                    alloc.text("C ").append(alloc.intersperse(
                        fields.iter().map(|x| x.to_doc(alloc, Parens::InTypeParam)),
                        " ",
                    ))
                });

                alloc
                    .text("[")
                    .append(alloc.intersperse(tags_doc, ", "))
                    .append(alloc.text("]"))
            }
            _ => alloc.text("TODO"),
        }
    }

    pub fn layout_at(self, tag_id: u8, index: usize) -> Layout<'a> {
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
                debug_assert_ne!(nullable_id, tag_id as i64);

                let tag_index = if (tag_id as i64) < nullable_id {
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

    fn tag_id_builtin_help(union_size: usize) -> Builtin<'a> {
        if union_size <= u8::MAX as usize {
            Builtin::Int8
        } else if union_size <= u16::MAX as usize {
            Builtin::Int16
        } else {
            panic!("tag union is too big")
        }
    }

    pub fn tag_id_builtin(&self) -> Builtin<'a> {
        match self {
            UnionLayout::NonRecursive(tags) | UnionLayout::Recursive(tags) => {
                let union_size = tags.len();

                Self::tag_id_builtin_help(union_size)
            }

            UnionLayout::NullableWrapped { other_tags, .. } => {
                Self::tag_id_builtin_help(other_tags.len() + 1)
            }
            UnionLayout::NonNullableUnwrapped(_) => Builtin::Int1,
            UnionLayout::NullableUnwrapped { .. } => Builtin::Int1,
        }
    }

    pub fn tag_id_layout(&self) -> Layout<'a> {
        Layout::Builtin(self.tag_id_builtin())
    }

    pub fn stores_tag_id(&self) -> bool {
        match self {
            UnionLayout::NonRecursive(_)
            | UnionLayout::Recursive(_)
            | UnionLayout::NullableWrapped { .. } => true,
            UnionLayout::NonNullableUnwrapped(_) | UnionLayout::NullableUnwrapped { .. } => false,
        }
    }

    pub fn tag_is_null(&self, tag_id: u8) -> bool {
        match self {
            UnionLayout::NonRecursive(_)
            | UnionLayout::NonNullableUnwrapped(_)
            | UnionLayout::Recursive(_) => false,
            UnionLayout::NullableWrapped { nullable_id, .. } => *nullable_id == tag_id as i64,
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
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LambdaSet<'a> {
    /// collection of function names and their closure arguments
    pub set: &'a [(Symbol, &'a [Layout<'a>])],
    /// how the closure will be represented at runtime
    representation: &'a Layout<'a>,
}

/// representation of the closure *for a particular function*
#[derive(Debug)]
pub enum ClosureRepresentation<'a> {
    /// the closure is represented as a union. Includes the tag ID!
    Union {
        tag_layout: &'a [Layout<'a>],
        tag_name: TagName,
        tag_id: u8,
        union_layout: UnionLayout<'a>,
    },
    /// the representation is anything but a union
    Other(Layout<'a>),
}

impl<'a> LambdaSet<'a> {
    pub fn runtime_representation(&self) -> Layout<'a> {
        *self.representation
    }

    pub fn is_represented(&self) -> Option<Layout<'a>> {
        if let Layout::Struct(&[]) = self.representation {
            None
        } else {
            Some(*self.representation)
        }
    }

    pub fn layout_for_member(&self, function_symbol: Symbol) -> ClosureRepresentation<'a> {
        debug_assert!(
            self.set.iter().any(|(s, _)| *s == function_symbol),
            "function symbol not in set"
        );

        match self.representation {
            Layout::Union(union) => {
                // here we rely on the fact that a union in a closure would be stored in a one-element record.
                // a closure representation that is itself union must be a of the shape `Closure1 ... | Closure2 ...`
                match union {
                    UnionLayout::NonRecursive(tags) => {
                        let index = self
                            .set
                            .iter()
                            .position(|(s, _)| *s == function_symbol)
                            .unwrap();

                        ClosureRepresentation::Union {
                            tag_id: index as u8,
                            tag_layout: tags[index],
                            tag_name: TagName::Closure(function_symbol),
                            union_layout: *union,
                        }
                    }
                    UnionLayout::Recursive(_) => todo!("recursive closures"),
                    UnionLayout::NonNullableUnwrapped(_) => todo!("recursive closures"),
                    UnionLayout::NullableWrapped {
                        nullable_id: _,
                        other_tags: _,
                    } => todo!("recursive closures"),
                    UnionLayout::NullableUnwrapped {
                        nullable_id: _,
                        other_fields: _,
                    } => todo!("recursive closures"),
                }
            }
            _ => ClosureRepresentation::Other(*self.representation),
        }
    }

    pub fn extend_argument_list(
        &self,
        arena: &'a Bump,
        argument_layouts: &'a [Layout<'a>],
    ) -> &'a [Layout<'a>] {
        if let [] = self.set {
            // TERRIBLE HACK for builting functions
            argument_layouts
        } else {
            match self.representation {
                Layout::Struct(&[]) => {
                    // this function does not have anything in its closure, and the lambda set is a
                    // singleton, so we pass no extra argument
                    argument_layouts
                }
                Layout::Builtin(Builtin::Int1) | Layout::Builtin(Builtin::Int8) => {
                    // we don't pass this along either
                    argument_layouts
                }
                _ => {
                    let mut arguments = Vec::with_capacity_in(argument_layouts.len() + 1, arena);
                    arguments.extend(argument_layouts);
                    arguments.push(self.runtime_representation());

                    arguments.into_bump_slice()
                }
            }
        }
    }

    pub fn from_var(
        arena: &'a Bump,
        subs: &Subs,
        closure_var: Variable,
    ) -> Result<Self, LayoutProblem> {
        let mut tags = std::vec::Vec::new();
        match roc_types::pretty_print::chase_ext_tag_union(subs, closure_var, &mut tags) {
            Ok(()) | Err((_, Content::FlexVar(_))) if !tags.is_empty() => {
                // sort the tags; make sure ordering stays intact!
                tags.sort();

                let mut set = Vec::with_capacity_in(tags.len(), arena);

                let mut env = Env {
                    arena,
                    subs,
                    seen: MutSet::default(),
                };

                for (tag_name, variables) in tags.iter() {
                    if let TagName::Closure(function_symbol) = tag_name {
                        let mut arguments = Vec::with_capacity_in(variables.len(), arena);

                        for var in variables {
                            arguments.push(Layout::from_var(&mut env, *var)?);
                        }

                        set.push((*function_symbol, arguments.into_bump_slice()));
                    } else {
                        unreachable!("non-closure tag name in lambda set");
                    }
                }

                let representation = arena.alloc(Self::make_representation(arena, subs, tags));

                Ok(LambdaSet {
                    set: set.into_bump_slice(),
                    representation,
                })
            }

            Ok(()) | Err((_, Content::FlexVar(_))) => {
                // TODO hack for builting functions.
                Ok(LambdaSet {
                    set: &[],
                    representation: arena.alloc(Layout::Struct(&[])),
                })
            }
            _ => panic!("called LambdaSet.from_var on invalid input"),
        }
    }

    fn make_representation(
        arena: &'a Bump,
        subs: &Subs,
        tags: std::vec::Vec<(TagName, std::vec::Vec<Variable>)>,
    ) -> Layout<'a> {
        // otherwise, this is a closure with a payload
        let variant = union_sorted_tags_help(arena, tags, None, subs);

        use UnionVariant::*;
        match variant {
            Never => Layout::Union(UnionLayout::NonRecursive(&[])),
            Unit | UnitWithArguments => Layout::Struct(&[]),
            BoolUnion { .. } => Layout::Builtin(Builtin::Int1),
            ByteUnion(_) => Layout::Builtin(Builtin::Int8),
            Newtype {
                arguments: layouts, ..
            } => Layout::Struct(layouts.into_bump_slice()),
            Wrapped(variant) => {
                use WrappedVariant::*;

                match variant {
                    NonRecursive {
                        sorted_tag_layouts: tags,
                    } => {
                        debug_assert!(tags.len() > 1);

                        // if the closed-over value is actually a layout, it should be wrapped in a 1-element record
                        debug_assert!(matches!(tags[0].0, TagName::Closure(_)));

                        let mut tag_arguments = Vec::with_capacity_in(tags.len(), arena);

                        for (_, tag_args) in tags.iter() {
                            tag_arguments.push(&tag_args[0..]);
                        }
                        Layout::Union(UnionLayout::NonRecursive(tag_arguments.into_bump_slice()))
                    }

                    _ => panic!("handle recursive layouts"),
                }
            }
        }
    }

    pub fn stack_size(&self, pointer_size: u32) -> u32 {
        self.representation.stack_size(pointer_size)
    }
    pub fn contains_refcounted(&self) -> bool {
        self.representation.contains_refcounted()
    }
    pub fn safe_to_memcpy(&self) -> bool {
        self.representation.safe_to_memcpy()
    }

    pub fn alignment_bytes(&self, pointer_size: u32) -> u32 {
        self.representation.alignment_bytes(pointer_size)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
    List(&'a Layout<'a>),
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

    fn remove_seen(&mut self, var: Variable) -> bool {
        let var = self.subs.get_root_key_without_compacting(var);

        self.seen.remove(&var)
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
            Struct(fields) => fields
                .iter()
                .all(|field_layout| field_layout.safe_to_memcpy()),
            Union(variant) => {
                use UnionLayout::*;

                match variant {
                    NonRecursive(tags) => tags
                        .iter()
                        .all(|tag_layout| tag_layout.iter().all(|field| field.safe_to_memcpy())),
                    Recursive(_)
                    | NullableWrapped { .. }
                    | NullableUnwrapped { .. }
                    | NonNullableUnwrapped(_) => {
                        // a recursive union will always contain a pointer, and is thus not safe to memcpy
                        false
                    }
                }
            }
            Closure(_, closure_layout, _) => closure_layout.safe_to_memcpy(),
            RecursivePointer => {
                // We cannot memcpy pointers, because then we would have the same pointer in multiple places!
                false
            }
            Boxed(_) => false,
        }
    }

    pub fn is_dropped_because_empty(&self) -> bool {
        // For this calculation, we don't need an accurate
        // stack size, we just need to know whether it's zero,
        // so it's fine to use a pointer size of 1.
        false
    }

    pub fn stack_size(&self, pointer_size: u32) -> u32 {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.stack_size(pointer_size),
            Struct(fields) => {
                let mut sum = 0;

                for field_layout in *fields {
                    sum += field_layout.stack_size(pointer_size);
                }

                sum
            }
            Union(variant) => {
                use UnionLayout::*;

                match variant {
                    NonRecursive(fields) => {
                        fields
                            .iter()
                            .map(|tag_layout| {
                                tag_layout
                                    .iter()
                                    .map(|field| field.stack_size(pointer_size))
                                    .sum::<u32>()
                            })
                            .max()
                            .unwrap_or_default()
                            // the size of the tag_id
                            + pointer_size
                    }

                    Recursive(_)
                    | NullableWrapped { .. }
                    | NullableUnwrapped { .. }
                    | NonNullableUnwrapped(_) => pointer_size,
                }
            }
            Closure(_, lambda_set, _) => lambda_set.stack_size(pointer_size),
            RecursivePointer => pointer_size,
            Boxed(_) => pointer_size,
        }
    }

    pub fn alignment_bytes(&self, pointer_size: u32) -> u32 {
        match self {
            Layout::Struct(fields) => fields
                .iter()
                .map(|x| x.alignment_bytes(pointer_size))
                .max()
                .unwrap_or(0),

            Layout::Union(variant) => {
                use UnionLayout::*;

                match variant {
                    NonRecursive(tags) => tags
                        .iter()
                        .map(|x| x.iter())
                        .flatten()
                        .map(|x| x.alignment_bytes(pointer_size))
                        .max()
                        .unwrap_or(0),
                    Recursive(_)
                    | NullableWrapped { .. }
                    | NullableUnwrapped { .. }
                    | NonNullableUnwrapped(_) => pointer_size,
                }
            }
            Layout::Builtin(builtin) => builtin.alignment_bytes(pointer_size),
            Layout::RecursivePointer => pointer_size,
            Layout::Boxed(_) => pointer_size,
            Layout::Closure(_, captured, _) => {
                pointer_size.max(captured.alignment_bytes(pointer_size))
            }
        }
    }

    pub fn is_refcounted(&self) -> bool {
        use self::Builtin::*;
        use Layout::*;

        match self {
            Union(variant) => {
                use UnionLayout::*;

                matches!(
                    variant,
                    Recursive(_) | NullableWrapped { .. } | NullableUnwrapped { .. }
                )
            }

            RecursivePointer => true,
            Boxed(_) => true,

            Builtin(List(_)) | Builtin(Str) => true,

            _ => false,
        }
    }

    /// Even if a value (say, a record) is not itself reference counted,
    /// it may contains values/fields that are. Therefore when this record
    /// goes out of scope, the refcount on those values/fields must  be decremented.
    pub fn contains_refcounted(&self) -> bool {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.is_refcounted(),
            Struct(fields) => fields.iter().any(|f| f.contains_refcounted()),
            Union(variant) => {
                use UnionLayout::*;

                match variant {
                    NonRecursive(fields) => fields
                        .iter()
                        .map(|ls| ls.iter())
                        .flatten()
                        .any(|f| f.contains_refcounted()),
                    Recursive(_)
                    | NullableWrapped { .. }
                    | NullableUnwrapped { .. }
                    | NonNullableUnwrapped(_) => true,
                }
            }
            RecursivePointer => true,

            Boxed(_) => {
                // technically we should look at layout of the box's content
                // but refcount insertion needs this to return true
                true
            }
            Closure(_, closure_layout, _) => closure_layout.contains_refcounted(),
        }
    }

    pub fn to_doc<D, A>(self, alloc: &'a D, parens: Parens) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.to_doc(alloc, parens),
            Struct(fields) => {
                let fields_doc = fields.iter().map(|x| x.to_doc(alloc, parens));

                alloc
                    .text("{")
                    .append(alloc.intersperse(fields_doc, ", "))
                    .append(alloc.text("}"))
            }
            Union(union_layout) => union_layout.to_doc(alloc, parens),
            RecursivePointer => alloc.text("*self"),
            Boxed(_) => alloc.text("unbox"),
            Closure(args, closure_layout, result) => {
                let args_doc = args.iter().map(|x| x.to_doc(alloc, Parens::InFunction));

                let bom = closure_layout
                    .representation
                    .to_doc(alloc, Parens::NotNeeded);

                alloc
                    .intersperse(args_doc, ", ")
                    .append(alloc.text(" {| "))
                    .append(bom)
                    .append(" |} -> ")
                    .append(result.to_doc(alloc, Parens::InFunction))
            }
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
                        Ok(layout) => Cached(*layout),
                        Err(problem) => Problem(problem.clone()),
                    };

                    self.layouts
                        .update_value(cached_var, |existing| existing.value = cached_layout);
                }

                result
            }
        }
    }

    pub fn raw_from_var(
        &mut self,
        arena: &'a Bump,
        var: Variable,
        subs: &Subs,
    ) -> Result<RawFunctionLayout<'a>, LayoutProblem> {
        // Store things according to the root Variable, to avoid duplicate work.
        let var = subs.get_root_key_without_compacting(var);

        let cached_var = CachedVariable::new(var);

        self.expand_to_fit(cached_var);

        use CachedLayout::*;
        match self.layouts.probe_value(cached_var) {
            Problem(problem) => Err(problem),
            Cached(_) | NotCached => {
                let mut env = Env {
                    arena,
                    subs,
                    seen: MutSet::default(),
                };

                Layout::from_var(&mut env, var).map(|l| match l {
                    Layout::Closure(a, b, c) => RawFunctionLayout::Function(a, b, c),
                    other => RawFunctionLayout::ZeroArgumentThunk(other),
                })
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
    pub const DICT_WORDS: u32 = 3;
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
            List(_) | EmptyList => Builtin::LIST_WORDS * pointer_size,
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
            List(_) | EmptyList => pointer_size,
        }
    }

    pub fn safe_to_memcpy(&self) -> bool {
        use Builtin::*;

        match self {
            Int128 | Int64 | Int32 | Int16 | Int8 | Int1 | Usize | Float128 | Float64 | Float32
            | Float16 | EmptyStr | EmptyDict | EmptyList | EmptySet => true,
            Str | Dict(_, _) | Set(_) | List(_) => false,
        }
    }

    // Question: does is_refcounted exactly correspond with the "safe to memcpy" property?
    pub fn is_refcounted(&self) -> bool {
        use Builtin::*;

        match self {
            Int128 | Int64 | Int32 | Int16 | Int8 | Int1 | Usize | Float128 | Float64 | Float32
            | Float16 | EmptyStr | EmptyDict | EmptyList | EmptySet => false,
            List(_) => true,

            Str | Dict(_, _) | Set(_) => true,
        }
    }

    pub fn to_doc<D, A>(self, alloc: &'a D, _parens: Parens) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use Builtin::*;

        match self {
            Int128 => alloc.text("Int128"),
            Int64 => alloc.text("Int64"),
            Int32 => alloc.text("Int32"),
            Int16 => alloc.text("Int16"),
            Int8 => alloc.text("Int8"),
            Int1 => alloc.text("Int1"),
            Usize => alloc.text("Usize"),
            Float128 => alloc.text("Float128"),
            Float64 => alloc.text("Float64"),
            Float32 => alloc.text("Float32"),
            Float16 => alloc.text("Float16"),

            EmptyStr => alloc.text("EmptyStr"),
            EmptyList => alloc.text("EmptyList"),
            EmptyDict => alloc.text("EmptyDict"),
            EmptySet => alloc.text("EmptySet"),

            Str => alloc.text("Str"),
            List(layout) => alloc
                .text("List ")
                .append(layout.to_doc(alloc, Parens::InTypeParam)),
            Set(layout) => alloc
                .text("Set ")
                .append(layout.to_doc(alloc, Parens::InTypeParam)),
            Dict(key_layout, value_layout) => alloc
                .text("Dict ")
                .append(key_layout.to_doc(alloc, Parens::InTypeParam))
                .append(" ")
                .append(value_layout.to_doc(alloc, Parens::InTypeParam)),
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
                Symbol::DICT_DICT => dict_layout_from_key_value(env, args[0], args[1]),
                Symbol::SET_SET => dict_layout_from_key_value(env, args[0], Variable::EMPTY_RECORD),
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

            let lambda_set = LambdaSet::from_var(env.arena, env.subs, closure_var)?;

            Ok(Layout::Closure(fn_args, lambda_set, ret))
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
        FunctionOrTagUnion(tag_name, _, ext_var) => {
            debug_assert!(ext_var_is_empty_tag_union(subs, ext_var));

            let mut tags = MutMap::default();
            tags.insert(tag_name, vec![]);

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
            let mut tag_layouts = Vec::with_capacity_in(tags.len(), arena);

            // VERY IMPORTANT: sort the tags
            let mut tags_vec: std::vec::Vec<_> = tags.into_iter().collect();
            tags_vec.sort();

            let mut nullable = None;

            if GENERATE_NULLABLE {
                for (index, (_name, variables)) in tags_vec.iter().enumerate() {
                    if variables.is_empty() {
                        nullable = Some(index as i64);
                        break;
                    }
                }
            }

            env.insert_seen(rec_var);
            for (index, (_name, variables)) in tags_vec.into_iter().enumerate() {
                if matches!(nullable, Some(i) if i == index as i64) {
                    // don't add the nullable case
                    continue;
                }

                let mut tag_layout = Vec::with_capacity_in(variables.len() + 1, arena);

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
                UnionLayout::NonNullableUnwrapped(&tag_layouts.pop().unwrap())
            } else {
                UnionLayout::Recursive(tag_layouts.into_bump_slice())
            };

            Ok(Layout::Union(union_layout))
        }
        EmptyTagUnion => {
            panic!("TODO make Layout for empty Tag Union");
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
    BoolUnion {
        ttrue: TagName,
        ffalse: TagName,
    },
    ByteUnion(Vec<'a, TagName>),
    Newtype {
        tag_name: TagName,
        arguments: Vec<'a, Layout<'a>>,
    },
    Wrapped(WrappedVariant<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WrappedVariant<'a> {
    Recursive {
        sorted_tag_layouts: Vec<'a, (TagName, &'a [Layout<'a>])>,
    },
    NonRecursive {
        sorted_tag_layouts: Vec<'a, (TagName, &'a [Layout<'a>])>,
    },
    NullableWrapped {
        nullable_id: i64,
        nullable_name: TagName,
        sorted_tag_layouts: Vec<'a, (TagName, &'a [Layout<'a>])>,
    },
    NonNullableUnwrapped {
        tag_name: TagName,
        fields: &'a [Layout<'a>],
    },
    NullableUnwrapped {
        nullable_id: bool,
        nullable_name: TagName,
        other_name: TagName,
        other_fields: &'a [Layout<'a>],
    },
}

impl<'a> WrappedVariant<'a> {
    pub fn tag_name_to_id(&self, tag_name: &TagName) -> (u8, &'a [Layout<'a>]) {
        use WrappedVariant::*;

        match self {
            Recursive { sorted_tag_layouts } | NonRecursive { sorted_tag_layouts } => {
                let (tag_id, (_, argument_layouts)) = sorted_tag_layouts
                    .iter()
                    .enumerate()
                    .find(|(_, (key, _))| key == tag_name)
                    .expect("tag name is not in its own type");

                debug_assert!(tag_id < 256);
                (tag_id as u8, *argument_layouts)
            }
            NullableWrapped {
                nullable_id,
                nullable_name,
                sorted_tag_layouts,
            } => {
                // assumption: the nullable_name is not included in sorted_tag_layouts

                if tag_name == nullable_name {
                    (*nullable_id as u8, &[] as &[_])
                } else {
                    let (mut tag_id, (_, argument_layouts)) = sorted_tag_layouts
                        .iter()
                        .enumerate()
                        .find(|(_, (key, _))| key == tag_name)
                        .expect("tag name is not in its own type");

                    if tag_id >= *nullable_id as usize {
                        tag_id += 1;
                    }

                    debug_assert!(tag_id < 256);
                    (tag_id as u8, *argument_layouts)
                }
            }
            NullableUnwrapped {
                nullable_id,
                nullable_name,
                other_name,
                other_fields,
            } => {
                if tag_name == nullable_name {
                    (*nullable_id as u8, &[] as &[_])
                } else {
                    debug_assert_eq!(other_name, tag_name);

                    (!*nullable_id as u8, *other_fields)
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
    arena: &'a Bump,
    var: Variable,
    subs: &Subs,
) -> Result<UnionVariant<'a>, LayoutProblem> {
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
        Err((_, Content::Error)) => return Err(LayoutProblem::Erroneous),
        Err(other) => panic!("invalid content in tag union variable: {:?}", other),
    };

    Ok(result)
}

fn get_recursion_var(subs: &Subs, var: Variable) -> Option<Variable> {
    match subs.get_without_compacting(var).content {
        Content::Structure(FlatType::RecursiveTagUnion(rec_var, _, _)) => Some(rec_var),
        Content::Alias(_, _, actual) => get_recursion_var(subs, actual),
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
                                // then it's zero-sized; In the future we may drop this argument
                                // completely, but for now we represent it with the empty struct
                                layouts.push(Layout::Struct(&[]))
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
            } else if opt_rec_var.is_some() {
                UnionVariant::Wrapped(WrappedVariant::NonNullableUnwrapped {
                    tag_name,
                    fields: layouts.into_bump_slice(),
                })
            } else {
                UnionVariant::Newtype {
                    tag_name,
                    arguments: layouts,
                }
            }
        }
        num_tags => {
            // default path
            let mut answer = Vec::with_capacity_in(tags_vec.len(), arena);
            let mut has_any_arguments = false;

            let mut nullable = None;

            // only recursive tag unions can be nullable
            let is_recursive = opt_rec_var.is_some();
            if is_recursive && GENERATE_NULLABLE {
                for (index, (name, variables)) in tags_vec.iter().enumerate() {
                    if variables.is_empty() {
                        nullable = Some((index as i64, name.clone()));
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

                let mut arg_layouts = Vec::with_capacity_in(arguments.len() + 1, arena);

                for var in arguments {
                    match Layout::from_var(&mut env, var) {
                        Ok(layout) => {
                            has_any_arguments = true;

                            // make sure to not unroll recursive types!
                            let self_recursion = opt_rec_var.is_some()
                                && subs.get_root_key_without_compacting(var)
                                    == subs.get_root_key_without_compacting(opt_rec_var.unwrap())
                                && is_recursive_tag_union(&layout);

                            if self_recursion {
                                arg_layouts.push(Layout::RecursivePointer);
                            } else {
                                arg_layouts.push(layout);
                            }
                        }
                        Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                            // If we encounter an unbound type var (e.g. `Ok *`)
                            // then it's zero-sized; In the future we may drop this argument
                            // completely, but for now we represent it with the empty struct
                            arg_layouts.push(Layout::Struct(&[]));
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
                _ => {
                    let variant = if let Some((nullable_id, nullable_name)) = nullable {
                        if answer.len() == 1 {
                            let (other_name, other_arguments) = answer.drain(..).next().unwrap();
                            let nullable_id = nullable_id != 0;

                            WrappedVariant::NullableUnwrapped {
                                nullable_id,
                                nullable_name,
                                other_name,
                                other_fields: other_arguments,
                            }
                        } else {
                            WrappedVariant::NullableWrapped {
                                nullable_id,
                                nullable_name,
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

                    UnionVariant::Wrapped(variant)
                }
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
                Never => Layout::Union(UnionLayout::NonRecursive(&[])),
                Unit | UnitWithArguments => Layout::Struct(&[]),
                BoolUnion { .. } => Layout::Builtin(Builtin::Int1),
                ByteUnion(_) => Layout::Builtin(Builtin::Int8),
                Newtype {
                    arguments: mut field_layouts,
                    ..
                } => {
                    if field_layouts.len() == 1 {
                        field_layouts.pop().unwrap()
                    } else {
                        Layout::Struct(field_layouts.into_bump_slice())
                    }
                }
                Wrapped(variant) => {
                    use WrappedVariant::*;

                    match variant {
                        NonRecursive {
                            sorted_tag_layouts: tags,
                        } => {
                            let mut tag_layouts = Vec::with_capacity_in(tags.len(), arena);
                            tag_layouts.extend(tags.iter().map(|r| r.1));

                            Layout::Union(UnionLayout::NonRecursive(tag_layouts.into_bump_slice()))
                        }

                        Recursive {
                            sorted_tag_layouts: tags,
                        } => {
                            let mut tag_layouts = Vec::with_capacity_in(tags.len(), arena);
                            tag_layouts.extend(tags.iter().map(|r| r.1));

                            debug_assert!(tag_layouts.len() > 1);
                            Layout::Union(UnionLayout::Recursive(tag_layouts.into_bump_slice()))
                        }

                        NullableWrapped {
                            nullable_id,
                            nullable_name: _,
                            sorted_tag_layouts: tags,
                        } => {
                            let mut tag_layouts = Vec::with_capacity_in(tags.len(), arena);
                            tag_layouts.extend(tags.iter().map(|r| r.1));

                            Layout::Union(UnionLayout::NullableWrapped {
                                nullable_id,
                                other_tags: tag_layouts.into_bump_slice(),
                            })
                        }

                        NullableUnwrapped { .. } => todo!(),
                        NonNullableUnwrapped { .. } => todo!(),
                    }
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

fn dict_layout_from_key_value<'a>(
    env: &mut Env<'a, '_>,
    key_var: Variable,
    value_var: Variable,
) -> Result<Layout<'a>, LayoutProblem> {
    match env.subs.get_without_compacting(key_var).content {
        Content::FlexVar(_) | Content::RigidVar(_) => {
            // If this was still a (Dict * *) then it must have been an empty dict
            Ok(Layout::Builtin(Builtin::EmptyDict))
        }
        key_content => {
            let value_content = env.subs.get_without_compacting(value_var).content;
            let key_layout = Layout::new_help(env, key_var, key_content)?;
            let value_layout = Layout::new_help(env, value_var, value_content)?;

            // This is a normal list.
            Ok(Layout::Builtin(Builtin::Dict(
                env.arena.alloc(key_layout),
                env.arena.alloc(value_layout),
            )))
        }
    }
}

pub fn list_layout_from_elem<'a>(
    env: &mut Env<'a, '_>,
    elem_var: Variable,
) -> Result<Layout<'a>, LayoutProblem> {
    match env.subs.get_without_compacting(elem_var).content {
        Content::FlexVar(_) | Content::RigidVar(_) => {
            // If this was still a (List *) then it must have been an empty list
            Ok(Layout::Builtin(Builtin::EmptyList))
        }
        _ => {
            let elem_layout = Layout::from_var(env, elem_var)?;

            Ok(Layout::Builtin(Builtin::List(env.arena.alloc(elem_layout))))
        }
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
    toplevels_by_id: MutMap<crate::ir::ProcLayout<'a>, u32>,
    next_id: u32,
}

#[derive(Default)]
pub struct LayoutIds<'a> {
    by_symbol: MutMap<Symbol, IdsByLayout<'a>>,
}

impl<'a> LayoutIds<'a> {
    /// Returns a LayoutId which is unique for the given symbol and layout.
    /// If given the same symbol and same layout, returns the same LayoutId.
    pub fn get<'b>(&mut self, symbol: Symbol, layout: &'b Layout<'a>) -> LayoutId {
        // Note: this function does some weird stuff to satisfy the borrow checker.
        // There's probably a nicer way to write it that still works.
        let ids = self.by_symbol.entry(symbol).or_insert_with(|| IdsByLayout {
            by_id: HashMap::with_capacity_and_hasher(1, default_hasher()),
            toplevels_by_id: Default::default(),
            next_id: 1,
        });

        // Get the id associated with this layout, or default to next_id.
        let answer = ids.by_id.get(&layout).copied().unwrap_or(ids.next_id);

        // If we had to default to next_id, it must not have been found;
        // store the ID we're going to return and increment next_id.
        if answer == ids.next_id {
            ids.by_id.insert(*layout, ids.next_id);

            ids.next_id += 1;
        }

        LayoutId(answer)
    }

    /// Returns a LayoutId which is unique for the given symbol and layout.
    /// If given the same symbol and same layout, returns the same LayoutId.
    pub fn get_toplevel<'b>(
        &mut self,
        symbol: Symbol,
        layout: &'b crate::ir::ProcLayout<'a>,
    ) -> LayoutId {
        // Note: this function does some weird stuff to satisfy the borrow checker.
        // There's probably a nicer way to write it that still works.
        let ids = self.by_symbol.entry(symbol).or_insert_with(|| IdsByLayout {
            by_id: Default::default(),
            toplevels_by_id: HashMap::with_capacity_and_hasher(1, default_hasher()),
            next_id: 1,
        });

        // Get the id associated with this layout, or default to next_id.
        let answer = ids
            .toplevels_by_id
            .get(&layout)
            .copied()
            .unwrap_or(ids.next_id);

        // If we had to default to next_id, it must not have been found;
        // store the ID we're going to return and increment next_id.
        if answer == ids.next_id {
            ids.toplevels_by_id.insert(*layout, ids.next_id);

            ids.next_id += 1;
        }

        LayoutId(answer)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ListLayout<'a> {
    EmptyList,
    List(&'a Layout<'a>),
}

impl<'a> std::convert::TryFrom<&Layout<'a>> for ListLayout<'a> {
    type Error = ();

    fn try_from(value: &Layout<'a>) -> Result<Self, Self::Error> {
        match value {
            Layout::Builtin(Builtin::EmptyList) => Ok(ListLayout::EmptyList),
            Layout::Builtin(Builtin::List(element)) => Ok(ListLayout::List(element)),
            _ => Err(()),
        }
    }
}
