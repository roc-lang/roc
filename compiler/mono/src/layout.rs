use crate::ir::Parens;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::all::{default_hasher, MutMap};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{Interns, Symbol};
use roc_problem::can::RuntimeError;
use roc_target::{PtrWidth, TargetInfo};
use roc_types::subs::{
    Content, FlatType, RecordFields, Subs, UnionTags, UnsortedUnionTags, Variable,
};
use roc_types::types::{gather_fields_unsorted_iter, RecordField};
use std::collections::hash_map::{DefaultHasher, Entry};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use ven_pretty::{DocAllocator, DocBuilder};

// if your changes cause this number to go down, great!
// please change it to the lower number.
// if it went up, maybe check that the change is really required
static_assertions::assert_eq_size!([usize; 3], Builtin);
static_assertions::assert_eq_size!([usize; 4], Layout);
static_assertions::assert_eq_size!([usize; 3], UnionLayout);
static_assertions::assert_eq_size!([usize; 3], LambdaSet);

pub type TagIdIntType = u16;
pub const MAX_ENUM_SIZE: usize = (std::mem::size_of::<TagIdIntType>() * 8) as usize;
const GENERATE_NULLABLE: bool = true;

#[derive(Debug, Clone)]
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
    ) -> Result<Self, LayoutProblem> {
        use roc_types::subs::Content::*;
        match content {
            FlexVar(_) | RigidVar(_) => Err(LayoutProblem::UnresolvedTypeVar(var)),
            RecursionVar { structure, .. } => {
                let structure_content = env.subs.get_content_without_compacting(structure);
                Self::new_help(env, structure, structure_content.clone())
            }
            Structure(flat_type) => Self::layout_from_flat_type(env, flat_type),
            RangedNumber(typ, _) => Self::from_var(env, typ),

            // Ints
            Alias(Symbol::NUM_I128, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::i128()))
            }
            Alias(Symbol::NUM_I64, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::i64()))
            }
            Alias(Symbol::NUM_I32, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::i32()))
            }
            Alias(Symbol::NUM_I16, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::i16()))
            }
            Alias(Symbol::NUM_I8, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::i8()))
            }

            // I think unsigned and signed use the same layout
            Alias(Symbol::NUM_U128, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::u128()))
            }
            Alias(Symbol::NUM_U64, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::u64()))
            }
            Alias(Symbol::NUM_U32, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::u32()))
            }
            Alias(Symbol::NUM_U16, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::u16()))
            }
            Alias(Symbol::NUM_U8, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::u8()))
            }

            // Floats
            Alias(Symbol::NUM_F64, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::f64()))
            }
            Alias(Symbol::NUM_F32, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::f32()))
            }

            // Nat
            Alias(Symbol::NUM_NAT, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::usize(env.target_info)))
            }

            Alias(symbol, _, _) if symbol.is_builtin() => Ok(Self::ZeroArgumentThunk(
                Layout::new_help(env, var, content)?,
            )),

            Alias(_, _, var) => Self::from_var(env, var),
            Error => Err(LayoutProblem::Erroneous),
        }
    }

    fn layout_from_flat_type(
        env: &mut Env<'a, '_>,
        flat_type: FlatType,
    ) -> Result<Self, LayoutProblem> {
        use roc_types::subs::FlatType::*;

        let arena = env.arena;

        match flat_type {
            Func(args, closure_var, ret_var) => {
                let mut fn_args = Vec::with_capacity_in(args.len(), arena);

                for index in args.into_iter() {
                    let arg_var = env.subs[index];
                    fn_args.push(Layout::from_var(env, arg_var)?);
                }

                let ret = Layout::from_var(env, ret_var)?;

                let fn_args = fn_args.into_bump_slice();
                let ret = arena.alloc(ret);

                let lambda_set =
                    LambdaSet::from_var(env.arena, env.subs, closure_var, env.target_info)?;

                Ok(Self::Function(fn_args, lambda_set, ret))
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
                let layout = layout_from_flat_type(env, flat_type)?;
                Ok(Self::ZeroArgumentThunk(layout))
            }
        }
    }

    /// Returns Err(()) if given an error, or Ok(Layout) if given a non-erroneous Structure.
    /// Panics if given a FlexVar or RigidVar, since those should have been
    /// monomorphized away already!
    fn from_var(env: &mut Env<'a, '_>, var: Variable) -> Result<Self, LayoutProblem> {
        if env.is_seen(var) {
            unreachable!("The initial variable of a signature cannot be seen already")
        } else {
            let content = env.subs.get_content_without_compacting(var);
            Self::new_help(env, var, content.clone())
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
        /// See also https://github.com/rtfeldman/roc/issues/2535.
        field_order_hash: FieldOrderHash,
        field_layouts: &'a [Layout<'a>],
    },
    Union(UnionLayout<'a>),
    LambdaSet(LambdaSet<'a>),
    RecursivePointer,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnionLayout<'a> {
    /// A non-recursive tag union
    /// e.g. `Result a e : [ Ok a, Err e ]`
    NonRecursive(&'a [&'a [Layout<'a>]]),
    /// A recursive tag union (general case)
    /// e.g. `Expr : [ Sym Str, Add Expr Expr ]`
    Recursive(&'a [&'a [Layout<'a>]]),
    /// A recursive tag union with just one constructor
    /// Optimization: No need to store a tag ID (the payload is "unwrapped")
    /// e.g. `RoseTree a : [ Tree a (List (RoseTree a)) ]`
    NonNullableUnwrapped(&'a [Layout<'a>]),
    /// A recursive tag union that has an empty variant
    /// Optimization: Represent the empty variant as null pointer => no memory usage & fast comparison
    /// It has more than one other variant, so they need tag IDs (payloads are "wrapped")
    /// e.g. `FingerTree a : [ Empty, Single a, More (Some a) (FingerTree (Tuple a)) (Some a) ]`
    /// see also: https://youtu.be/ip92VMpf_-A?t=164
    NullableWrapped {
        nullable_id: u16,
        other_tags: &'a [&'a [Layout<'a>]],
    },
    /// A recursive tag union with only two variants, where one is empty.
    /// Optimizations: Use null for the empty variant AND don't store a tag ID for the other variant.
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

    fn tag_id_builtin_help(union_size: usize) -> Builtin<'a> {
        if union_size <= u8::MAX as usize {
            Builtin::Int(IntWidth::U8)
        } else if union_size <= u16::MAX as usize {
            Builtin::Int(IntWidth::U16)
        } else {
            panic!("tag union is too big")
        }
    }

    pub fn tag_id_builtin(&self) -> Builtin<'a> {
        match self {
            UnionLayout::NonRecursive(tags) => {
                let union_size = tags.len();
                Self::tag_id_builtin_help(union_size)
            }
            UnionLayout::Recursive(tags) => {
                let union_size = tags.len();

                Self::tag_id_builtin_help(union_size)
            }

            UnionLayout::NullableWrapped { other_tags, .. } => {
                Self::tag_id_builtin_help(other_tags.len() + 1)
            }
            UnionLayout::NonNullableUnwrapped(_) => Builtin::Bool,
            UnionLayout::NullableUnwrapped { .. } => Builtin::Bool,
        }
    }

    pub fn tag_id_layout(&self) -> Layout<'a> {
        Layout::Builtin(self.tag_id_builtin())
    }

    fn stores_tag_id_in_pointer_bits(tags: &[&[Layout<'a>]], target_info: TargetInfo) -> bool {
        tags.len() < target_info.ptr_width() as usize
    }

    pub fn tag_id_pointer_bits_and_mask(target_info: TargetInfo) -> (usize, usize) {
        match target_info.ptr_width() {
            PtrWidth::Bytes8 => (3, 0b0000_0111),
            PtrWidth::Bytes4 => (2, 0b0000_0011),
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

    fn tags_alignment_bytes(tags: &[&[Layout]], target_info: TargetInfo) -> u32 {
        tags.iter()
            .map(|field_layouts| {
                Layout::struct_no_name_order(field_layouts).alignment_bytes(target_info)
            })
            .max()
            .unwrap_or(0)
    }

    pub fn allocation_alignment_bytes(&self, target_info: TargetInfo) -> u32 {
        let allocation = match self {
            UnionLayout::NonRecursive(_) => unreachable!("not heap-allocated"),
            UnionLayout::Recursive(tags) => Self::tags_alignment_bytes(tags, target_info),
            UnionLayout::NonNullableUnwrapped(field_layouts) => {
                Layout::struct_no_name_order(field_layouts).alignment_bytes(target_info)
            }
            UnionLayout::NullableWrapped { other_tags, .. } => {
                Self::tags_alignment_bytes(other_tags, target_info)
            }
            UnionLayout::NullableUnwrapped { other_fields, .. } => {
                Layout::struct_no_name_order(other_fields).alignment_bytes(target_info)
            }
        };

        // because we store a refcount, the alignment must be at least the size of a pointer
        allocation.max(target_info.ptr_width() as u32)
    }

    /// Size of the data in memory, whether it's stack or heap (for non-null tag ids)
    pub fn data_size_and_alignment(&self, target_info: TargetInfo) -> (u32, u32) {
        let id_data_layout = if self.stores_tag_id_as_data(target_info) {
            Some(self.tag_id_layout())
        } else {
            None
        };

        self.data_size_and_alignment_help_match(id_data_layout, target_info)
    }

    /// Size of the data before the tag_id, if it exists.
    /// Returns None if the tag_id is not stored as data in the layout.
    pub fn data_size_without_tag_id(&self, target_info: TargetInfo) -> Option<u32> {
        if !self.stores_tag_id_as_data(target_info) {
            return None;
        };

        Some(self.data_size_and_alignment_help_match(None, target_info).0)
    }

    fn data_size_and_alignment_help_match(
        &self,
        id_data_layout: Option<Layout>,
        target_info: TargetInfo,
    ) -> (u32, u32) {
        match self {
            Self::NonRecursive(tags) => {
                Self::data_size_and_alignment_help(tags, id_data_layout, target_info)
            }
            Self::Recursive(tags) => {
                Self::data_size_and_alignment_help(tags, id_data_layout, target_info)
            }
            Self::NonNullableUnwrapped(fields) => {
                Self::data_size_and_alignment_help(&[fields], id_data_layout, target_info)
            }
            Self::NullableWrapped { other_tags, .. } => {
                Self::data_size_and_alignment_help(other_tags, id_data_layout, target_info)
            }
            Self::NullableUnwrapped { other_fields, .. } => {
                Self::data_size_and_alignment_help(&[other_fields], id_data_layout, target_info)
            }
        }
    }

    fn data_size_and_alignment_help(
        variant_field_layouts: &[&[Layout]],
        id_data_layout: Option<Layout>,
        target_info: TargetInfo,
    ) -> (u32, u32) {
        let mut size = 0;
        let mut alignment_bytes = 0;

        for field_layouts in variant_field_layouts {
            let mut data = Layout::struct_no_name_order(field_layouts);

            let fields_and_id;
            if let Some(id_layout) = id_data_layout {
                fields_and_id = [data, id_layout];
                data = Layout::struct_no_name_order(&fields_and_id);
            }

            let (variant_size, variant_alignment) = data.stack_size_and_alignment(target_info);
            alignment_bytes = alignment_bytes.max(variant_alignment);
            size = size.max(variant_size);
        }

        (size, alignment_bytes)
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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
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
        alphabetic_order_fields: &'a [Layout<'a>],
        tag_name: TagName,
        tag_id: TagIdIntType,
        union_layout: UnionLayout<'a>,
    },
    /// The closure is represented as a struct. The layouts are sorted
    /// alphabetically by the identifier that is captured.
    ///
    /// We MUST sort these according to their stack size before code gen!
    AlphabeticOrderStruct(&'a [Layout<'a>]),
    /// the representation is anything but a union
    Other(Layout<'a>),
}

impl<'a> LambdaSet<'a> {
    pub fn runtime_representation(&self) -> Layout<'a> {
        *self.representation
    }

    /// Does the lambda set contain the given symbol?
    pub fn contains(&self, symbol: Symbol) -> bool {
        self.set.iter().any(|(s, _)| *s == symbol)
    }

    pub fn is_represented(&self) -> Option<Layout<'a>> {
        if let Layout::Struct {
            field_layouts: &[], ..
        } = self.representation
        {
            None
        } else {
            Some(*self.representation)
        }
    }

    pub fn member_does_not_need_closure_argument(&self, function_symbol: Symbol) -> bool {
        match self.layout_for_member(function_symbol) {
            ClosureRepresentation::Union {
                alphabetic_order_fields,
                ..
            } => alphabetic_order_fields.is_empty(),
            ClosureRepresentation::AlphabeticOrderStruct(fields) => fields.is_empty(),
            ClosureRepresentation::Other(_) => false,
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
                    UnionLayout::NonRecursive(_) => {
                        // get the fields from the set, where they are sorted in alphabetic order
                        // (and not yet sorted by their alignment)
                        let (index, (_, fields)) = self
                            .set
                            .iter()
                            .enumerate()
                            .find(|(_, (s, _))| *s == function_symbol)
                            .unwrap();

                        ClosureRepresentation::Union {
                            tag_id: index as TagIdIntType,
                            alphabetic_order_fields: fields,
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
            Layout::Struct { .. } => {
                // get the fields from the set, where they are sorted in alphabetic order
                // (and not yet sorted by their alignment)
                let (_, fields) = self
                    .set
                    .iter()
                    .find(|(s, _)| *s == function_symbol)
                    .unwrap();

                ClosureRepresentation::AlphabeticOrderStruct(fields)
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
                Layout::Struct {
                    field_layouts: &[], ..
                } => {
                    // this function does not have anything in its closure, and the lambda set is a
                    // singleton, so we pass no extra argument
                    argument_layouts
                }
                Layout::Builtin(Builtin::Bool)
                | Layout::Builtin(Builtin::Int(IntWidth::I8 | IntWidth::U8)) => {
                    // we don't pass this along either
                    argument_layouts
                }
                _ => {
                    let mut arguments = Vec::with_capacity_in(argument_layouts.len() + 1, arena);
                    arguments.extend(argument_layouts);
                    arguments.push(Layout::LambdaSet(*self));

                    arguments.into_bump_slice()
                }
            }
        }
    }

    pub fn from_var(
        arena: &'a Bump,
        subs: &Subs,
        closure_var: Variable,
        target_info: TargetInfo,
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
                    seen: Vec::new_in(arena),
                    target_info,
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

                let representation =
                    arena.alloc(Self::make_representation(arena, subs, tags, target_info));

                Ok(LambdaSet {
                    set: set.into_bump_slice(),
                    representation,
                })
            }

            Ok(()) | Err((_, Content::FlexVar(_))) => {
                // this can happen when there is a type error somewhere
                Ok(LambdaSet {
                    set: &[],
                    representation: arena.alloc(Layout::UNIT),
                })
            }
            _ => panic!("called LambdaSet.from_var on invalid input"),
        }
    }

    fn make_representation(
        arena: &'a Bump,
        subs: &Subs,
        tags: std::vec::Vec<(TagName, std::vec::Vec<Variable>)>,
        target_info: TargetInfo,
    ) -> Layout<'a> {
        // otherwise, this is a closure with a payload
        let variant = union_sorted_tags_help(arena, tags, None, subs, target_info);

        use UnionVariant::*;
        match variant {
            Never => Layout::VOID,
            BoolUnion { .. } => Layout::bool(),
            ByteUnion { .. } => Layout::u8(),
            Unit | UnitWithArguments => {
                // no useful information to store
                Layout::UNIT
            }
            Newtype {
                arguments: layouts, ..
            } => Layout::struct_no_name_order(layouts.into_bump_slice()),
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

    pub fn stack_size(&self, target_info: TargetInfo) -> u32 {
        self.representation.stack_size(target_info)
    }
    pub fn contains_refcounted(&self) -> bool {
        self.representation.contains_refcounted()
    }
    pub fn safe_to_memcpy(&self) -> bool {
        self.representation.safe_to_memcpy()
    }

    pub fn alignment_bytes(&self, target_info: TargetInfo) -> u32 {
        self.representation.alignment_bytes(target_info)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Builtin<'a> {
    Int(IntWidth),
    Float(FloatWidth),
    Bool,
    Decimal,
    Str,
    Dict(&'a Layout<'a>, &'a Layout<'a>),
    Set(&'a Layout<'a>),
    List(&'a Layout<'a>),
}

pub struct Env<'a, 'b> {
    target_info: TargetInfo,
    arena: &'a Bump,
    seen: Vec<'a, Variable>,
    subs: &'b Subs,
}

impl<'a, 'b> Env<'a, 'b> {
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
}

pub const fn round_up_to_alignment(width: u32, alignment: u32) -> u32 {
    if alignment != 0 && width % alignment > 0 {
        width + alignment - (width % alignment)
    } else {
        width
    }
}

impl<'a> Layout<'a> {
    pub const VOID: Self = Layout::Union(UnionLayout::NonRecursive(&[]));
    pub const UNIT: Self = Layout::Struct {
        field_layouts: &[],
        field_order_hash: FieldOrderHash::ZERO_FIELD_HASH,
    };

    fn new_help<'b>(
        env: &mut Env<'a, 'b>,
        var: Variable,
        content: Content,
    ) -> Result<Self, LayoutProblem> {
        use roc_types::subs::Content::*;
        match content {
            FlexVar(_) | RigidVar(_) => Err(LayoutProblem::UnresolvedTypeVar(var)),
            RecursionVar { structure, .. } => {
                let structure_content = env.subs.get_content_without_compacting(structure);
                Self::new_help(env, structure, structure_content.clone())
            }
            Structure(flat_type) => layout_from_flat_type(env, flat_type),

            Alias(symbol, _args, actual_var) => {
                if let Some(int_width) = IntWidth::try_from_symbol(symbol) {
                    return Ok(Layout::Builtin(Builtin::Int(int_width)));
                }

                if let Some(float_width) = FloatWidth::try_from_symbol(symbol) {
                    return Ok(Layout::Builtin(Builtin::Float(float_width)));
                }

                match symbol {
                    Symbol::NUM_DECIMAL | Symbol::NUM_AT_DECIMAL => {
                        return Ok(Layout::Builtin(Builtin::Decimal))
                    }

                    Symbol::NUM_NAT | Symbol::NUM_NATURAL | Symbol::NUM_AT_NATURAL => {
                        return Ok(Layout::usize(env.target_info))
                    }

                    _ => Self::from_var(env, actual_var),
                }
            }

            RangedNumber(typ, _) => Self::from_var(env, typ),

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
            let content = env.subs.get_content_without_compacting(var);
            Self::new_help(env, var, content.clone())
        }
    }

    pub fn safe_to_memcpy(&self) -> bool {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.safe_to_memcpy(),
            Struct { field_layouts, .. } => field_layouts
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
            LambdaSet(lambda_set) => lambda_set.runtime_representation().safe_to_memcpy(),
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
        false
    }

    pub fn is_passed_by_reference(&self) -> bool {
        match self {
            Layout::Union(UnionLayout::NonRecursive(_)) => true,
            Layout::LambdaSet(lambda_set) => {
                lambda_set.runtime_representation().is_passed_by_reference()
            }
            _ => false,
        }
    }

    pub fn stack_size(&self, target_info: TargetInfo) -> u32 {
        let width = self.stack_size_without_alignment(target_info);
        let alignment = self.alignment_bytes(target_info);

        round_up_to_alignment(width, alignment)
    }

    pub fn stack_size_and_alignment(&self, target_info: TargetInfo) -> (u32, u32) {
        let width = self.stack_size_without_alignment(target_info);
        let alignment = self.alignment_bytes(target_info);

        let size = round_up_to_alignment(width, alignment);
        (size, alignment)
    }

    fn stack_size_without_alignment(&self, target_info: TargetInfo) -> u32 {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.stack_size(target_info),
            Struct { field_layouts, .. } => {
                let mut sum = 0;

                for field_layout in *field_layouts {
                    sum += field_layout.stack_size(target_info);
                }

                sum
            }
            Union(variant) => {
                use UnionLayout::*;

                match variant {
                    NonRecursive(_) => variant.data_size_and_alignment(target_info).0,

                    Recursive(_)
                    | NullableWrapped { .. }
                    | NullableUnwrapped { .. }
                    | NonNullableUnwrapped(_) => target_info.ptr_width() as u32,
                }
            }
            LambdaSet(lambda_set) => lambda_set
                .runtime_representation()
                .stack_size_without_alignment(target_info),
            RecursivePointer => target_info.ptr_width() as u32,
        }
    }

    pub fn alignment_bytes(&self, target_info: TargetInfo) -> u32 {
        match self {
            Layout::Struct { field_layouts, .. } => field_layouts
                .iter()
                .map(|x| x.alignment_bytes(target_info))
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
                                    .map(|layout| layout.alignment_bytes(target_info))
                            })
                            .max();

                        let tag_id_builtin = variant.tag_id_builtin();
                        match max_alignment {
                            Some(align) => round_up_to_alignment(
                                align.max(tag_id_builtin.alignment_bytes(target_info)),
                                tag_id_builtin.alignment_bytes(target_info),
                            ),
                            None => {
                                // none of the tags had any payload, but the tag id still contains information
                                tag_id_builtin.alignment_bytes(target_info)
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
                .runtime_representation()
                .alignment_bytes(target_info),
            Layout::Builtin(builtin) => builtin.alignment_bytes(target_info),
            Layout::RecursivePointer => target_info.ptr_width() as u32,
        }
    }

    pub fn allocation_alignment_bytes(&self, target_info: TargetInfo) -> u32 {
        match self {
            Layout::Builtin(builtin) => builtin.allocation_alignment_bytes(target_info),
            Layout::Struct { .. } => unreachable!("not heap-allocated"),
            Layout::Union(union_layout) => union_layout.allocation_alignment_bytes(target_info),
            Layout::LambdaSet(lambda_set) => lambda_set
                .runtime_representation()
                .allocation_alignment_bytes(target_info),
            Layout::RecursivePointer => unreachable!("should be looked up to get an actual layout"),
        }
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
    pub fn contains_refcounted(&self) -> bool {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.is_refcounted(),
            Struct { field_layouts, .. } => field_layouts.iter().any(|f| f.contains_refcounted()),
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
            LambdaSet(lambda_set) => lambda_set.runtime_representation().contains_refcounted(),
            RecursivePointer => true,
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
            Struct { field_layouts, .. } => {
                let fields_doc = field_layouts.iter().map(|x| x.to_doc(alloc, parens));

                alloc
                    .text("{")
                    .append(alloc.intersperse(fields_doc, ", "))
                    .append(alloc.text("}"))
            }
            Union(union_layout) => union_layout.to_doc(alloc, parens),
            LambdaSet(lambda_set) => lambda_set.runtime_representation().to_doc(alloc, parens),
            RecursivePointer => alloc.text("*self"),
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
}

/// Avoid recomputing Layout from Variable multiple times.
/// We use `ena` for easy snapshots and rollbacks of the cache.
/// During specialization, a type variable `a` can be specialized to different layouts,
/// e.g. `identity : a -> a` could be specialized to `Bool -> Bool` or `Str -> Str`.
/// Therefore in general it's invalid to store a map from variables to layouts
/// But if we're careful when to invalidate certain keys, we still get some benefit
#[derive(Debug)]
pub struct LayoutCache<'a> {
    target_info: TargetInfo,
    _marker: std::marker::PhantomData<&'a u8>,
}

#[derive(Debug, Clone)]
pub enum CachedLayout<'a> {
    Cached(Layout<'a>),
    NotCached,
    Problem(LayoutProblem),
}

impl<'a> LayoutCache<'a> {
    pub fn new(target_info: TargetInfo) -> Self {
        Self {
            target_info,
            _marker: Default::default(),
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
        };

        Layout::from_var(&mut env, var)
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
        };
        RawFunctionLayout::from_var(&mut env, var)
    }

    pub fn snapshot(&mut self) -> SnapshotKeyPlaceholder {
        SnapshotKeyPlaceholder
    }

    pub fn rollback_to(&mut self, _snapshot: SnapshotKeyPlaceholder) {}
}

// placeholder for the type ven_ena::unify::Snapshot<ven_ena::unify::InPlace<CachedVariable<'a>>>
pub struct SnapshotKeyPlaceholder;

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
}

impl<'a> Builtin<'a> {
    const I1_SIZE: u32 = std::mem::size_of::<bool>() as u32;
    const DECIMAL_SIZE: u32 = std::mem::size_of::<i128>() as u32;

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

    pub fn stack_size(&self, target_info: TargetInfo) -> u32 {
        use Builtin::*;

        let ptr_width = target_info.ptr_width() as u32;

        match self {
            Int(int) => int.stack_size(),
            Float(float) => float.stack_size(),
            Bool => Builtin::I1_SIZE,
            Decimal => Builtin::DECIMAL_SIZE,
            Str => Builtin::STR_WORDS * ptr_width,
            Dict(_, _) => Builtin::DICT_WORDS * ptr_width,
            Set(_) => Builtin::SET_WORDS * ptr_width,
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
            Dict(_, _) => ptr_width,
            Set(_) => ptr_width,
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

            Str | Dict(_, _) | Set(_) | List(_) => false,
        }
    }

    // Question: does is_refcounted exactly correspond with the "safe to memcpy" property?
    pub fn is_refcounted(&self) -> bool {
        use Builtin::*;

        match self {
            Int(_) | Float(_) | Bool | Decimal => false,
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

    pub fn allocation_alignment_bytes(&self, target_info: TargetInfo) -> u32 {
        let ptr_width = target_info.ptr_width() as u32;

        let allocation = match self {
            Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal => {
                unreachable!("not heap-allocated")
            }
            Builtin::Str => ptr_width,
            Builtin::Dict(k, v) => k
                .alignment_bytes(target_info)
                .max(v.alignment_bytes(target_info))
                .max(ptr_width),
            Builtin::Set(k) => k.alignment_bytes(target_info).max(ptr_width),
            Builtin::List(e) => e.alignment_bytes(target_info).max(ptr_width),
        };

        allocation.max(ptr_width)
    }
}

fn layout_from_flat_type<'a>(
    env: &mut Env<'a, '_>,
    flat_type: FlatType,
) -> Result<Layout<'a>, LayoutProblem> {
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
                    Ok(Layout::usize(env.target_info))
                }

                Symbol::NUM_I128 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::i128())
                }
                Symbol::NUM_I64 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::i64())
                }
                Symbol::NUM_I32 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::i32())
                }
                Symbol::NUM_I16 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::i16())
                }
                Symbol::NUM_I8 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::i8())
                }

                Symbol::NUM_U128 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::u128())
                }
                Symbol::NUM_U64 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::u64())
                }
                Symbol::NUM_U32 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::u32())
                }
                Symbol::NUM_U16 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::u16())
                }
                Symbol::NUM_U8 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::u8())
                }

                // Floats
                Symbol::NUM_DEC => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Decimal))
                }
                Symbol::NUM_F64 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::f64())
                }
                Symbol::NUM_F32 => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::f32())
                }

                Symbol::NUM_NUM | Symbol::NUM_AT_NUM => {
                    // Num.Num should only ever have 1 argument, e.g. Num.Num Int.Integer
                    debug_assert_eq!(args.len(), 1);

                    let var = args[0];
                    let content = subs.get_content_without_compacting(var);

                    layout_from_num_content(content, target_info)
                }

                Symbol::STR_STR => Ok(Layout::Builtin(Builtin::Str)),
                Symbol::LIST_LIST => list_layout_from_elem(env, args[0]),
                Symbol::DICT_DICT => dict_layout_from_key_value(env, args[0], args[1]),
                Symbol::SET_SET => dict_layout_from_key_value(env, args[0], Variable::EMPTY_RECORD),
                _ => {
                    panic!(
                        "TODO layout_from_flat_type for Apply({:?}, {:?})",
                        symbol, args
                    );
                }
            }
        }
        Func(_, closure_var, _) => {
            let lambda_set =
                LambdaSet::from_var(env.arena, env.subs, closure_var, env.target_info)?;

            Ok(Layout::LambdaSet(lambda_set))
        }
        Record(fields, ext_var) => {
            // extract any values from the ext_var

            let mut pairs = Vec::with_capacity_in(fields.len(), arena);
            for (label, field) in fields.unsorted_iterator(subs, ext_var) {
                // drop optional fields
                let var = match field {
                    RecordField::Optional(_) => continue,
                    RecordField::Required(var) => var,
                    RecordField::Demanded(var) => var,
                };

                pairs.push((label, Layout::from_var(env, var)?));
            }

            pairs.sort_by(|(label1, layout1), (label2, layout2)| {
                let size1 = layout1.alignment_bytes(target_info);
                let size2 = layout2.alignment_bytes(target_info);

                size2.cmp(&size1).then(label1.cmp(label2))
            });

            let ordered_field_names =
                Vec::from_iter_in(pairs.iter().map(|(label, _)| *label), arena);
            let field_order_hash =
                FieldOrderHash::from_ordered_fields(ordered_field_names.as_slice());

            let mut layouts = Vec::from_iter_in(pairs.into_iter().map(|t| t.1), arena);

            if layouts.len() == 1 {
                // If the record has only one field that isn't zero-sized,
                // unwrap it.
                Ok(layouts.pop().unwrap())
            } else {
                Ok(Layout::Struct {
                    field_order_hash,
                    field_layouts: layouts.into_bump_slice(),
                })
            }
        }
        TagUnion(tags, ext_var) => {
            let (tags, ext_var) = tags.unsorted_tags_and_ext(subs, ext_var);

            debug_assert!(ext_var_is_empty_tag_union(subs, ext_var));

            Ok(layout_from_tag_union(arena, &tags, subs, env.target_info))
        }
        FunctionOrTagUnion(tag_name, _, ext_var) => {
            debug_assert!(
                ext_var_is_empty_tag_union(subs, ext_var),
                "If ext_var wasn't empty, this wouldn't be a FunctionOrTagUnion!"
            );

            let union_tags = UnionTags::from_tag_name_index(tag_name);
            let (tags, _) = union_tags.unsorted_tags_and_ext(subs, ext_var);

            Ok(layout_from_tag_union(arena, &tags, subs, env.target_info))
        }
        RecursiveTagUnion(rec_var, tags, ext_var) => {
            let (tags, ext_var) = tags.unsorted_tags_and_ext(subs, ext_var);

            debug_assert!(ext_var_is_empty_tag_union(subs, ext_var));

            // some observations
            //
            // * recursive tag unions are always recursive
            // * therefore at least one tag has a pointer (non-zero sized) field
            // * they must (to be instantiated) have 2 or more tags
            //
            // That means none of the optimizations for enums or single tag tag unions apply

            let rec_var = subs.get_root_key_without_compacting(rec_var);
            let tags_vec = tags.tags;
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

                    tag_layout.push(Layout::from_var(env, var)?);
                }

                tag_layout.sort_by(|layout1, layout2| {
                    let size1 = layout1.alignment_bytes(target_info);
                    let size2 = layout2.alignment_bytes(target_info);

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

            Ok(Layout::Union(union_layout))
        }
        EmptyTagUnion => Ok(Layout::VOID),
        Erroneous(_) => Err(LayoutProblem::Erroneous),
        EmptyRecord => Ok(Layout::UNIT),
    }
}

pub type SortedField<'a> = (Lowercase, Variable, Result<Layout<'a>, Layout<'a>>);

pub fn sort_record_fields<'a>(
    arena: &'a Bump,
    var: Variable,
    subs: &Subs,
    target_info: TargetInfo,
) -> Result<Vec<'a, SortedField<'a>>, LayoutProblem> {
    let mut env = Env {
        arena,
        subs,
        seen: Vec::new_in(arena),
        target_info,
    };

    let (it, _) = match gather_fields_unsorted_iter(subs, RecordFields::empty(), var) {
        Ok(it) => it,
        Err(_) => return Err(LayoutProblem::Erroneous),
    };

    let it = it
        .into_iter()
        .map(|(field, field_type)| (field.clone(), field_type));

    sort_record_fields_help(&mut env, it)
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
                let layout = Layout::from_var(env, v)?;
                sorted_fields.push((label, v, Ok(layout)));
            }
            RecordField::Optional(v) => {
                let layout = Layout::from_var(env, v)?;
                sorted_fields.push((label, v, Err(layout)));
            }
        };
    }

    sorted_fields.sort_by(
        |(label1, _, res_layout1), (label2, _, res_layout2)| match res_layout1 {
            Ok(layout1) | Err(layout1) => match res_layout2 {
                Ok(layout2) | Err(layout2) => {
                    let size1 = layout1.alignment_bytes(target_info);
                    let size2 = layout2.alignment_bytes(target_info);

                    size2.cmp(&size1).then(label1.cmp(label2))
                }
            },
        },
    );

    Ok(sorted_fields)
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
        nullable_id: TagIdIntType,
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
    pub fn tag_name_to_id(&self, tag_name: &TagName) -> (TagIdIntType, &'a [Layout<'a>]) {
        use WrappedVariant::*;

        match self {
            Recursive { sorted_tag_layouts } | NonRecursive { sorted_tag_layouts } => {
                let (tag_id, (_, argument_layouts)) = sorted_tag_layouts
                    .iter()
                    .enumerate()
                    .find(|(_, (key, _))| key == tag_name)
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

                if tag_name == nullable_name {
                    (*nullable_id as TagIdIntType, &[] as &[_])
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
                    (tag_id as TagIdIntType, *argument_layouts)
                }
            }
            NullableUnwrapped {
                nullable_id,
                nullable_name,
                other_name,
                other_fields,
            } => {
                if tag_name == nullable_name {
                    (*nullable_id as TagIdIntType, &[] as &[_])
                } else {
                    debug_assert_eq!(other_name, tag_name);

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
    arena: &'a Bump,
    var: Variable,
    subs: &Subs,
    target_info: TargetInfo,
) -> Result<UnionVariant<'a>, LayoutProblem> {
    let var =
        if let Content::RecursionVar { structure, .. } = subs.get_content_without_compacting(var) {
            *structure
        } else {
            var
        };

    let mut tags_vec = std::vec::Vec::new();
    let result = match roc_types::pretty_print::chase_ext_tag_union(subs, var, &mut tags_vec) {
        Ok(()) | Err((_, Content::FlexVar(_))) | Err((_, Content::RecursionVar { .. })) => {
            let opt_rec_var = get_recursion_var(subs, var);
            union_sorted_tags_help(arena, tags_vec, opt_rec_var, subs, target_info)
        }
        Err((_, Content::Error)) => return Err(LayoutProblem::Erroneous),
        Err(other) => panic!("invalid content in tag union variable: {:?}", other),
    };

    Ok(result)
}

fn get_recursion_var(subs: &Subs, var: Variable) -> Option<Variable> {
    match subs.get_content_without_compacting(var) {
        Content::Structure(FlatType::RecursiveTagUnion(rec_var, _, _)) => Some(*rec_var),
        Content::Alias(_, _, actual) => get_recursion_var(subs, *actual),
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

fn union_sorted_tags_help_new<'a>(
    arena: &'a Bump,
    tags_list: &[(&'_ TagName, &[Variable])],
    opt_rec_var: Option<Variable>,
    subs: &Subs,
    target_info: TargetInfo,
) -> UnionVariant<'a> {
    // sort up front; make sure the ordering stays intact!
    let mut tags_list = Vec::from_iter_in(tags_list.iter(), arena);
    tags_list.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));

    let mut env = Env {
        arena,
        subs,
        seen: Vec::new_in(arena),
        target_info,
    };

    match tags_list.len() {
        0 => {
            // trying to instantiate a type with no values
            UnionVariant::Never
        }
        1 => {
            let &(tag_name, arguments) = tags_list.remove(0);
            let tag_name = tag_name.clone();

            // just one tag in the union (but with arguments) can be a struct
            let mut layouts = Vec::with_capacity_in(tags_list.len(), arena);

            // special-case NUM_AT_NUM: if its argument is a FlexVar, make it Int
            match tag_name {
                TagName::Private(Symbol::NUM_AT_NUM) => {
                    let var = arguments[0];
                    layouts
                        .push(unwrap_num_tag(subs, var, target_info).expect("invalid num layout"));
                }
                _ => {
                    for &var in arguments {
                        match Layout::from_var(&mut env, var) {
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
                }
            }

            layouts.sort_by(|layout1, layout2| {
                let size1 = layout1.alignment_bytes(target_info);
                let size2 = layout2.alignment_bytes(target_info);

                size2.cmp(&size1)
            });

            if layouts.is_empty() {
                UnionVariant::Unit
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
            let mut answer = Vec::with_capacity_in(tags_list.len(), arena);
            let mut has_any_arguments = false;

            let mut nullable: Option<(TagIdIntType, TagName)> = None;

            // only recursive tag unions can be nullable
            let is_recursive = opt_rec_var.is_some();
            if is_recursive && GENERATE_NULLABLE {
                for (index, (name, variables)) in tags_list.iter().enumerate() {
                    if variables.is_empty() {
                        nullable = Some((index as TagIdIntType, (*name).clone()));
                        break;
                    }
                }
            }

            for (index, &(tag_name, arguments)) in tags_list.into_iter().enumerate() {
                // reserve space for the tag discriminant
                if matches!(nullable, Some((i, _)) if i  as usize == index) {
                    debug_assert!(arguments.is_empty());
                    continue;
                }

                let mut arg_layouts = Vec::with_capacity_in(arguments.len() + 1, arena);

                for &var in arguments {
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
                    let size1 = layout1.alignment_bytes(target_info);
                    let size2 = layout2.alignment_bytes(target_info);

                    size2.cmp(&size1)
                });

                answer.push((tag_name.clone(), arg_layouts.into_bump_slice()));
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

pub fn union_sorted_tags_help<'a>(
    arena: &'a Bump,
    mut tags_vec: std::vec::Vec<(TagName, std::vec::Vec<Variable>)>,
    opt_rec_var: Option<Variable>,
    subs: &Subs,
    target_info: TargetInfo,
) -> UnionVariant<'a> {
    // sort up front; make sure the ordering stays intact!
    tags_vec.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));

    let mut env = Env {
        arena,
        subs,
        seen: Vec::new_in(arena),
        target_info,
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
                    layouts.push(
                        unwrap_num_tag(subs, arguments[0], target_info)
                            .expect("invalid num layout"),
                    );
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
                                // completely, but for now we represent it with the empty tag union
                                layouts.push(Layout::VOID)
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
                let size1 = layout1.alignment_bytes(target_info);
                let size2 = layout2.alignment_bytes(target_info);

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
                    let size1 = layout1.alignment_bytes(target_info);
                    let size2 = layout2.alignment_bytes(target_info);

                    size2.cmp(&size1)
                });

                answer.push((tag_name.clone(), arg_layouts.into_bump_slice()));
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
                        tag_names.push(tag_name.clone());
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

fn layout_from_newtype<'a>(
    arena: &'a Bump,
    tags: &UnsortedUnionTags,
    subs: &Subs,
    target_info: TargetInfo,
) -> Layout<'a> {
    debug_assert!(tags.is_newtype_wrapper(subs));

    let (tag_name, var) = tags.get_newtype(subs);

    if tag_name == &TagName::Private(Symbol::NUM_AT_NUM) {
        unwrap_num_tag(subs, var, target_info).expect("invalid Num argument")
    } else {
        let mut env = Env {
            arena,
            subs,
            seen: Vec::new_in(arena),
            target_info,
        };

        match Layout::from_var(&mut env, var) {
            Ok(layout) => layout,
            Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                // If we encounter an unbound type var (e.g. `Ok *`)
                // then it's zero-sized; In the future we may drop this argument
                // completely, but for now we represent it with the empty tag union
                Layout::VOID
            }
            Err(LayoutProblem::Erroneous) => {
                // An erroneous type var will code gen to a runtime
                // error, so we don't need to store any data for it.
                todo!()
            }
        }
    }
}

fn layout_from_tag_union<'a>(
    arena: &'a Bump,
    tags: &UnsortedUnionTags,
    subs: &Subs,
    target_info: TargetInfo,
) -> Layout<'a> {
    use UnionVariant::*;

    if tags.is_newtype_wrapper(subs) {
        return layout_from_newtype(arena, tags, subs, target_info);
    }

    let tags_vec = &tags.tags;

    match tags_vec.get(0) {
        Some((tag_name, arguments)) if *tag_name == &TagName::Private(Symbol::NUM_AT_NUM) => {
            debug_assert_eq!(arguments.len(), 1);

            let &var = arguments.iter().next().unwrap();

            unwrap_num_tag(subs, var, target_info).expect("invalid Num argument")
        }
        _ => {
            let opt_rec_var = None;
            let variant =
                union_sorted_tags_help_new(arena, tags_vec, opt_rec_var, subs, target_info);

            match variant {
                Never => Layout::VOID,
                Unit | UnitWithArguments => Layout::UNIT,
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
    // the ext_var is empty
    let mut ext_fields = std::vec::Vec::new();
    match roc_types::pretty_print::chase_ext_tag_union(subs, ext_var, &mut ext_fields) {
        Ok(()) | Err((_, Content::FlexVar(_))) => ext_fields.is_empty(),
        Err(content) => panic!("invalid content in ext_var: {:?}", content),
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
) -> Result<Layout<'a>, LayoutProblem> {
    use roc_types::subs::Content::*;
    use roc_types::subs::FlatType::*;

    match content {
        RecursionVar { .. } => panic!("recursion var in num"),
        FlexVar(_) | RigidVar(_) => {
            // If a Num makes it all the way through type checking with an unbound
            // type variable, then assume it's a 64-bit integer.
            //
            // (e.g. for (5 + 5) assume both 5s are 64-bit integers.)
            Ok(Layout::default_integer())
        }
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
        Alias(_, _, _) => {
            todo!("TODO recursively resolve type aliases in num_from_content");
        }
        Structure(_) | RangedNumber(..) => {
            panic!("Invalid Num.Num type application: {:?}", content);
        }
        Error => Err(LayoutProblem::Erroneous),
    }
}

fn unwrap_num_tag<'a>(
    subs: &Subs,
    var: Variable,
    target_info: TargetInfo,
) -> Result<Layout<'a>, LayoutProblem> {
    match subs.get_content_without_compacting(var) {
        Content::Alias(Symbol::NUM_INTEGER, args, _) => {
            debug_assert!(args.len() == 1);

            let precision_var = subs[args.variables().into_iter().next().unwrap()];

            let precision = subs.get_content_without_compacting(precision_var);

            match precision {
                Content::Alias(symbol, args, _) => {
                    debug_assert!(args.is_empty());

                    let layout = match *symbol {
                        Symbol::NUM_SIGNED128 => Layout::i128(),
                        Symbol::NUM_SIGNED64 => Layout::i64(),
                        Symbol::NUM_SIGNED32 => Layout::i32(),
                        Symbol::NUM_SIGNED16 => Layout::i16(),
                        Symbol::NUM_SIGNED8 => Layout::i8(),
                        Symbol::NUM_UNSIGNED128 => Layout::u128(),
                        Symbol::NUM_UNSIGNED64 => Layout::u64(),
                        Symbol::NUM_UNSIGNED32 => Layout::u32(),
                        Symbol::NUM_UNSIGNED16 => Layout::u16(),
                        Symbol::NUM_UNSIGNED8 => Layout::u8(),
                        Symbol::NUM_NATURAL => Layout::usize(target_info),

                        _ => unreachable!("not a valid int variant: {:?} {:?}", symbol, args),
                    };

                    Ok(layout)
                }
                Content::FlexVar(_) | Content::RigidVar(_) => {
                    // default to i64
                    Ok(Layout::i64())
                }
                _ => unreachable!("not a valid int variant: {:?}", precision),
            }
        }
        Content::Alias(Symbol::NUM_FLOATINGPOINT, args, _) => {
            debug_assert!(args.len() == 1);

            let precision_var = subs[args.variables().into_iter().next().unwrap()];

            let precision = subs.get_content_without_compacting(precision_var);

            match precision {
                Content::Alias(Symbol::NUM_BINARY32, args, _) => {
                    debug_assert!(args.is_empty());

                    Ok(Layout::f32())
                }
                Content::Alias(Symbol::NUM_BINARY64, args, _) => {
                    debug_assert!(args.is_empty());

                    Ok(Layout::f64())
                }
                Content::Alias(Symbol::NUM_DECIMAL, args, _) => {
                    debug_assert!(args.is_empty());

                    Ok(Layout::Builtin(Builtin::Decimal))
                }
                Content::FlexVar(_) | Content::RigidVar(_) => {
                    // default to f64
                    Ok(Layout::f64())
                }
                _ => unreachable!("not a valid float variant: {:?}", precision),
            }
        }
        Content::FlexVar(_) | Content::RigidVar(_) => {
            // If this was still a (Num *) then default to compiling it to i64
            Ok(Layout::default_integer())
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
    let is_variable = |content| matches!(content, &Content::FlexVar(_) | &Content::RigidVar(_));

    let key_content = env.subs.get_content_without_compacting(key_var);
    let value_content = env.subs.get_content_without_compacting(value_var);

    let key_layout = if is_variable(key_content) {
        Layout::VOID
    } else {
        // NOTE: cannot re-use Content, because it may be recursive
        // then some state is not correctly kept, we have to go through from_var
        Layout::from_var(env, key_var)?
    };

    let value_layout = if is_variable(value_content) {
        Layout::VOID
    } else {
        // NOTE: cannot re-use Content, because it may be recursive
        // then some state is not correctly kept, we have to go through from_var
        Layout::from_var(env, value_var)?
    };

    // This is a normal list.
    Ok(Layout::Builtin(Builtin::Dict(
        env.arena.alloc(key_layout),
        env.arena.alloc(value_layout),
    )))
}

pub fn list_layout_from_elem<'a>(
    env: &mut Env<'a, '_>,
    element_var: Variable,
) -> Result<Layout<'a>, LayoutProblem> {
    let is_variable = |content| matches!(content, &Content::FlexVar(_) | &Content::RigidVar(_));

    let element_content = env.subs.get_content_without_compacting(element_var);

    let element_layout = if is_variable(element_content) {
        // If this was still a (List *) then it must have been an empty list
        Layout::VOID
    } else {
        // NOTE: cannot re-use Content, because it may be recursive
        // then some state is not correctly kept, we have to go through from_var
        Layout::from_var(env, element_var)?
    };

    Ok(Layout::Builtin(Builtin::List(
        env.arena.alloc(element_layout),
    )))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LayoutId(u32);

impl LayoutId {
    // Returns something like "foo#1" when given a symbol that interns to "foo"
    // and a LayoutId of 1.
    pub fn to_symbol_string(self, symbol: Symbol, interns: &Interns) -> String {
        let ident_string = symbol.ident_str(interns);
        let module_string = interns.module_ids.get_name(symbol.module_id()).unwrap();
        format!("{}_{}_{}", module_string, ident_string, self.0)
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn width_and_alignment_union_empty_struct() {
        let lambda_set = LambdaSet {
            set: &[(Symbol::LIST_MAP, &[])],
            representation: &Layout::UNIT,
        };

        let a = &[Layout::UNIT] as &[_];
        let b = &[Layout::LambdaSet(lambda_set)] as &[_];
        let tt = [a, b];

        let layout = Layout::Union(UnionLayout::NonRecursive(&tt));

        let target_info = TargetInfo::default_x86_64();
        assert_eq!(layout.stack_size(target_info), 1);
        assert_eq!(layout.alignment_bytes(target_info), 1);
    }
}
