use crate::ir::Parens;
use bumpalo::collections::{CollectIn, Vec};
use bumpalo::Bump;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::all::{default_hasher, MutMap};
use roc_collections::VecMap;
use roc_error_macros::{internal_error, todo_abilities};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{IdentIds, Interns, ModuleId, Symbol};
use roc_problem::can::RuntimeError;
use roc_target::{PtrWidth, TargetInfo};
use roc_types::subs::{
    self, Content, FlatType, Label, RecordFields, Subs, UnionTags, UnsortedUnionLabels, Variable,
};
use roc_types::types::{gather_fields_unsorted_iter, RecordField, RecordFieldsError};
use std::cmp::Ordering;
use std::collections::hash_map::{DefaultHasher, Entry};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Mutex};
use ven_pretty::{DocAllocator, DocBuilder};

// if your changes cause this number to go down, great!
// please change it to the lower number.
// if it went up, maybe check that the change is really required
roc_error_macros::assert_sizeof_aarch64!(Builtin, 3 * 8);
roc_error_macros::assert_sizeof_aarch64!(Layout, 4 * 8);
roc_error_macros::assert_sizeof_aarch64!(UnionLayout, 3 * 8);
roc_error_macros::assert_sizeof_aarch64!(LambdaSet, 3 * 8);

roc_error_macros::assert_sizeof_wasm!(Builtin, 3 * 4);
roc_error_macros::assert_sizeof_wasm!(Layout, 6 * 4);
roc_error_macros::assert_sizeof_wasm!(UnionLayout, 3 * 4);
roc_error_macros::assert_sizeof_wasm!(LambdaSet, 3 * 4);

roc_error_macros::assert_sizeof_default!(Builtin, 3 * 8);
roc_error_macros::assert_sizeof_default!(Layout, 4 * 8);
roc_error_macros::assert_sizeof_default!(UnionLayout, 3 * 8);
roc_error_macros::assert_sizeof_default!(LambdaSet, 3 * 8);

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
            FlexAbleVar(_, _) | RigidAbleVar(_, _) => todo_abilities!("Not reachable yet"),
            RecursionVar { structure, .. } => {
                let structure_content = env.subs.get_content_without_compacting(structure);
                Self::new_help(env, structure, *structure_content)
            }
            LambdaSet(lset) => Self::layout_from_lambda_set(env, lset),
            Structure(flat_type) => Self::layout_from_flat_type(env, flat_type),
            RangedNumber(typ, _) => Self::from_var(env, typ),

            // Ints
            Alias(Symbol::NUM_I128, args, _, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::i128()))
            }
            Alias(Symbol::NUM_I64, args, _, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::i64()))
            }
            Alias(Symbol::NUM_I32, args, _, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::i32()))
            }
            Alias(Symbol::NUM_I16, args, _, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::i16()))
            }
            Alias(Symbol::NUM_I8, args, _, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::i8()))
            }

            // I think unsigned and signed use the same layout
            Alias(Symbol::NUM_U128, args, _, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::u128()))
            }
            Alias(Symbol::NUM_U64, args, _, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::u64()))
            }
            Alias(Symbol::NUM_U32, args, _, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::u32()))
            }
            Alias(Symbol::NUM_U16, args, _, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::u16()))
            }
            Alias(Symbol::NUM_U8, args, _, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::u8()))
            }

            // Floats
            Alias(Symbol::NUM_F64, args, _, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::f64()))
            }
            Alias(Symbol::NUM_F32, args, _, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::f32()))
            }

            // Nat
            Alias(Symbol::NUM_NAT, args, _, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::usize(env.target_info)))
            }

            Alias(symbol, _, _, _) if symbol.is_builtin() => Ok(Self::ZeroArgumentThunk(
                Layout::new_help(env, var, content)?,
            )),

            Alias(_, _, var, _) => Self::from_var(env, var),
            Error => Err(LayoutProblem::Erroneous),
        }
    }

    fn layout_from_lambda_set(
        _env: &mut Env<'a, '_>,
        _lset: subs::LambdaSet,
    ) -> Result<Self, LayoutProblem> {
        unreachable!()
        // Lambda set is just a tag union from the layout's perspective.
        // Self::layout_from_flat_type(env, lset.as_tag_union())
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

                let lambda_set = LambdaSet::from_var(
                    env.arena,
                    env.subs,
                    closure_var,
                    env.target_info,
                    env.multimorphic_names,
                )?;

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
            Self::new_help(env, var, *content)
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
    Boxed(&'a Layout<'a>),
    Union(UnionLayout<'a>),
    LambdaSet(LambdaSet<'a>),
    RecursivePointer,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
            Recursive(tags) => {
                let tags_doc = tags.iter().map(|fields| {
                    alloc.text("C ").append(alloc.intersperse(
                        fields.iter().map(|x| x.to_doc(alloc, Parens::InTypeParam)),
                        " ",
                    ))
                });
                alloc
                    .text("[<r>")
                    .append(alloc.intersperse(tags_doc, ", "))
                    .append(alloc.text("]"))
            }
            NonNullableUnwrapped(fields) => {
                let fields_doc = alloc.text("C ").append(alloc.intersperse(
                    fields.iter().map(|x| x.to_doc(alloc, Parens::InTypeParam)),
                    " ",
                ));
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
                            .map(|x| x.to_doc(alloc, Parens::InTypeParam)),
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

    pub fn discriminant_size(num_tags: usize) -> IntWidth {
        if num_tags <= u8::MAX as usize {
            IntWidth::U8
        } else if num_tags <= u16::MAX as usize {
            IntWidth::U16
        } else {
            panic!("tag union is too big")
        }
    }

    pub fn tag_id_builtin(&self) -> Builtin<'a> {
        match self {
            UnionLayout::NonRecursive(tags) => {
                let union_size = tags.len();
                Builtin::Int(Self::discriminant_size(union_size))
            }
            UnionLayout::Recursive(tags) => {
                let union_size = tags.len();

                Builtin::Int(Self::discriminant_size(union_size))
            }

            UnionLayout::NullableWrapped { other_tags, .. } => {
                Builtin::Int(Self::discriminant_size(other_tags.len() + 1))
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
            UnionLayout::NonRecursive(tags) => Self::tags_alignment_bytes(tags, target_info),
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

    /// Very important to use this when doing a memcpy!
    fn stack_size_without_alignment(&self, target_info: TargetInfo) -> u32 {
        match self {
            UnionLayout::NonRecursive(tags) => {
                let id_layout = self.tag_id_layout();

                let mut size = 0;

                for field_layouts in tags.iter() {
                    let fields = Layout::struct_no_name_order(field_layouts);
                    let fields_and_id = [fields, id_layout];

                    let data = Layout::struct_no_name_order(&fields_and_id);
                    size = size.max(data.stack_size_without_alignment(target_info));
                }

                size
            }
            UnionLayout::Recursive(_)
            | UnionLayout::NonNullableUnwrapped(_)
            | UnionLayout::NullableWrapped { .. }
            | UnionLayout::NullableUnwrapped { .. } => target_info.ptr_width() as u32,
        }
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
            set: &'a [(LambdaName, &'a [Layout<'a>])],
        }

        impl std::fmt::Debug for Helper<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let entries = self.set.iter().map(|x| SetElement {
                    symbol: match (x.0).0 {
                        LambdaNameInner::Name(name) => name,
                        LambdaNameInner::Multimorphic { alias, .. } => alias,
                    },
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

#[derive(Default, Debug)]
struct MultimorphicNamesTable {
    /// (source symbol, captures layouts) -> multimorphic alias
    ///
    /// SAFETY: actually, the `Layout` is alive only as long as the `arena` is alive. We take care
    /// to promote new layouts to the owned arena. Since we are using a bump-allocating arena, the
    /// references will never be invalidated until the arena is dropped, which happens when this
    /// struct is dropped.
    /// Also, the `Layout`s we owned are never exposed back via the public API.
    inner: VecMap<(Symbol, &'static [Layout<'static>]), Symbol>,
    arena: Bump,
    ident_ids: IdentIds,
}

impl MultimorphicNamesTable {
    fn get<'b>(&self, name: Symbol, captures_layouts: &'b [Layout<'b>]) -> Option<Symbol> {
        self.inner.get(&(name, captures_layouts)).copied()
    }

    fn insert<'b>(&mut self, name: Symbol, captures_layouts: &'b [Layout<'b>]) -> Symbol {
        debug_assert!(!self.inner.contains_key(&(name, captures_layouts)));

        let new_ident = self.ident_ids.gen_unique();
        let new_symbol = Symbol::new(ModuleId::MULTIMORPHIC, new_ident);

        let captures_layouts = self.promote_layout_slice(captures_layouts);

        self.inner.insert((name, captures_layouts), new_symbol);

        new_symbol
    }

    fn alloc_st<T>(&self, v: T) -> &'static T {
        unsafe { std::mem::transmute::<_, &'static Bump>(&self.arena) }.alloc(v)
    }

    fn promote_layout<'b>(&self, layout: Layout<'b>) -> Layout<'static> {
        match layout {
            Layout::Builtin(builtin) => Layout::Builtin(self.promote_builtin(builtin)),
            Layout::Struct {
                field_order_hash,
                field_layouts,
            } => Layout::Struct {
                field_order_hash,
                field_layouts: self.promote_layout_slice(field_layouts),
            },
            Layout::Boxed(layout) => Layout::Boxed(self.alloc_st(self.promote_layout(*layout))),
            Layout::Union(union_layout) => Layout::Union(self.promote_union_layout(union_layout)),
            Layout::LambdaSet(lambda_set) => Layout::LambdaSet(self.promote_lambda_set(lambda_set)),
            Layout::RecursivePointer => Layout::RecursivePointer,
        }
    }

    fn promote_layout_slice<'b>(&self, layouts: &'b [Layout<'b>]) -> &'static [Layout<'static>] {
        layouts
            .iter()
            .map(|layout| self.promote_layout(*layout))
            .collect_in::<Vec<_>>(unsafe { std::mem::transmute(&self.arena) })
            .into_bump_slice()
    }

    fn promote_layout_slice_slices<'b>(
        &self,
        layout_slices: &'b [&'b [Layout<'b>]],
    ) -> &'static [&'static [Layout<'static>]] {
        layout_slices
            .iter()
            .map(|slice| self.promote_layout_slice(slice))
            .collect_in::<Vec<_>>(unsafe { std::mem::transmute(&self.arena) })
            .into_bump_slice()
    }

    fn promote_builtin(&self, builtin: Builtin) -> Builtin<'static> {
        match builtin {
            Builtin::Int(w) => Builtin::Int(w),
            Builtin::Float(w) => Builtin::Float(w),
            Builtin::Bool => Builtin::Bool,
            Builtin::Decimal => Builtin::Decimal,
            Builtin::Str => Builtin::Str,
            Builtin::Dict(k, v) => Builtin::Dict(
                self.alloc_st(self.promote_layout(*k)),
                self.alloc_st(self.promote_layout(*v)),
            ),
            Builtin::Set(k) => Builtin::Set(self.alloc_st(self.promote_layout(*k))),
            Builtin::List(l) => Builtin::Set(self.alloc_st(self.promote_layout(*l))),
        }
    }

    fn promote_union_layout(&self, union_layout: UnionLayout) -> UnionLayout<'static> {
        match union_layout {
            UnionLayout::NonRecursive(slices) => {
                UnionLayout::NonRecursive(self.promote_layout_slice_slices(slices))
            }
            UnionLayout::Recursive(slices) => {
                UnionLayout::Recursive(self.promote_layout_slice_slices(slices))
            }
            UnionLayout::NonNullableUnwrapped(slice) => {
                UnionLayout::NonNullableUnwrapped(self.promote_layout_slice(slice))
            }
            UnionLayout::NullableWrapped {
                nullable_id,
                other_tags,
            } => UnionLayout::NullableWrapped {
                nullable_id,
                other_tags: self.promote_layout_slice_slices(other_tags),
            },
            UnionLayout::NullableUnwrapped {
                nullable_id,
                other_fields,
            } => UnionLayout::NullableUnwrapped {
                nullable_id,
                other_fields: self.promote_layout_slice(other_fields),
            },
        }
    }

    fn promote_lambda_set(&self, lambda_set: LambdaSet) -> LambdaSet<'static> {
        let LambdaSet {
            set,
            representation,
        } = lambda_set;
        let set = set
            .iter()
            .map(|(name, slice)| (*name, self.promote_layout_slice(slice)))
            .collect_in::<Vec<_>>(unsafe { std::mem::transmute(&self.arena) })
            .into_bump_slice();
        LambdaSet {
            set,
            representation: self.alloc_st(self.promote_layout(*representation)),
        }
    }
}

#[derive(Default, Debug)]
pub struct MultimorphicNames(Arc<Mutex<MultimorphicNamesTable>>);

impl Clone for MultimorphicNames {
    fn clone(&self) -> Self {
        Self(Arc::clone(&self.0))
    }
}

impl MultimorphicNames {
    fn get<'b>(&self, name: Symbol, captures_layouts: &'b [Layout<'b>]) -> Option<Symbol> {
        self.0.lock().unwrap().get(name, captures_layouts)
    }

    fn insert<'b>(&mut self, name: Symbol, captures_layouts: &'b [Layout<'b>]) -> Symbol {
        self.0.lock().unwrap().insert(name, captures_layouts)
    }

    /// Assumes there is only one clone still alive.
    /// If there is more than one clone alive, `self` is returned.
    pub fn try_unwrap_names(self) -> Result<IdentIds, Self> {
        let mutex = Arc::try_unwrap(self.0).map_err(Self)?;
        let table = mutex
            .into_inner()
            .expect("how can there be another lock if we consumed the only ref?");
        Ok(table.ident_ids)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum LambdaNameInner {
    /// Standard lambda name assigned during canonicalize/constrain
    Name(Symbol),
    /// Sometimes we can end up with lambdas of the same name and different captures in the same
    /// lambda set, like [[Thunk U8, Thunk Str]]. See also https://github.com/rtfeldman/roc/issues/3336.
    /// We call such lambdas "multi-morphic".
    ///
    /// The current compilation scheme in such cases is to assign an alias name for subsequent such
    /// lambda names, and then code-gen those lambda variants under a different `Proc`. In our
    /// example, the lambda set would be transformed to something like
    /// [[Thunk U8, Multimorphic(Thunk, ThunkAliasStr) Str]] which tells us to specialize the
    /// second variant using the proc `Thunk` but under the name `ThunkAliasStr`, with that
    /// particular closure layout.
    ///
    /// Currently we do no de-duplication of alias names. This does make compilation faster, but
    /// also we should expect redundant multimorphic aliases to be somewhat rare, as that means a
    /// non-unitary lambda set is the same in multiple areas of a program.
    Multimorphic {
        /// The lambda we came from, e.g. `Thunk` in the example
        source: Symbol,
        /// The lambda we become, e.g. `ThunkAliasStr` in the example
        alias: Symbol,
    },
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct LambdaName(LambdaNameInner);

impl LambdaName {
    #[inline(always)]
    pub fn source_name(&self) -> Symbol {
        match self.0 {
            LambdaNameInner::Name(name) => name,
            LambdaNameInner::Multimorphic { source, .. } => source,
        }
    }

    #[inline(always)]
    pub fn call_name(&self) -> Symbol {
        match self.0 {
            LambdaNameInner::Name(name) => name,
            LambdaNameInner::Multimorphic { alias, .. } => alias,
        }
    }

    #[inline(always)]
    pub fn is_multimorphic(&self) -> bool {
        matches!(self.0, LambdaNameInner::Multimorphic { .. })
    }

    #[inline(always)]
    pub fn from_non_multimorphic(name: Symbol) -> Self {
        Self(LambdaNameInner::Name(name))
    }

    #[inline(always)]
    pub fn thunk(name: Symbol) -> Self {
        Self(LambdaNameInner::Name(name))
    }

    // When the function name is known, so there can only be one possible receiver, in such cases
    // the lambda cannot be multimorphic.
    #[inline(always)]
    pub fn only_receiver(name: Symbol) -> Self {
        Self(LambdaNameInner::Name(name))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct LambdaSet<'a> {
    /// collection of function names and their closure arguments
    pub set: &'a [(LambdaName, &'a [Layout<'a>])],
    /// how the closure will be represented at runtime
    representation: &'a Layout<'a>,
}

/// representation of the closure *for a particular function*
#[derive(Debug)]
pub enum ClosureRepresentation<'a> {
    /// the closure is represented as a union. Includes the tag ID!
    Union {
        alphabetic_order_fields: &'a [Layout<'a>],
        closure_name: Symbol,
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
    /// NOTE: for multimorphic variants, this checks the alias name.
    pub fn contains(&self, symbol: Symbol) -> bool {
        self.set.iter().any(|(s, _)| match s.0 {
            LambdaNameInner::Name(name) => name == symbol,
            LambdaNameInner::Multimorphic { alias, .. } => alias == symbol,
        })
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

    pub fn layout_for_member_with_lambda_name(
        &self,
        lambda_name: LambdaName,
    ) -> ClosureRepresentation<'a> {
        debug_assert!(
            self.set.iter().any(|(s, _)| *s == lambda_name),
            "lambda not in set"
        );

        let comparator =
            |other_name: LambdaName, _other_captures_layouts: &[Layout]| other_name == lambda_name;

        self.layout_for_member(comparator)
    }

    fn contains_source(&self, symbol: Symbol) -> bool {
        self.set.iter().any(|(s, _)| match s.0 {
            LambdaNameInner::Name(name) => name == symbol,
            LambdaNameInner::Multimorphic { source, .. } => source == symbol,
        })
    }

    /// Finds an alias name for a possible-multimorphic lambda variant in the lambda set.
    pub fn find_lambda_name(
        &self,
        function_symbol: Symbol,
        captures_layouts: &[Layout],
    ) -> LambdaName {
        debug_assert!(
            self.contains_source(function_symbol),
            "function symbol not in set"
        );

        let comparator = |other_name: LambdaName, other_captures_layouts: &[Layout]| {
            let other_name = match other_name.0 {
                LambdaNameInner::Name(name) => name,
                // Take the source, since we'll want to pick out the multimorphic name if it
                // matches
                LambdaNameInner::Multimorphic { source, .. } => source,
            };
            other_name == function_symbol
                && captures_layouts.iter().eq(other_captures_layouts.iter())
        };

        let (name, _) = self
            .set
            .iter()
            .find(|(name, layouts)| comparator(*name, layouts))
            .expect("no lambda set found");

        *name
    }

    fn layout_for_member<F>(&self, comparator: F) -> ClosureRepresentation<'a>
    where
        F: Fn(LambdaName, &[Layout]) -> bool,
    {
        match self.representation {
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

                        let closure_name = match name.0 {
                            LambdaNameInner::Name(name) => name,
                            LambdaNameInner::Multimorphic { alias, .. } => alias,
                        };

                        ClosureRepresentation::Union {
                            tag_id: index as TagIdIntType,
                            alphabetic_order_fields: fields,
                            closure_name,
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
        multimorphic_names: &mut MultimorphicNames,
    ) -> Result<Self, LayoutProblem> {
        match resolve_lambda_set(subs, closure_var) {
            ResolvedLambdaSet::Set(mut lambdas) => {
                // sort the tags; make sure ordering stays intact!
                lambdas.sort_by_key(|(sym, _)| *sym);

                let mut set: Vec<(LambdaName, &[Layout])> =
                    Vec::with_capacity_in(lambdas.len(), arena);
                let mut set_for_making_repr: std::vec::Vec<(Symbol, std::vec::Vec<Variable>)> =
                    std::vec::Vec::with_capacity(lambdas.len());

                let mut last_function_symbol = None;
                let mut lambdas_it = lambdas.iter().peekable();

                let mut has_multimorphic = false;
                while let Some((function_symbol, variables)) = lambdas_it.next() {
                    let mut arguments = Vec::with_capacity_in(variables.len(), arena);

                    let mut env = Env {
                        arena,
                        subs,
                        seen: Vec::new_in(arena),
                        target_info,
                        multimorphic_names,
                    };

                    for var in variables {
                        arguments.push(Layout::from_var(&mut env, *var)?);
                    }

                    let arguments = arguments.into_bump_slice();

                    let is_multimorphic = match (last_function_symbol, lambdas_it.peek()) {
                        (None, None) => false,
                        (Some(sym), None) | (None, Some((sym, _))) => function_symbol == sym,
                        (Some(sym1), Some((sym2, _))) => {
                            function_symbol == sym1 || function_symbol == sym2
                        }
                    };

                    let lambda_name = if is_multimorphic {
                        let alias = match multimorphic_names.get(*function_symbol, arguments) {
                            Some(alias) => alias,
                            None => multimorphic_names.insert(*function_symbol, arguments),
                        };

                        has_multimorphic = true;

                        LambdaNameInner::Multimorphic {
                            source: *function_symbol,
                            alias,
                        }
                    } else {
                        LambdaNameInner::Name(*function_symbol)
                    };
                    let lambda_name = LambdaName(lambda_name);

                    set.push((lambda_name, arguments));
                    set_for_making_repr.push((lambda_name.call_name(), variables.to_vec()));

                    last_function_symbol = Some(function_symbol);
                }

                if has_multimorphic {
                    // Must re-sort the set in case we added multimorphic lambdas since they may under
                    // another name
                    set.sort_by_key(|(name, _)| name.call_name());
                    set_for_making_repr.sort_by_key(|(name, _)| *name);
                }

                let representation = arena.alloc(Self::make_representation(
                    arena,
                    subs,
                    set_for_making_repr,
                    target_info,
                    multimorphic_names,
                ));

                Ok(LambdaSet {
                    set: set.into_bump_slice(),
                    representation,
                })
            }
            ResolvedLambdaSet::Unbound => {
                // The lambda set is unbound which means it must be unused. Just give it the empty lambda set.
                // See also https://github.com/rtfeldman/roc/issues/3163.
                Ok(LambdaSet {
                    set: &[],
                    representation: arena.alloc(Layout::UNIT),
                })
            }
        }
    }

    fn make_representation(
        arena: &'a Bump,
        subs: &Subs,
        tags: std::vec::Vec<(Symbol, std::vec::Vec<Variable>)>,
        target_info: TargetInfo,
        multimorphic_names: &mut MultimorphicNames,
    ) -> Layout<'a> {
        // otherwise, this is a closure with a payload
        let variant =
            union_sorted_tags_help(arena, tags, None, subs, target_info, multimorphic_names);

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
                        debug_assert!(matches!(tags[0].0, TagOrClosure::Closure(_)));

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

enum ResolvedLambdaSet {
    Set(std::vec::Vec<(Symbol, std::vec::Vec<Variable>)>),
    /// TODO: figure out if this can happen in a correct program, or is the result of a bug in our
    /// compiler. See https://github.com/rtfeldman/roc/issues/3163.
    Unbound,
}

fn resolve_lambda_set(subs: &Subs, mut var: Variable) -> ResolvedLambdaSet {
    let mut set = vec![];
    loop {
        match subs.get_content_without_compacting(var) {
            Content::LambdaSet(subs::LambdaSet {
                solved,
                recursion_var: _,
                unspecialized,
            }) => {
                debug_assert!(
                    unspecialized.is_empty(),
                    "unspecialized lambda sets left over during resolution: {:?}",
                    roc_types::subs::SubsFmtContent(subs.get_content_without_compacting(var), subs),
                );
                roc_types::pretty_print::push_union(subs, solved, &mut set);
                return ResolvedLambdaSet::Set(set);
            }
            Content::RecursionVar { structure, .. } => {
                var = *structure;
            }
            Content::FlexVar(_) => return ResolvedLambdaSet::Unbound,

            c => internal_error!("called with a non-lambda set {:?}", c),
        }
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
    multimorphic_names: &'b mut MultimorphicNames,
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

#[inline(always)]
pub fn is_unresolved_var(subs: &Subs, var: Variable) -> bool {
    use Content::*;
    let content = subs.get_content_without_compacting(var);
    matches!(
        content,
        FlexVar(..) | RigidVar(..) | FlexAbleVar(..) | RigidAbleVar(..),
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
        var: Variable,
        content: Content,
    ) -> Result<Self, LayoutProblem> {
        use roc_types::subs::Content::*;
        match content {
            FlexVar(_) | RigidVar(_) => Err(LayoutProblem::UnresolvedTypeVar(var)),
            FlexAbleVar(_, _) | RigidAbleVar(_, _) => todo_abilities!("Not reachable yet"),
            RecursionVar { structure, .. } => {
                let structure_content = env.subs.get_content_without_compacting(structure);
                Self::new_help(env, structure, *structure_content)
            }
            LambdaSet(lset) => layout_from_lambda_set(env, lset),
            Structure(flat_type) => layout_from_flat_type(env, flat_type),

            Alias(symbol, _args, actual_var, _) => {
                if let Some(int_width) = IntWidth::try_from_symbol(symbol) {
                    return Ok(Layout::Builtin(Builtin::Int(int_width)));
                }

                if let Some(float_width) = FloatWidth::try_from_symbol(symbol) {
                    return Ok(Layout::Builtin(Builtin::Float(float_width)));
                }

                match symbol {
                    Symbol::NUM_DECIMAL => return Ok(Layout::Builtin(Builtin::Decimal)),

                    Symbol::NUM_NAT | Symbol::NUM_NATURAL => {
                        return Ok(Layout::usize(env.target_info))
                    }

                    Symbol::NUM_NUM | Symbol::NUM_INT | Symbol::NUM_INTEGER
                        if is_unresolved_var(env.subs, actual_var) =>
                    {
                        // default to i64
                        return Ok(Layout::i64());
                    }

                    Symbol::NUM_FRAC | Symbol::NUM_FLOATINGPOINT
                        if is_unresolved_var(env.subs, actual_var) =>
                    {
                        // default to f64
                        return Ok(Layout::f64());
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
            Self::new_help(env, var, *content)
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

    pub fn is_passed_by_reference(&self, target_info: TargetInfo) -> bool {
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
                .runtime_representation()
                .is_passed_by_reference(target_info),
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

    /// Very important to use this when doing a memcpy!
    pub fn stack_size_without_alignment(&self, target_info: TargetInfo) -> u32 {
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
            Union(variant) => variant.stack_size_without_alignment(target_info),
            LambdaSet(lambda_set) => lambda_set
                .runtime_representation()
                .stack_size_without_alignment(target_info),
            RecursivePointer => target_info.ptr_width() as u32,
            Boxed(_) => target_info.ptr_width() as u32,
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
            Layout::Boxed(_) => target_info.ptr_width() as u32,
        }
    }

    pub fn allocation_alignment_bytes(&self, target_info: TargetInfo) -> u32 {
        let ptr_width = target_info.ptr_width() as u32;

        match self {
            Layout::Builtin(builtin) => builtin.allocation_alignment_bytes(target_info),
            Layout::Struct { .. } => self.alignment_bytes(target_info).max(ptr_width),
            Layout::Union(union_layout) => union_layout.allocation_alignment_bytes(target_info),
            Layout::LambdaSet(lambda_set) => lambda_set
                .runtime_representation()
                .allocation_alignment_bytes(target_info),
            Layout::RecursivePointer => unreachable!("should be looked up to get an actual layout"),
            Layout::Boxed(inner) => inner.allocation_alignment_bytes(target_info),
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
                        .flat_map(|ls| ls.iter())
                        .any(|f| f.contains_refcounted()),
                    Recursive(_)
                    | NullableWrapped { .. }
                    | NullableUnwrapped { .. }
                    | NonNullableUnwrapped(_) => true,
                }
            }
            LambdaSet(lambda_set) => lambda_set.runtime_representation().contains_refcounted(),
            RecursivePointer => true,
            Boxed(_) => true,
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
            Boxed(inner) => alloc
                .text("Boxed(")
                .append(inner.to_doc(alloc, parens))
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
}

/// Avoid recomputing Layout from Variable multiple times.
/// We use `ena` for easy snapshots and rollbacks of the cache.
/// During specialization, a type variable `a` can be specialized to different layouts,
/// e.g. `identity : a -> a` could be specialized to `Bool -> Bool` or `Str -> Str`.
/// Therefore in general it's invalid to store a map from variables to layouts
/// But if we're careful when to invalidate certain keys, we still get some benefit
#[derive(Debug)]
pub struct LayoutCache<'a> {
    pub target_info: TargetInfo,
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
        multimorphic_names: &mut MultimorphicNames,
    ) -> Result<Layout<'a>, LayoutProblem> {
        // Store things according to the root Variable, to avoid duplicate work.
        let var = subs.get_root_key_without_compacting(var);

        let mut env = Env {
            arena,
            subs,
            seen: Vec::new_in(arena),
            target_info: self.target_info,
            multimorphic_names,
        };

        Layout::from_var(&mut env, var)
    }

    pub fn raw_from_var(
        &mut self,
        arena: &'a Bump,
        var: Variable,
        subs: &Subs,
        multimorphic_names: &mut MultimorphicNames,
    ) -> Result<RawFunctionLayout<'a>, LayoutProblem> {
        // Store things according to the root Variable, to avoid duplicate work.
        let var = subs.get_root_key_without_compacting(var);

        let mut env = Env {
            arena,
            subs,
            seen: Vec::new_in(arena),
            target_info: self.target_info,
            multimorphic_names,
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
    pub const STR_WORDS: u32 = 3;
    pub const DICT_WORDS: u32 = 3;
    pub const SET_WORDS: u32 = Builtin::DICT_WORDS; // Set is an alias for Dict with {} for value
    pub const LIST_WORDS: u32 = 3;

    /// Layout of collection wrapper for List and Str - a struct of (pointer, length, capacity).
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
            Builtin::Str => ptr_width,
            Builtin::Dict(k, v) => k
                .alignment_bytes(target_info)
                .max(v.alignment_bytes(target_info))
                .max(ptr_width),
            Builtin::Set(k) => k.alignment_bytes(target_info).max(ptr_width),
            Builtin::List(e) => e.alignment_bytes(target_info).max(ptr_width),
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
) -> Result<Layout<'a>, LayoutProblem> {
    // Lambda set is just a tag union from the layout's perspective.
    let subs::LambdaSet {
        solved,
        recursion_var,
        unspecialized,
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
            Ok(layout_from_union(env, &labels))
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

                Symbol::NUM_NUM => {
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
                Symbol::BOX_BOX_TYPE => {
                    // Num.Num should only ever have 1 argument, e.g. Num.Num Int.Integer
                    debug_assert_eq!(args.len(), 1);

                    let inner_var = args[0];
                    let inner_layout = Layout::from_var(env, inner_var)?;

                    Ok(Layout::Boxed(env.arena.alloc(inner_layout)))
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
            let lambda_set = LambdaSet::from_var(
                env.arena,
                env.subs,
                closure_var,
                env.target_info,
                env.multimorphic_names,
            )?;

            Ok(Layout::LambdaSet(lambda_set))
        }
        Record(fields, ext_var) => {
            // extract any values from the ext_var

            let mut sortables = Vec::with_capacity_in(fields.len(), arena);
            let it = match fields.unsorted_iterator(subs, ext_var) {
                Ok(it) => it,
                Err(RecordFieldsError) => return Err(LayoutProblem::Erroneous),
            };

            for (label, field) in it {
                match field {
                    RecordField::Required(field_var) | RecordField::Demanded(field_var) => {
                        sortables.push((label, Layout::from_var(env, field_var)?));
                    }
                    RecordField::Optional(_) => {
                        // drop optional fields
                    }
                }
            }

            sortables.sort_by(|(label1, layout1), (label2, layout2)| {
                cmp_fields(label1, layout1, label2, layout2, target_info)
            });

            let ordered_field_names =
                Vec::from_iter_in(sortables.iter().map(|(label, _)| *label), arena);
            let field_order_hash =
                FieldOrderHash::from_ordered_fields(ordered_field_names.as_slice());

            if sortables.len() == 1 {
                // If the record has only one field that isn't zero-sized,
                // unwrap it.
                Ok(sortables.pop().unwrap().1)
            } else {
                let layouts = Vec::from_iter_in(sortables.into_iter().map(|t| t.1), arena);

                Ok(Layout::Struct {
                    field_order_hash,
                    field_layouts: layouts.into_bump_slice(),
                })
            }
        }
        TagUnion(tags, ext_var) => {
            let (tags, ext_var) = tags.unsorted_tags_and_ext(subs, ext_var);

            debug_assert!(ext_var_is_empty_tag_union(subs, ext_var));

            Ok(layout_from_union(env, &tags))
        }
        FunctionOrTagUnion(tag_name, _, ext_var) => {
            debug_assert!(
                ext_var_is_empty_tag_union(subs, ext_var),
                "If ext_var wasn't empty, this wouldn't be a FunctionOrTagUnion!"
            );

            let union_tags = UnionTags::from_tag_name_index(tag_name);
            let (tags, _) = union_tags.unsorted_tags_and_ext(subs, ext_var);

            Ok(layout_from_union(env, &tags))
        }
        RecursiveTagUnion(rec_var, tags, ext_var) => {
            let (tags, ext_var) = tags.unsorted_tags_and_ext(subs, ext_var);

            debug_assert!(ext_var_is_empty_tag_union(subs, ext_var));

            layout_from_recursive_union(env, rec_var, &tags)
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
    multimorphic_names: &mut MultimorphicNames,
) -> Result<Vec<'a, SortedField<'a>>, LayoutProblem> {
    let mut env = Env {
        arena,
        subs,
        seen: Vec::new_in(arena),
        target_info,
        multimorphic_names,
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
                    cmp_fields(label1, layout1, label2, layout2, target_info)
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
    UnitWithArguments,
    BoolUnion {
        ttrue: TagOrClosure,
        ffalse: TagOrClosure,
    },
    ByteUnion(Vec<'a, TagOrClosure>),
    Newtype {
        tag_name: TagOrClosure,
        arguments: Vec<'a, Layout<'a>>,
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
    arena: &'a Bump,
    var: Variable,
    subs: &Subs,
    target_info: TargetInfo,
    multimorphic_names: &mut MultimorphicNames,
) -> Result<UnionVariant<'a>, LayoutProblem> {
    let var =
        if let Content::RecursionVar { structure, .. } = subs.get_content_without_compacting(var) {
            *structure
        } else {
            var
        };

    let mut tags_vec = std::vec::Vec::new();
    let result = match roc_types::pretty_print::chase_ext_tag_union(subs, var, &mut tags_vec) {
        Ok(())
        // Admit type variables in the extension for now. This may come from things that never got
        // monomorphized, like in
        //   x : [A]*
        //   x = A
        //   x
        // In such cases it's fine to drop the variable. We may be proven wrong in the future...
        | Err((_, Content::FlexVar(_) | Content::RigidVar(_)))
        | Err((_, Content::RecursionVar { .. })) => {
            let opt_rec_var = get_recursion_var(subs, var);
            union_sorted_tags_help(arena, tags_vec, opt_rec_var, subs, target_info, multimorphic_names)
        }
        Err((_, Content::Error)) => return Err(LayoutProblem::Erroneous),
        Err(other) => panic!("invalid content in tag union variable: {:?}", other),
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

fn union_sorted_tags_help_new<'a, L>(
    env: &mut Env<'a, '_>,
    tags_list: &[(&'_ L, &[Variable])],
    opt_rec_var: Option<Variable>,
) -> UnionVariant<'a>
where
    L: Label + Ord + Clone + Into<TagOrClosure>,
{
    // sort up front; make sure the ordering stays intact!
    let mut tags_list = Vec::from_iter_in(tags_list.iter(), env.arena);
    tags_list.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));

    match tags_list.len() {
        0 => {
            // trying to instantiate a type with no values
            UnionVariant::Never
        }
        1 => {
            let &(tag_name, arguments) = tags_list.remove(0);
            let tag_name = tag_name.clone().into();

            // just one tag in the union (but with arguments) can be a struct
            let mut layouts = Vec::with_capacity_in(tags_list.len(), env.arena);

            for &var in arguments {
                match Layout::from_var(env, var) {
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
                let size1 = layout1.alignment_bytes(env.target_info);
                let size2 = layout2.alignment_bytes(env.target_info);

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
            let mut answer: Vec<(TagOrClosure, &[Layout])> =
                Vec::with_capacity_in(tags_list.len(), env.arena);
            let mut has_any_arguments = false;

            let mut nullable: Option<(TagIdIntType, TagOrClosure)> = None;

            // only recursive tag unions can be nullable
            let is_recursive = opt_rec_var.is_some();
            if is_recursive && GENERATE_NULLABLE {
                for (index, (name, variables)) in tags_list.iter().enumerate() {
                    if variables.is_empty() {
                        nullable = Some((index as TagIdIntType, (*name).clone().into()));
                        break;
                    }
                }
            }

            for (index, &(tag_name, arguments)) in tags_list.into_iter().enumerate() {
                // reserve space for the tag discriminant
                if matches!(nullable, Some((i, _)) if i as usize == index) {
                    debug_assert!(arguments.is_empty());
                    continue;
                }

                let mut arg_layouts = Vec::with_capacity_in(arguments.len() + 1, env.arena);

                for &var in arguments {
                    match Layout::from_var(env, var) {
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
                    let size1 = layout1.alignment_bytes(env.target_info);
                    let size2 = layout2.alignment_bytes(env.target_info);

                    size2.cmp(&size1)
                });

                answer.push((tag_name.clone().into(), arg_layouts.into_bump_slice()));
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
                    let mut tag_names = Vec::with_capacity_in(answer.len(), env.arena);

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

pub fn union_sorted_tags_help<'a, L>(
    arena: &'a Bump,
    mut tags_vec: std::vec::Vec<(L, std::vec::Vec<Variable>)>,
    opt_rec_var: Option<Variable>,
    subs: &Subs,
    target_info: TargetInfo,
    multimorphic_names: &mut MultimorphicNames,
) -> UnionVariant<'a>
where
    L: Into<TagOrClosure> + Ord + Clone,
{
    // sort up front; make sure the ordering stays intact!
    tags_vec.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));

    let mut env = Env {
        arena,
        subs,
        seen: Vec::new_in(arena),
        target_info,
        multimorphic_names,
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
                    tag_name: tag_name.into(),
                    fields: layouts.into_bump_slice(),
                })
            } else {
                UnionVariant::Newtype {
                    tag_name: tag_name.into(),
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

                answer.push((tag_name.into(), arg_layouts.into_bump_slice()));
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

                    UnionVariant::Wrapped(variant)
                }
            }
        }
    }
}

fn layout_from_newtype<'a, L: Label>(
    env: &mut Env<'a, '_>,
    tags: &UnsortedUnionLabels<L>,
) -> Layout<'a> {
    debug_assert!(tags.is_newtype_wrapper(env.subs));

    let (_tag_name, var) = tags.get_newtype(env.subs);

    match Layout::from_var(env, var) {
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

fn layout_from_union<'a, L>(env: &mut Env<'a, '_>, tags: &UnsortedUnionLabels<L>) -> Layout<'a>
where
    L: Label + Ord + Into<TagOrClosure>,
{
    use UnionVariant::*;

    if tags.is_newtype_wrapper(env.subs) {
        return layout_from_newtype(env, tags);
    }

    let tags_vec = &tags.tags;

    let opt_rec_var = None;
    let variant = union_sorted_tags_help_new(env, tags_vec, opt_rec_var);

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
                    let mut tag_layouts = Vec::with_capacity_in(tags.len(), env.arena);
                    tag_layouts.extend(tags.iter().map(|r| r.1));

                    Layout::Union(UnionLayout::NonRecursive(tag_layouts.into_bump_slice()))
                }

                Recursive {
                    sorted_tag_layouts: tags,
                } => {
                    let mut tag_layouts = Vec::with_capacity_in(tags.len(), env.arena);
                    tag_layouts.extend(tags.iter().map(|r| r.1));

                    debug_assert!(tag_layouts.len() > 1);
                    Layout::Union(UnionLayout::Recursive(tag_layouts.into_bump_slice()))
                }

                NullableWrapped {
                    nullable_id,
                    nullable_name: _,
                    sorted_tag_layouts: tags,
                } => {
                    let mut tag_layouts = Vec::with_capacity_in(tags.len(), env.arena);
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

fn layout_from_recursive_union<'a, L>(
    env: &mut Env<'a, '_>,
    rec_var: Variable,
    tags: &UnsortedUnionLabels<L>,
) -> Result<Layout<'a>, LayoutProblem>
where
    L: Label + Ord + Into<TagOrClosure>,
{
    let arena = env.arena;
    let subs = env.subs;
    let target_info = env.target_info;

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
        Ok(()) | Err((_, Content::FlexVar(_) | Content::RigidVar(_))) => ext_fields.is_empty(),
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

pub trait FreshMultimorphicSymbol: FnMut() -> Symbol {}
impl<T> FreshMultimorphicSymbol for T where T: FnMut() -> Symbol {}

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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn width_and_alignment_union_empty_struct() {
        let lambda_set = LambdaSet {
            set: &[(LambdaName::from_non_multimorphic(Symbol::LIST_MAP), &[])],
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

    #[test]
    fn memcpy_size_result_u32_unit() {
        let ok_tag = &[Layout::Builtin(Builtin::Int(IntWidth::U32))];
        let err_tag = &[Layout::UNIT];
        let tags = [ok_tag as &[_], err_tag as &[_]];
        let union_layout = UnionLayout::NonRecursive(&tags as &[_]);
        let layout = Layout::Union(union_layout);

        let target_info = TargetInfo::default_x86_64();
        assert_eq!(layout.stack_size_without_alignment(target_info), 5);
    }
}

/// Compare two fields when sorting them for code gen.
/// This is called by both code gen and bindgen, so that
/// their field orderings agree.
#[inline(always)]
pub fn cmp_fields<L: Ord>(
    label1: &L,
    layout1: &Layout<'_>,
    label2: &L,
    layout2: &Layout<'_>,
    target_info: TargetInfo,
) -> Ordering {
    let size1 = layout1.alignment_bytes(target_info);
    let size2 = layout2.alignment_bytes(target_info);

    size2.cmp(&size1).then(label1.cmp(label2))
}
