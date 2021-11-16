use crate::ir::Parens;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::{default_hasher, MutMap};
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::{Interns, Symbol};
use roc_types::subs::{
    Content, FlatType, RecordFields, Subs, UnionTags, Variable, VariableSubsSlice,
};
use roc_types::types::{gather_fields_unsorted_iter, RecordField};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use ven_pretty::{DocAllocator, DocBuilder};

// if your changes cause this number to go down, great!
// please change it to the lower number.
// if it went up, maybe check that the change is really required
static_assertions::assert_eq_size!([u8; 3 * 8], Builtin);
static_assertions::assert_eq_size!([u8; 4 * 8], Layout);
static_assertions::assert_eq_size!([u8; 3 * 8], UnionLayout);
static_assertions::assert_eq_size!([u8; 3 * 8], LambdaSet);

pub type TagIdIntType = u16;
pub const MAX_ENUM_SIZE: usize = (std::mem::size_of::<TagIdIntType>() * 8) as usize;
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

            // Ints
            Alias(Symbol::NUM_I128, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::Builtin(Builtin::Int128)))
            }
            Alias(Symbol::NUM_I64, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::Builtin(Builtin::Int64)))
            }
            Alias(Symbol::NUM_I32, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::Builtin(Builtin::Int32)))
            }
            Alias(Symbol::NUM_I16, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::Builtin(Builtin::Int16)))
            }
            Alias(Symbol::NUM_I8, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::Builtin(Builtin::Int8)))
            }

            // I think unsigned and signed use the same layout
            Alias(Symbol::NUM_U128, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::Builtin(Builtin::Int128)))
            }
            Alias(Symbol::NUM_U64, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::Builtin(Builtin::Int64)))
            }
            Alias(Symbol::NUM_U32, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::Builtin(Builtin::Int32)))
            }
            Alias(Symbol::NUM_U16, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::Builtin(Builtin::Int16)))
            }
            Alias(Symbol::NUM_U8, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::Builtin(Builtin::Int8)))
            }

            Alias(Symbol::NUM_NAT, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::Builtin(Builtin::Usize)))
            }

            // Floats
            Alias(Symbol::NUM_F64, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::Builtin(Builtin::Float64)))
            }
            Alias(Symbol::NUM_F32, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Self::ZeroArgumentThunk(Layout::Builtin(Builtin::Float32)))
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
                    LambdaSet::from_var(env.arena, env.subs, closure_var, env.ptr_bytes)?;

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

/// Types for code gen must be monomorphic. No type variables allowed!
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Layout<'a> {
    Builtin(Builtin<'a>),
    /// A layout that is empty (turns into the empty struct in LLVM IR
    /// but for our purposes, not zero-sized, so it does not get dropped from data structures
    /// this is important for closures that capture zero-sized values
    Struct(&'a [Layout<'a>]),
    Union(UnionLayout<'a>),
    LambdaSet(LambdaSet<'a>),
    RecursivePointer,
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
        nullable_id: u16,
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
            Builtin::Int8
        } else if union_size <= u16::MAX as usize {
            Builtin::Int16
        } else {
            panic!("tag union is too big")
        }
    }

    pub fn tag_id_builtin(&self) -> Builtin<'a> {
        match self {
            UnionLayout::NonRecursive(_tags) => {
                // let union_size = tags.len();
                // Self::tag_id_builtin_help(union_size)

                // The quicksort-benchmarks version of Quicksort.roc segfaults when
                // this number is not I64. There must be some dependence on that fact
                // somewhere in the code, I have not found where that is yet...
                Builtin::Int64
            }
            UnionLayout::Recursive(tags) => {
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

    fn stores_tag_id_in_pointer_bits(tags: &[&[Layout<'a>]], ptr_bytes: u32) -> bool {
        tags.len() <= ptr_bytes as usize
    }

    // i.e. it is not implicit and not stored in the pointer bits
    pub fn stores_tag_id_as_data(&self, ptr_bytes: u32) -> bool {
        match self {
            UnionLayout::NonRecursive(_) => true,
            UnionLayout::Recursive(tags)
            | UnionLayout::NullableWrapped {
                other_tags: tags, ..
            } => !Self::stores_tag_id_in_pointer_bits(tags, ptr_bytes),
            UnionLayout::NonNullableUnwrapped(_) | UnionLayout::NullableUnwrapped { .. } => false,
        }
    }

    pub fn stores_tag_id_in_pointer(&self, ptr_bytes: u32) -> bool {
        match self {
            UnionLayout::NonRecursive(_) => false,
            UnionLayout::Recursive(tags)
            | UnionLayout::NullableWrapped {
                other_tags: tags, ..
            } => Self::stores_tag_id_in_pointer_bits(tags, ptr_bytes),
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

    fn tags_alignment_bytes(tags: &[&[Layout]], pointer_size: u32) -> u32 {
        tags.iter()
            .map(|fields| Layout::Struct(fields).alignment_bytes(pointer_size))
            .max()
            .unwrap_or(0)
    }

    pub fn allocation_alignment_bytes(&self, pointer_size: u32) -> u32 {
        let allocation = match self {
            UnionLayout::NonRecursive(_) => unreachable!("not heap-allocated"),
            UnionLayout::Recursive(tags) => Self::tags_alignment_bytes(tags, pointer_size),
            UnionLayout::NonNullableUnwrapped(fields) => {
                Layout::Struct(fields).alignment_bytes(pointer_size)
            }
            UnionLayout::NullableWrapped { other_tags, .. } => {
                Self::tags_alignment_bytes(other_tags, pointer_size)
            }
            UnionLayout::NullableUnwrapped { other_fields, .. } => {
                Layout::Struct(other_fields).alignment_bytes(pointer_size)
            }
        };

        // because we store a refcount, the alignment must be at least the size of a pointer
        allocation.max(pointer_size)
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

    pub fn is_represented(&self) -> Option<Layout<'a>> {
        if let Layout::Struct(&[]) = self.representation {
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
            Layout::Struct(_) => {
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
        ptr_bytes: u32,
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
                    ptr_bytes,
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
                    arena.alloc(Self::make_representation(arena, subs, tags, ptr_bytes));

                Ok(LambdaSet {
                    set: set.into_bump_slice(),
                    representation,
                })
            }

            Ok(()) | Err((_, Content::FlexVar(_))) => {
                // this can happen when there is a type error somewhere
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
        ptr_bytes: u32,
    ) -> Layout<'a> {
        // otherwise, this is a closure with a payload
        let variant = union_sorted_tags_help(arena, tags, None, subs, ptr_bytes);

        use UnionVariant::*;
        match variant {
            Never => Layout::Union(UnionLayout::NonRecursive(&[])),
            BoolUnion { .. } => Layout::Builtin(Builtin::Int1),
            ByteUnion { .. } => Layout::Builtin(Builtin::Int8),
            Unit | UnitWithArguments => {
                // no useful information to store
                Layout::Struct(&[])
            }
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
    Decimal,
    Float128,
    Float64,
    Float32,
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
    ptr_bytes: u32,
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

const fn round_up_to_alignment(width: u32, alignment: u32) -> u32 {
    if alignment != 0 && width % alignment > 0 {
        width + alignment - (width % alignment)
    } else {
        width
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
                let structure_content = env.subs.get_content_without_compacting(structure);
                Self::new_help(env, structure, structure_content.clone())
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

            // Nat
            Alias(Symbol::NUM_NAT, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Usize))
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
            let content = env.subs.get_content_without_compacting(var);
            Self::new_help(env, var, content.clone())
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

    pub fn stack_size(&self, pointer_size: u32) -> u32 {
        let width = self.stack_size_without_alignment(pointer_size);
        let alignment = self.alignment_bytes(pointer_size);

        round_up_to_alignment(width, alignment)
    }

    fn stack_size_without_alignment(&self, pointer_size: u32) -> u32 {
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
                        let tag_id_builtin = variant.tag_id_builtin();

                        fields
                            .iter()
                            .map(|tag_layout| {
                                tag_layout
                                    .iter()
                                    .map(|field| field.stack_size(pointer_size))
                                    .sum::<u32>()
                            })
                            .max()
                            .map(|w| round_up_to_alignment(w, tag_id_builtin.alignment_bytes(pointer_size)))
                            .unwrap_or_default()
                            // the size of the tag_id
                            + tag_id_builtin.stack_size(pointer_size)
                    }

                    Recursive(_)
                    | NullableWrapped { .. }
                    | NullableUnwrapped { .. }
                    | NonNullableUnwrapped(_) => pointer_size,
                }
            }
            LambdaSet(lambda_set) => lambda_set
                .runtime_representation()
                .stack_size_without_alignment(pointer_size),
            RecursivePointer => pointer_size,
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
                    NonRecursive(tags) => {
                        let max_alignment = tags
                            .iter()
                            .flat_map(|layouts| {
                                layouts
                                    .iter()
                                    .map(|layout| layout.alignment_bytes(pointer_size))
                            })
                            .max();

                        let tag_id_builtin = variant.tag_id_builtin();
                        match max_alignment {
                            Some(align) => round_up_to_alignment(
                                align.max(tag_id_builtin.alignment_bytes(pointer_size)),
                                tag_id_builtin.alignment_bytes(pointer_size),
                            ),
                            None => {
                                // none of the tags had any payload, but the tag id still contains information
                                tag_id_builtin.alignment_bytes(pointer_size)
                            }
                        }
                    }
                    Recursive(_)
                    | NullableWrapped { .. }
                    | NullableUnwrapped { .. }
                    | NonNullableUnwrapped(_) => pointer_size,
                }
            }
            Layout::LambdaSet(lambda_set) => lambda_set
                .runtime_representation()
                .alignment_bytes(pointer_size),
            Layout::Builtin(builtin) => builtin.alignment_bytes(pointer_size),
            Layout::RecursivePointer => pointer_size,
        }
    }

    pub fn allocation_alignment_bytes(&self, pointer_size: u32) -> u32 {
        match self {
            Layout::Builtin(builtin) => builtin.allocation_alignment_bytes(pointer_size),
            Layout::Struct(_) => unreachable!("not heap-allocated"),
            Layout::Union(union_layout) => union_layout.allocation_alignment_bytes(pointer_size),
            Layout::LambdaSet(lambda_set) => lambda_set
                .runtime_representation()
                .allocation_alignment_bytes(pointer_size),
            Layout::RecursivePointer => unreachable!("should be looked up to get an actual layout"),
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
            Struct(fields) => {
                let fields_doc = fields.iter().map(|x| x.to_doc(alloc, parens));

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
}

/// Avoid recomputing Layout from Variable multiple times.
/// We use `ena` for easy snapshots and rollbacks of the cache.
/// During specialization, a type variable `a` can be specialized to different layouts,
/// e.g. `identity : a -> a` could be specialized to `Bool -> Bool` or `Str -> Str`.
/// Therefore in general it's invalid to store a map from variables to layouts
/// But if we're careful when to invalidate certain keys, we still get some benefit
#[derive(Debug)]
pub struct LayoutCache<'a> {
    ptr_bytes: u32,
    _marker: std::marker::PhantomData<&'a u8>,
}

#[derive(Debug, Clone)]
pub enum CachedLayout<'a> {
    Cached(Layout<'a>),
    NotCached,
    Problem(LayoutProblem),
}

impl<'a> LayoutCache<'a> {
    pub fn new(ptr_bytes: u32) -> Self {
        Self {
            ptr_bytes,
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
            ptr_bytes: self.ptr_bytes,
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
            ptr_bytes: self.ptr_bytes,
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

impl<'a> Builtin<'a> {
    const I128_SIZE: u32 = std::mem::size_of::<i128>() as u32;
    const I64_SIZE: u32 = std::mem::size_of::<i64>() as u32;
    const I32_SIZE: u32 = std::mem::size_of::<i32>() as u32;
    const I16_SIZE: u32 = std::mem::size_of::<i16>() as u32;
    const I8_SIZE: u32 = std::mem::size_of::<i8>() as u32;
    const I1_SIZE: u32 = std::mem::size_of::<bool>() as u32;
    const DECIMAL_SIZE: u32 = std::mem::size_of::<i128>() as u32;
    const F128_SIZE: u32 = 16;
    const F64_SIZE: u32 = std::mem::size_of::<f64>() as u32;
    const F32_SIZE: u32 = std::mem::size_of::<f32>() as u32;

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
            Usize => pointer_size,
            Decimal => Builtin::DECIMAL_SIZE,
            Float128 => Builtin::F128_SIZE,
            Float64 => Builtin::F64_SIZE,
            Float32 => Builtin::F32_SIZE,
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
            Usize => pointer_size,
            Decimal => align_of::<i128>() as u32,
            Float128 => align_of::<i128>() as u32,
            Float64 => align_of::<f64>() as u32,
            Float32 => align_of::<f32>() as u32,
            Dict(_, _) | EmptyDict => pointer_size,
            Set(_) | EmptySet => pointer_size,
            // we often treat these as i128 (64-bit systems)
            // or i64 (32-bit systems).
            //
            // In webassembly, For that to be safe
            // they must be aligned to allow such access
            List(_) | EmptyList => pointer_size,
            Str | EmptyStr => pointer_size,
        }
    }

    pub fn safe_to_memcpy(&self) -> bool {
        use Builtin::*;

        match self {
            Int128 | Int64 | Int32 | Int16 | Int8 | Int1 | Usize | Decimal | Float128 | Float64
            | Float32 | EmptyStr | EmptyDict | EmptyList | EmptySet => true,
            Str | Dict(_, _) | Set(_) | List(_) => false,
        }
    }

    // Question: does is_refcounted exactly correspond with the "safe to memcpy" property?
    pub fn is_refcounted(&self) -> bool {
        use Builtin::*;

        match self {
            Int128 | Int64 | Int32 | Int16 | Int8 | Int1 | Usize | Decimal | Float128 | Float64
            | Float32 | EmptyStr | EmptyDict | EmptyList | EmptySet => false,
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
            Decimal => alloc.text("Decimal"),
            Float128 => alloc.text("Float128"),
            Float64 => alloc.text("Float64"),
            Float32 => alloc.text("Float32"),

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

    pub fn allocation_alignment_bytes(&self, pointer_size: u32) -> u32 {
        let allocation = match self {
            Builtin::Int128
            | Builtin::Int64
            | Builtin::Int32
            | Builtin::Int16
            | Builtin::Int8
            | Builtin::Int1
            | Builtin::Usize
            | Builtin::Decimal
            | Builtin::Float128
            | Builtin::Float64
            | Builtin::Float32 => unreachable!("not heap-allocated"),
            Builtin::Str => pointer_size,
            Builtin::Dict(k, v) => k
                .alignment_bytes(pointer_size)
                .max(v.alignment_bytes(pointer_size))
                .max(pointer_size),
            Builtin::Set(k) => k.alignment_bytes(pointer_size).max(pointer_size),
            Builtin::List(e) => e.alignment_bytes(pointer_size).max(pointer_size),
            Builtin::EmptyStr | Builtin::EmptyList | Builtin::EmptyDict | Builtin::EmptySet => {
                unreachable!("not heap-allocated")
            }
        };

        allocation.max(pointer_size)
    }
}

fn layout_from_flat_type<'a>(
    env: &mut Env<'a, '_>,
    flat_type: FlatType,
) -> Result<Layout<'a>, LayoutProblem> {
    use roc_types::subs::FlatType::*;

    let arena = env.arena;
    let subs = env.subs;
    let ptr_bytes = env.ptr_bytes;

    match flat_type {
        Apply(symbol, args) => {
            let args = Vec::from_iter_in(args.into_iter().map(|index| subs[index]), arena);

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
                Symbol::NUM_DEC => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Decimal))
                }
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

                    let var = args[0];
                    let content = subs.get_content_without_compacting(var);

                    layout_from_num_content(content)
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
            let lambda_set = LambdaSet::from_var(env.arena, env.subs, closure_var, env.ptr_bytes)?;

            Ok(Layout::LambdaSet(lambda_set))
        }
        Record(fields, ext_var) => {
            // extract any values from the ext_var

            let pairs_it = fields
                .unsorted_iterator(subs, ext_var)
                .filter_map(|(label, field)| {
                    // drop optional fields
                    let var = match field {
                        RecordField::Optional(_) => return None,
                        RecordField::Required(var) => var,
                        RecordField::Demanded(var) => var,
                    };

                    Some((
                        label,
                        Layout::from_var(env, var).expect("invalid layout from var"),
                    ))
                });

            let mut pairs = Vec::from_iter_in(pairs_it, arena);

            pairs.sort_by(|(label1, layout1), (label2, layout2)| {
                let size1 = layout1.alignment_bytes(ptr_bytes);
                let size2 = layout2.alignment_bytes(ptr_bytes);

                size2.cmp(&size1).then(label1.cmp(label2))
            });

            let mut layouts = Vec::from_iter_in(pairs.into_iter().map(|t| t.1), arena);

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

            Ok(layout_from_tag_union(arena, tags, subs, env.ptr_bytes))
        }
        FunctionOrTagUnion(tag_name, _, ext_var) => {
            debug_assert!(ext_var_is_empty_tag_union(subs, ext_var));

            let tags = UnionTags::from_tag_name_index(tag_name);

            Ok(layout_from_tag_union(arena, tags, subs, env.ptr_bytes))
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

            let tags_vec = cheap_sort_tags(arena, tags, subs);

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
            for (index, (_name, variables)) in tags_vec.into_iter().enumerate() {
                if matches!(nullable, Some(i) if i == index as TagIdIntType) {
                    // don't add the nullable case
                    continue;
                }

                let mut tag_layout = Vec::with_capacity_in(variables.len() + 1, arena);

                for var_index in variables {
                    let var = subs[var_index];
                    // TODO does this cause problems with mutually recursive unions?
                    if rec_var == subs.get_root_key_without_compacting(var) {
                        tag_layout.push(Layout::RecursivePointer);
                        continue;
                    }

                    tag_layout.push(Layout::from_var(env, var)?);
                }

                tag_layout.sort_by(|layout1, layout2| {
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
                UnionLayout::NonNullableUnwrapped(tag_layouts.pop().unwrap())
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
    ptr_bytes: u32,
) -> Vec<'a, (Lowercase, Variable, Result<Layout<'a>, Layout<'a>>)> {
    let mut env = Env {
        arena,
        subs,
        seen: Vec::new_in(arena),
        ptr_bytes,
    };

    let (it, _) = gather_fields_unsorted_iter(subs, RecordFields::empty(), var);

    let it = it
        .into_iter()
        .map(|(field, field_type)| (field.clone(), field_type));

    sort_record_fields_help(&mut env, it)
}

fn sort_record_fields_help<'a>(
    env: &mut Env<'a, '_>,
    fields_map: impl Iterator<Item = (Lowercase, RecordField<Variable>)>,
) -> Vec<'a, (Lowercase, Variable, Result<Layout<'a>, Layout<'a>>)> {
    let ptr_bytes = env.ptr_bytes;

    // Sort the fields by label
    let mut sorted_fields = Vec::with_capacity_in(fields_map.size_hint().0, env.arena);

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

        sorted_fields.push((label, var, Ok(layout)));
    }

    sorted_fields.sort_by(
        |(label1, _, res_layout1), (label2, _, res_layout2)| match res_layout1 {
            Ok(layout1) | Err(layout1) => match res_layout2 {
                Ok(layout2) | Err(layout2) => {
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
    ptr_bytes: u32,
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
            union_sorted_tags_help(arena, tags_vec, opt_rec_var, subs, ptr_bytes)
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
    mut tags_vec: Vec<(&'_ TagName, VariableSubsSlice)>,
    opt_rec_var: Option<Variable>,
    subs: &Subs,
    ptr_bytes: u32,
) -> UnionVariant<'a> {
    // sort up front; make sure the ordering stays intact!
    tags_vec.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));

    let mut env = Env {
        arena,
        subs,
        seen: Vec::new_in(arena),
        ptr_bytes,
    };

    match tags_vec.len() {
        0 => {
            // trying to instantiate a type with no values
            UnionVariant::Never
        }
        1 => {
            let (tag_name, arguments) = tags_vec.remove(0);
            let tag_name = tag_name.clone();

            // just one tag in the union (but with arguments) can be a struct
            let mut layouts = Vec::with_capacity_in(tags_vec.len(), arena);

            // special-case NUM_AT_NUM: if its argument is a FlexVar, make it Int
            match tag_name {
                TagName::Private(Symbol::NUM_AT_NUM) => {
                    let var = subs[arguments.into_iter().next().unwrap()];
                    layouts.push(unwrap_num_tag(subs, var).expect("invalid num layout"));
                }
                _ => {
                    for var_index in arguments {
                        let var = subs[var_index];
                        match Layout::from_var(&mut env, var) {
                            Ok(layout) => {
                                layouts.push(layout);
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
                let size1 = layout1.alignment_bytes(ptr_bytes);
                let size2 = layout2.alignment_bytes(ptr_bytes);

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
            let mut answer = Vec::with_capacity_in(tags_vec.len(), arena);
            let mut has_any_arguments = false;

            let mut nullable: Option<(TagIdIntType, TagName)> = None;

            // only recursive tag unions can be nullable
            let is_recursive = opt_rec_var.is_some();
            if is_recursive && GENERATE_NULLABLE {
                for (index, (name, variables)) in tags_vec.iter().enumerate() {
                    if variables.is_empty() {
                        nullable = Some((index as TagIdIntType, (*name).clone()));
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

                for var_index in arguments {
                    let var = subs[var_index];
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
                    let size1 = layout1.alignment_bytes(ptr_bytes);
                    let size2 = layout2.alignment_bytes(ptr_bytes);

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
    ptr_bytes: u32,
) -> UnionVariant<'a> {
    // sort up front; make sure the ordering stays intact!
    tags_vec.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));

    let mut env = Env {
        arena,
        subs,
        seen: Vec::new_in(arena),
        ptr_bytes,
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
                    let size1 = layout1.alignment_bytes(ptr_bytes);
                    let size2 = layout2.alignment_bytes(ptr_bytes);

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

fn cheap_sort_tags<'a, 'b>(
    arena: &'a Bump,
    tags: UnionTags,
    subs: &'b Subs,
) -> Vec<'a, (&'b TagName, VariableSubsSlice)> {
    let mut tags_vec = Vec::with_capacity_in(tags.len(), arena);

    for (tag_index, index) in tags.iter_all() {
        let tag = &subs[tag_index];
        let slice = subs[index];

        tags_vec.push((tag, slice));
    }

    tags_vec
}

fn layout_from_newtype<'a>(
    arena: &'a Bump,
    tags: UnionTags,
    subs: &Subs,
    ptr_bytes: u32,
) -> Layout<'a> {
    debug_assert!(tags.is_newtype_wrapper(subs));

    let slice_index = tags.variables().into_iter().next().unwrap();
    let slice = subs[slice_index];
    let var_index = slice.into_iter().next().unwrap();
    let var = subs[var_index];

    let tag_name_index = tags.tag_names().into_iter().next().unwrap();
    let tag_name = &subs[tag_name_index];

    if tag_name == &TagName::Private(Symbol::NUM_AT_NUM) {
        unwrap_num_tag(subs, var).expect("invalid Num argument")
    } else {
        let mut env = Env {
            arena,
            subs,
            seen: Vec::new_in(arena),
            ptr_bytes,
        };

        match Layout::from_var(&mut env, var) {
            Ok(layout) => layout,
            Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                // If we encounter an unbound type var (e.g. `Ok *`)
                // then it's zero-sized; In the future we may drop this argument
                // completely, but for now we represent it with the empty struct
                Layout::Struct(&[])
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
    tags: UnionTags,
    subs: &Subs,
    ptr_bytes: u32,
) -> Layout<'a> {
    use UnionVariant::*;

    if tags.is_newtype_wrapper(subs) {
        return layout_from_newtype(arena, tags, subs, ptr_bytes);
    }

    let tags_vec = cheap_sort_tags(arena, tags, subs);

    match tags_vec.get(0) {
        Some((tag_name, arguments)) if *tag_name == &TagName::Private(Symbol::NUM_AT_NUM) => {
            debug_assert_eq!(arguments.len(), 1);

            let var_index = arguments.into_iter().next().unwrap();
            let var = subs[var_index];

            unwrap_num_tag(subs, var).expect("invalid Num argument")
        }
        _ => {
            let opt_rec_var = None;
            let variant = union_sorted_tags_help_new(arena, tags_vec, opt_rec_var, subs, ptr_bytes);

            match variant {
                Never => Layout::Union(UnionLayout::NonRecursive(&[])),
                Unit | UnitWithArguments => Layout::Struct(&[]),
                BoolUnion { .. } => Layout::Builtin(Builtin::Int1),
                ByteUnion(_) => Layout::Builtin(Builtin::Int8),
                Newtype {
                    arguments: field_layouts,
                    ..
                } => {
                    let answer1 = if field_layouts.len() == 1 {
                        field_layouts[0]
                    } else {
                        Layout::Struct(field_layouts.into_bump_slice())
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
fn ext_var_is_empty_record(subs: &Subs, ext_var: Variable) -> bool {
    // the ext_var is empty
    let fields = roc_types::types::gather_fields(subs, RecordFields::empty(), ext_var);

    fields.fields.is_empty()
}

#[cfg(not(debug_assertions))]
fn ext_var_is_empty_record(_subs: &Subs, _ext_var: Variable) -> bool {
    // This should only ever be used in debug_assert! macros
    unreachable!();
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

fn layout_from_num_content<'a>(content: &Content) -> Result<Layout<'a>, LayoutProblem> {
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
        Structure(Apply(symbol, args)) => match *symbol {
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
            Symbol::NUM_DEC => Ok(Layout::Builtin(Builtin::Decimal)),
            Symbol::NUM_F64 => Ok(Layout::Builtin(Builtin::Float64)),
            Symbol::NUM_F32 => Ok(Layout::Builtin(Builtin::Float32)),

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
        Structure(_) => {
            panic!("Invalid Num.Num type application: {:?}", content);
        }
        Error => Err(LayoutProblem::Erroneous),
    }
}

fn unwrap_num_tag<'a>(subs: &Subs, var: Variable) -> Result<Layout<'a>, LayoutProblem> {
    match subs.get_content_without_compacting(var) {
        Content::Alias(Symbol::NUM_INTEGER, args, _) => {
            debug_assert!(args.len() == 1);

            let precision_var = subs[args.variables().into_iter().next().unwrap()];

            let precision = subs.get_content_without_compacting(precision_var);

            match precision {
                Content::Alias(symbol, args, _) => {
                    debug_assert!(args.is_empty());

                    let builtin = match *symbol {
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

            let precision_var = subs[args.variables().into_iter().next().unwrap()];

            let precision = subs.get_content_without_compacting(precision_var);

            match precision {
                Content::Alias(Symbol::NUM_BINARY32, args, _) => {
                    debug_assert!(args.is_empty());

                    Ok(Layout::Builtin(Builtin::Float32))
                }
                Content::Alias(Symbol::NUM_BINARY64, args, _) => {
                    debug_assert!(args.is_empty());

                    Ok(Layout::Builtin(Builtin::Float64))
                }
                Content::Alias(Symbol::NUM_DECIMAL, args, _) => {
                    debug_assert!(args.is_empty());

                    Ok(Layout::Builtin(Builtin::Decimal))
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
    match env.subs.get_content_without_compacting(key_var) {
        Content::FlexVar(_) | Content::RigidVar(_) => {
            // If this was still a (Dict * *) then it must have been an empty dict
            Ok(Layout::Builtin(Builtin::EmptyDict))
        }
        key_content => {
            let value_content = env.subs.get_content_without_compacting(value_var);
            let key_layout = Layout::new_help(env, key_var, key_content.clone())?;
            let value_layout = Layout::new_help(env, value_var, value_content.clone())?;

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
    match env.subs.get_content_without_compacting(elem_var) {
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn width_and_alignment_union_empty_struct() {
        let lambda_set = LambdaSet {
            set: &[(Symbol::LIST_MAP, &[])],
            representation: &Layout::Struct(&[]),
        };

        let a = &[Layout::Struct(&[])] as &[_];
        let b = &[Layout::LambdaSet(lambda_set)] as &[_];
        let tt = [a, b];

        let layout = Layout::Union(UnionLayout::NonRecursive(&tt));

        // at the moment, the tag id uses an I64, so
        let ptr_width = 8;
        assert_eq!(layout.stack_size(ptr_width), 8);
        assert_eq!(layout.alignment_bytes(ptr_width), 8);
    }
}
