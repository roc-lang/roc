use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::MutMap;
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_types::subs::{Content, FlatType, Subs, Variable};
use std::collections::BTreeMap;

pub const MAX_ENUM_SIZE: usize = (std::mem::size_of::<u8>() * 8) as usize;

/// If a (Num *) gets translated to a Layout, this is the numeric type it defaults to.
const DEFAULT_NUM_BUILTIN: Builtin<'_> = Builtin::Int64;

#[derive(Debug, Clone)]
pub enum LayoutProblem {
    UnresolvedTypeVar,
    Erroneous,
}

/// Types for code gen must be monomorphic. No type variables allowed!
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Layout<'a> {
    Builtin(Builtin<'a>),
    Struct(&'a [Layout<'a>]),
    Union(&'a [&'a [Layout<'a>]]),
    /// A function. The types of its arguments, then the type of its return value.
    FunctionPointer(&'a [Layout<'a>], &'a Layout<'a>),
    Pointer(&'a Layout<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Builtin<'a> {
    Int128,
    Int64,
    Int32,
    Int16,
    Int8,
    Int1,
    Float128,
    Float64,
    Float32,
    Float16,
    Str,
    Map(&'a Layout<'a>, &'a Layout<'a>),
    Set(&'a Layout<'a>),
    List(&'a Layout<'a>),
    EmptyStr,
    EmptyList,
    EmptyMap,
    EmptySet,
}

impl<'a> Layout<'a> {
    pub fn new(
        arena: &'a Bump,
        content: Content,
        subs: &Subs,
        pointer_size: u32,
    ) -> Result<Self, LayoutProblem> {
        use roc_types::subs::Content::*;

        match content.dbg(subs) {
            FlexVar(_) | RigidVar(_) => Err(LayoutProblem::UnresolvedTypeVar),
            Structure(flat_type) => layout_from_flat_type(arena, flat_type, subs, pointer_size),

            Alias(Symbol::NUM_INT, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Int64))
            }
            Alias(Symbol::NUM_FLOAT, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Float64))
            }
            Alias(_, _, var) => Self::new(
                arena,
                subs.get_without_compacting(var).content,
                subs,
                pointer_size,
            ),
            Error => Err(LayoutProblem::Erroneous),
        }
    }

    /// Returns Err(()) if given an error, or Ok(Layout) if given a non-erroneous Structure.
    /// Panics if given a FlexVar or RigidVar, since those should have been
    /// monomorphized away already!
    fn from_var(
        arena: &'a Bump,
        var: Variable,
        subs: &Subs,
        pointer_size: u32,
    ) -> Result<Self, LayoutProblem> {
        let content = subs.get_without_compacting(var).content;

        Self::new(arena, content, subs, pointer_size)
    }

    pub fn safe_to_memcpy(&self) -> bool {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.safe_to_memcpy(),
            Struct(fields) => fields
                .iter()
                .all(|field_layout| field_layout.safe_to_memcpy()),
            Union(tags) => tags
                .iter()
                .all(|tag_layout| tag_layout.iter().all(|field| field.safe_to_memcpy())),
            FunctionPointer(_, _) => {
                // Function pointers are immutable and can always be safely copied
                true
            }
            Pointer(_) => {
                // We cannot memcpy pointers, because then we would have the same pointer in multiple places!
                false
            }
        }
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
            FunctionPointer(_, _) => pointer_size,
            Pointer(_) => pointer_size,
        }
    }
}

/// Avoid recomputing Layout from Variable multiple times.
#[derive(Default)]
pub struct LayoutCache<'a> {
    layouts: MutMap<Variable, Result<Layout<'a>, LayoutProblem>>,
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
        pointer_size: u32,
    ) -> Result<Layout<'a>, LayoutProblem> {
        // Store things according to the root Variable, to avoid duplicate work.
        let var = subs.get_root_key_without_compacting(var);

        self.layouts
            .entry(var)
            .or_insert_with(|| {
                let content = subs.get_without_compacting(var).content;

                Layout::new(arena, content, subs, pointer_size)
            })
            .clone()
    }
}

impl<'a> Builtin<'a> {
    const I128_SIZE: u32 = std::mem::size_of::<i128>() as u32;
    const I64_SIZE: u32 = std::mem::size_of::<i64>() as u32;
    const I32_SIZE: u32 = std::mem::size_of::<i32>() as u32;
    const I16_SIZE: u32 = std::mem::size_of::<i16>() as u32;
    const I8_SIZE: u32 = std::mem::size_of::<i8>() as u32;
    const I1_SIZE: u32 = std::mem::size_of::<bool>() as u32;
    const F128_SIZE: u32 = 16;
    const F64_SIZE: u32 = std::mem::size_of::<f64>() as u32;
    const F32_SIZE: u32 = std::mem::size_of::<f32>() as u32;
    const F16_SIZE: u32 = 2;

    /// Number of machine words in an empty one of these
    pub const STR_WORDS: u32 = 2;
    pub const MAP_WORDS: u32 = 6;
    pub const SET_WORDS: u32 = Builtin::MAP_WORDS; // Set is an alias for Map with {} for value
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
            Float128 => Builtin::F128_SIZE,
            Float64 => Builtin::F64_SIZE,
            Float32 => Builtin::F32_SIZE,
            Float16 => Builtin::F16_SIZE,
            Str | EmptyStr => Builtin::STR_WORDS * pointer_size,
            Map(_, _) | EmptyMap => Builtin::MAP_WORDS * pointer_size,
            Set(_) | EmptySet => Builtin::SET_WORDS * pointer_size,
            List(_) | EmptyList => Builtin::LIST_WORDS * pointer_size,
        }
    }

    pub fn safe_to_memcpy(&self) -> bool {
        use Builtin::*;

        match self {
            Int128 | Int64 | Int32 | Int16 | Int8 | Int1 | Float128 | Float64 | Float32
            | Float16 | EmptyStr | EmptyMap | EmptyList | EmptySet => true,
            Str | Map(_, _) | Set(_) | List(_) => false,
        }
    }
}

fn layout_from_flat_type<'a>(
    arena: &'a Bump,
    flat_type: FlatType,
    subs: &Subs,
    pointer_size: u32,
) -> Result<Layout<'a>, LayoutProblem> {
    use roc_types::subs::FlatType::*;

    match flat_type {
        Apply(symbol, args) => {
            match symbol {
                Symbol::NUM_INT => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Int64))
                }
                Symbol::NUM_FLOAT => {
                    debug_assert_eq!(args.len(), 0);
                    Ok(Layout::Builtin(Builtin::Float64))
                }
                Symbol::NUM_NUM | Symbol::NUM_AT_NUM => {
                    // Num.Num should only ever have 1 argument, e.g. Num.Num Int.Integer
                    debug_assert_eq!(args.len(), 1);

                    let var = args.iter().next().unwrap();
                    let content = subs.get_without_compacting(*var).content;

                    layout_from_num_content(content)
                }
                Symbol::STR_STR => Ok(Layout::Builtin(Builtin::Str)),
                Symbol::LIST_LIST => list_layout_from_elem(arena, subs, args[0], pointer_size),
                Symbol::ATTR_ATTR => {
                    debug_assert_eq!(args.len(), 2);

                    // The first argument is the uniqueness info;
                    // that doesn't affect layout, so we don't need it here.
                    let wrapped_var = args[1];

                    // For now, layout is unaffected by uniqueness.
                    // (Incorporating refcounting may change this.)
                    // Unwrap and continue
                    Layout::from_var(arena, wrapped_var, subs, pointer_size)
                }
                _ => {
                    panic!("TODO layout_from_flat_type for {:?}", Apply(symbol, args));
                }
            }
        }
        Func(args, ret_var) => {
            let mut fn_args = Vec::with_capacity_in(args.len(), arena);

            for arg_var in args {
                let arg_content = subs.get_without_compacting(arg_var).content;

                fn_args.push(Layout::new(arena, arg_content, subs, pointer_size)?);
            }

            let ret_content = subs.get_without_compacting(ret_var).content;
            let ret = Layout::new(arena, ret_content, subs, pointer_size)?;

            Ok(Layout::FunctionPointer(
                fn_args.into_bump_slice(),
                arena.alloc(ret),
            ))
        }
        Record(fields, ext_var) => {
            debug_assert!(ext_var_is_empty_record(subs, ext_var));

            let btree = fields
                .into_iter()
                .collect::<BTreeMap<Lowercase, Variable>>();

            let mut layouts = Vec::with_capacity_in(btree.len(), arena);

            for (_, field_var) in btree {
                let field_content = subs.get_without_compacting(field_var).content;
                let field_layout = match Layout::new(arena, field_content, subs, pointer_size) {
                    Ok(layout) => layout,
                    Err(_) => {
                        // Invalid field!
                        panic!("TODO gracefully handle record with invalid field.var");
                    }
                };

                layouts.push(field_layout);
            }

            Ok(Layout::Struct(layouts.into_bump_slice()))
        }
        TagUnion(tags, ext_var) => {
            debug_assert!(ext_var_is_empty_tag_union(subs, ext_var));

            Ok(layout_from_tag_union(arena, tags, subs, pointer_size))
        }
        RecursiveTagUnion(_, _, _) => {
            panic!("TODO make Layout for non-empty Tag Union");
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

pub fn record_fields_btree<'a>(
    arena: &'a Bump,
    var: Variable,
    subs: &Subs,
    pointer_size: u32,
) -> BTreeMap<Lowercase, Layout<'a>> {
    let mut fields_map = MutMap::default();
    match roc_types::pretty_print::chase_ext_record(subs, var, &mut fields_map) {
        Ok(()) | Err((_, Content::FlexVar(_))) => {
            // collect into btreemap to sort
            fields_map
                .into_iter()
                .map(|(label, var)| {
                    (
                        label,
                        Layout::from_var(arena, var, subs, pointer_size)
                            .expect("invalid layout from var"),
                    )
                })
                .collect::<BTreeMap<Lowercase, Layout<'a>>>()
        }
        Err(other) => panic!("invalid content in record variable: {:?}", other),
    }
}

#[derive(Debug)]
pub enum UnionVariant<'a> {
    Never,
    Unit,
    BoolUnion { ttrue: TagName, ffalse: TagName },
    ByteUnion(Vec<'a, TagName>),
    Unwrapped(Vec<'a, Layout<'a>>),
    Wrapped(Vec<'a, (TagName, &'a [Layout<'a>])>),
}

pub fn union_sorted_tags<'a>(
    arena: &'a Bump,
    var: Variable,
    subs: &Subs,
    pointer_size: u32,
) -> UnionVariant<'a> {
    let mut tags_vec = std::vec::Vec::new();
    match roc_types::pretty_print::chase_ext_tag_union(subs, var, &mut tags_vec) {
        Ok(()) | Err((_, Content::FlexVar(_))) => {
            union_sorted_tags_help(arena, tags_vec, subs, pointer_size)
        }
        Err(other) => panic!("invalid content in tag union variable: {:?}", other),
    }
}

fn union_sorted_tags_help<'a>(
    arena: &'a Bump,
    mut tags_vec: std::vec::Vec<(TagName, std::vec::Vec<Variable>)>,
    subs: &Subs,
    pointer_size: u32,
) -> UnionVariant<'a> {
    // sort up front; make sure the ordering stays intact!
    tags_vec.sort();

    match tags_vec.len() {
        0 => {
            // trying to instantiate a type with no values
            UnionVariant::Never
        }
        1 => {
            let (tag_name, arguments) = tags_vec.remove(0);

            // just one tag in the union (but with arguments) can be a struct
            let mut layouts = Vec::with_capacity_in(tags_vec.len(), arena);

            // special-case NUM_AT_NUM: if its argument is a FlexVar, make it Int
            match tag_name {
                TagName::Private(Symbol::NUM_AT_NUM) => {
                    layouts.push(unwrap_num_tag(subs, arguments[0]).expect("invalid num layout"));
                }
                _ => {
                    for var in arguments {
                        match Layout::from_var(arena, var, subs, pointer_size) {
                            Ok(layout) => {
                                // Drop any zero-sized arguments like {}
                                if layout.stack_size(pointer_size) != 0 {
                                    layouts.push(layout);
                                }
                            }
                            Err(LayoutProblem::UnresolvedTypeVar) => {
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

            if layouts.is_empty() {
                UnionVariant::Unit
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
                arg_layouts.push(Layout::Builtin(Builtin::Int64));

                for var in arguments {
                    match Layout::from_var(arena, var, subs, pointer_size) {
                        Ok(layout) => {
                            // Drop any zero-sized arguments like {}
                            if layout.stack_size(pointer_size) != 0 {
                                has_any_arguments = true;

                                arg_layouts.push(layout);
                            }
                        }
                        Err(LayoutProblem::UnresolvedTypeVar) => {
                            // If we encounter an unbound type var (e.g. `Ok *`)
                            // then it's zero-sized; drop the argument.
                        }
                        Err(LayoutProblem::Erroneous) => {
                            // An erroneous type var will code gen to a runtime
                            // error, so we don't need to store any data for it.
                        }
                    }
                }

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
    pointer_size: u32,
) -> Layout<'a> {
    use UnionVariant::*;

    let tags_vec: std::vec::Vec<_> = tags.into_iter().collect();

    if tags_vec[0].0 != TagName::Private(Symbol::NUM_AT_NUM) {
        let variant = union_sorted_tags_help(arena, tags_vec, subs, pointer_size);

        match variant {
            Never => panic!("TODO gracefully handle trying to instantiate Never"),
            Unit => Layout::Struct(&[]),
            BoolUnion { .. } => Layout::Builtin(Builtin::Int1),
            ByteUnion(_) => Layout::Builtin(Builtin::Int8),
            Unwrapped(field_layouts) => Layout::Struct(field_layouts.into_bump_slice()),
            Wrapped(tags) => {
                let mut tag_layouts = Vec::with_capacity_in(tags.len(), arena);

                for (_, tag_layout) in tags {
                    tag_layouts.push(tag_layout);
                }
                Layout::Union(tag_layouts.into_bump_slice())
            }
        }
    } else {
        let arguments = &tags_vec[0].1;

        debug_assert_eq!(arguments.len(), 1);

        let var = arguments.iter().next().unwrap();

        unwrap_num_tag(subs, *var).expect("invalid Num argument")
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

#[cfg(debug_assertions)]
fn ext_var_is_empty_record(subs: &Subs, ext_var: Variable) -> bool {
    // the ext_var is empty
    let mut ext_fields = MutMap::default();
    match roc_types::pretty_print::chase_ext_record(subs, ext_var, &mut ext_fields) {
        Ok(()) | Err((_, Content::FlexVar(_))) => ext_fields.is_empty(),
        Err((_, content)) => panic!("invalid content in ext_var: {:?}", content),
    }
}

fn layout_from_num_content<'a>(content: Content) -> Result<Layout<'a>, LayoutProblem> {
    use roc_types::subs::Content::*;
    use roc_types::subs::FlatType::*;

    match content {
        FlexVar(_) | RigidVar(_) => {
            // If a Num makes it all the way through type checking with an unbound
            // type variable, then assume it's a 64-bit integer.
            //
            // (e.g. for (5 + 5) assume both 5s are 64-bit integers.)
            Ok(Layout::Builtin(DEFAULT_NUM_BUILTIN))
        }
        Structure(Apply(symbol, args)) => match symbol {
            Symbol::NUM_INTEGER => Ok(Layout::Builtin(Builtin::Int64)),
            Symbol::NUM_FLOATINGPOINT => Ok(Layout::Builtin(Builtin::Float64)),
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
            debug_assert!(args.is_empty());
            Ok(Layout::Builtin(Builtin::Int64))
        }
        Content::Alias(Symbol::NUM_FLOATINGPOINT, args, _) => {
            debug_assert!(args.is_empty());
            Ok(Layout::Builtin(Builtin::Float64))
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
    arena: &'a Bump,
    subs: &Subs,
    var: Variable,
    pointer_size: u32,
) -> Result<Layout<'a>, LayoutProblem> {
    match subs.get_without_compacting(var).content {
        Content::Structure(FlatType::Apply(Symbol::ATTR_ATTR, args)) => {
            debug_assert_eq!(args.len(), 2);

            let arg_var = args.get(1).unwrap();

            list_layout_from_elem(arena, subs, *arg_var, pointer_size)
        }
        Content::FlexVar(_) | Content::RigidVar(_) => {
            // If this was still a (List *) then it must have been an empty list
            Ok(Layout::Builtin(Builtin::EmptyList))
        }
        content => {
            let elem_layout = Layout::new(arena, content, subs, pointer_size)?;

            // This is a normal list.
            Ok(Layout::Builtin(Builtin::List(arena.alloc(elem_layout))))
        }
    }
}
