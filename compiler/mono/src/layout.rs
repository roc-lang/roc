use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::MutMap;
use roc_module::ident::{Lowercase, TagName};
use roc_module::symbol::Symbol;
use roc_types::subs::{Content, FlatType, Subs, Variable};

/// Types for code gen must be monomorphic. No type variables allowed!
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Layout<'a> {
    Builtin(Builtin<'a>),
    Struct(&'a [(Lowercase, Layout<'a>)]),
    Union(&'a MutMap<TagName, &'a [Layout<'a>]>),
    /// A function. The types of its arguments, then the type of its return value.
    FunctionPointer(&'a [Layout<'a>], &'a Layout<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Builtin<'a> {
    Int64,
    Float64,
    Bool,
    Byte,
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
    /// Returns Err(()) if given an error, or Ok(Layout) if given a non-erroneous Structure.
    /// Panics if given a FlexVar or RigidVar, since those should have been
    /// monomorphized away already!
    pub fn from_var(
        arena: &'a Bump,
        var: Variable,
        subs: &Subs,
        pointer_size: u32,
    ) -> Result<Self, ()> {
        let content = subs.get_without_compacting(var).content;

        Self::from_content(arena, content, subs, pointer_size)
    }

    pub fn from_content(
        arena: &'a Bump,
        content: Content,
        subs: &Subs,
        pointer_size: u32,
    ) -> Result<Self, ()> {
        use roc_types::subs::Content::*;

        match content {
            var @ FlexVar(_) | var @ RigidVar(_) => {
                panic!("Layout::from_content encountered an unresolved {:?}", var);
            }
            Structure(flat_type) => layout_from_flat_type(arena, flat_type, subs, pointer_size),

            Alias(Symbol::INT_INT, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Int64))
            }
            Alias(Symbol::FLOAT_FLOAT, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Float64))
            }
            Alias(_, _, var) => Self::from_content(
                arena,
                subs.get_without_compacting(var).content,
                subs,
                pointer_size,
            ),
            Error => Err(()),
        }
    }

    pub fn safe_to_memcpy(&self) -> bool {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.safe_to_memcpy(),
            Struct(fields) => fields
                .iter()
                .all(|(_, field_layout)| field_layout.safe_to_memcpy()),
            Union(tags) => tags
                .iter()
                .all(|(_, tag_layout)| tag_layout.iter().all(|field| field.safe_to_memcpy())),
            FunctionPointer(_, _) => {
                // Function pointers are immutable and can always be safely copied
                true
            }
        }
    }

    pub fn stack_size(&self, pointer_size: u32) -> u32 {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.stack_size(pointer_size),
            Struct(fields) => {
                let mut sum = 0;

                for (_, field_layout) in *fields {
                    sum += field_layout.stack_size(pointer_size);
                }

                sum
            }
            Union(fields) => {
                // the tag gets converted to a u8, so 1 byte.
                // But for one-tag unions, we don't store the tag, so 0 bytes
                let discriminant_size: u32 = if fields.len() > 1 { pointer_size } else { 0 };

                let max_tag_size: u32 = fields
                    .values()
                    .map(|tag_layout| {
                        tag_layout
                            .iter()
                            .map(|field| field.stack_size(pointer_size))
                            .sum()
                    })
                    .max()
                    .unwrap_or_default();

                discriminant_size + max_tag_size
            }
            FunctionPointer(_, _) => pointer_size,
        }
    }
}

impl<'a> Builtin<'a> {
    const I64_SIZE: u32 = std::mem::size_of::<i64>() as u32;
    const F64_SIZE: u32 = std::mem::size_of::<f64>() as u32;
    const BOOL_SIZE: u32 = std::mem::size_of::<bool>() as u32;
    const BYTE_SIZE: u32 = std::mem::size_of::<u8>() as u32;

    /// Number of machine words in an empty one of these
    pub const STR_WORDS: u32 = 2;
    pub const MAP_WORDS: u32 = 6;
    pub const SET_WORDS: u32 = Builtin::MAP_WORDS; // Set is an alias for Map with {} for value
    pub const LIST_WORDS: u32 = 2;

    /// Layout of collection wrapper - a struct of (pointer, length, capacity)
    pub const WRAPPER_PTR: u32 = 0;
    pub const WRAPPER_LEN: u32 = 1;
    pub const WRAPPER_CAPACITY: u32 = 2;

    pub fn stack_size(&self, pointer_size: u32) -> u32 {
        use Builtin::*;

        match self {
            Int64 => Builtin::I64_SIZE,
            Float64 => Builtin::F64_SIZE,
            Bool => Builtin::BOOL_SIZE,
            Byte => Builtin::BYTE_SIZE,
            Str | EmptyStr => Builtin::STR_WORDS * pointer_size,
            Map(_, _) | EmptyMap => Builtin::MAP_WORDS * pointer_size,
            Set(_) | EmptySet => Builtin::SET_WORDS * pointer_size,
            List(_) | EmptyList => Builtin::LIST_WORDS * pointer_size,
        }
    }

    pub fn safe_to_memcpy(&self) -> bool {
        use Builtin::*;

        match self {
            Int64 | Float64 | Bool | Byte | EmptyStr | EmptyMap | EmptyList | EmptySet => true,
            Str | Map(_, _) | Set(_) | List(_) => false,
        }
    }
}

#[allow(clippy::cognitive_complexity)]
fn layout_from_flat_type<'a>(
    arena: &'a Bump,
    flat_type: FlatType,
    subs: &Subs,
    pointer_size: u32,
) -> Result<Layout<'a>, ()> {
    use roc_types::subs::FlatType::*;

    match flat_type {
        Apply(symbol, args) => {
            match symbol {
                Symbol::INT_INT => {
                    debug_assert!(args.is_empty());
                    Ok(Layout::Builtin(Builtin::Int64))
                }
                Symbol::FLOAT_FLOAT => {
                    debug_assert!(args.is_empty());
                    Ok(Layout::Builtin(Builtin::Float64))
                }
                Symbol::NUM_NUM => {
                    // Num.Num should only ever have 1 argument, e.g. Num.Num Int.Integer
                    debug_assert!(args.len() == 1);

                    let var = args.iter().next().unwrap();
                    let content = subs.get_without_compacting(*var).content;

                    layout_from_num_content(content)
                }
                Symbol::STR_STR => Ok(Layout::Builtin(Builtin::Str)),
                Symbol::LIST_LIST => {
                    use roc_types::subs::Content::*;

                    match subs.get_without_compacting(args[0]).content {
                        FlexVar(_) | RigidVar(_) => Ok(Layout::Builtin(Builtin::EmptyList)),
                        content => {
                            let elem_layout =
                                Layout::from_content(arena, content, subs, pointer_size)?;

                            Ok(Layout::Builtin(Builtin::List(arena.alloc(elem_layout))))
                        }
                    }
                }
                Symbol::ATTR_ATTR => {
                    debug_assert!(args.len() == 2);

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

                fn_args.push(Layout::from_content(
                    arena,
                    arg_content,
                    subs,
                    pointer_size,
                )?);
            }

            let ret_content = subs.get_without_compacting(ret_var).content;
            let ret = Layout::from_content(arena, ret_content, subs, pointer_size)?;

            Ok(Layout::FunctionPointer(
                fn_args.into_bump_slice(),
                arena.alloc(ret),
            ))
        }
        Record(fields, ext_var) => {
            debug_assert!(ext_var_is_empty_record(subs, ext_var));
            let ext_content = subs.get_without_compacting(ext_var).content;
            let ext_layout = match Layout::from_content(arena, ext_content, subs, pointer_size) {
                Ok(layout) => layout,
                Err(()) => {
                    // Invalid record!
                    panic!("TODO gracefully handle record with invalid ext_var");
                }
            };

            let mut field_layouts: Vec<'a, (Lowercase, Layout<'a>)>;

            match ext_layout {
                Layout::Struct(more_fields) => {
                    field_layouts = Vec::with_capacity_in(fields.len() + more_fields.len(), arena);

                    for (label, field) in more_fields {
                        field_layouts.push((label.clone(), field.clone()));
                    }
                }
                _ => {
                    panic!(
                        "TODO handle Layout for invalid record extension, specifically {:?}",
                        ext_layout
                    );
                }
            }

            for (label, field_var) in fields {
                let field_content = subs.get_without_compacting(field_var).content;
                let field_layout =
                    match Layout::from_content(arena, field_content, subs, pointer_size) {
                        Ok(layout) => layout,
                        Err(()) => {
                            // Invalid field!
                            panic!("TODO gracefully handle record with invalid field.var");
                        }
                    };

                field_layouts.push((label.clone(), field_layout));
            }

            // Sort fields by label
            field_layouts.sort_by(|(a, _), (b, _)| a.cmp(b));

            Ok(Layout::Struct(field_layouts.into_bump_slice()))
        }
        TagUnion(tags, ext_var) => {
            debug_assert!(ext_var_is_empty_tag_union(subs, ext_var));

            match tags.len() {
                0 => {
                    panic!("TODO gracefully handle trying to instantiate Never");
                }
                // We can only unwrap a wrapper if it never becomes part of a bigger union
                // therefore, the ext_var must be the literal empty tag union
                1 => {
                    // This is a wrapper. Unwrap it!
                    let (tag_name, arguments) = tags.into_iter().next().unwrap();

                    match &tag_name {
                        TagName::Private(Symbol::NUM_AT_NUM) => {
                            debug_assert!(arguments.len() == 1);

                            let var = arguments.into_iter().next().unwrap();

                            unwrap_num_tag(subs, var)
                        }
                        TagName::Private(_) | TagName::Global(_) => {
                            let mut layouts = MutMap::default();
                            let mut arg_layouts = Vec::with_capacity_in(arguments.len(), arena);

                            for arg in arguments {
                                arg_layouts.push(Layout::from_var(arena, arg, subs, pointer_size)?);
                            }

                            layouts.insert(tag_name.clone(), arg_layouts.into_bump_slice());

                            Ok(Layout::Union(arena.alloc(layouts)))
                        }
                    }
                }
                _ => {
                    // Check if we can turn this tag union into an enum
                    // The arguments of all tags must have size 0.
                    // That is trivially the case when there are no arguments
                    //
                    //  [ Orange, Apple, Banana ]
                    //
                    //  But when one-tag tag unions are optimized away, we can also use an enum for
                    //
                    //  [ Foo [ Unit ], Bar [ Unit ] ]

                    let arguments_have_size_0 = || {
                        tags.iter().all(|(_, args)| {
                            args.iter().all(|var| {
                                Layout::from_var(arena, *var, subs, pointer_size)
                                    .map(|v| v.stack_size(pointer_size))
                                    == Ok(0)
                            })
                        })
                    };

                    // up to 256 enum keys can be stored in a byte
                    if tags.len() <= std::u8::MAX as usize + 1 && arguments_have_size_0() {
                        if tags.len() <= 2 {
                            Ok(Layout::Builtin(Builtin::Bool))
                        } else {
                            // up to 256 enum tags can be stored in a byte
                            Ok(Layout::Builtin(Builtin::Byte))
                        }
                    } else {
                        let mut layouts = MutMap::default();
                        for (tag_name, arguments) in tags {
                            let mut arg_layouts = Vec::with_capacity_in(arguments.len(), arena);

                            for arg in arguments {
                                arg_layouts.push(Layout::from_var(arena, arg, subs, pointer_size)?);
                            }

                            layouts.insert(tag_name, arg_layouts.into_bump_slice());
                        }

                        Ok(Layout::Union(arena.alloc(layouts)))
                    }
                }
            }
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
        Erroneous(_) => Err(()),
        EmptyRecord => Ok(Layout::Struct(&[])),
    }
}

fn ext_var_is_empty_tag_union(subs: &Subs, ext_var: Variable) -> bool {
    // the ext_var is empty
    let mut ext_fields = std::vec::Vec::new();
    match roc_types::pretty_print::chase_ext_tag_union(subs, ext_var, &mut ext_fields) {
        Ok(()) | Err((_, Content::FlexVar(_))) => ext_fields.is_empty(),
        Err(content) => panic!("invalid content in ext_var: {:?}", content),
    }
}

fn ext_var_is_empty_record(subs: &Subs, ext_var: Variable) -> bool {
    // the ext_var is empty
    let mut ext_fields = MutMap::default();
    match roc_types::pretty_print::chase_ext_record(subs, ext_var, &mut ext_fields) {
        Ok(()) | Err((_, Content::FlexVar(_))) => ext_fields.is_empty(),
        Err((_, content)) => panic!("invalid content in ext_var: {:?}", content),
    }
}

fn layout_from_num_content<'a>(content: Content) -> Result<Layout<'a>, ()> {
    use roc_types::subs::Content::*;
    use roc_types::subs::FlatType::*;

    match content {
        var @ FlexVar(_) | var @ RigidVar(_) => {
            panic!("Layout::from_content encountered an unresolved {:?}", var);
        }
        Structure(Apply(symbol, args)) => match symbol {
            Symbol::INT_INTEGER => Ok(Layout::Builtin(Builtin::Int64)),
            Symbol::FLOAT_FLOATINGPOINT => Ok(Layout::Builtin(Builtin::Float64)),
            _ => {
                panic!(
                    "Invalid Num.Num type application: {:?}",
                    Apply(symbol, args)
                );
            }
        },
        Structure(_) => {
            panic!("Invalid Num.Num type application: {:?}", content);
        }
        Alias(_, _, _) => {
            panic!("TODO recursively resolve type aliases in num_from_content");
        }
        Error => Err(()),
    }
}

fn unwrap_num_tag<'a>(subs: &Subs, var: Variable) -> Result<Layout<'a>, ()> {
    match subs.get_without_compacting(var).content {
        Content::Structure(flat_type) => match flat_type {
            FlatType::Apply(Symbol::ATTR_ATTR, args) => {
                debug_assert!(args.len() == 2);

                let arg_var = args.get(1).unwrap();

                unwrap_num_tag(subs, *arg_var)
            }
            _ => {
                panic!("TODO handle Num.@Num flat_type {:?}", flat_type);
            }
        },
        Content::Alias(Symbol::INT_INTEGER, args, _) => {
            debug_assert!(args.is_empty());
            Ok(Layout::Builtin(Builtin::Int64))
        }
        Content::Alias(Symbol::FLOAT_FLOATINGPOINT, args, _) => {
            debug_assert!(args.is_empty());
            Ok(Layout::Builtin(Builtin::Float64))
        }
        Content::FlexVar(_) => {
            // If this was still a (Num *) then default to compiling it to i64
            Ok(Layout::Builtin(Builtin::Int64))
        }
        other => {
            panic!("TODO non structure Num.@Num flat_type {:?}", other);
        }
    }
}
