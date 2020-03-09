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
    Pointer(&'a Layout<'a>),
    /// A function. The types of its arguments, then the type of its return value.
    FunctionPointer(&'a [Layout<'a>], &'a Layout<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Builtin<'a> {
    Int64,
    Float64,
    Bool(TagName, TagName),
    Byte(MutMap<TagName, u8>),
    Str,
    Map(&'a Layout<'a>, &'a Layout<'a>),
    Set(&'a Layout<'a>),
    List(&'a Layout<'a>),
}

impl<'a> Layout<'a> {
    /// Returns Err(()) if given an error, or Ok(Layout) if given a non-erroneous Structure.
    /// Panics if given a FlexVar or RigidVar, since those should have been
    /// monomorphized away already!
    pub fn from_var(arena: &'a Bump, var: Variable, subs: &Subs) -> Result<Self, ()> {
        let content = subs.get_without_compacting(var).content;

        Self::from_content(arena, content, subs)
    }

    pub fn from_content(arena: &'a Bump, content: Content, subs: &Subs) -> Result<Self, ()> {
        use roc_types::subs::Content::*;

        match content {
            var @ FlexVar(_) | var @ RigidVar(_) => {
                panic!("Layout::from_content encountered an unresolved {:?}", var);
            }
            Structure(flat_type) => layout_from_flat_type(arena, flat_type, subs),

            Alias(Symbol::INT_INT, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Int64))
            }
            Alias(Symbol::FLOAT_FLOAT, args, _) => {
                debug_assert!(args.is_empty());
                Ok(Layout::Builtin(Builtin::Float64))
            }
            Alias(_, _, var) => {
                Self::from_content(arena, subs.get_without_compacting(var).content, subs)
            }
            Error => Err(()),
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
            Pointer(_) | FunctionPointer(_, _) => pointer_size,
        }
    }
}

impl<'a> Builtin<'a> {
    const I64_SIZE: u32 = std::mem::size_of::<i64>() as u32;
    const F64_SIZE: u32 = std::mem::size_of::<f64>() as u32;
    const BOOL_SIZE: u32 = std::mem::size_of::<bool>() as u32;
    const BYTE_SIZE: u32 = std::mem::size_of::<u8>() as u32;

    /// Number of machine words in an empty one of these
    const STR_WORDS: u32 = 3;
    const MAP_WORDS: u32 = 6;
    const SET_WORDS: u32 = Builtin::MAP_WORDS; // Set is an alias for Map with {} for value
    const LIST_WORDS: u32 = 3;

    pub fn stack_size(&self, pointer_size: u32) -> u32 {
        use Builtin::*;

        match self {
            Int64 => Builtin::I64_SIZE,
            Float64 => Builtin::F64_SIZE,
            Bool(_, _) => Builtin::BOOL_SIZE,
            Byte(_) => Builtin::BYTE_SIZE,
            Str => Builtin::STR_WORDS * pointer_size,
            Map(_, _) => Builtin::MAP_WORDS * pointer_size,
            Set(_) => Builtin::SET_WORDS * pointer_size,
            List(_) => Builtin::LIST_WORDS * pointer_size,
        }
    }
}

fn layout_from_flat_type<'a>(
    arena: &'a Bump,
    flat_type: FlatType,
    subs: &Subs,
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
                    let elem_layout = Layout::from_var(arena, args[0], subs)?;

                    Ok(Layout::Builtin(Builtin::List(arena.alloc(elem_layout))))
                }
                Symbol::ATTR_ATTR => {
                    debug_assert!(args.len() == 2);

                    // The first argument is the uniqueness info;
                    // that doesn't affect layout, so we don't need it here.
                    let wrapped_var = args[1];

                    // For now, layout is unaffected by uniqueness.
                    // (Incorporating refcounting may change this.)
                    // Unwrap and continue
                    Layout::from_var(arena, wrapped_var, subs)
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

                fn_args.push(Layout::from_content(arena, arg_content, subs)?);
            }

            let ret_content = subs.get_without_compacting(ret_var).content;
            let ret = Layout::from_content(arena, ret_content, subs)?;

            Ok(Layout::FunctionPointer(
                fn_args.into_bump_slice(),
                arena.alloc(ret),
            ))
        }
        Record(mut fields, ext_var) => {
            flatten_record(&mut fields, ext_var, subs);
            let ext_content = subs.get_without_compacting(ext_var).content;
            let ext_layout = match Layout::from_content(arena, ext_content, subs) {
                Ok(layout) => layout,
                Err(()) => {
                    // Invalid record!
                    panic!("TODO gracefully handle record with invalid ext_var");
                }
            };

            let mut field_layouts;

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
                let field_layout = match Layout::from_content(arena, field_content, subs) {
                    Ok(layout) => layout,
                    Err(()) => {
                        // Invalid field!
                        panic!("TODO gracefully handle record with invalid field.var");
                    }
                };

                field_layouts.push((label.clone(), field_layout));
            }

            Ok(Layout::Struct(field_layouts.into_bump_slice()))
        }
        TagUnion(mut tags, ext_var) => {
            // Recursively inject the contents of ext_var into tags
            // until we have all the tags in one map.
            flatten_union(&mut tags, ext_var, subs);

            match tags.len() {
                0 => {
                    panic!("TODO gracefully handle trying to instantiate Never");
                }
                1 => {
                    // This is a wrapper. Unwrap it!
                    let (tag, args) = tags.into_iter().next().unwrap();

                    match tag {
                        TagName::Private(Symbol::NUM_AT_NUM) => {
                            debug_assert!(args.len() == 1);

                            let var = args.into_iter().next().unwrap();

                            unwrap_num_tag(subs, var)
                        }
                        TagName::Private(symbol) => {
                            panic!("TODO emit wrapped private tag for {:?} {:?}", symbol, args);
                        }
                        TagName::Global(ident) => {
                            panic!("TODO emit wrapped global tag for {:?} {:?}", ident, args);
                        }
                    }
                }
                _ => {
                    // Check if we can turn this tag union into an enum
                    // TODO rather than the arguments being empty, check whether their layout has size 0.
                    if tags.len() <= 256 && tags.iter().all(|(_, args)| args.is_empty()) {
                        if tags.len() <= 2 {
                            // Up to 2 enum tags can be stored (in theory) in one bit
                            let mut it = tags.keys();
                            let a: TagName = it.next().unwrap().clone();
                            let b: TagName = it.next().unwrap().clone();

                            if a < b {
                                Ok(Layout::Builtin(Builtin::Bool(a, b)))
                            } else {
                                Ok(Layout::Builtin(Builtin::Bool(b, a)))
                            }
                        } else {
                            // up to 256 enum tags can be stored in a byte
                            let mut counter = 0u8;
                            let mut tag_to_u8 = MutMap::default();

                            for (name, _) in tags {
                                tag_to_u8.insert(name, counter);
                                counter += 1;
                            }
                            Ok(Layout::Builtin(Builtin::Byte(tag_to_u8)))
                        }
                    } else {
                        panic!("TODO handle a tag union with mutliple tags: {:?}", tags);
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

/// Recursively inline the contents ext_var into this union until we have
/// a flat union containing all the tags.
fn flatten_union(
    tags: &mut MutMap<TagName, std::vec::Vec<Variable>>,
    ext_var: Variable,
    subs: &Subs,
) {
    use roc_types::subs::Content::*;
    use roc_types::subs::FlatType::*;

    match subs.get_without_compacting(ext_var).content {
        Structure(EmptyTagUnion) => (),
        Structure(TagUnion(new_tags, new_ext_var))
        | Structure(RecursiveTagUnion(_, new_tags, new_ext_var)) => {
            for (tag_name, vars) in new_tags {
                tags.insert(tag_name, vars);
            }

            flatten_union(tags, new_ext_var, subs)
        }
        Alias(_, _, actual) => flatten_union(tags, actual, subs),
        invalid => {
            panic!("Compiler error: flatten_union got an ext_var in a tag union that wasn't itself a tag union; instead, it was: {:?}", invalid);
        }
    };
}

/// Recursively inline the contents ext_var into this record until we have
/// a flat record containing all the fields.
fn flatten_record(fields: &mut MutMap<Lowercase, Variable>, ext_var: Variable, subs: &Subs) {
    use roc_types::subs::Content::*;
    use roc_types::subs::FlatType::*;

    match subs.get_without_compacting(ext_var).content {
        Structure(EmptyRecord) => (),
        Structure(Record(new_tags, new_ext_var)) => {
            for (label, var) in new_tags {
                fields.insert(label, var);
            }

            flatten_record(fields, new_ext_var, subs)
        }
        Alias(_, _, actual) => flatten_record(fields, actual, subs),
        invalid => {
            panic!("Compiler error: flatten_record encountered an ext_var in a record that wasn't itself a record; instead, it was: {:?}", invalid);
        }
    };
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
        other => {
            panic!("TODO non structure Num.@Num flat_type {:?}", other);
        }
    }
}
