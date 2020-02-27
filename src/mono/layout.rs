use crate::can::ident::{Lowercase, TagName};
use crate::collections::MutMap;
use crate::module::symbol::Symbol;
use crate::subs::{Content, FlatType, Subs, Variable};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use cranelift_codegen::isa::TargetFrontendConfig;

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
    Str,
    Map(&'a Layout<'a>, &'a Layout<'a>),
    Set(&'a Layout<'a>),
}

impl<'a> Layout<'a> {
    pub fn from_var(arena: &'a Bump, var: Variable, subs: &Subs) -> Result<Self, ()> {
        let content = subs.get_without_compacting(var).content;

        Self::from_content(arena, content, subs)
    }
    /// Returns Err(()) if given an error, or Ok(Layout) if given a non-erroneous Structure.
    /// Panics if given a FlexVar or RigidVar, since those should have been
    /// monomorphized away already!
    pub fn from_content(arena: &'a Bump, content: Content, subs: &Subs) -> Result<Self, ()> {
        use crate::subs::Content::*;

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

    pub fn stack_size(&self, cfg: TargetFrontendConfig) -> u32 {
        use Layout::*;

        match self {
            Builtin(builtin) => builtin.stack_size(cfg),
            Struct(fields) => {
                let mut sum = 0;

                for (_, field_layout) in *fields {
                    sum += field_layout.stack_size(cfg);
                }

                sum
            }
            Pointer(_) | FunctionPointer(_, _) => pointer_size(cfg),
        }
    }
}

fn pointer_size(cfg: TargetFrontendConfig) -> u32 {
    cfg.pointer_bytes() as u32
}

impl<'a> Builtin<'a> {
    const I64_SIZE: u32 = std::mem::size_of::<i64>() as u32;
    const F64_SIZE: u32 = std::mem::size_of::<f64>() as u32;

    /// Number of machine words in an empty one of these
    const STR_WORDS: u32 = 3;
    const MAP_WORDS: u32 = 6;
    const SET_WORDS: u32 = Builtin::MAP_WORDS; // Set is an alias for Map with {} for value

    pub fn stack_size(&self, cfg: TargetFrontendConfig) -> u32 {
        use Builtin::*;

        match self {
            Int64 => Builtin::I64_SIZE,
            Float64 => Builtin::F64_SIZE,
            Str => Builtin::STR_WORDS * pointer_size(cfg),
            Map(_, _) => Builtin::MAP_WORDS * pointer_size(cfg),
            Set(_) => Builtin::SET_WORDS * pointer_size(cfg),
        }
    }
}

fn layout_from_flat_type<'a>(
    arena: &'a Bump,
    flat_type: FlatType,
    subs: &Subs,
) -> Result<Layout<'a>, ()> {
    use crate::subs::FlatType::*;

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
                        TagName::Private(Symbol::NUM_AT_NUM) if args.len() == 1 => {
                            let var = args.into_iter().next().unwrap();

                            match subs.get_without_compacting(var).content {
                                Content::Structure(flat_type) => match flat_type {
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
                                } // Symbol::INT_INT => {
                                  //     debug_assert!(args.is_empty());
                                  //     types::I64
                                  // }
                                  // Symbol::FLOAT_FLOAT => {
                                  //     debug_assert!(args.is_empty());
                                  //     types::F64
                                  // }
                                  // tag => {
                                  //     panic!("TODO gracefully handle unrecognized Num.Num variant {:?} {:?}", tag, args);
                                  // }
                            }
                        }
                        TagName::Private(Symbol::STR_AT_STR) => Ok(Layout::Builtin(Builtin::Str)),
                        TagName::Private(symbol) => {
                            panic!("TODO emit wrapped private tag for {:?} {:?}", symbol, args);
                        }
                        TagName::Global(ident) => {
                            panic!("TODO emit wrapped global tag for {:?} {:?}", ident, args);
                        }
                    }
                }
                _ => {
                    panic!("TODO handle a tag union with mutliple tags.");
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
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

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
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

    match subs.get_without_compacting(ext_var).content {
        Structure(EmptyTagUnion) => (),
        Structure(TagUnion(new_tags, new_ext_var)) => {
            for (tag_name, vars) in new_tags {
                tags.insert(tag_name, vars);
            }

            flatten_union(tags, new_ext_var, subs)
        }
        invalid => {
            panic!("Compiler error: flatten_union got an ext_var in a tag union that wasn't itself a tag union; instead, it was: {:?}", invalid);
        }
    };
}

/// Recursively inline the contents ext_var into this record until we have
/// a flat record containing all the fields.
fn flatten_record(fields: &mut MutMap<Lowercase, Variable>, ext_var: Variable, subs: &Subs) {
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

    match subs.get_without_compacting(ext_var).content {
        Structure(EmptyRecord) => (),
        Structure(Record(new_tags, new_ext_var)) => {
            for (label, var) in new_tags {
                fields.insert(label, var);
            }

            flatten_record(fields, new_ext_var, subs)
        }
        invalid => {
            panic!("Compiler error: flatten_record encountered an ext_var in a record that wasn't itself a record; instead, it was: {:?}", invalid);
        }
    };
}
