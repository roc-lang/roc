use crate::subs::{Content, FlatType, Subs};
use crate::types;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use inlinable_string::InlinableString;

/// Types for code gen must be monomorphic. No type variables allowed!
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Layout<'a> {
    Builtin(Builtin<'a>),
    Struct(&'a [(InlinableString, Layout<'a>)]),
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
            Alias(_, _, _, _) => {
                panic!("TODO recursively resolve type aliases in Layout::from_content");
            }
            Error => Err(()),
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
        Apply {
            module_name,
            name,
            args,
        } => {
            // TODO use SIMD string comparisons to speed these up
            if module_name.as_str() == types::MOD_NUM && name.as_str() == types::TYPE_NUM {
                // Num.Num should only ever have 1 argument, e.g. Num.Num Int.Integer
                debug_assert!(args.len() == 1);

                let var = args.iter().next().unwrap();
                let content = subs.get_without_compacting(*var).content;

                layout_from_num_content(content)
            } else {
                panic!(
                    "TODO layout_from_flat_type for {:?}",
                    Apply {
                        module_name,
                        name,
                        args,
                    }
                );
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
        Record(_, _) => {
            panic!("TODO make Layout for non-empty Record");
        }
        TagUnion(_, _) => {
            panic!("TODO make Layout for non-empty Tag Union");
        }
        EmptyTagUnion => {
            panic!("TODO make Layout for empty Tag Union");
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
        Structure(Apply {
            module_name,
            name,
            args,
        }) => {
            // TODO use SIMD string comparisons to speed these up
            if module_name.as_str() == types::MOD_INT && name.as_str() == types::TYPE_INTEGER {
                Ok(Layout::Builtin(Builtin::Int64))
            } else if module_name.as_str() == types::MOD_FLOAT
                && name.as_str() == types::TYPE_FLOATINGPOINT
            {
                Ok(Layout::Builtin(Builtin::Float64))
            } else {
                panic!(
                    "Invalid Num.Num type application: {:?}",
                    Apply {
                        module_name,
                        name,
                        args
                    }
                );
            }
        }
        Structure(_) => {
            panic!("Invalid Num.Num type application: {:?}", content);
        }
        Alias(_, _, _, _) => {
            panic!("TODO recursively resolve type aliases in num_from_content");
        }
        Error => Err(()),
    }
}
