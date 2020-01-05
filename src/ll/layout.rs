use crate::subs::{Content, FlatType, Subs};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use inlinable_string::InlinableString;

/// Types for code gen must be monomorphic. No type variables allowed!
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Layout<'a> {
    /// A unary type like {} or [ NoOtherTags ] - this does not need to be a value at runtime.
    ZeroSized,
    /// A function. The types of its arguments, then the type of its return value.
    FunctionPointer(&'a [Layout<'a>], &'a Layout<'a>),
    Struct(&'a [(InlinableString, Layout<'a>)]),

    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply {
        module_name: InlinableString,
        name: InlinableString,
        args: &'a [Layout<'a>],
    },

    Pointer(&'a Layout<'a>),
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
            let mut layout_args = Vec::with_capacity_in(args.len(), arena);

            for arg_var in args {
                let arg_content = subs.get_without_compacting(arg_var).content;

                layout_args.push(Layout::from_content(arena, arg_content, subs)?);
            }

            Ok(Layout::Apply {
                module_name: module_name.as_str().into(),
                name: name.as_str().into(),
                args: layout_args.into_bump_slice(),
            })
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
        Erroneous(_) => Err(()),
        EmptyRecord => Ok(Layout::Struct(&[])),
    }
}
