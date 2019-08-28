use subs::{Descriptor, FlatType};
use subs::Content::{self, *};

pub fn unify(left: &Descriptor, right: &Descriptor) -> Descriptor {
    let answer = match left.content {
        FlexVar(ref opt_name) => {
            unify_flex(opt_name, &right.content)
        },
        RigidVar(ref name) => {
            unify_rigid(name, &right.content)
        },
        Structure(ref flat_type) => {
            unify_structure(flat_type, &right.content)
        }
        Error => {
            // Error propagates. Whatever we're comparing it to doesn't matter!
            from_content(Error)
        }
    };

    println!("\nUnifying:\n\n\t{:?}\n\n\t{:?}\n\n\t-----\n\n\t{:?}\n\n", left.content, right.content, answer.content);

    answer
}

#[inline(always)]
fn unify_structure(flat_type: &FlatType, other: &Content) -> Descriptor {
    match other {
        FlexVar(_) => {
            // If the other is flex, Structure wins!
            from_content(Structure(flat_type.clone()))
        },
        RigidVar(_) => {
            // Type mismatch! Rigid can only unify with flex.
            from_content(Error)
        },
        Structure(ref other_flat_type) => {
            // Type mismatch! Rigid can only unify with flex.
            unify_flat_type(flat_type, other_flat_type)
        },
        Error => {
            // Error propagates.
            from_content(Error)
        },
    }
}

#[inline(always)]
fn unify_flat_type(left: &FlatType, right: &FlatType) -> Descriptor {
    use subs::FlatType::*;

    match (left, right) {
        (EmptyRecord, EmptyRecord) => from_content(Structure(left.clone())),
        (
            Apply(l_module_name, l_type_name, l_args),
            Apply(r_module_name, r_type_name, r_args)
        ) if l_module_name == r_module_name && l_type_name == r_type_name => {
            panic!("TODO fix this by forking ena");
            // let args = unify_args(l_args.iter(), r_args.iter());
            // let flat_type = Apply(l_module_name.clone(), l_type_name.clone(), args);

            // from_content(Structure(flat_type))
        },
        (Func(_, _), Func(_, _)) => panic!("TODO unify_flat_type for Func"),
        _ => from_content(Error)
    }
}

fn unify_args<'a, I>(left_iter: I, right_iter: I) -> Vec<Content>
where I: Iterator<Item = &'a Content> 
{
    left_iter.zip(right_iter).map(|(l_content, r_content)| {
        let l_descriptor = from_content(l_content.clone());
        let r_descriptor = from_content(r_content.clone());
        let descriptor = unify(&l_descriptor, &r_descriptor);

        descriptor.content
    }).collect()
}


#[inline(always)]
fn unify_rigid(name: &String, other: &Content) -> Descriptor {
    match other {
        FlexVar(_) => {
            // If the other is flex, rigid wins!
            from_content(RigidVar(name.clone()))
        },
        RigidVar(_) | Structure(_) => {
            // Type mismatch! Rigid can only unify with flex, even if the
            // rigid names are the same.
            from_content(Error)
        },
        Error => {
            // Error propagates.
            from_content(Error)
        },
    }
}


#[inline(always)]
fn unify_flex(opt_name: &Option<String>, other: &Content) -> Descriptor {
    match other {
        FlexVar(None) => {
            // If both are flex, and only left has a name, keep the name around.
            from_content(FlexVar(opt_name.clone()))
        },
        FlexVar(Some(_)) | RigidVar(_) | Structure(_) | Error => {
            // In all other cases, if left is flex, defer to right.
            // (This includes using right's name if both are flex and named.)
            from_content(other.clone())
        },
    }
}

/// TODO this was f/k/a merge() - got rid of the rank stuff...good idea? Bad?
/// TODO it used to be { rank: std::cmp::min(left_rank, right_rank), ... }
fn from_content(content: Content) -> Descriptor {
    Descriptor {
        content,
        rank: 0,
        mark: 2, // no mark
        copy: None
    }
}

