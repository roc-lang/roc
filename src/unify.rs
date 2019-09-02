use subs::Content::{self, *};
use subs::{Descriptor, FlatType, Subs, Variable};
use types::Problem;

#[inline(always)]
pub fn unify_vars(subs: &mut Subs, left_key: Variable, right_key: Variable) -> Descriptor {
    let right = subs.get(right_key);

    unify_var_val(subs, left_key, &right)
}

#[inline(always)]
pub fn unify_var_val(subs: &mut Subs, left_key: Variable, right: &Descriptor) -> Descriptor {
    let left = subs.get(left_key);

    unify(subs, &left, right)
}

pub fn unify(subs: &mut Subs, left: &Descriptor, right: &Descriptor) -> Descriptor {
    let answer = match left.content {
        FlexVar(ref opt_name) => unify_flex(opt_name, &right.content),
        RigidVar(ref name) => unify_rigid(name, &right.content),
        Structure(ref flat_type) => unify_structure(subs, flat_type, &right.content),
        Error(ref problem) => {
            // Error propagates. Whatever we're comparing it to doesn't matter!
            from_content(Error(problem.clone()))
        }
    };

    // println!("\nUnifying:\n\n\t{:?}\n\n\t{:?}\n\n\t-----\n\n\t{:?}\n\n", left.content, right.content, answer.content);

    answer
}

#[inline(always)]
fn unify_structure(subs: &mut Subs, flat_type: &FlatType, other: &Content) -> Descriptor {
    match other {
        FlexVar(_) => {
            // If the other is flex, Structure wins!
            from_content(Structure(flat_type.clone()))
        }
        RigidVar(_) => {
            // Type mismatch! Rigid can only unify with flex.
            from_content(Error(Problem::GenericMismatch))
        }
        Structure(ref other_flat_type) => {
            // Type mismatch! Rigid can only unify with flex.
            unify_flat_type(subs, flat_type, other_flat_type)
        }
        Error(problem) => {
            // Error propagates.
            from_content(Error(problem.clone()))
        }
    }
}

#[inline(always)]
fn unify_flat_type(subs: &mut Subs, left: &FlatType, right: &FlatType) -> Descriptor {
    use subs::FlatType::*;

    match (left, right) {
        (EmptyRecord, EmptyRecord) => from_content(Structure(left.clone())),
        (Apply(l_module_name, l_type_name, l_args), Apply(r_module_name, r_type_name, r_args))
            if l_module_name == r_module_name && l_type_name == r_type_name =>
        {
            let args = unify_args(subs, l_args.iter(), r_args.iter());
            let flat_type = Apply(l_module_name.clone(), l_type_name.clone(), args);

            from_content(Structure(flat_type))
        }
        (Func(l_args, l_ret), Func(r_args, r_ret)) => {
            if l_args.len() == r_args.len() {
                let args = unify_args(subs, l_args.iter(), r_args.iter());
                let ret = union_vars(subs, l_ret.clone(), r_ret.clone());
                let flat_type = Func(args, ret);

                from_content(Structure(flat_type))
            } else if l_args.len() > r_args.len() {
                from_content(Error(Problem::ExtraArguments))
            } else {
                from_content(Error(Problem::MissingArguments))
            }
        }
        (Operator(l_l_arg, l_r_arg, l_ret), Operator(r_l_arg, r_r_arg, r_ret)) => {
            let l_arg = union_vars(subs, l_l_arg.clone(), r_l_arg.clone());
            let r_arg = union_vars(subs, l_r_arg.clone(), r_r_arg.clone());
            let ret = union_vars(subs, l_ret.clone(), r_ret.clone());
            let flat_type = Operator(l_arg, r_arg, ret);

            from_content(Structure(flat_type))
        }
        _ => from_content(Error(Problem::GenericMismatch)),
    }
}

fn unify_args<'a, I>(subs: &mut Subs, left_iter: I, right_iter: I) -> Vec<Variable>
where
    I: Iterator<Item = &'a Variable>,
{
    left_iter
        .zip(right_iter)
        .map(|(l_var, r_var)| {
            // Look up the descriptors we have for these variables, and unify them.
            let descriptor = unify_vars(subs, l_var.clone(), r_var.clone());

            // set r_var to be the unioned value, then union l_var to r_var
            subs.set(r_var.clone(), descriptor);
            subs.union(l_var.clone(), r_var.clone());

            r_var.clone()
        })
        .collect()
}

fn union_vars(subs: &mut Subs, l_var: Variable, r_var: Variable) -> Variable {
    // Look up the descriptors we have for these variables, and unify them.
    let descriptor = unify_vars(subs, l_var.clone(), r_var.clone());

    // set r_var to be the unioned value, then union l_var to r_var
    subs.set(r_var.clone(), descriptor);
    subs.union(l_var.clone(), r_var.clone());

    r_var.clone()
}

#[inline(always)]
fn unify_rigid(name: &String, other: &Content) -> Descriptor {
    match other {
        FlexVar(_) => {
            // If the other is flex, rigid wins!
            from_content(RigidVar(name.clone()))
        }
        RigidVar(_) | Structure(_) => {
            // Type mismatch! Rigid can only unify with flex, even if the
            // rigid names are the same.
            from_content(Error(Problem::GenericMismatch))
        }
        Error(problem) => {
            // Error propagates.
            from_content(Error(problem.clone()))
        }
    }
}

#[inline(always)]
fn unify_flex(opt_name: &Option<String>, other: &Content) -> Descriptor {
    match other {
        FlexVar(None) => {
            // If both are flex, and only left has a name, keep the name around.
            from_content(FlexVar(opt_name.clone()))
        }
        FlexVar(Some(_)) | RigidVar(_) | Structure(_) | Error(_) => {
            // In all other cases, if left is flex, defer to right.
            // (This includes using right's name if both are flex and named.)
            from_content(other.clone())
        }
    }
}

/// TODO this was f/k/a merge() - got rid of the rank stuff...good idea? Bad?
/// TODO it used to be { rank: std::cmp::min(left_rank, right_rank), ... }
fn from_content(content: Content) -> Descriptor {
    Descriptor {
        content,
        rank: 0,
        mark: 2, // no mark
        copy: None,
    }
}
