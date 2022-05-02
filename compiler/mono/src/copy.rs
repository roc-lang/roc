use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_types::subs::{
    AliasVariables, Descriptor, OptVariable, RecordFields, Subs, SubsSlice, UnionTags, Variable,
    VariableSubsSlice,
};

pub fn deep_copy_type_vars<'a>(
    arena: &'a Bump,
    subs: &mut Subs,
    var: Variable,
) -> Vec<'a, (Variable, Variable)> {
    let mut copied = Vec::with_capacity_in(16, arena);

    let cloned_var = help(arena, subs, &mut copied, var);

    debug_assert!(match cloned_var {
        Some(_) => copied.is_empty(),
        None => !copied.is_empty(),
    });

    // we have tracked all visited variables, and can now traverse them
    // in one go (without looking at the UnificationTable) and clear the copy field
    let mut result = Vec::with_capacity_in(copied.len(), arena);
    for var in copied {
        let descriptor = subs.get_ref_mut(var);

        if let Some(copy) = descriptor.copy.into_variable() {
            result.push((var, copy));
            descriptor.copy = OptVariable::NONE;
        } else {
            debug_assert!(false, "{:?} marked as copied but it wasn't", var);
        }
    }

    return result;

    #[must_use]
    fn help(
        arena: &Bump,
        subs: &mut Subs,
        visited: &mut Vec<Variable>,
        var: Variable,
    ) -> Option<Variable> {
        use roc_types::subs::Content::*;
        use roc_types::subs::FlatType::*;

        let desc = subs.get_ref_mut(var);
        let content = desc.content;
        let rank = desc.rank;
        let mark = desc.mark;

        // Unlike `deep_copy_var` in solve, here we are cloning *all* flex and rigid vars.
        // So we only want to fast-return if we've already done the cloning work for a particular
        // var.
        if let Some(copy) = desc.copy.into_variable() {
            return Some(copy);
        }

        macro_rules! descend_slice {
            ($slice:expr, $needs_clone:ident) => {
                for var_index in $slice {
                    let var = subs[var_index];
                    $needs_clone = $needs_clone || help(arena, subs, visited, var).is_some();
                }
            };
        }

        macro_rules! descend_var {
            ($var:expr, $needs_clone:ident) => {{
                let new_var = help(arena, subs, visited, $var).unwrap_or($var);
                $needs_clone = $needs_clone || new_var != $var;
                new_var
            }};
        }

        macro_rules! clone_var_slice {
            ($slice:expr) => {{
                let new_arguments = VariableSubsSlice::reserve_into_subs(subs, $slice.len());
                for (target_index, var_index) in (new_arguments.indices()).zip($slice) {
                    let var = subs[var_index];
                    let copy_var = subs.get_ref(var).copy.into_variable().unwrap_or(var);
                    subs.variables[target_index] = copy_var;
                }
                new_arguments
            }};
        }

        // Now we recursively copy the content of the variable.
        // We have already marked the variable as copied, so we
        // will not repeat this work or crawl this variable again.
        let opt_new_content = match content {
            // The vars for which we want to do something interesting.
            FlexVar(opt_name) => Some(FlexVar(opt_name)),
            FlexAbleVar(opt_name, ability) => Some(FlexAbleVar(opt_name, ability)),
            RigidVar(name) => Some(RigidVar(name)),
            RigidAbleVar(name, ability) => Some(RigidAbleVar(name, ability)),

            // Everything else is a mechanical descent.
            Structure(flat_type) => match flat_type {
                EmptyRecord | EmptyTagUnion | Erroneous(_) => None,
                Apply(symbol, arguments) => {
                    let mut needs_clone = false;
                    descend_slice!(arguments, needs_clone);

                    if needs_clone {
                        let new_arguments = clone_var_slice!(arguments);
                        Some(Structure(Apply(symbol, new_arguments)))
                    } else {
                        None
                    }
                }
                Func(arguments, closure_var, ret_var) => {
                    let mut needs_clone = false;

                    descend_slice!(arguments, needs_clone);

                    let new_closure_var = descend_var!(closure_var, needs_clone);
                    let new_ret_var = descend_var!(ret_var, needs_clone);

                    if needs_clone {
                        let new_arguments = clone_var_slice!(arguments);
                        Some(Structure(Func(new_arguments, new_closure_var, new_ret_var)))
                    } else {
                        None
                    }
                }
                Record(fields, ext_var) => {
                    let mut needs_clone = false;

                    let new_ext_var = descend_var!(ext_var, needs_clone);

                    descend_slice!(fields.variables(), needs_clone);

                    if needs_clone {
                        let new_variables = clone_var_slice!(fields.variables());
                        let new_fields = {
                            RecordFields {
                                length: fields.length,
                                field_names_start: fields.field_names_start,
                                variables_start: new_variables.start,
                                field_types_start: fields.field_types_start,
                            }
                        };
                        Some(Structure(Record(new_fields, new_ext_var)))
                    } else {
                        None
                    }
                }
                TagUnion(tags, ext_var) => {
                    let mut needs_clone = false;

                    let new_ext_var = descend_var!(ext_var, needs_clone);

                    for variables_slice_index in tags.variables() {
                        let variables_slice = subs[variables_slice_index];
                        descend_slice!(variables_slice, needs_clone);
                    }

                    if needs_clone {
                        let new_variable_slices =
                            SubsSlice::reserve_variable_slices(subs, tags.len());
                        let it = (new_variable_slices.indices()).zip(tags.variables());
                        for (target_index, index) in it {
                            let slice = subs[index];
                            let new_variables = clone_var_slice!(slice);
                            subs.variable_slices[target_index] = new_variables;
                        }

                        let new_union_tags =
                            UnionTags::from_slices(tags.tag_names(), new_variable_slices);

                        Some(Structure(TagUnion(new_union_tags, new_ext_var)))
                    } else {
                        None
                    }
                }
                RecursiveTagUnion(rec_var, tags, ext_var) => {
                    let mut needs_clone = false;

                    let new_ext_var = descend_var!(ext_var, needs_clone);
                    let new_rec_var = descend_var!(rec_var, needs_clone);

                    for variables_slice_index in tags.variables() {
                        let variables_slice = subs[variables_slice_index];
                        descend_slice!(variables_slice, needs_clone);
                    }

                    if needs_clone {
                        let new_variable_slices =
                            SubsSlice::reserve_variable_slices(subs, tags.len());
                        let it = (new_variable_slices.indices()).zip(tags.variables());
                        for (target_index, index) in it {
                            let slice = subs[index];
                            let new_variables = clone_var_slice!(slice);
                            subs.variable_slices[target_index] = new_variables;
                        }

                        let new_union_tags =
                            UnionTags::from_slices(tags.tag_names(), new_variable_slices);

                        Some(Structure(RecursiveTagUnion(
                            new_rec_var,
                            new_union_tags,
                            new_ext_var,
                        )))
                    } else {
                        None
                    }
                }
                FunctionOrTagUnion(tag_name, symbol, ext_var) => {
                    let mut needs_clone = false;
                    let new_ext_var = descend_var!(ext_var, needs_clone);
                    if needs_clone {
                        Some(Structure(FunctionOrTagUnion(tag_name, symbol, new_ext_var)))
                    } else {
                        None
                    }
                }
            },

            RecursionVar {
                opt_name,
                structure,
            } => {
                let mut needs_clone = false;

                let new_structure = descend_var!(structure, needs_clone);

                if needs_clone {
                    Some(RecursionVar {
                        opt_name,
                        structure: new_structure,
                    })
                } else {
                    None
                }
            }

            Alias(symbol, arguments, real_type_var, kind) => {
                let mut needs_clone = false;

                let new_real_type_var = descend_var!(real_type_var, needs_clone);
                descend_slice!(arguments.all_variables(), needs_clone);

                if needs_clone {
                    let new_variables = clone_var_slice!(arguments.all_variables());
                    let new_arguments = AliasVariables {
                        variables_start: new_variables.start,
                        ..arguments
                    };

                    Some(Alias(symbol, new_arguments, new_real_type_var, kind))
                } else {
                    None
                }
            }

            RangedNumber(typ, range_vars) => {
                let mut needs_clone = false;

                let new_typ = descend_var!(typ, needs_clone);
                descend_slice!(range_vars, needs_clone);

                if needs_clone {
                    let new_range_vars = clone_var_slice!(range_vars);

                    Some(RangedNumber(new_typ, new_range_vars))
                } else {
                    None
                }
            }
            Error => None,
        };

        if let Some(new_content) = opt_new_content {
            visited.push(var);

            let copy_descriptor = Descriptor {
                content: new_content,
                rank,
                mark,
                copy: OptVariable::NONE,
            };

            let copy = subs.fresh(copy_descriptor);
            // Set the copy on the original var
            subs.get_ref_mut(var).copy = copy.into();

            // We had to create a fresh var for this type, so anything that depends on it should be
            // freshened too, and use this fresh var.
            return Some(copy);
        }

        // Doesn't need to be freshened; use the old var.
        None
    }
}
