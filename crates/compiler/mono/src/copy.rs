use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_can::{
    def::Def,
    expr::{AccessorData, ClosureData, Expr, Field, WhenBranch},
};
use roc_types::{
    subs::{
        self, AliasVariables, Descriptor, OptVariable, RecordFields, Subs, SubsSlice, UnionLambdas,
        UnionTags, Variable, VariableSubsSlice,
    },
    types::Uls,
};

/// Deep copies the type variables in the type hosted by [`var`] into [`expr`].
/// Returns [`None`] if the expression does not need to be copied.
pub fn deep_copy_type_vars_into_expr<'a>(
    arena: &'a Bump,
    subs: &mut Subs,
    var: Variable,
    expr: &Expr,
) -> Option<(Variable, Expr)> {
    // Always deal with the root, so that aliases propagate correctly.
    let var = subs.get_root_key_without_compacting(var);

    let substitutions = deep_copy_type_vars(arena, subs, var);

    if substitutions.is_empty() {
        return None;
    }

    let new_var = substitutions
        .iter()
        .find_map(|&(original, new)| if original == var { Some(new) } else { None })
        .expect("Variable marked as cloned, but it isn't");

    return Some((new_var, help(subs, expr, &substitutions)));

    fn help(subs: &Subs, expr: &Expr, substitutions: &[(Variable, Variable)]) -> Expr {
        use Expr::*;

        macro_rules! sub {
            ($var:expr) => {{
                // Always deal with the root, so that aliases propagate correctly.
                let root = subs.get_root_key_without_compacting($var);
                substitutions
                    .iter()
                    .find_map(|&(original, new)| if original == root { Some(new) } else { None })
                    .unwrap_or($var)
            }};
        }

        let go_help = |e: &Expr| help(subs, e, substitutions);

        match expr {
            Num(var, str, val, bound) => Num(sub!(*var), str.clone(), *val, *bound),
            Int(v1, v2, str, val, bound) => Int(sub!(*v1), sub!(*v2), str.clone(), *val, *bound),
            Float(v1, v2, str, val, bound) => {
                Float(sub!(*v1), sub!(*v2), str.clone(), *val, *bound)
            }
            Str(str) => Str(str.clone()),
            SingleQuote(char) => SingleQuote(*char),
            List {
                elem_var,
                loc_elems,
            } => List {
                elem_var: sub!(*elem_var),
                loc_elems: loc_elems.iter().map(|le| le.map(go_help)).collect(),
            },
            Var(sym) => Var(*sym),
            &AbilityMember(sym, specialization, specialization_var) => {
                AbilityMember(sym, specialization, specialization_var)
            }
            When {
                loc_cond,
                cond_var,
                expr_var,
                region,
                branches,
                branches_cond_var,
                exhaustive,
            } => When {
                loc_cond: Box::new(loc_cond.map(go_help)),
                cond_var: sub!(*cond_var),
                expr_var: sub!(*expr_var),
                region: *region,
                branches: branches
                    .iter()
                    .map(
                        |WhenBranch {
                             patterns,
                             value,
                             guard,
                             redundant,
                         }| WhenBranch {
                            patterns: patterns.clone(),
                            value: value.map(go_help),
                            guard: guard.as_ref().map(|le| le.map(go_help)),
                            redundant: *redundant,
                        },
                    )
                    .collect(),
                branches_cond_var: sub!(*branches_cond_var),
                exhaustive: *exhaustive,
            },
            If {
                cond_var,
                branch_var,
                branches,
                final_else,
            } => If {
                cond_var: sub!(*cond_var),
                branch_var: sub!(*branch_var),
                branches: branches
                    .iter()
                    .map(|(c, e)| (c.map(go_help), e.map(go_help)))
                    .collect(),
                final_else: Box::new(final_else.map(go_help)),
            },

            LetRec(defs, body, cycle_mark) => LetRec(
                defs.iter()
                    .map(
                        |Def {
                             loc_pattern,
                             loc_expr,
                             expr_var,
                             pattern_vars,
                             annotation,
                         }| Def {
                            loc_pattern: loc_pattern.clone(),
                            loc_expr: loc_expr.map(go_help),
                            expr_var: sub!(*expr_var),
                            pattern_vars: pattern_vars
                                .iter()
                                .map(|(s, v)| (*s, sub!(*v)))
                                .collect(),
                            annotation: annotation.clone(),
                        },
                    )
                    .collect(),
                Box::new(body.map(go_help)),
                *cycle_mark,
            ),
            LetNonRec(def, body) => {
                let Def {
                    loc_pattern,
                    loc_expr,
                    expr_var,
                    pattern_vars,
                    annotation,
                } = &**def;
                let def = Def {
                    loc_pattern: loc_pattern.clone(),
                    loc_expr: loc_expr.map(go_help),
                    expr_var: sub!(*expr_var),
                    pattern_vars: pattern_vars.iter().map(|(s, v)| (*s, sub!(*v))).collect(),
                    annotation: annotation.clone(),
                };
                LetNonRec(Box::new(def), Box::new(body.map(go_help)))
            }

            Call(f, args, called_via) => {
                let (fn_var, fn_expr, clos_var, ret_var) = &**f;
                Call(
                    Box::new((
                        sub!(*fn_var),
                        fn_expr.map(go_help),
                        sub!(*clos_var),
                        sub!(*ret_var),
                    )),
                    args.iter()
                        .map(|(var, expr)| (sub!(*var), expr.map(go_help)))
                        .collect(),
                    *called_via,
                )
            }
            RunLowLevel { op, args, ret_var } => RunLowLevel {
                op: *op,
                args: args
                    .iter()
                    .map(|(var, expr)| (sub!(*var), go_help(expr)))
                    .collect(),
                ret_var: sub!(*ret_var),
            },
            ForeignCall {
                foreign_symbol,
                args,
                ret_var,
            } => ForeignCall {
                foreign_symbol: foreign_symbol.clone(),
                args: args
                    .iter()
                    .map(|(var, expr)| (sub!(*var), go_help(expr)))
                    .collect(),
                ret_var: sub!(*ret_var),
            },

            Closure(ClosureData {
                function_type,
                closure_type,
                return_type,
                name,
                captured_symbols,
                recursive,
                arguments,
                loc_body,
            }) => Closure(ClosureData {
                function_type: sub!(*function_type),
                closure_type: sub!(*closure_type),
                return_type: sub!(*return_type),
                name: *name,
                captured_symbols: captured_symbols
                    .iter()
                    .map(|(s, v)| (*s, sub!(*v)))
                    .collect(),
                recursive: *recursive,
                arguments: arguments
                    .iter()
                    .map(|(v, mark, pat)| (sub!(*v), *mark, pat.clone()))
                    .collect(),
                loc_body: Box::new(loc_body.map(go_help)),
            }),

            Record { record_var, fields } => Record {
                record_var: sub!(*record_var),
                fields: fields
                    .iter()
                    .map(
                        |(
                            k,
                            Field {
                                var,
                                region,
                                loc_expr,
                            },
                        )| {
                            (
                                k.clone(),
                                Field {
                                    var: sub!(*var),
                                    region: *region,
                                    loc_expr: Box::new(loc_expr.map(go_help)),
                                },
                            )
                        },
                    )
                    .collect(),
            },

            EmptyRecord => EmptyRecord,

            Access {
                record_var,
                ext_var,
                field_var,
                loc_expr,
                field,
            } => Access {
                record_var: sub!(*record_var),
                ext_var: sub!(*ext_var),
                field_var: sub!(*field_var),
                loc_expr: Box::new(loc_expr.map(go_help)),
                field: field.clone(),
            },

            Accessor(AccessorData {
                name,
                function_var,
                record_var,
                closure_var,
                ext_var,
                field_var,
                field,
            }) => Accessor(AccessorData {
                name: *name,
                function_var: sub!(*function_var),
                record_var: sub!(*record_var),
                closure_var: sub!(*closure_var),
                ext_var: sub!(*ext_var),
                field_var: sub!(*field_var),
                field: field.clone(),
            }),

            Update {
                record_var,
                ext_var,
                symbol,
                updates,
            } => Update {
                record_var: sub!(*record_var),
                ext_var: sub!(*ext_var),
                symbol: *symbol,
                updates: updates
                    .iter()
                    .map(
                        |(
                            k,
                            Field {
                                var,
                                region,
                                loc_expr,
                            },
                        )| {
                            (
                                k.clone(),
                                Field {
                                    var: sub!(*var),
                                    region: *region,
                                    loc_expr: Box::new(loc_expr.map(go_help)),
                                },
                            )
                        },
                    )
                    .collect(),
            },

            Tag {
                variant_var,
                ext_var,
                name,
                arguments,
            } => Tag {
                variant_var: sub!(*variant_var),
                ext_var: sub!(*ext_var),
                name: name.clone(),
                arguments: arguments
                    .iter()
                    .map(|(v, e)| (sub!(*v), e.map(go_help)))
                    .collect(),
            },

            ZeroArgumentTag {
                closure_name,
                variant_var,
                ext_var,
                name,
            } => ZeroArgumentTag {
                closure_name: *closure_name,
                variant_var: sub!(*variant_var),
                ext_var: sub!(*ext_var),
                name: name.clone(),
            },

            OpaqueRef {
                opaque_var,
                name,
                argument,
                specialized_def_type,
                type_arguments,
                lambda_set_variables,
            } => OpaqueRef {
                opaque_var: sub!(*opaque_var),
                name: *name,
                argument: Box::new((sub!(argument.0), argument.1.map(go_help))),
                // These shouldn't matter for opaques during mono, because they are only used for reporting
                // and pretty-printing to the user. During mono we decay immediately into the argument.
                // NB: if there are bugs, check if not substituting here is the problem!
                specialized_def_type: specialized_def_type.clone(),
                type_arguments: type_arguments.clone(),
                lambda_set_variables: lambda_set_variables.clone(),
            },

            Expect {
                loc_condition,
                loc_continuation,
                lookups_in_cond,
            } => Expect {
                loc_condition: Box::new(loc_condition.map(go_help)),
                loc_continuation: Box::new(loc_continuation.map(go_help)),
                lookups_in_cond: lookups_in_cond.to_vec(),
            },

            TypedHole(v) => TypedHole(sub!(*v)),

            RuntimeError(err) => RuntimeError(err.clone()),
        }
    }
}

/// Deep copies the type variables in [`var`], returning a map of original -> new type variable for
/// all type variables copied.
fn deep_copy_type_vars<'a>(
    arena: &'a Bump,
    subs: &mut Subs,
    var: Variable,
) -> Vec<'a, (Variable, Variable)> {
    // Always deal with the root, so that unified variables are treated the same.
    let var = subs.get_root_key_without_compacting(var);

    let mut copied = Vec::with_capacity_in(16, arena);

    let cloned_var = help(arena, subs, &mut copied, var);

    // we have tracked all visited variables, and can now traverse them
    // in one go (without looking at the UnificationTable) and clear the copy field
    let mut result = Vec::with_capacity_in(copied.len(), arena);
    for var in copied {
        subs.modify(var, |descriptor| {
            if let Some(copy) = descriptor.copy.into_variable() {
                result.push((var, copy));
                descriptor.copy = OptVariable::NONE;
            } else {
                debug_assert!(false, "{:?} marked as copied but it wasn't", var);
            }
        })
    }

    debug_assert!(result.contains(&(var, cloned_var)));

    return result;

    #[must_use]
    fn help(arena: &Bump, subs: &mut Subs, visited: &mut Vec<Variable>, var: Variable) -> Variable {
        use roc_types::subs::Content::*;
        use roc_types::subs::FlatType::*;

        // Always deal with the root, so that unified variables are treated the same.
        let var = subs.get_root_key_without_compacting(var);

        let desc = subs.get(var);

        // Unlike `deep_copy_var` in solve, here we are cloning *all* flex and rigid vars.
        // So we only want to short-circuit if we've already done the cloning work for a particular
        // var.
        if let Some(copy) = desc.copy.into_variable() {
            return copy;
        }

        let content = desc.content;

        let copy_descriptor = Descriptor {
            content: Error, // we'll update this below
            rank: desc.rank,
            mark: desc.mark,
            copy: OptVariable::NONE,
        };

        let copy = subs.fresh(copy_descriptor);
        subs.set_copy(var, copy.into());

        visited.push(var);

        macro_rules! descend_slice {
            ($slice:expr) => {
                for var_index in $slice {
                    let var = subs[var_index];
                    let _ = help(arena, subs, visited, var);
                }
            };
        }

        macro_rules! descend_var {
            ($var:expr) => {{
                help(arena, subs, visited, $var)
            }};
        }

        macro_rules! clone_var_slice {
            ($slice:expr) => {{
                let new_arguments = VariableSubsSlice::reserve_into_subs(subs, $slice.len());
                for (target_index, var_index) in (new_arguments.indices()).zip($slice) {
                    let var = subs[var_index];
                    let copy_var = subs.get_copy(var).into_variable().unwrap_or(var);
                    subs.variables[target_index] = copy_var;
                }
                new_arguments
            }};
        }

        macro_rules! perform_clone {
            ($do_clone:expr) => {{
                // It may the case that while deep-copying nested variables of this type, we
                // ended up copying the type itself (notably if it was self-referencing, in a
                // recursive type). In that case, short-circuit with the known copy.
                // if let Some(copy) = subs.get_ref(var).copy.into_variable() {
                //     return copy;
                // }
                // Perform the clone.
                $do_clone
            }};
        }

        // Now we recursively copy the content of the variable.
        // We have already marked the variable as copied, so we
        // will not repeat this work or crawl this variable again.
        let new_content = match content {
            // The vars for which we want to do something interesting.
            FlexVar(opt_name) => FlexVar(opt_name),
            FlexAbleVar(opt_name, ability) => FlexAbleVar(opt_name, ability),
            RigidVar(name) => RigidVar(name),
            RigidAbleVar(name, ability) => RigidAbleVar(name, ability),

            // Everything else is a mechanical descent.
            Structure(flat_type) => match flat_type {
                EmptyRecord | EmptyTagUnion | Erroneous(_) => Structure(flat_type),
                Apply(symbol, arguments) => {
                    descend_slice!(arguments);

                    perform_clone!({
                        let new_arguments = clone_var_slice!(arguments);
                        Structure(Apply(symbol, new_arguments))
                    })
                }
                Func(arguments, closure_var, ret_var) => {
                    descend_slice!(arguments);

                    let new_closure_var = descend_var!(closure_var);
                    let new_ret_var = descend_var!(ret_var);

                    perform_clone!({
                        let new_arguments = clone_var_slice!(arguments);
                        Structure(Func(new_arguments, new_closure_var, new_ret_var))
                    })
                }
                Record(fields, ext_var) => {
                    let new_ext_var = descend_var!(ext_var);

                    descend_slice!(fields.variables());

                    perform_clone!({
                        let new_variables = clone_var_slice!(fields.variables());
                        let new_fields = {
                            RecordFields {
                                length: fields.length,
                                field_names_start: fields.field_names_start,
                                variables_start: new_variables.start,
                                field_types_start: fields.field_types_start,
                            }
                        };

                        Structure(Record(new_fields, new_ext_var))
                    })
                }
                TagUnion(tags, ext_var) => {
                    let new_ext_var = descend_var!(ext_var);

                    for variables_slice_index in tags.variables() {
                        let variables_slice = subs[variables_slice_index];
                        descend_slice!(variables_slice);
                    }

                    perform_clone!({
                        let new_variable_slices =
                            SubsSlice::reserve_variable_slices(subs, tags.len());
                        let it = (new_variable_slices.indices()).zip(tags.variables());
                        for (target_index, index) in it {
                            let slice = subs[index];
                            let new_variables = clone_var_slice!(slice);
                            subs.variable_slices[target_index] = new_variables;
                        }

                        let new_union_tags =
                            UnionTags::from_slices(tags.labels(), new_variable_slices);

                        Structure(TagUnion(new_union_tags, new_ext_var))
                    })
                }
                RecursiveTagUnion(rec_var, tags, ext_var) => {
                    let new_ext_var = descend_var!(ext_var);
                    let new_rec_var = descend_var!(rec_var);

                    for variables_slice_index in tags.variables() {
                        let variables_slice = subs[variables_slice_index];
                        descend_slice!(variables_slice);
                    }

                    perform_clone!({
                        let new_variable_slices =
                            SubsSlice::reserve_variable_slices(subs, tags.len());
                        let it = (new_variable_slices.indices()).zip(tags.variables());
                        for (target_index, index) in it {
                            let slice = subs[index];
                            let new_variables = clone_var_slice!(slice);
                            subs.variable_slices[target_index] = new_variables;
                        }

                        let new_union_tags =
                            UnionTags::from_slices(tags.labels(), new_variable_slices);

                        Structure(RecursiveTagUnion(new_rec_var, new_union_tags, new_ext_var))
                    })
                }
                FunctionOrTagUnion(tag_name, symbol, ext_var) => {
                    let new_ext_var = descend_var!(ext_var);
                    perform_clone!(Structure(FunctionOrTagUnion(tag_name, symbol, new_ext_var)))
                }
            },

            RecursionVar {
                opt_name,
                structure,
            } => {
                let new_structure = descend_var!(structure);

                perform_clone!({
                    RecursionVar {
                        opt_name,
                        structure: new_structure,
                    }
                })
            }

            Alias(symbol, arguments, real_type_var, kind) => {
                let new_real_type_var = descend_var!(real_type_var);
                descend_slice!(arguments.all_variables());

                perform_clone!({
                    let new_variables = clone_var_slice!(arguments.all_variables());
                    let new_arguments = AliasVariables {
                        variables_start: new_variables.start,
                        ..arguments
                    };

                    Alias(symbol, new_arguments, new_real_type_var, kind)
                })
            }

            LambdaSet(subs::LambdaSet {
                solved,
                recursion_var,
                unspecialized,
                ambient_function,
            }) => {
                let new_rec_var = recursion_var.map(|var| descend_var!(var));
                for variables_slice_index in solved.variables() {
                    let variables_slice = subs[variables_slice_index];
                    descend_slice!(variables_slice);
                }
                for uls_index in unspecialized {
                    let Uls(var, _, _) = subs[uls_index];
                    descend_var!(var);
                }
                let new_ambient_function = descend_var!(ambient_function);

                perform_clone!({
                    let new_variable_slices =
                        SubsSlice::reserve_variable_slices(subs, solved.len());
                    let it = (new_variable_slices.indices()).zip(solved.variables());
                    for (target_index, index) in it {
                        let slice = subs[index];
                        let new_variables = clone_var_slice!(slice);
                        subs.variable_slices[target_index] = new_variables;
                    }

                    let new_solved =
                        UnionLambdas::from_slices(solved.labels(), new_variable_slices);

                    let new_unspecialized = SubsSlice::reserve_uls_slice(subs, unspecialized.len());
                    for (target_index, uls_index) in
                        (new_unspecialized.into_iter()).zip(unspecialized.into_iter())
                    {
                        let Uls(var, sym, region) = subs[uls_index];
                        let copy_var = subs.get_copy(var).into_variable().unwrap_or(var);
                        subs[target_index] = Uls(copy_var, sym, region);
                    }

                    LambdaSet(subs::LambdaSet {
                        solved: new_solved,
                        recursion_var: new_rec_var,
                        unspecialized: new_unspecialized,
                        ambient_function: new_ambient_function,
                    })
                })
            }

            RangedNumber(range) => {
                perform_clone!(RangedNumber(range))
            }
            Error => Error,
        };

        subs.set_content(copy, new_content);

        copy
    }
}

#[cfg(test)]
mod test {
    use super::deep_copy_type_vars;
    use bumpalo::Bump;
    use roc_error_macros::internal_error;
    use roc_module::symbol::Symbol;
    use roc_types::subs::{
        Content, Content::*, Descriptor, Mark, OptVariable, Rank, Subs, SubsIndex, Variable,
    };

    #[cfg(test)]
    fn new_var(subs: &mut Subs, content: Content) -> Variable {
        subs.fresh(Descriptor {
            content,
            rank: Rank::toplevel(),
            mark: Mark::NONE,
            copy: OptVariable::NONE,
        })
    }

    #[test]
    fn copy_flex_var() {
        let mut subs = Subs::new();
        let arena = Bump::new();

        let field_name = SubsIndex::push_new(&mut subs.field_names, "a".into());
        let var = new_var(&mut subs, FlexVar(Some(field_name)));

        let mut copies = deep_copy_type_vars(&arena, &mut subs, var);

        assert_eq!(copies.len(), 1);
        let (original, new) = copies.pop().unwrap();
        assert_ne!(original, new);

        assert_eq!(original, var);
        match subs.get_content_without_compacting(new) {
            FlexVar(Some(name)) => {
                assert_eq!(subs[*name].as_str(), "a");
            }
            it => unreachable!("{:?}", it),
        }
    }

    #[test]
    fn copy_rigid_var() {
        let mut subs = Subs::new();
        let arena = Bump::new();

        let field_name = SubsIndex::push_new(&mut subs.field_names, "a".into());
        let var = new_var(&mut subs, RigidVar(field_name));

        let mut copies = deep_copy_type_vars(&arena, &mut subs, var);

        assert_eq!(copies.len(), 1);
        let (original, new) = copies.pop().unwrap();
        assert_ne!(original, new);

        assert_eq!(original, var);
        match subs.get_content_without_compacting(new) {
            RigidVar(name) => {
                assert_eq!(subs[*name].as_str(), "a");
            }
            it => unreachable!("{:?}", it),
        }
    }

    #[test]
    fn copy_flex_able_var() {
        let mut subs = Subs::new();
        let arena = Bump::new();

        let field_name = SubsIndex::push_new(&mut subs.field_names, "a".into());
        let var = new_var(&mut subs, FlexAbleVar(Some(field_name), Symbol::UNDERSCORE));

        let mut copies = deep_copy_type_vars(&arena, &mut subs, var);

        assert_eq!(copies.len(), 1);
        let (original, new) = copies.pop().unwrap();
        assert_ne!(original, new);

        assert_eq!(original, var);
        match subs.get_content_without_compacting(new) {
            FlexAbleVar(Some(name), Symbol::UNDERSCORE) => {
                assert_eq!(subs[*name].as_str(), "a");
            }
            it => unreachable!("{:?}", it),
        }
    }

    #[test]
    fn copy_rigid_able_var() {
        let mut subs = Subs::new();
        let arena = Bump::new();

        let field_name = SubsIndex::push_new(&mut subs.field_names, "a".into());
        let var = new_var(&mut subs, RigidAbleVar(field_name, Symbol::UNDERSCORE));

        let mut copies = deep_copy_type_vars(&arena, &mut subs, var);

        assert_eq!(copies.len(), 1);
        let (original, new) = copies.pop().unwrap();
        assert_ne!(original, new);

        assert_eq!(original, var);
        match subs.get_content_without_compacting(new) {
            RigidAbleVar(name, Symbol::UNDERSCORE) => {
                assert_eq!(subs[*name].as_str(), "a");
            }
            it => internal_error!("{:?}", it),
        }
    }
}
