use crate::builtins;
use crate::expr::{constrain_expr, Env};
use roc_can::constraint::{Constraint, Constraints, PExpectedTypeIndex, TypeOrVar};
use roc_can::expected::{Expected, PExpected};
use roc_can::pattern::Pattern::{self, *};
use roc_can::pattern::{DestructType, ListPatterns, RecordDestruct, TupleDestruct};
use roc_collections::all::{HumanIndex, SendMap};
use roc_collections::VecMap;
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::Variable;
use roc_types::types::{
    AliasKind, AliasShared, Category, OptAbleType, PReason, PatternCategory, Reason, RecordField,
    Type, TypeExtension, TypeTag, Types,
};
use soa::Index;

#[derive(Default, Debug)]
pub struct PatternState {
    pub headers: VecMap<Symbol, Loc<TypeOrVar>>,
    pub vars: Vec<Variable>,
    pub constraints: Vec<Constraint>,
    pub delayed_is_open_constraints: Vec<Constraint>,
    pub delayed_fx_suffix_constraints: Vec<Constraint>,
}

/// If there is a type annotation, the pattern state headers can be optimized by putting the
/// annotation in the headers. Normally
///
/// x = 4
///
/// Would add `x => <42>` to the headers (i.e., symbol points to a type variable). If the
/// definition has an annotation, we instead now add `x => Int`.
pub fn headers_from_annotation(
    types: &Types,
    constraints: &mut Constraints,
    pattern: &Pattern,
    annotation: &Loc<Index<TypeTag>>,
) -> Option<VecMap<Symbol, Loc<TypeOrVar>>> {
    let mut headers = VecMap::default();
    // Check that the annotation structurally agrees with the pattern, preventing e.g. `{ x, y } : Int`
    // in such incorrect cases we don't put the full annotation in headers, just a variable, and let
    // inference generate a proper error.
    let is_structurally_valid =
        headers_from_annotation_help(types, constraints, pattern, annotation, &mut headers);

    if is_structurally_valid {
        Some(headers)
    } else {
        None
    }
}

fn headers_from_annotation_help(
    types: &Types,
    constraints: &mut Constraints,
    pattern: &Pattern,
    annotation: &Loc<Index<TypeTag>>,
    headers: &mut VecMap<Symbol, Loc<TypeOrVar>>,
) -> bool {
    let typ = annotation.value;
    match pattern {
        Identifier(symbol)
        | Shadowed(_, _, symbol)
        | AbilityMemberSpecialization {
            ident: symbol,
            specializes: _,
        } => {
            let annotation_index = constraints.push_type(types, typ);
            let typ = Loc::at(annotation.region, annotation_index);
            headers.insert(*symbol, typ);
            true
        }

        As(subpattern, symbol) => {
            let annotation_index = constraints.push_type(types, typ);
            let typ = Loc::at(annotation.region, annotation_index);
            headers.insert(*symbol, typ);

            headers_from_annotation_help(types, constraints, &subpattern.value, annotation, headers)
        }

        Underscore
        | MalformedPattern(_, _)
        | UnsupportedPattern(_)
        | OpaqueNotInScope(..)
        | NumLiteral(..)
        | IntLiteral(..)
        | FloatLiteral(..)
        | SingleQuote(..)
        | StrLiteral(_) => true,

        RecordDestructure { destructs, .. } => {
            let dealiased = types.shallow_dealias(annotation.value);
            match types[dealiased] {
                TypeTag::Record(fields) => {
                    let (field_names, _, field_types) = types.record_fields_slices(fields);
                    let field_names = &types[field_names];

                    for loc_destruct in destructs {
                        let destruct = &loc_destruct.value;

                        // NOTE: We ignore both Guard and optionality when
                        // determining the type of the assigned def (which is what
                        // gets added to the header here).
                        //
                        // For example, no matter whether it's `{ x } = rec` or
                        // `{ x ? 0 } = rec` or `{ x: 5 } -> ...` in all cases
                        // the type of `x` within the binding itself is the same.
                        if let Some(i) = field_names
                            .iter()
                            .position(|field| field == &destruct.label)
                        {
                            let field_type_index = {
                                let typ = field_types.at(i);
                                constraints.push_type(types, typ)
                            };
                            headers.insert(
                                destruct.symbol,
                                Loc::at(annotation.region, field_type_index),
                            );
                        } else {
                            return false;
                        }
                    }
                    true
                }
                TypeTag::EmptyRecord => destructs.is_empty(),
                _ => false,
            }
        }

        TupleDestructure { destructs: _, .. } => {
            todo!();
        }

        List { patterns, .. } => {
            if let Some((_, Some(rest))) = patterns.opt_rest {
                let annotation_index = {
                    let typ = annotation.value;
                    constraints.push_type(types, typ)
                };
                let typ = Loc::at(annotation.region, annotation_index);
                headers.insert(rest.value, typ);

                false
            } else {
                // There are no interesting headers to introduce for list patterns, since the only
                // exhaustive list pattern is
                //   \[..] -> <body>
                // which does not introduce any symbols.
                false
            }
        }

        AppliedTag {
            tag_name,
            arguments,
            ..
        } => {
            let dealiased = types.shallow_dealias(annotation.value);
            match types[dealiased] {
                TypeTag::TagUnion(tags, _) => {
                    let (tags, payloads) = types.union_tag_slices(tags);
                    let tags = &types[tags];

                    if let Some(i) = tags.iter().position(|name| name == tag_name) {
                        let arg_types_slice = types[payloads.at(i)];

                        if !arguments.len() == arg_types_slice.len() {
                            return false;
                        }

                        arguments
                            .iter()
                            .zip(arg_types_slice)
                            .all(|(arg_pattern, arg_type)| {
                                headers_from_annotation_help(
                                    types,
                                    constraints,
                                    &arg_pattern.1.value,
                                    &Loc::at(annotation.region, arg_type),
                                    headers,
                                )
                            })
                    } else {
                        false
                    }
                }
                _ => false,
            }
        }

        UnwrappedOpaque {
            whole_var: _,
            opaque,
            argument,
            specialized_def_type: _,
            type_arguments: pat_type_arguments,
            lambda_set_variables: pat_lambda_set_variables,
        } => {
            let typ = annotation.value;

            match types[typ] {
                TypeTag::OpaqueAlias { shared, actual } => {
                    let AliasShared {
                        symbol,
                        lambda_set_variables,
                        ..
                    } = types[shared];
                    let type_arguments = types.get_type_arguments(typ);

                    if symbol == *opaque
                        && type_arguments.len() == pat_type_arguments.len()
                        && lambda_set_variables.len() == pat_lambda_set_variables.len()
                    {
                        let annotation_index = constraints.push_type(types, typ);
                        let typ = Loc::at(annotation.region, annotation_index);
                        headers.insert(*opaque, typ);

                        let (_, argument_pat) = &**argument;
                        headers_from_annotation_help(
                            types,
                            constraints,
                            &argument_pat.value,
                            &Loc::at(annotation.region, actual),
                            headers,
                        )
                    } else {
                        false
                    }
                }
                _ => false,
            }
        }
    }
}

/// This accepts PatternState (rather than returning it) so that the caller can
/// initialize the Vecs in PatternState using with_capacity
/// based on its knowledge of their lengths.
pub fn constrain_pattern(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    pattern: &Pattern,
    region: Region,
    expected: PExpectedTypeIndex,
    state: &mut PatternState,
) {
    constrain_pattern_help(types, constraints, env, pattern, region, expected, state);
}

#[allow(clippy::too_many_arguments)]
pub fn constrain_pattern_help(
    types: &mut Types,
    constraints: &mut Constraints,
    env: &mut Env,
    pattern: &Pattern,
    region: Region,
    expected: PExpectedTypeIndex,
    state: &mut PatternState,
) {
    match pattern {
        Underscore => {
            // This is an underscore in a position where we destruct a variable,
            // like a when expression:
            //   when x is
            //     A -> ""
            //     _ -> ""
            // so, we know that "x" (in this case, a tag union) must be open.
            let expected_type = *constraints[expected].get_type_ref();
            if could_be_a_tag_union(types, expected_type) {
                state
                    .delayed_is_open_constraints
                    .push(constraints.is_open_type(expected_type));
            }
        }
        UnsupportedPattern(_) | MalformedPattern(_, _) | OpaqueNotInScope(..) => {
            // Erroneous patterns don't add any constraints.
        }

        Identifier(symbol) | Shadowed(_, _, symbol) => {
            let type_index = *constraints[expected].get_type_ref();

            if could_be_a_tag_union(types, type_index) {
                state
                    .delayed_is_open_constraints
                    .push(constraints.is_open_type(type_index));
            }

            state.headers.insert(
                *symbol,
                Loc {
                    region,
                    value: type_index,
                },
            );
        }

        As(subpattern, symbol) => {
            // NOTE: we don't use `could_be_a_tag_union` here. The `PATTERN as name` should
            // just use the type of the PATTERN, and not influence what type is inferred for PATTERN

            state.headers.insert(
                *symbol,
                Loc {
                    region,
                    value: *constraints[expected].get_type_ref(),
                },
            );

            constrain_pattern_help(
                types,
                constraints,
                env,
                &subpattern.value,
                subpattern.region,
                expected,
                state,
            )
        }

        AbilityMemberSpecialization {
            ident: symbol,
            specializes: _,
        } => {
            let expected = &constraints[expected];
            let type_index = *expected.get_type_ref();

            if could_be_a_tag_union(types, type_index) {
                state.constraints.push(constraints.is_open_type(type_index));
            }

            state.headers.insert(
                *symbol,
                Loc {
                    region,
                    value: type_index,
                },
            );
        }

        &NumLiteral(precision_var, _, _, bound) => {
            state.vars.push(precision_var);

            let num_type = builtins::add_numeric_bound_constr(
                types,
                constraints,
                &mut state.constraints,
                precision_var,
                precision_var,
                bound,
                region,
                Category::Num,
            );
            let num_type = {
                let typ = types.from_old_type(&num_type);
                constraints.push_type(types, typ)
            };

            state.constraints.push(constraints.equal_pattern_types(
                num_type,
                expected,
                PatternCategory::Num,
                region,
            ));
        }

        &IntLiteral(num_precision_var, precision_var, _, _, bound) => {
            // First constraint on the free num var; this improves the resolved type quality in
            // case the bound is an alias.
            let num_type = builtins::add_numeric_bound_constr(
                types,
                constraints,
                &mut state.constraints,
                num_precision_var,
                num_precision_var,
                bound,
                region,
                Category::Int,
            );
            let num_type = {
                let typ = types.from_old_type(&num_type);
                constraints.push_type(types, typ)
            };

            // Link the free num var with the int var and our expectation.
            let int_type = {
                let typ = types.from_old_type(&builtins::num_int(Type::Variable(precision_var)));
                constraints.push_type(types, typ)
            };

            state.constraints.push({
                let expected_index =
                    constraints.push_expected_type(Expected::NoExpectation(int_type));
                constraints.equal_types(num_type, expected_index, Category::Int, region)
            });

            // Also constrain the pattern against the num var, again to reuse aliases if they're present.
            state.constraints.push(constraints.equal_pattern_types(
                num_type,
                expected,
                PatternCategory::Int,
                region,
            ));
        }

        &FloatLiteral(num_precision_var, precision_var, _, _, bound) => {
            // First constraint on the free num var; this improves the resolved type quality in
            // case the bound is an alias.
            let num_type = builtins::add_numeric_bound_constr(
                types,
                constraints,
                &mut state.constraints,
                num_precision_var,
                num_precision_var,
                bound,
                region,
                Category::Frac,
            );
            let num_type_index = {
                let typ = types.from_old_type(&num_type);
                constraints.push_type(types, typ)
            }; // NOTE: check me if something breaks!

            // Link the free num var with the float var and our expectation.
            let float_type = {
                let typ = types.from_old_type(&builtins::num_float(Type::Variable(precision_var)));
                constraints.push_type(types, typ)
            };

            state.constraints.push({
                let expected_index =
                    constraints.push_expected_type(Expected::NoExpectation(float_type));
                constraints.equal_types(num_type_index, expected_index, Category::Frac, region)
            });

            // Also constrain the pattern against the num var, again to reuse aliases if they're present.
            state.constraints.push(constraints.equal_pattern_types(
                num_type_index,
                expected,
                PatternCategory::Float,
                region,
            ));
        }

        StrLiteral(_) => {
            let str_type = constraints.push_type(types, Types::STR);
            state.constraints.push(constraints.equal_pattern_types(
                str_type,
                expected,
                PatternCategory::Str,
                region,
            ));
        }

        &SingleQuote(num_var, precision_var, _, bound) => {
            // First constraint on the free num var; this improves the resolved type quality in
            // case the bound is an alias.
            let num_type = builtins::add_numeric_bound_constr(
                types,
                constraints,
                &mut state.constraints,
                num_var,
                num_var,
                bound,
                region,
                Category::Int,
            );

            let num_type_index = {
                let typ = types.from_old_type(&num_type);
                constraints.push_type(types, typ)
            };

            // Link the free num var with the int var and our expectation.
            let int_type = {
                let typ = types.from_old_type(&builtins::num_int(Type::Variable(precision_var)));
                constraints.push_type(types, typ)
            };

            state.constraints.push({
                let expected_index =
                    constraints.push_expected_type(Expected::NoExpectation(int_type));
                constraints.equal_types(
                    num_type_index, // TODO check me if something breaks!
                    expected_index,
                    Category::Int,
                    region,
                )
            });

            // Also constrain the pattern against the num var, again to reuse aliases if they're present.
            state.constraints.push(constraints.equal_pattern_types(
                num_type_index,
                expected,
                PatternCategory::Character,
                region,
            ));
        }

        TupleDestructure {
            whole_var,
            ext_var,
            destructs,
        } => {
            state.vars.push(*whole_var);
            state.vars.push(*ext_var);
            let ext_type = Type::Variable(*ext_var);

            let mut elem_types: VecMap<usize, Type> = VecMap::default();

            for Loc {
                value:
                    TupleDestruct {
                        destruct_index: index,
                        var,
                        typ,
                    },
                ..
            } in destructs.iter()
            {
                let pat_type = Type::Variable(*var);
                let pat_type_index = constraints.push_variable(*var);
                let expected =
                    constraints.push_pat_expected_type(PExpected::NoExpectation(pat_type_index));

                let (guard_var, loc_pattern) = typ;
                let elem_type = {
                    let guard_type = constraints.push_variable(*guard_var);
                    let expected_pat = constraints.push_pat_expected_type(PExpected::ForReason(
                        PReason::PatternGuard,
                        pat_type_index,
                        loc_pattern.region,
                    ));

                    state.constraints.push(constraints.pattern_presence(
                        guard_type,
                        expected_pat,
                        PatternCategory::PatternGuard,
                        region,
                    ));
                    state.vars.push(*guard_var);

                    constrain_pattern_help(
                        types,
                        constraints,
                        env,
                        &loc_pattern.value,
                        loc_pattern.region,
                        expected,
                        state,
                    );

                    pat_type
                };

                elem_types.insert(*index, elem_type);

                state.vars.push(*var);
            }

            let tuple_type = {
                let typ = types.from_old_type(&Type::Tuple(
                    elem_types,
                    TypeExtension::from_non_annotation_type(ext_type),
                ));
                constraints.push_type(types, typ)
            };

            let whole_var_index = constraints.push_variable(*whole_var);
            let expected_record =
                constraints.push_expected_type(Expected::NoExpectation(tuple_type));
            let whole_con = constraints.equal_types(
                whole_var_index,
                expected_record,
                Category::Storage(std::file!(), std::line!()),
                region,
            );

            let record_con = constraints.pattern_presence(
                whole_var_index,
                expected,
                PatternCategory::Record,
                region,
            );

            state.constraints.push(whole_con);
            state.constraints.push(record_con);
        }

        RecordDestructure {
            whole_var,
            ext_var,
            destructs,
            opt_spread,
        } => {
            state.vars.push(*whole_var);
            state.vars.push(*ext_var);
            let ext_type = Type::Variable(*ext_var);

            let mut field_types: SendMap<Lowercase, RecordField<Type>> = SendMap::default();

            for Loc {
                value:
                    RecordDestruct {
                        var,
                        label,
                        symbol,
                        typ,
                    },
                ..
            } in destructs
            {
                let pat_type = Type::Variable(*var);
                let pat_type_index = constraints.push_variable(*var);
                let expected =
                    constraints.push_pat_expected_type(PExpected::NoExpectation(pat_type_index));

                if !state.headers.contains_key(symbol) {
                    state
                        .headers
                        .insert(*symbol, Loc::at(region, pat_type_index));
                }

                let field_type = match typ {
                    DestructType::Guard(guard_var, loc_guard) => {
                        let guard_type = constraints.push_variable(*guard_var);
                        let expected_pat =
                            constraints.push_pat_expected_type(PExpected::ForReason(
                                PReason::PatternGuard,
                                pat_type_index,
                                loc_guard.region,
                            ));

                        state.constraints.push(constraints.pattern_presence(
                            guard_type,
                            expected_pat,
                            PatternCategory::PatternGuard,
                            region,
                        ));
                        state.vars.push(*guard_var);

                        constrain_pattern_help(
                            types,
                            constraints,
                            env,
                            &loc_guard.value,
                            loc_guard.region,
                            expected,
                            state,
                        );

                        RecordField::Demanded(pat_type)
                    }
                    DestructType::Optional(expr_var, loc_expr) => {
                        let expr_type = constraints.push_variable(*expr_var);
                        let expected_pat =
                            constraints.push_pat_expected_type(PExpected::ForReason(
                                PReason::OptionalField,
                                pat_type_index,
                                loc_expr.region,
                            ));

                        state.constraints.push(constraints.pattern_presence(
                            expr_type,
                            expected_pat,
                            PatternCategory::PatternDefault,
                            region,
                        ));

                        state.vars.push(*expr_var);

                        let expr_expected = constraints.push_expected_type(Expected::ForReason(
                            Reason::RecordDefaultField(label.clone()),
                            pat_type_index,
                            loc_expr.region,
                        ));

                        let expr_con = constrain_expr(
                            types,
                            constraints,
                            env,
                            loc_expr.region,
                            &loc_expr.value,
                            expr_expected,
                        );
                        state.constraints.push(expr_con);

                        RecordField::Optional(pat_type)
                    }
                    DestructType::Required => {
                        // Named destructures like
                        //   {foo} -> ...
                        // are equivalent to wildcards on the type of `foo`, so if `foo` is a tag
                        // union, we must add a constraint to ensure that this destructure opens it
                        // up.
                        if could_be_a_tag_union(types, pat_type_index) {
                            state
                                .delayed_is_open_constraints
                                .push(constraints.is_open_type(pat_type_index));
                        }

                        // No extra constraints necessary.
                        RecordField::Demanded(pat_type)
                    }
                };

                field_types.insert(label.clone(), field_type);

                state.vars.push(*var);
            }

            let record_type = {
                let typ = types.from_old_type(&Type::Record(
                    field_types,
                    TypeExtension::from_non_annotation_type(ext_type),
                ));
                constraints.push_type(types, typ)
            };

            let whole_var_index = constraints.push_variable(*whole_var);
            let expected_record =
                constraints.push_expected_type(Expected::NoExpectation(record_type));
            let whole_con = constraints.equal_types(
                whole_var_index,
                expected_record,
                Category::Storage(std::file!(), std::line!()),
                region,
            );

            let record_con = constraints.pattern_presence(
                whole_var_index,
                expected,
                PatternCategory::Record,
                region,
            );

            state.constraints.push(whole_con);
            state.constraints.push(record_con);
        }

        List {
            list_var,
            elem_var,
            patterns: ListPatterns { patterns, opt_rest },
        } => {
            let elem_var_index = constraints.push_variable(*elem_var);

            if let Some((_, Some(rest))) = opt_rest {
                state.headers.insert(
                    rest.value,
                    Loc {
                        region,
                        value: *constraints[expected].get_type_ref(),
                    },
                );
            }

            for loc_pat in patterns.iter() {
                let expected = constraints.push_pat_expected_type(PExpected::ForReason(
                    PReason::ListElem,
                    elem_var_index,
                    loc_pat.region,
                ));

                constrain_pattern_help(
                    types,
                    constraints,
                    env,
                    &loc_pat.value,
                    loc_pat.region,
                    expected,
                    state,
                );
            }

            let list_var_index = constraints.push_variable(*list_var);
            let solved_list = {
                let typ = types.from_old_type(&Type::Apply(
                    Symbol::LIST_LIST,
                    vec![Loc::at(region, Type::Variable(*elem_var))],
                    region,
                ));
                constraints.push_type(types, typ)
            };
            let store_solved_list = constraints.store(solved_list, *list_var, file!(), line!());

            let expected_constraint = constraints.pattern_presence(
                list_var_index,
                expected,
                PatternCategory::List,
                region,
            );

            state.vars.push(*list_var);
            state.vars.push(*elem_var);
            state.constraints.push(store_solved_list);
            state.constraints.push(expected_constraint);
        }

        AppliedTag {
            whole_var,
            ext_var,
            tag_name,
            arguments,
        } => {
            let argument_types = constraints.variable_slice(arguments.iter().map(|(var, _)| *var));

            for (index, (pattern_var, loc_pattern)) in arguments.iter().enumerate() {
                state.vars.push(*pattern_var);

                let pattern_type = constraints.push_variable(*pattern_var);

                let expected = constraints.push_pat_expected_type(PExpected::ForReason(
                    PReason::TagArg {
                        tag_name: tag_name.clone(),
                        index: HumanIndex::zero_based(index),
                    },
                    pattern_type,
                    region,
                ));
                constrain_pattern_help(
                    types,
                    constraints,
                    env,
                    &loc_pattern.value,
                    loc_pattern.region,
                    expected,
                    state,
                );
            }

            let pat_category = PatternCategory::Ctor(tag_name.clone());
            let expected_type = *constraints[expected].get_type_ref();

            let whole_con = constraints.includes_tag(
                expected_type,
                tag_name.clone(),
                argument_types,
                pat_category.clone(),
                region,
            );

            let whole_type = constraints.push_variable(*whole_var);

            let tag_con = constraints.pattern_presence(whole_type, expected, pat_category, region);

            state.vars.push(*whole_var);
            state.vars.push(*ext_var);
            state.constraints.push(whole_con);
            state.constraints.push(tag_con);
        }

        UnwrappedOpaque {
            whole_var,
            opaque,
            argument,
            specialized_def_type,
            type_arguments,
            lambda_set_variables,
        } => {
            // Suppose we are constraining the pattern \@Id who, where Id n := [Id U64 n]
            let (arg_pattern_var, loc_arg_pattern) = &**argument;
            let arg_pattern_type_index = constraints.push_variable(*arg_pattern_var);

            let opaque_type = {
                let typ = types.from_old_type(&Type::Alias {
                    symbol: *opaque,
                    type_arguments: type_arguments
                        .iter()
                        .map(|v| OptAbleType {
                            typ: Type::Variable(v.var),
                            opt_abilities: v.opt_abilities.clone(),
                        })
                        .collect(),
                    lambda_set_variables: lambda_set_variables.clone(),
                    infer_ext_in_output_types: vec![],
                    actual: Box::new(Type::Variable(*arg_pattern_var)),
                    kind: AliasKind::Opaque,
                });
                constraints.push_type(types, typ)
            };

            // First, add a constraint for the argument "who"
            let arg_pattern_expected = constraints
                .push_pat_expected_type(PExpected::NoExpectation(arg_pattern_type_index));
            constrain_pattern_help(
                types,
                constraints,
                env,
                &loc_arg_pattern.value,
                loc_arg_pattern.region,
                arg_pattern_expected,
                state,
            );

            // Next, link `whole_var` to the opaque type of "@Id who"
            let whole_var_index = constraints.push_variable(*whole_var);
            let expected_opaque =
                constraints.push_expected_type(Expected::NoExpectation(opaque_type));
            let whole_con = constraints.equal_types(
                whole_var_index,
                expected_opaque,
                Category::Storage(std::file!(), std::line!()),
                region,
            );

            // Link the entire wrapped opaque type (with the now-constrained argument) to the type
            // variables of the opaque type.
            //
            // For example, suppose we have `O k := [A k, B k]`, and the pattern `@O (A s) -> s == ""`.
            // Previous constraints will have solved `typeof s ~ Str`, and we have the
            // `specialized_def_type` being `[A k1, B k1]`, specializing `k` as `k1` for this opaque
            // usage.
            // We now want to link `typeof s ~ k1`, so to capture this relationship, we link
            // the type of `A s` (the arg type) to `[A k1, B k1]` (the specialized opaque type).
            //
            // This must **always** be a presence constraint, that is enforcing
            // `[A k1, B k1] += typeof (A s)`, because we are in a destructure position and not
            // all constructors are covered in this branch!
            let arg_pattern_type = constraints.push_variable(*arg_pattern_var);
            let specialized_type_index = {
                let typ = types.from_old_type(specialized_def_type);
                constraints.push_type(types, typ)
            };
            let specialized_type_expected = constraints
                .push_pat_expected_type(PExpected::NoExpectation(specialized_type_index));

            let link_type_variables_con = constraints.pattern_presence(
                arg_pattern_type,
                specialized_type_expected,
                PatternCategory::Opaque(*opaque),
                loc_arg_pattern.region,
            );

            // Next, link `whole_var` (the type of "@Id who") to the expected type
            let whole_type = constraints.push_variable(*whole_var);
            let opaque_pattern_con = constraints.pattern_presence(
                whole_type,
                expected,
                PatternCategory::Opaque(*opaque),
                region,
            );

            state
                .vars
                .extend_from_slice(&[*arg_pattern_var, *whole_var]);
            // Also add the fresh variables we created for the type argument and lambda sets
            state.vars.extend(type_arguments.iter().map(|v| v.var));
            state.vars.extend(lambda_set_variables.iter().map(|v| {
                v.0.expect_variable("all lambda sets should be fresh variables here")
            }));

            state.constraints.extend_from_slice(&[
                whole_con,
                link_type_variables_con,
                opaque_pattern_con,
            ]);
        }
    }
}

fn could_be_a_tag_union(types: &Types, typ: TypeOrVar) -> bool {
    match typ.split() {
        Ok(typ_index) => !matches!(
            types[typ_index],
            TypeTag::Apply { .. } | TypeTag::Function(..) | TypeTag::Record(..)
        ),
        Err(_) => {
            // Variables are opaque at this point, assume yes
            true
        }
    }
}
