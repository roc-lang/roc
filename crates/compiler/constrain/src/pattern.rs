use crate::builtins;
use crate::expr::{constrain_expr, Env};
use roc_can::constraint::{Constraint, Constraints};
use roc_can::expected::{Expected, PExpected};
use roc_can::pattern::Pattern::{self, *};
use roc_can::pattern::{DestructType, RecordDestruct};
use roc_collections::all::{HumanIndex, SendMap};
use roc_collections::VecMap;
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::Variable;
use roc_types::types::{
    AliasKind, Category, OptAbleType, PReason, PatternCategory, Reason, RecordField, Type,
    TypeExtension,
};

#[derive(Default, Debug)]
pub struct PatternState {
    pub headers: VecMap<Symbol, Loc<Type>>,
    pub vars: Vec<Variable>,
    pub constraints: Vec<Constraint>,
    pub delayed_is_open_constraints: Vec<Constraint>,
}

/// If there is a type annotation, the pattern state headers can be optimized by putting the
/// annotation in the headers. Normally
///
/// x = 4
///
/// Would add `x => <42>` to the headers (i.e., symbol points to a type variable). If the
/// definition has an annotation, we instead now add `x => Int`.
pub fn headers_from_annotation(
    pattern: &Pattern,
    annotation: &Loc<&Type>,
) -> Option<VecMap<Symbol, Loc<Type>>> {
    let mut headers = VecMap::default();
    // Check that the annotation structurally agrees with the pattern, preventing e.g. `{ x, y } : Int`
    // in such incorrect cases we don't put the full annotation in headers, just a variable, and let
    // inference generate a proper error.
    let is_structurally_valid = headers_from_annotation_help(pattern, annotation, &mut headers);

    if is_structurally_valid {
        Some(headers)
    } else {
        None
    }
}

fn headers_from_annotation_help(
    pattern: &Pattern,
    annotation: &Loc<&Type>,
    headers: &mut VecMap<Symbol, Loc<Type>>,
) -> bool {
    match pattern {
        Identifier(symbol)
        | Shadowed(_, _, symbol)
        // TODO(abilities): handle linking the member def to the specialization ident
        | AbilityMemberSpecialization {
            ident: symbol,
            specializes: _,
        } => {
            let typ = Loc::at(annotation.region, annotation.value.clone());
            headers.insert(*symbol, typ);
            true
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

        RecordDestructure { destructs, .. } => match annotation.value.shallow_dealias() {
            Type::Record(fields, _) => {
                for loc_destruct in destructs {
                    let destruct = &loc_destruct.value;

                    // NOTE: We ignore both Guard and optionality when
                    // determining the type of the assigned def (which is what
                    // gets added to the header here).
                    //
                    // For example, no matter whether it's `{ x } = rec` or
                    // `{ x ? 0 } = rec` or `{ x: 5 } -> ...` in all cases
                    // the type of `x` within the binding itself is the same.
                    if let Some(field_type) = fields.get(&destruct.label) {
                        headers.insert(
                            destruct.symbol,
                            Loc::at(annotation.region, field_type.clone().into_inner()),
                        );
                    } else {
                        return false;
                    }
                }
                true
            }
            Type::EmptyRec => destructs.is_empty(),
            _ => false,
        },

        AppliedTag {
            tag_name,
            arguments,
            ..
        } => match annotation.value.shallow_dealias() {
            Type::TagUnion(tags, _) => {
                if let Some((_, arg_types)) = tags.iter().find(|(name, _)| name == tag_name) {
                    if !arguments.len() == arg_types.len() {
                        return false;
                    }

                    arguments
                        .iter()
                        .zip(arg_types.iter())
                        .all(|(arg_pattern, arg_type)| {
                            headers_from_annotation_help(
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
        },

        UnwrappedOpaque {
            whole_var: _,
            opaque,
            argument,
            specialized_def_type: _,
            type_arguments: pat_type_arguments,
            lambda_set_variables: pat_lambda_set_variables,
        } => match &annotation.value {
            Type::Alias {
                symbol,
                kind: AliasKind::Opaque,
                actual,
                type_arguments,
                lambda_set_variables,
            } if symbol == opaque
                && type_arguments.len() == pat_type_arguments.len()
                && lambda_set_variables.len() == pat_lambda_set_variables.len() =>
            {
                let typ = Loc::at(annotation.region, annotation.value.clone());
                headers.insert(*opaque, typ);

                let (_, argument_pat) = &**argument;
                headers_from_annotation_help(
                    &argument_pat.value,
                    &Loc::at(annotation.region, actual),
                    headers,
                )
            }
            _ => false,
        },
    }
}

/// This accepts PatternState (rather than returning it) so that the caller can
/// initialize the Vecs in PatternState using with_capacity
/// based on its knowledge of their lengths.
pub fn constrain_pattern(
    constraints: &mut Constraints,
    env: &mut Env,
    pattern: &Pattern,
    region: Region,
    expected: PExpected<Type>,
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
            if could_be_a_tag_union(expected.get_type_ref()) {
                state
                    .delayed_is_open_constraints
                    .push(constraints.is_open_type(expected.get_type()));
            }
        }
        UnsupportedPattern(_) | MalformedPattern(_, _) | OpaqueNotInScope(..) => {
            // Erroneous patterns don't add any constraints.
        }

        Identifier(symbol) | Shadowed(_, _, symbol) => {
            if could_be_a_tag_union(expected.get_type_ref()) {
                state
                    .delayed_is_open_constraints
                    .push(constraints.is_open_type(expected.get_type_ref().clone()));
            }

            state.headers.insert(
                *symbol,
                Loc {
                    region,
                    value: expected.get_type(),
                },
            );
        }

        AbilityMemberSpecialization {
            ident: symbol,
            specializes: _,
        } => {
            if could_be_a_tag_union(expected.get_type_ref()) {
                state
                    .constraints
                    .push(constraints.is_open_type(expected.get_type_ref().clone()));
            }

            state.headers.insert(
                *symbol,
                Loc {
                    region,
                    value: expected.get_type(),
                },
            );
        }

        &NumLiteral(precision_var, _, _, bound) => {
            state.vars.push(precision_var);

            let num_type = builtins::add_numeric_bound_constr(
                constraints,
                &mut state.constraints,
                precision_var,
                precision_var,
                bound,
                region,
                Category::Num,
            );

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
                constraints,
                &mut state.constraints,
                num_precision_var,
                num_precision_var,
                bound,
                region,
                Category::Int,
            );

            // Link the free num var with the int var and our expectation.
            let int_type = builtins::num_int(Type::Variable(precision_var));

            state.constraints.push(constraints.equal_types(
                num_type.clone(), // TODO check me if something breaks!
                Expected::NoExpectation(int_type),
                Category::Int,
                region,
            ));

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
                constraints,
                &mut state.constraints,
                num_precision_var,
                num_precision_var,
                bound,
                region,
                Category::Frac,
            );

            // Link the free num var with the float var and our expectation.
            let float_type = builtins::num_float(Type::Variable(precision_var));

            state.constraints.push(constraints.equal_types(
                num_type.clone(), // TODO check me if something breaks!
                Expected::NoExpectation(float_type),
                Category::Frac,
                region,
            ));

            // Also constrain the pattern against the num var, again to reuse aliases if they're present.
            state.constraints.push(constraints.equal_pattern_types(
                num_type, // TODO check me if something breaks!
                expected,
                PatternCategory::Float,
                region,
            ));
        }

        StrLiteral(_) => {
            state.constraints.push(constraints.equal_pattern_types(
                builtins::str_type(),
                expected,
                PatternCategory::Str,
                region,
            ));
        }

        &SingleQuote(num_var, precision_var, _, bound) => {
            // First constraint on the free num var; this improves the resolved type quality in
            // case the bound is an alias.
            let num_type = builtins::add_numeric_bound_constr(
                constraints,
                &mut state.constraints,
                num_var,
                num_var,
                bound,
                region,
                Category::Int,
            );

            // Link the free num var with the int var and our expectation.
            let int_type = builtins::num_int(Type::Variable(precision_var));

            state.constraints.push(constraints.equal_types(
                num_type.clone(), // TODO check me if something breaks!
                Expected::NoExpectation(int_type),
                Category::Int,
                region,
            ));

            // Also constrain the pattern against the num var, again to reuse aliases if they're present.
            state.constraints.push(constraints.equal_pattern_types(
                num_type,
                expected,
                PatternCategory::Character,
                region,
            ));
        }

        RecordDestructure {
            whole_var,
            ext_var,
            destructs,
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
                let expected = PExpected::NoExpectation(pat_type.clone());

                if !state.headers.contains_key(symbol) {
                    state
                        .headers
                        .insert(*symbol, Loc::at(region, pat_type.clone()));
                }

                let field_type = match typ {
                    DestructType::Guard(guard_var, loc_guard) => {
                        state.constraints.push(constraints.pattern_presence(
                            Type::Variable(*guard_var),
                            PExpected::ForReason(
                                PReason::PatternGuard,
                                pat_type.clone(),
                                loc_guard.region,
                            ),
                            PatternCategory::PatternGuard,
                            region,
                        ));
                        state.vars.push(*guard_var);

                        constrain_pattern(
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
                        state.constraints.push(constraints.pattern_presence(
                            Type::Variable(*expr_var),
                            PExpected::ForReason(
                                PReason::OptionalField,
                                pat_type.clone(),
                                loc_expr.region,
                            ),
                            PatternCategory::PatternDefault,
                            region,
                        ));

                        state.vars.push(*expr_var);

                        let expr_expected = Expected::ForReason(
                            Reason::RecordDefaultField(label.clone()),
                            pat_type.clone(),
                            loc_expr.region,
                        );

                        let expr_con = constrain_expr(
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
                        // No extra constraints necessary.
                        RecordField::Demanded(pat_type)
                    }
                };

                field_types.insert(label.clone(), field_type);

                state.vars.push(*var);
            }

            let record_type = Type::Record(field_types, TypeExtension::from_type(ext_type));

            let whole_con = constraints.equal_types(
                Type::Variable(*whole_var),
                Expected::NoExpectation(record_type),
                Category::Storage(std::file!(), std::line!()),
                region,
            );

            let record_con = constraints.pattern_presence(
                Type::Variable(*whole_var),
                expected,
                PatternCategory::Record,
                region,
            );

            state.constraints.push(whole_con);
            state.constraints.push(record_con);
        }
        AppliedTag {
            whole_var,
            ext_var,
            tag_name,
            arguments,
        } => {
            let mut argument_types = Vec::with_capacity(arguments.len());
            for (index, (pattern_var, loc_pattern)) in arguments.iter().enumerate() {
                state.vars.push(*pattern_var);

                let pattern_type = Type::Variable(*pattern_var);
                argument_types.push(pattern_type.clone());

                let expected = PExpected::ForReason(
                    PReason::TagArg {
                        tag_name: tag_name.clone(),
                        index: HumanIndex::zero_based(index),
                    },
                    pattern_type,
                    region,
                );
                constrain_pattern(
                    constraints,
                    env,
                    &loc_pattern.value,
                    loc_pattern.region,
                    expected,
                    state,
                );
            }

            let pat_category = PatternCategory::Ctor(tag_name.clone());

            let whole_con = constraints.includes_tag(
                expected.clone().get_type(),
                tag_name.clone(),
                argument_types.clone(),
                pat_category.clone(),
                region,
            );

            let tag_con = constraints.pattern_presence(
                Type::Variable(*whole_var),
                expected,
                pat_category,
                region,
            );

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
            let arg_pattern_type = Type::Variable(*arg_pattern_var);

            let opaque_type = Type::Alias {
                symbol: *opaque,
                type_arguments: type_arguments
                    .iter()
                    .map(|v| OptAbleType {
                        typ: Type::Variable(v.var),
                        opt_abilities: v.opt_abilities.clone(),
                    })
                    .collect(),
                lambda_set_variables: lambda_set_variables.clone(),
                actual: Box::new(arg_pattern_type.clone()),
                kind: AliasKind::Opaque,
            };

            // First, add a constraint for the argument "who"
            let arg_pattern_expected = PExpected::NoExpectation(arg_pattern_type.clone());
            constrain_pattern(
                constraints,
                env,
                &loc_arg_pattern.value,
                loc_arg_pattern.region,
                arg_pattern_expected,
                state,
            );

            // Next, link `whole_var` to the opaque type of "@Id who"
            let whole_con = constraints.equal_types(
                Type::Variable(*whole_var),
                Expected::NoExpectation(opaque_type),
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
            let link_type_variables_con = constraints.pattern_presence(
                arg_pattern_type,
                PExpected::NoExpectation((**specialized_def_type).clone()),
                PatternCategory::Opaque(*opaque),
                loc_arg_pattern.region,
            );

            // Next, link `whole_var` (the type of "@Id who") to the expected type
            let opaque_pattern_con = constraints.pattern_presence(
                Type::Variable(*whole_var),
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

fn could_be_a_tag_union(typ: &Type) -> bool {
    !matches!(typ, Type::Apply(..) | Type::Function(..) | Type::Record(..))
}
