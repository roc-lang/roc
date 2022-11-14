use crate::expr::{constrain_def_make_constraint, constrain_def_pattern, Env};
use roc_can::abilities::{PendingAbilitiesStore, PendingMemberType};
use roc_can::constraint::{Constraint, Constraints};
use roc_can::expected::Expected;
use roc_can::expr::Declarations;
use roc_can::pattern::Pattern;
use roc_module::symbol::{ModuleId, Symbol};
use roc_region::all::{Loc, Region};
use roc_types::types::{AnnotationSource, Category, Type, Types};

pub fn constrain_module(
    types: &mut Types,
    constraints: &mut Constraints,
    symbols_from_requires: Vec<(Loc<Symbol>, Loc<Type>)>,
    abilities_store: &PendingAbilitiesStore,
    declarations: &Declarations,
    home: ModuleId,
) -> Constraint {
    let constraint = crate::expr::constrain_decls(types, constraints, home, declarations);
    let constraint = constrain_symbols_from_requires(
        types,
        constraints,
        symbols_from_requires,
        home,
        constraint,
    );
    let constraint =
        frontload_ability_constraints(types, constraints, abilities_store, home, constraint);

    // The module constraint should always save the environment at the end.
    debug_assert!(constraints.contains_save_the_environment(&constraint));

    constraint
}

fn constrain_symbols_from_requires(
    types: &mut Types,
    constraints: &mut Constraints,
    symbols_from_requires: Vec<(Loc<Symbol>, Loc<Type>)>,
    home: ModuleId,
    constraint: Constraint,
) -> Constraint {
    symbols_from_requires
        .into_iter()
        .fold(constraint, |constraint, (loc_symbol, loc_type)| {
            if loc_symbol.value.module_id() == home {
                // 1. Required symbols can only be specified in package modules
                // 2. Required symbols come from app modules
                // But, if we are running e.g. `roc check` on a package module, there is no app
                // module, and we will have instead put the required symbols in the package module
                // namespace. If this is the case, we want to introduce the symbols as if they had
                // the types they are annotated with.
                let rigids = Default::default();
                let mut env = Env {
                    home,
                    rigids,
                    resolutions_to_make: vec![],
                };
                let pattern = Loc::at_zero(roc_can::pattern::Pattern::Identifier(loc_symbol.value));

                let type_index = {
                    let typ = types.from_old_type(&loc_type.value);
                    constraints.push_type(types, typ)
                };
                let def_pattern_state =
                    constrain_def_pattern(types, constraints, &mut env, &pattern, type_index);

                debug_assert!(env.resolutions_to_make.is_empty());

                constrain_def_make_constraint(
                    constraints,
                    // No new rigids or flex vars because they are represented in the type
                    // annotation.
                    std::iter::empty(),
                    std::iter::empty(),
                    Constraint::True,
                    constraint,
                    def_pattern_state,
                )
            } else {
                // Otherwise, this symbol comes from an app module - we want to check that the type
                // provided by the app is in fact what the package module requires.
                let arity = loc_type.value.arity();
                let typ = loc_type.value;
                let type_index = {
                    let typ = types.from_old_type(&typ);
                    constraints.push_type(types, typ)
                };
                let expected = constraints.push_expected_type(Expected::FromAnnotation(
                    loc_symbol.map(|&s| Pattern::Identifier(s)),
                    arity,
                    AnnotationSource::RequiredSymbol {
                        region: loc_type.region,
                    },
                    type_index,
                ));
                let provided_eq_requires_constr =
                    constraints.lookup(loc_symbol.value, expected, loc_type.region);
                constraints.and_constraint([provided_eq_requires_constr, constraint])
            }
        })
}

pub fn frontload_ability_constraints(
    types: &mut Types,
    constraints: &mut Constraints,
    abilities_store: &PendingAbilitiesStore,
    home: ModuleId,
    mut constraint: Constraint,
) -> Constraint {
    for (member_name, member_data) in abilities_store.root_ability_members().iter() {
        if let PendingMemberType::Local {
            signature_var,
            variables: vars,
            signature,
        } = &member_data.typ
        {
            let signature_var = *signature_var;
            let rigids = Default::default();
            let mut env = Env {
                home,
                rigids,
                resolutions_to_make: vec![],
            };
            let pattern = Loc::at_zero(roc_can::pattern::Pattern::Identifier(*member_name));

            let signature_index = {
                let typ = types.from_old_type(&signature.clone());
                constraints.push_type(types, typ)
            };

            let mut def_pattern_state =
                constrain_def_pattern(types, constraints, &mut env, &pattern, signature_index);

            debug_assert!(env.resolutions_to_make.is_empty());

            def_pattern_state.vars.push(signature_var);

            let rigid_variables = vars.rigid_vars.iter().chain(vars.able_vars.iter()).copied();
            let infer_variables = vars.flex_vars.iter().copied();

            let signature_expectation =
                constraints.push_expected_type(Expected::NoExpectation(signature_index));

            def_pattern_state
                .constraints
                .push(constraints.equal_types_var(
                    signature_var,
                    signature_expectation,
                    Category::Storage(file!(), line!()),
                    Region::zero(),
                ));

            constraint = constrain_def_make_constraint(
                constraints,
                rigid_variables,
                infer_variables,
                Constraint::True,
                constraint,
                def_pattern_state,
            );
        }
    }
    constraint
}
