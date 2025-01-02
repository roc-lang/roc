use crate::ability::{
    resolve_ability_specialization, type_implementing_specialization, AbilityImplError,
    CheckedDerives, ObligationCache, PendingDerivesTable, Resolved,
};
use crate::deep_copy::deep_copy_var_in;
use crate::env::{DerivedEnv, InferenceEnv};
use crate::module::{SolveConfig, Solved};
use crate::pools::Pools;
use crate::specialize::{
    compact_lambda_sets_of_vars, AwaitingSpecializations, CompactionResult, SolvePhase,
};
use crate::to_var::{either_type_index_to_var, type_to_var};
use crate::Aliases;
use bumpalo::Bump;
use roc_can::abilities::{AbilitiesStore, MemberSpecializationInfo};
use roc_can::constraint::Constraint::{self, *};
use roc_can::constraint::{
    Cycle, FxCallConstraint, FxSuffixConstraint, FxSuffixKind, LetConstraint, OpportunisticResolve,
    TryTargetConstraint,
};
use roc_can::expected::{Expected, PExpected};
use roc_can::module::ModuleParams;
use roc_collections::{VecMap, VecSet};
use roc_debug_flags::dbg_do;
#[cfg(debug_assertions)]
use roc_debug_flags::ROC_VERIFY_RIGID_LET_GENERALIZED;
use roc_error_macros::internal_error;
use roc_module::ident::IdentSuffix;
use roc_module::symbol::{ModuleId, Symbol};
use roc_problem::can::CycleEntry;
use roc_region::all::{Loc, Region};
use roc_solve_problem::TypeError;
use roc_solve_schema::UnificationMode;
use roc_types::subs::{
    self, Content, FlatType, GetSubsSlice, Mark, OptVariable, Rank, Subs, TagExt, UlsOfVar,
    Variable,
};
use roc_types::types::{Category, Polarity, Reason, RecordField, Type, TypeExtension, Types, Uls};
use roc_unify::unify::{
    unify, unify_introduced_ability_specialization, Obligated, SpecializationLsetCollector,
    Unified::*,
};

mod scope;
pub use scope::Scope;

// Type checking system adapted from Elm by Evan Czaplicki, BSD-3-Clause Licensed
// https://github.com/elm/compiler
// Thank you, Evan!

// A lot of energy was put into making type inference fast. That means it's pretty intimidating.
//
// Fundamentally, type inference assigns very general types based on syntax, and then tries to
// make all the pieces fit together. For instance when writing
//
// > f x
//
// We know that `f` is a function, and thus must have some type `a -> b`.
// `x` is just a variable, that gets the type `c`
//
// Next comes constraint generation. For `f x` to be well-typed,
// it must be the case that `c = a`, So a constraint `Eq(c, a)` is generated.
// But `Eq` is a bit special: `c` does not need to equal `a` exactly, but they need to be equivalent.
// This allows for instance the use of aliases. `c` could be an alias, and so looks different from
// `a`, but they still represent the same type.
//
// Then we get to solving, which happens in this file.
//
// When we hit an `Eq` constraint, then we check whether the two involved types are in fact
// equivalent using unification, and when they are, we can substitute one for the other.
//
// When all constraints are processed, and no unification errors have occurred, then the program
// is type-correct. Otherwise the errors are reported.
//
// Now, coming back to efficiency, this type checker uses *ranks* to optimize
// The rank tracks the number of let-bindings a variable is "under". Top-level definitions
// have rank 1. A let in a top-level definition gets rank 2, and so on.
//
// This has to do with generalization of type variables. This is described here
//
//      http://okmij.org/ftp/ML/generalization.html#levels
//
// The problem is that when doing inference naively, this program would fail to typecheck
//
//  f =
//      id = \x -> x
//
//      { a: id 1, b: id "foo" }
//
// Because `id` is applied to an integer, the type `Int -> Int` is inferred, which then gives a
// type error for `id "foo"`.
//
// Thus instead the inferred type for `id` is generalized (see the `generalize` function) to `a -> a`.
// Ranks are used to limit the number of type variables considered for generalization. Only those inside
// of the let (so those used in inferring the type of `\x -> x`) are considered.

#[derive(Clone)]
struct State {
    scope: Scope,
    mark: Mark,
}

pub struct RunSolveOutput {
    pub solved: Solved<Subs>,
    pub scope: Scope,

    #[cfg(debug_assertions)]
    pub checkmate: Option<roc_checkmate::Collector>,
}

pub fn run(
    config: SolveConfig,
    problems: &mut Vec<TypeError>,
    subs: Subs,
    aliases: &mut Aliases,
    abilities_store: &mut AbilitiesStore,
) -> RunSolveOutput {
    run_help(config, problems, subs, aliases, abilities_store)
}

fn run_help(
    config: SolveConfig,
    problems: &mut Vec<TypeError>,
    mut owned_subs: Subs,
    aliases: &mut Aliases,
    abilities_store: &mut AbilitiesStore,
) -> RunSolveOutput {
    let subs = &mut owned_subs;
    let SolveConfig {
        home: _,
        constraints,
        root_constraint,
        mut types,
        pending_derives,
        exposed_by_module,
        derived_module,
        function_kind,
        module_params,
        module_params_vars,
        host_exposed_symbols,
        ..
    } = config;

    let mut pools = Pools::default();

    let rank = Rank::toplevel();
    let arena = Bump::new();

    let mut obligation_cache = ObligationCache::default();
    let mut awaiting_specializations = AwaitingSpecializations::default();

    let derived_env = DerivedEnv {
        derived_module: &derived_module,
        exposed_types: exposed_by_module,
    };

    let mut env = InferenceEnv {
        arena: &arena,
        constraints,
        function_kind,
        derived_env: &derived_env,
        subs,
        pools: &mut pools,
        #[cfg(debug_assertions)]
        checkmate: config.checkmate,
    };

    let pending_derives = PendingDerivesTable::new(
        &mut env,
        &mut types,
        aliases,
        pending_derives,
        problems,
        abilities_store,
        &mut obligation_cache,
    );
    let CheckedDerives {
        legal_derives: _,
        problems: derives_problems,
    } = obligation_cache.check_derives(env.subs, abilities_store, pending_derives);
    problems.extend(derives_problems);

    let state = solve(
        &mut env,
        types,
        rank,
        problems,
        aliases,
        &root_constraint,
        abilities_store,
        &mut obligation_cache,
        &mut awaiting_specializations,
        module_params,
        module_params_vars,
        host_exposed_symbols,
    );

    RunSolveOutput {
        scope: state.scope,
        #[cfg(debug_assertions)]
        checkmate: env.checkmate,
        solved: Solved(owned_subs),
    }
}

#[derive(Debug)]
enum Work<'a> {
    Constraint {
        scope: &'a Scope,
        rank: Rank,
        constraint: &'a Constraint,
    },
    CheckForInfiniteTypes(LocalDefVarsVec<(Symbol, Loc<Variable>)>),
    CheckSuffixFx(LocalDefVarsVec<(Symbol, Loc<Variable>)>),
    /// The ret_con part of a let constraint that introduces rigid and/or flex variables
    ///
    /// These introduced variables must be generalized, hence this variant
    /// is more complex than `LetConNoVariables`.
    LetConIntroducesVariables {
        scope: &'a Scope,
        rank: Rank,
        let_con: &'a LetConstraint,

        /// The variables used to store imported types in the Subs.
        /// The `Contents` are copied from the source module, but to
        /// mimic `type_to_var`, we must add these variables to `Pools`
        /// at the correct rank
        pool_variables: &'a [Variable],
    },
}

fn solve(
    env: &mut InferenceEnv,
    mut can_types: Types,
    rank: Rank,
    problems: &mut Vec<TypeError>,
    aliases: &mut Aliases,
    constraint: &Constraint,
    abilities_store: &mut AbilitiesStore,
    obligation_cache: &mut ObligationCache,
    awaiting_specializations: &mut AwaitingSpecializations,
    module_params: Option<ModuleParams>,
    module_params_vars: VecMap<ModuleId, Variable>,
    host_exposed_symbols: Option<&VecSet<Symbol>>,
) -> State {
    let scope = Scope::new(module_params);

    let initial = Work::Constraint {
        scope: &scope.clone(),
        rank,
        constraint,
    };

    let mut stack = vec![initial];

    let mut state = State {
        scope,
        mark: Mark::NONE.next(),
    };

    while let Some(work_item) = stack.pop() {
        let (scope, rank, constraint) = match work_item {
            Work::Constraint {
                scope,
                rank,
                constraint,
            } => {
                // the default case; actually solve this constraint
                (scope, rank, constraint)
            }
            Work::CheckForInfiniteTypes(def_vars) => {
                // after a LetCon, we must check if any of the variables that we introduced
                // loop back to themselves after solving the ret_constraint
                for (symbol, loc_var) in def_vars.iter() {
                    check_for_infinite_type(env, problems, *symbol, *loc_var);
                }

                continue;
            }
            Work::LetConIntroducesVariables {
                scope,
                rank,
                let_con,
                pool_variables,
            } => {
                // NOTE be extremely careful with shadowing here
                let offset = let_con.defs_and_ret_constraint.index();
                let ret_constraint = &env.constraints.constraints[offset + 1];

                let mark = state.mark;
                let saved_scope = state.scope;

                let young_mark = mark;
                let visit_mark = young_mark.next();
                let final_mark = visit_mark.next();

                let intro_rank = if let_con.generalizable.0 {
                    rank.next()
                } else {
                    rank
                };

                // Add a variable for each def to local_def_vars.
                let local_def_vars = LocalDefVarsVec::from_def_types(
                    env,
                    intro_rank,
                    problems,
                    abilities_store,
                    obligation_cache,
                    &mut can_types,
                    aliases,
                    let_con.def_types,
                );

                // If the let-binding can be generalized, introduce all variables at the next rank;
                // those that persist at the next rank after rank-adjustment will be generalized.
                //
                // Otherwise, introduce all variables at the current rank; since none of them will
                // end up at the next rank, none will be generalized.
                if let_con.generalizable.0 {
                    env.pools.get_mut(rank.next()).extend(pool_variables);
                } else {
                    env.pools.get_mut(rank).extend(pool_variables);
                }

                debug_assert_eq!(
                    // Check that no variable ended up in a higher rank than the next rank.. that
                    // would mean we generalized one level more than we need to!
                    {
                        let offenders = env
                            .pools
                            .get(rank.next())
                            .iter()
                            .filter(|var| {
                                env.subs.get_rank(**var).into_usize() > rank.next().into_usize()
                            })
                            .collect::<Vec<_>>();

                        let result = offenders.len();

                        if result > 0 {
                            eprintln!("subs = {:?}", &env.subs);
                            eprintln!("offenders = {:?}", &offenders);
                            eprintln!("let_con.def_types = {:?}", &let_con.def_types);
                        }

                        result
                    },
                    0
                );

                // If the let-binding is eligible for generalization, it was solved at the
                // next rank. The variables introduced in the let-binding that are still at
                // that rank (intuitively, they did not "escape" into the lower level
                // before or after the let-binding) now get to be generalized.
                generalize(env, young_mark, visit_mark, rank.next());
                debug_assert!(env.pools.get(rank.next()).is_empty(), "variables left over in let-binding scope, but they should all be in a lower scope or generalized now");

                // check that things went well
                dbg_do!(ROC_VERIFY_RIGID_LET_GENERALIZED, {
                    let rigid_vars = &env.constraints[let_con.rigid_vars];

                    // NOTE the `subs.redundant` check does not come from elm.
                    // It's unclear whether this is a bug with our implementation
                    // (something is redundant that shouldn't be)
                    // or that it just never came up in elm.
                    let mut it = rigid_vars
                        .iter()
                        .filter(|loc_var| {
                            let var = loc_var.value;
                            !env.subs.redundant(var) && env.subs.get_rank(var) != Rank::GENERALIZED
                        })
                        .peekable();

                    if it.peek().is_some() {
                        let failing: Vec<_> = it.collect();
                        println!("Rigids {:?}", &rigid_vars);
                        println!("Failing {failing:?}");
                        debug_assert!(false);
                    }
                });

                let mut new_scope = scope.clone();
                for (symbol, loc_var) in local_def_vars.iter() {
                    check_ability_specialization(
                        env,
                        rank,
                        abilities_store,
                        obligation_cache,
                        awaiting_specializations,
                        problems,
                        *symbol,
                        *loc_var,
                    );

                    new_scope.insert_symbol_var_if_vacant(*symbol, loc_var.value);

                    // At the time of introduction, promote explicitly-effectful symbols.
                    promote_effectful_symbol(env, FxSuffixKind::Let(*symbol), loc_var.value);
                }

                // Note that this vars_by_symbol is the one returned by the
                // previous call to solve()
                let state_for_ret_con = State {
                    scope: saved_scope,
                    mark: final_mark,
                };

                let next_work = [
                    // Check for infinite types first
                    Work::CheckForInfiniteTypes(local_def_vars.clone()),
                    // Now solve the body, using the new vars_by_symbol which includes
                    // the assignments' name-to-variable mappings.
                    Work::Constraint {
                        scope: env.arena.alloc(new_scope),
                        rank,
                        constraint: ret_constraint,
                    },
                    // Finally, check the suffix fx, after we have solved all types.
                    Work::CheckSuffixFx(local_def_vars),
                ];

                for work in next_work.into_iter().rev() {
                    stack.push(work);
                }

                state = state_for_ret_con;

                continue;
            }

            Work::CheckSuffixFx(local_def_vars) => {
                for (symbol, loc_var) in local_def_vars.iter() {
                    solve_suffix_fx(
                        env,
                        problems,
                        host_exposed_symbols,
                        FxSuffixKind::Let(*symbol),
                        loc_var.value,
                        &loc_var.region,
                    );
                }
                continue;
            }
        };

        state = match constraint {
            True => state,
            SaveTheEnvironment => {
                let mut copy = state;

                copy.scope = scope.clone();

                copy
            }
            Eq(roc_can::constraint::Eq(type_index, expectation_index, category_index, region)) => {
                let category = &env.constraints.categories[category_index.index()];

                let actual = either_type_index_to_var(
                    env,
                    rank,
                    problems,
                    abilities_store,
                    obligation_cache,
                    &mut can_types,
                    aliases,
                    *type_index,
                );

                let expectation = &env.constraints.expectations[expectation_index.index()];
                let expected = either_type_index_to_var(
                    env,
                    rank,
                    problems,
                    abilities_store,
                    obligation_cache,
                    &mut can_types,
                    aliases,
                    *expectation.get_type_ref(),
                );

                match unify(
                    &mut env.uenv(),
                    actual,
                    expected,
                    UnificationMode::EQ,
                    Polarity::OF_VALUE,
                ) {
                    Success {
                        vars,
                        must_implement_ability,
                        lambda_sets_to_specialize,
                        extra_metadata: _,
                    } => {
                        env.introduce(rank, &vars);

                        if !must_implement_ability.is_empty() {
                            let new_problems = obligation_cache.check_obligations(
                                env.subs,
                                abilities_store,
                                must_implement_ability,
                                AbilityImplError::BadExpr(*region, category.clone(), actual),
                            );
                            problems.extend(new_problems);
                        }
                        compact_lambdas_and_check_obligations(
                            env,
                            problems,
                            abilities_store,
                            obligation_cache,
                            awaiting_specializations,
                            lambda_sets_to_specialize,
                        );

                        state
                    }
                    Failure(vars, actual_type, expected_type, _bad_impls) => {
                        env.introduce(rank, &vars);

                        let problem = TypeError::BadExpr(
                            *region,
                            category.clone(),
                            actual_type,
                            expectation.replace_ref(expected_type),
                        );

                        problems.push(problem);

                        state
                    }
                }
            }
            Store(source_index, target, _filename, _linenr) => {
                // a special version of Eq that is used to store types in the AST.
                // IT DOES NOT REPORT ERRORS!
                let actual = either_type_index_to_var(
                    env,
                    rank,
                    &mut vec![], // don't report any extra errors
                    abilities_store,
                    obligation_cache,
                    &mut can_types,
                    aliases,
                    *source_index,
                );

                let actual_desc = env.subs.get(actual);
                env.subs.union(*target, actual, actual_desc);
                state
            }
            Lookup(symbol, expectation_index, region) => {
                match scope.get_var_by_symbol(symbol) {
                    Some(var) => {
                        // Deep copy the vars associated with this symbol before unifying them.
                        // Otherwise, suppose we have this:
                        //
                        // identity = \a -> a
                        //
                        // x = identity 5
                        //
                        // When we call (identity 5), it's important that we not unify
                        // on identity's original vars. If we do, the type of `identity` will be
                        // mutated to be `Int -> Int` instead of `a -> `, which would be incorrect;
                        // the type of `identity` is more general than that!
                        //
                        // Instead, we want to unify on a *copy* of its vars. If the copy unifies
                        // successfully (in this case, to `Int -> Int`), we can use that to
                        // infer the type of this lookup (in this case, `Int`) without ever
                        // having mutated the original.
                        //
                        // If this Lookup is targeting a value in another module,
                        // then we copy from that module's Subs into our own. If the value
                        // is being looked up in this module, then we use our Subs as both
                        // the source and destination.
                        let actual = {
                            let mut solve_env = env.as_solve_env();
                            let solve_env = &mut solve_env;
                            deep_copy_var_in(solve_env, rank, var, solve_env.arena)
                        };
                        let expectation = &env.constraints.expectations[expectation_index.index()];

                        let expected = either_type_index_to_var(
                            env,
                            rank,
                            problems,
                            abilities_store,
                            obligation_cache,
                            &mut can_types,
                            aliases,
                            *expectation.get_type_ref(),
                        );

                        match unify(
                            &mut env.uenv(),
                            actual,
                            expected,
                            UnificationMode::EQ,
                            Polarity::OF_VALUE,
                        ) {
                            Success {
                                vars,
                                must_implement_ability,
                                lambda_sets_to_specialize,
                                extra_metadata: _,
                            } => {
                                env.introduce(rank, &vars);

                                if !must_implement_ability.is_empty() {
                                    let new_problems = obligation_cache.check_obligations(
                                        env.subs,
                                        abilities_store,
                                        must_implement_ability,
                                        AbilityImplError::BadExpr(
                                            *region,
                                            Category::Lookup(*symbol),
                                            actual,
                                        ),
                                    );
                                    problems.extend(new_problems);
                                }
                                compact_lambdas_and_check_obligations(
                                    env,
                                    problems,
                                    abilities_store,
                                    obligation_cache,
                                    awaiting_specializations,
                                    lambda_sets_to_specialize,
                                );

                                state
                            }

                            Failure(vars, actual_type, expected_type, _bad_impls) => {
                                env.introduce(rank, &vars);

                                let problem = TypeError::BadExpr(
                                    *region,
                                    Category::Lookup(*symbol),
                                    actual_type,
                                    expectation.replace_ref(expected_type),
                                );

                                problems.push(problem);

                                state
                            }
                        }
                    }
                    None => {
                        problems.push(TypeError::UnexposedLookup(*region, *symbol));

                        state
                    }
                }
            }
            And(slice) => {
                let it = env.constraints.constraints[slice.indices()].iter().rev();
                for sub_constraint in it {
                    stack.push(Work::Constraint {
                        scope,
                        rank,
                        constraint: sub_constraint,
                    })
                }

                state
            }
            Pattern(type_index, expectation_index, category_index, region)
            | PatternPresence(type_index, expectation_index, category_index, region) => {
                let category = &env.constraints.pattern_categories[category_index.index()];

                let actual = either_type_index_to_var(
                    env,
                    rank,
                    problems,
                    abilities_store,
                    obligation_cache,
                    &mut can_types,
                    aliases,
                    *type_index,
                );

                let expectation = &env.constraints.pattern_expectations[expectation_index.index()];
                let expected = either_type_index_to_var(
                    env,
                    rank,
                    problems,
                    abilities_store,
                    obligation_cache,
                    &mut can_types,
                    aliases,
                    *expectation.get_type_ref(),
                );

                let mode = match constraint {
                    PatternPresence(..) => UnificationMode::PRESENT,
                    _ => UnificationMode::EQ,
                };

                match unify(
                    &mut env.uenv(),
                    actual,
                    expected,
                    mode,
                    Polarity::OF_PATTERN,
                ) {
                    Success {
                        vars,
                        must_implement_ability,
                        lambda_sets_to_specialize,
                        extra_metadata: _,
                    } => {
                        env.introduce(rank, &vars);

                        if !must_implement_ability.is_empty() {
                            let new_problems = obligation_cache.check_obligations(
                                env.subs,
                                abilities_store,
                                must_implement_ability,
                                AbilityImplError::BadPattern(*region, category.clone(), actual),
                            );
                            problems.extend(new_problems);
                        }
                        compact_lambdas_and_check_obligations(
                            env,
                            problems,
                            abilities_store,
                            obligation_cache,
                            awaiting_specializations,
                            lambda_sets_to_specialize,
                        );

                        state
                    }
                    Failure(vars, actual_type, expected_type, _bad_impls) => {
                        env.introduce(rank, &vars);

                        let problem = TypeError::BadPattern(
                            *region,
                            category.clone(),
                            actual_type,
                            expectation.replace_ref(expected_type),
                        );

                        problems.push(problem);

                        state
                    }
                }
            }
            FxCall(index) => {
                let FxCallConstraint {
                    call_fx_var,
                    call_kind,
                    call_region,
                    expectation,
                } = &env.constraints.fx_call_constraints[index.index()];

                let actual_desc = env.subs.get(*call_fx_var);

                match (actual_desc.content, expectation) {
                    (Content::Pure, _) | (Content::FlexVar(_), _) | (Content::Error, _) => state,
                    (Content::Effectful, None) => {
                        let problem = TypeError::FxInTopLevel(*call_region, *call_kind);
                        problems.push(problem);
                        state
                    }
                    (Content::Effectful, Some(expectation)) => {
                        match env.subs.get_content_without_compacting(expectation.fx_var) {
                            Content::Effectful | Content::Error => state,
                            Content::FlexVar(_) => {
                                env.subs
                                    .union(expectation.fx_var, *call_fx_var, actual_desc);
                                state
                            }
                            Content::Pure => {
                                let problem = TypeError::FxInPureFunction(
                                    *call_region,
                                    *call_kind,
                                    expectation.ann_region,
                                );
                                problems.push(problem);
                                state
                            }
                            expected_content => {
                                internal_error!(
                                    "CallFx: unexpected content: {:?}",
                                    expected_content
                                )
                            }
                        }
                    }
                    actual_content => {
                        internal_error!("CallFx: unexpected content: {:?}", actual_content)
                    }
                }
            }
            FxSuffix(constraint_index) => {
                let FxSuffixConstraint {
                    type_index,
                    kind,
                    region,
                } = &env.constraints.fx_suffix_constraints[constraint_index.index()];

                let actual = either_type_index_to_var(
                    env,
                    rank,
                    problems,
                    abilities_store,
                    obligation_cache,
                    &mut can_types,
                    aliases,
                    *type_index,
                );

                solve_suffix_fx(env, problems, host_exposed_symbols, *kind, actual, region);
                state
            }
            ExpectEffectful(variable, reason, region) => {
                let content = env.subs.get_content_without_compacting(*variable);

                match content {
                    Content::Pure | Content::FlexVar(_) => {
                        let problem = TypeError::ExpectedEffectful(*region, *reason);
                        problems.push(problem);

                        state
                    }
                    Content::Effectful | Content::Error => state,
                    Content::RigidVar(_)
                    | Content::FlexAbleVar(_, _)
                    | Content::RigidAbleVar(_, _)
                    | Content::RecursionVar { .. }
                    | Content::LambdaSet(_)
                    | Content::ErasedLambda
                    | Content::Structure(_)
                    | Content::Alias(_, _, _, _)
                    | Content::RangedNumber(_) => {
                        internal_error!("ExpectEffectful: unexpected content: {:?}", content)
                    }
                }
            }
            FlexToPure(variable) => {
                let content = env.subs.get_content_without_compacting(*variable);

                match content {
                    Content::FlexVar(_) => {
                        let desc = env.subs.get(Variable::PURE);
                        env.subs.union(*variable, Variable::PURE, desc);

                        state
                    }
                    Content::Pure | Content::Effectful | Content::Error => state,
                    Content::RigidVar(_)
                    | Content::FlexAbleVar(_, _)
                    | Content::RigidAbleVar(_, _)
                    | Content::RecursionVar { .. }
                    | Content::LambdaSet(_)
                    | Content::ErasedLambda
                    | Content::Structure(_)
                    | Content::Alias(_, _, _, _)
                    | Content::RangedNumber(_) => {
                        internal_error!("FlexToPure: unexpected content: {:?}", content)
                    }
                }
            }
            TryTarget(index) => {
                let try_target_constraint = &env.constraints.try_target_constraints[index.index()];

                let TryTargetConstraint {
                    target_type_index,
                    ok_payload_var,
                    err_payload_var,
                    region,
                    kind,
                } = try_target_constraint;

                let target_actual = either_type_index_to_var(
                    env,
                    rank,
                    problems,
                    abilities_store,
                    obligation_cache,
                    &mut can_types,
                    aliases,
                    *target_type_index,
                );

                let wanted_result_ty = can_types.from_old_type(&Type::TagUnion(
                    vec![
                        ("Ok".into(), vec![Type::Variable(*ok_payload_var)]),
                        ("Err".into(), vec![Type::Variable(*err_payload_var)]),
                    ],
                    TypeExtension::Closed,
                ));
                let wanted_result_var = type_to_var(
                    env,
                    rank,
                    problems,
                    abilities_store,
                    obligation_cache,
                    &mut can_types,
                    aliases,
                    wanted_result_ty,
                );

                match unify(
                    &mut env.uenv(),
                    target_actual,
                    wanted_result_var,
                    UnificationMode::EQ,
                    Polarity::OF_VALUE,
                ) {
                    Success {
                        vars,
                        must_implement_ability,
                        lambda_sets_to_specialize,
                        extra_metadata: _,
                    } => {
                        env.introduce(rank, &vars);

                        if !must_implement_ability.is_empty() {
                            let new_problems = obligation_cache.check_obligations(
                                env.subs,
                                abilities_store,
                                must_implement_ability,
                                AbilityImplError::BadExpr(
                                    *region,
                                    Category::TryTarget,
                                    target_actual,
                                ),
                            );
                            problems.extend(new_problems);
                        }
                        compact_lambdas_and_check_obligations(
                            env,
                            problems,
                            abilities_store,
                            obligation_cache,
                            awaiting_specializations,
                            lambda_sets_to_specialize,
                        );

                        state
                    }
                    Failure(vars, actual_type, _expected_type, _bad_impls) => {
                        env.introduce(rank, &vars);

                        let problem = TypeError::InvalidTryTarget(*region, actual_type, *kind);

                        problems.push(problem);

                        state
                    }
                }
            }
            Let(index, pool_slice) => {
                let let_con = &env.constraints.let_constraints[index.index()];

                let offset = let_con.defs_and_ret_constraint.index();
                let defs_constraint = &env.constraints.constraints[offset];

                let flex_vars = &env.constraints.variables[let_con.flex_vars.indices()];
                let rigid_vars = &env.constraints[let_con.rigid_vars];

                let pool_variables = &env.constraints.variables[pool_slice.indices()];

                // If the let-binding is generalizable, work at the next rank (which will be
                // the rank at which introduced variables will become generalized, if they end up
                // staying there); otherwise, stay at the current level.
                let binding_rank = if let_con.generalizable.0 {
                    rank.next()
                } else {
                    rank
                };

                // determine the next pool
                if binding_rank.into_usize() < env.pools.len() {
                    // Nothing to do, we already accounted for the next rank, no need to
                    // adjust the pools
                } else {
                    // we should be off by one at this point
                    debug_assert_eq!(binding_rank.into_usize(), 1 + env.pools.len());
                    env.pools.extend_to(binding_rank.into_usize());
                }

                let pool: &mut Vec<Variable> = env.pools.get_mut(binding_rank);

                // Introduce the variables of this binding, and extend the pool at our binding
                // rank.
                for &var in rigid_vars.iter().map(|v| &v.value).chain(flex_vars.iter()) {
                    env.subs.set_rank(var, binding_rank);
                }
                pool.reserve(rigid_vars.len() + flex_vars.len());
                pool.extend(rigid_vars.iter().map(|v| &v.value));
                pool.extend(flex_vars.iter());

                // Now, run our binding constraint, generalize, then solve the rest of the
                // program.
                //
                // Items are popped from the stack in reverse order. That means that we'll
                // first solve the defs_constraint, and then (eventually) the ret_constraint.
                //
                // NB: LetCon gets the current scope's env and rank, not the env/rank from after solving the defs_constraint.
                // That's because the defs constraints will be solved in next_rank if it is eligible for generalization.
                // The LetCon will then generalize variables that are at a higher rank than the rank of the current scope.
                stack.push(Work::LetConIntroducesVariables {
                    scope,
                    rank,
                    let_con,
                    pool_variables,
                });
                stack.push(Work::Constraint {
                    scope,
                    rank: binding_rank,
                    constraint: defs_constraint,
                });

                state
            }
            IsOpenType(type_index) => {
                let actual = either_type_index_to_var(
                    env,
                    rank,
                    problems,
                    abilities_store,
                    obligation_cache,
                    &mut can_types,
                    aliases,
                    *type_index,
                );

                open_tag_union(env, actual);

                state
            }
            IncludesTag(index) => {
                let includes_tag = &env.constraints.includes_tags[index.index()];

                let roc_can::constraint::IncludesTag {
                    type_index,
                    tag_name,
                    types,
                    pattern_category,
                    region,
                } = includes_tag;

                let pattern_category =
                    &env.constraints.pattern_categories[pattern_category.index()];

                let actual = either_type_index_to_var(
                    env,
                    rank,
                    problems,
                    abilities_store,
                    obligation_cache,
                    &mut can_types,
                    aliases,
                    *type_index,
                );

                let payload_types = env.constraints.variables[types.indices()]
                    .iter()
                    .map(|v| Type::Variable(*v))
                    .collect();

                let tag_ty = can_types.from_old_type(&Type::TagUnion(
                    vec![(tag_name.clone(), payload_types)],
                    TypeExtension::Closed,
                ));
                let includes = type_to_var(
                    env,
                    rank,
                    problems,
                    abilities_store,
                    obligation_cache,
                    &mut can_types,
                    aliases,
                    tag_ty,
                );

                match unify(
                    &mut env.uenv(),
                    actual,
                    includes,
                    UnificationMode::PRESENT,
                    Polarity::OF_PATTERN,
                ) {
                    Success {
                        vars,
                        must_implement_ability,
                        lambda_sets_to_specialize,
                        extra_metadata: _,
                    } => {
                        env.introduce(rank, &vars);

                        if !must_implement_ability.is_empty() {
                            let new_problems = obligation_cache.check_obligations(
                                env.subs,
                                abilities_store,
                                must_implement_ability,
                                AbilityImplError::BadPattern(
                                    *region,
                                    pattern_category.clone(),
                                    actual,
                                ),
                            );
                            problems.extend(new_problems);
                        }
                        compact_lambdas_and_check_obligations(
                            env,
                            problems,
                            abilities_store,
                            obligation_cache,
                            awaiting_specializations,
                            lambda_sets_to_specialize,
                        );

                        state
                    }
                    Failure(vars, actual_type, expected_to_include_type, _bad_impls) => {
                        env.introduce(rank, &vars);

                        let problem = TypeError::BadPattern(
                            *region,
                            pattern_category.clone(),
                            expected_to_include_type,
                            PExpected::NoExpectation(actual_type),
                        );
                        problems.push(problem);

                        state
                    }
                }
            }
            &Exhaustive(eq, sketched_rows, context, exhaustive_mark) => {
                // A few cases:
                //  1. Either condition or branch types already have a type error. In this case just
                //     propagate it.
                //  2. Types are correct, but there are redundancies. In this case we want
                //     exhaustiveness checking to pull those out.
                //  3. Condition and branch types are "almost equal", that is one or the other is
                //     only missing a few more tags. In this case we want to run
                //     exhaustiveness checking both ways, to see which one is missing tags.
                //  4. Condition and branch types aren't "almost equal", this is just a normal type
                //     error.

                let (real_var, real_region, branches_var, category_and_expected) = match eq {
                    Ok(eq) => {
                        let roc_can::constraint::Eq(real_var, expected, category, real_region) =
                            env.constraints.eq[eq.index()];
                        let expected = &env.constraints.expectations[expected.index()];

                        (
                            real_var,
                            real_region,
                            *expected.get_type_ref(),
                            Ok((category, expected)),
                        )
                    }
                    Err(peq) => {
                        let roc_can::constraint::PatternEq(
                            real_var,
                            expected,
                            category,
                            real_region,
                        ) = env.constraints.pattern_eq[peq.index()];
                        let expected = &env.constraints.pattern_expectations[expected.index()];

                        (
                            real_var,
                            real_region,
                            *expected.get_type_ref(),
                            Err((category, expected)),
                        )
                    }
                };

                let real_var = either_type_index_to_var(
                    env,
                    rank,
                    problems,
                    abilities_store,
                    obligation_cache,
                    &mut can_types,
                    aliases,
                    real_var,
                );

                let branches_var = either_type_index_to_var(
                    env,
                    rank,
                    problems,
                    abilities_store,
                    obligation_cache,
                    &mut can_types,
                    aliases,
                    branches_var,
                );

                let cond_source_is_likely_positive_value = category_and_expected.is_ok();
                let cond_polarity = if cond_source_is_likely_positive_value {
                    Polarity::OF_VALUE
                } else {
                    Polarity::OF_PATTERN
                };

                let real_content = env.subs.get_content_without_compacting(real_var);
                let branches_content = env.subs.get_content_without_compacting(branches_var);
                let already_have_error = matches!(
                    (real_content, branches_content),
                    (Content::Error, _) | (_, Content::Error)
                );

                let snapshot = env.subs.snapshot();
                let unify_cond_and_patterns_outcome = unify(
                    &mut env.uenv(),
                    branches_var,
                    real_var,
                    UnificationMode::EQ,
                    cond_polarity,
                );

                let should_check_exhaustiveness;
                let has_unification_error =
                    !matches!(unify_cond_and_patterns_outcome, Success { .. });
                match unify_cond_and_patterns_outcome {
                    Success {
                        vars,
                        must_implement_ability,
                        lambda_sets_to_specialize,
                        extra_metadata: _,
                    } => {
                        env.subs.commit_snapshot(snapshot);

                        env.introduce(rank, &vars);

                        problems.extend(obligation_cache.check_obligations(
                            env.subs,
                            abilities_store,
                            must_implement_ability,
                            AbilityImplError::DoesNotImplement,
                        ));
                        compact_lambdas_and_check_obligations(
                            env,
                            problems,
                            abilities_store,
                            obligation_cache,
                            awaiting_specializations,
                            lambda_sets_to_specialize,
                        );

                        // Case 1: unify error types, but don't check exhaustiveness.
                        // Case 2: run exhaustiveness to check for redundant branches.
                        should_check_exhaustiveness = !already_have_error;
                    }
                    Failure(..) => {
                        // Rollback and check for almost-equality.
                        env.subs.rollback_to(snapshot);

                        let almost_eq_snapshot = env.subs.snapshot();
                        // TODO: turn this on for bidirectional exhaustiveness checking
                        // open_tag_union(subs, real_var);
                        open_tag_union(env, branches_var);
                        let almost_eq = matches!(
                            unify(
                                &mut env.uenv(),
                                real_var,
                                branches_var,
                                UnificationMode::EQ,
                                cond_polarity,
                            ),
                            Success { .. }
                        );

                        env.subs.rollback_to(almost_eq_snapshot);

                        if almost_eq {
                            // Case 3: almost equal, check exhaustiveness.
                            should_check_exhaustiveness = true;
                        } else {
                            // Case 4: incompatible types, report type error.
                            // Re-run first failed unification to get the type diff.
                            match unify(
                                &mut env.uenv(),
                                real_var,
                                branches_var,
                                UnificationMode::EQ,
                                cond_polarity,
                            ) {
                                Failure(vars, actual_type, expected_type, _bad_impls) => {
                                    env.introduce(rank, &vars);

                                    // Figure out the problem - it might be pattern or value
                                    // related.
                                    let problem = match category_and_expected {
                                        Ok((category, expected)) => {
                                            let real_category = env.constraints.categories
                                                [category.index()]
                                            .clone();
                                            TypeError::BadExpr(
                                                real_region,
                                                real_category,
                                                actual_type,
                                                expected.replace_ref(expected_type),
                                            )
                                        }

                                        Err((category, expected)) => {
                                            let real_category = env.constraints.pattern_categories
                                                [category.index()]
                                            .clone();
                                            TypeError::BadPattern(
                                                real_region,
                                                real_category,
                                                expected_type,
                                                expected.replace_ref(actual_type),
                                            )
                                        }
                                    };

                                    problems.push(problem);
                                    should_check_exhaustiveness = false;
                                }
                                _ => internal_error!("Must be failure"),
                            }
                        }
                    }
                }

                let sketched_rows = env.constraints.sketched_rows[sketched_rows.index()].clone();

                if should_check_exhaustiveness {
                    use roc_can::exhaustive::{check, ExhaustiveSummary};

                    // If the condition type likely comes from an positive-position value (e.g. a
                    // literal or a return type), rather than an input position, we employ the
                    // heuristic that the positive-position value would only need to be open if the
                    // branches of the `when` constrained them as open. To avoid suggesting
                    // catch-all branches, now mark the condition type as closed, so that we only
                    // show the variants that explicitly not matched.
                    //
                    // We avoid this heuristic if the condition type likely comes from a negative
                    // position, e.g. a function parameter, since in that case if the condition
                    // type is open, we definitely want to show the catch-all branch as necessary.
                    //
                    // For example:
                    //
                    //   x : [A, B, C]
                    //
                    //   when x is
                    //      A -> ..
                    //      B -> ..
                    //
                    // This is checked as "almost equal" and hence exhaustiveness-checked with
                    // [A, B] compared to [A, B, C]*. However, we really want to compare against
                    // [A, B, C] (notice the closed union), so we optimistically close the
                    // condition type here.
                    //
                    // On the other hand, in a case like
                    //
                    //   f : [A, B, C]* -> ..
                    //   f = \x -> when x is
                    //     A -> ..
                    //     B -> ..
                    //
                    // we want to show `C` and/or `_` as necessary branches, so this heuristic is
                    // not applied.
                    //
                    // In the above case, notice it would not be safe to apply this heuristic if
                    // `C` was matched as well. Since the positive/negative value determination is
                    // only an estimate, we also only apply this heursitic in the "almost equal"
                    // case, when there was in fact a unification error.
                    //
                    // TODO: this can likely be removed after remodelling tag extension types
                    // (#4440).
                    if cond_source_is_likely_positive_value && has_unification_error {
                        close_pattern_matched_tag_unions(env.subs, real_var);
                    }

                    if let Ok(ExhaustiveSummary {
                        errors,
                        exhaustive,
                        redundancies,
                    }) = check(env.subs, real_var, sketched_rows, context)
                    {
                        // Store information about whether the "when" is exhaustive, and
                        // which (if any) of its branches are redundant. Codegen may use
                        // this for branch-fixing and redundant elimination.
                        if !exhaustive {
                            exhaustive_mark.set_non_exhaustive(env.subs);
                        }
                        for redundant_mark in redundancies {
                            redundant_mark.set_redundant(env.subs);
                        }

                        // Store the errors.
                        problems.extend(errors.into_iter().map(TypeError::Exhaustive));
                    } else {
                        // Otherwise there were type errors deeper in the pattern; we will have
                        // already reported them.
                    }
                }

                state
            }
            &Resolve(OpportunisticResolve {
                specialization_variable,
                member,
                specialization_id,
            }) => {
                if let Ok(Resolved::Specialization(specialization)) = resolve_ability_specialization(
                    env.subs,
                    abilities_store,
                    member,
                    specialization_variable,
                ) {
                    abilities_store.insert_resolved(specialization_id, specialization);
                }

                state
            }
            CheckCycle(cycle, cycle_mark) => {
                let Cycle {
                    def_names,
                    expr_regions,
                } = &env.constraints.cycles[cycle.index()];
                let symbols = &env.constraints.loc_symbols[def_names.indices()];

                // If the type of a symbol is not a function, that's an error.
                // Roc is strict, so only functions can be mutually recursive.
                let any_is_bad = {
                    use Content::*;

                    symbols.iter().any(|(s, _)| {
                        let var = scope.get_var_by_symbol(s).expect("Symbol not solved!");
                        let (_, underlying_content) = chase_alias_content(env.subs, var);

                        !matches!(underlying_content, Error | Structure(FlatType::Func(..)))
                    })
                };

                if any_is_bad {
                    // expr regions are stored in loc_symbols (that turned out to be convenient).
                    // The symbol is just a dummy, and should not be used
                    let expr_regions = &env.constraints.loc_symbols[expr_regions.indices()];

                    let cycle = symbols
                        .iter()
                        .zip(expr_regions.iter())
                        .map(|(&(symbol, symbol_region), &(_, expr_region))| CycleEntry {
                            symbol,
                            symbol_region,
                            expr_region,
                        })
                        .collect();

                    problems.push(TypeError::CircularDef(cycle));

                    cycle_mark.set_illegal(env.subs);
                }

                state
            }
            IngestedFile(type_index, file_path, bytes) => {
                let actual = either_type_index_to_var(
                    env,
                    rank,
                    problems,
                    abilities_store,
                    obligation_cache,
                    &mut can_types,
                    aliases,
                    *type_index,
                );

                let snapshot = env.subs.snapshot();
                if let Success {
                    vars,
                    must_implement_ability,
                    lambda_sets_to_specialize,
                    extra_metadata: _,
                } = unify(
                    &mut env.uenv(),
                    actual,
                    Variable::LIST_U8,
                    UnificationMode::EQ,
                    Polarity::OF_VALUE,
                ) {
                    // List U8 always valid.
                    env.introduce(rank, &vars);

                    debug_assert!(
                        must_implement_ability.is_empty() && lambda_sets_to_specialize.is_empty(),
                        "List U8 will never need to implement abilities or specialize lambda sets"
                    );

                    state
                } else {
                    env.subs.rollback_to(snapshot);

                    // We explicitly match on the last unify to get the type in the case it errors.
                    match unify(
                        &mut env.uenv(),
                        actual,
                        Variable::STR,
                        UnificationMode::EQ,
                        Polarity::OF_VALUE,
                    ) {
                        Success {
                            vars,
                            must_implement_ability,
                            lambda_sets_to_specialize,
                            extra_metadata: _,
                        } => {
                            env.introduce(rank, &vars);

                            debug_assert!(
                                must_implement_ability.is_empty() && lambda_sets_to_specialize.is_empty(),
                                "Str will never need to implement abilities or specialize lambda sets"
                            );

                            // Str only valid if valid utf8.
                            if let Err(err) = std::str::from_utf8(bytes) {
                                let problem =
                                    TypeError::IngestedFileBadUtf8(file_path.clone(), err);
                                problems.push(problem);
                            }

                            state
                        }
                        Failure(vars, actual_type, _, _) => {
                            env.introduce(rank, &vars);

                            let problem = TypeError::IngestedFileUnsupportedType(
                                file_path.clone(),
                                actual_type,
                            );
                            problems.push(problem);
                            state
                        }
                    }
                }
            }
            ImportParams(opt_provided, module_id, region) => {
                match (module_params_vars.get(module_id), opt_provided) {
                    (Some(expected_og), Some(provided)) => {
                        let actual = either_type_index_to_var(
                            env,
                            rank,
                            problems,
                            abilities_store,
                            obligation_cache,
                            &mut can_types,
                            aliases,
                            *provided,
                        );

                        let expected = {
                            // Similar to Lookup, we need to unify on a copy of the module params variable
                            // Otherwise, this import might make it less general than it really is
                            let mut solve_env = env.as_solve_env();
                            let solve_env = &mut solve_env;
                            deep_copy_var_in(solve_env, rank, *expected_og, solve_env.arena)
                        };

                        match unify(
                            &mut env.uenv(),
                            actual,
                            expected,
                            UnificationMode::EQ,
                            Polarity::OF_VALUE,
                        ) {
                            Success {
                                vars,
                                must_implement_ability,
                                lambda_sets_to_specialize,
                                extra_metadata: _,
                            } => {
                                env.introduce(rank, &vars);

                                problems.extend(obligation_cache.check_obligations(
                                    env.subs,
                                    abilities_store,
                                    must_implement_ability,
                                    AbilityImplError::DoesNotImplement,
                                ));
                                compact_lambdas_and_check_obligations(
                                    env,
                                    problems,
                                    abilities_store,
                                    obligation_cache,
                                    awaiting_specializations,
                                    lambda_sets_to_specialize,
                                );

                                state
                            }

                            Failure(vars, actual_type, expected_type, _) => {
                                env.introduce(rank, &vars);

                                problems.push(TypeError::ModuleParamsMismatch(
                                    *region,
                                    *module_id,
                                    actual_type,
                                    expected_type,
                                ));

                                state
                            }
                        }
                    }
                    (Some(expected), None) => {
                        let expected_type = env.uenv().var_to_error_type(*expected, Polarity::Neg);

                        problems.push(TypeError::MissingModuleParams(
                            *region,
                            *module_id,
                            expected_type,
                        ));

                        state
                    }
                    (None, Some(_)) => {
                        problems.push(TypeError::UnexpectedModuleParams(*region, *module_id));

                        state
                    }
                    (None, None) => state,
                }
            }
        };
    }

    state
}

fn solve_suffix_fx(
    env: &mut InferenceEnv<'_>,
    problems: &mut Vec<TypeError>,
    host_exposed_symbols: Option<&VecSet<Symbol>>,
    kind: FxSuffixKind,
    variable: Variable,
    region: &Region,
) {
    match kind.suffix() {
        IdentSuffix::None => {
            if let Content::Structure(FlatType::Func(_, _, _, fx)) =
                env.subs.get_content_without_compacting(variable)
            {
                let fx = *fx;
                match env.subs.get_content_without_compacting(fx) {
                    Content::Effectful => {
                        problems.push(TypeError::UnsuffixedEffectfulFunction(*region, kind));
                    }
                    Content::FlexVar(_) => {
                        env.subs.set_content(fx, Content::Pure);
                    }
                    _ => {}
                }
            }
        }
        IdentSuffix::Bang => match env.subs.get_content_without_compacting(variable) {
            Content::Structure(FlatType::Func(_, _, _, fx)) => {
                let fx = *fx;
                match env.subs.get_content_without_compacting(fx) {
                    Content::Pure => {
                        match (kind.symbol(), host_exposed_symbols) {
                            (Some(sym), Some(host_exposed)) if host_exposed.contains(sym) => {
                                // If exposed to the platform, it's allowed to be suffixed but pure
                                // The platform might require a `main!` function that could perform
                                // effects, but that's not a requirement.
                            }
                            _ => {
                                problems.push(TypeError::SuffixedPureFunction(*region, kind));
                            }
                        }
                    }
                    Content::FlexVar(_) => {
                        env.subs.set_content(fx, Content::Effectful);
                    }
                    _ => {}
                }
            }
            Content::FlexVar(_) => {
                env.subs
                    .set_content(variable, Content::Structure(FlatType::EffectfulFunc));
            }
            _ => {}
        },
    }
}

fn promote_effectful_symbol(env: &mut InferenceEnv<'_>, kind: FxSuffixKind, variable: Variable) {
    if kind.suffix() != IdentSuffix::Bang {
        return;
    }
    if !matches!(
        env.subs.get_content_without_compacting(variable),
        Content::FlexVar(_)
    ) {
        return;
    }
    env.subs
        .set_content(variable, Content::Structure(FlatType::EffectfulFunc));
}

fn chase_alias_content(subs: &Subs, mut var: Variable) -> (Variable, &Content) {
    loop {
        match subs.get_content_without_compacting(var) {
            Content::Alias(_, _, real_var, _) => {
                var = *real_var;
            }
            content => return (var, content),
        }
    }
}

fn compact_lambdas_and_check_obligations(
    env: &mut InferenceEnv,
    problems: &mut Vec<TypeError>,
    abilities_store: &mut AbilitiesStore,
    obligation_cache: &mut ObligationCache,
    awaiting_specialization: &mut AwaitingSpecializations,
    lambda_sets_to_specialize: UlsOfVar,
) {
    let CompactionResult {
        obligations,
        awaiting_specialization: new_awaiting,
    } = compact_lambda_sets_of_vars(
        &mut env.as_solve_env(),
        lambda_sets_to_specialize,
        &SolvePhase { abilities_store },
    );
    problems.extend(obligation_cache.check_obligations(
        env.subs,
        abilities_store,
        obligations,
        AbilityImplError::DoesNotImplement,
    ));
    awaiting_specialization.union(new_awaiting);
}

fn open_tag_union(env: &mut InferenceEnv, var: Variable) {
    let mut stack = vec![var];
    while let Some(var) = stack.pop() {
        use {Content::*, FlatType::*};

        let desc = env.subs.get(var);
        match desc.content {
            Structure(TagUnion(tags, ext)) => {
                if let Structure(EmptyTagUnion) = env.subs.get_content_without_compacting(ext.var())
                {
                    let new_ext_var = env.register(desc.rank, Content::FlexVar(None));

                    let new_union = Structure(TagUnion(tags, TagExt::Any(new_ext_var)));
                    env.subs.set_content(var, new_union);
                }

                // Also open up all nested tag unions.
                let all_vars = tags.variables().into_iter();
                stack.extend(
                    all_vars
                        .flat_map(|slice| env.subs[slice])
                        .map(|var| env.subs[var]),
                );
            }

            Structure(Record(fields, _)) => {
                // Open up all nested tag unions.
                stack.extend(env.subs.get_subs_slice(fields.variables()));
            }

            Structure(Tuple(elems, _)) => {
                // Open up all nested tag unions.
                stack.extend(env.subs.get_subs_slice(elems.variables()));
            }

            Structure(Apply(Symbol::LIST_LIST, args)) => {
                // Open up nested tag unions.
                stack.extend(env.subs.get_subs_slice(args));
            }

            _ => {
                // Everything else is not a structural type that can be opened
                // (i.e. cannot be matched in a pattern-match)
            }
        }

        // Today, an "open" constraint doesn't affect any types
        // other than tag unions. Recursive tag unions are constructed
        // at a later time (during occurs checks after tag unions are
        // resolved), so that's not handled here either.
    }
}

/// Optimistically closes the positive type of a value matched in a `when` statement, to produce
/// better exhaustiveness error messages.
///
/// This should only be applied if it's already known that a `when` expression is not exhaustive.
///
/// See [Constraint::Exhaustive].
fn close_pattern_matched_tag_unions(subs: &mut Subs, var: Variable) {
    let mut stack = vec![var];
    while let Some(var) = stack.pop() {
        use {Content::*, FlatType::*};

        let desc = subs.get(var);
        match desc.content {
            Structure(TagUnion(tags, mut ext)) => {
                // Close the extension, chasing it as far as it goes.
                loop {
                    match subs.get_content_without_compacting(ext.var()) {
                        Structure(FlatType::EmptyTagUnion) => {
                            break;
                        }
                        FlexVar(..) | FlexAbleVar(..) => {
                            subs.set_content_unchecked(
                                ext.var(),
                                Structure(FlatType::EmptyTagUnion),
                            );
                            break;
                        }
                        RigidVar(..) | RigidAbleVar(..) => {
                            // Don't touch rigids, they tell us more information than the heuristic
                            // of closing tag unions does for better exhaustiveness checking does.
                            break;
                        }
                        Structure(FlatType::TagUnion(_, deep_ext))
                        | Structure(FlatType::RecursiveTagUnion(_, _, deep_ext))
                        | Structure(FlatType::FunctionOrTagUnion(_, _, deep_ext)) => {
                            ext = *deep_ext;
                        }
                        other => internal_error!(
                            "not a tag union: {:?}",
                            roc_types::subs::SubsFmtContent(other, subs)
                        ),
                    }
                }

                // Also open up all nested tag unions.
                let all_vars = tags.variables().into_iter();
                stack.extend(all_vars.flat_map(|slice| subs[slice]).map(|var| subs[var]));
            }

            Structure(Record(fields, _)) => {
                // Close up all nested tag unions.
                stack.extend(subs.get_subs_slice(fields.variables()));
            }

            Structure(Apply(Symbol::LIST_LIST, args)) => {
                // Close up nested tag unions.
                stack.extend(subs.get_subs_slice(args));
            }

            Alias(_, _, real_var, _) => {
                stack.push(real_var);
            }

            _ => {
                // Everything else is not a type that can be opened/matched in a pattern match.
            }
        }

        // Recursive tag unions are constructed at a later time
        // (during occurs checks after tag unions are resolved),
        // so that's not handled here.
    }
}

/// If a symbol claims to specialize an ability member, check that its solved type in fact
/// does specialize the ability, and record the specialization.
// Aggressive but necessary - there aren't many usages.
#[inline(always)]
fn check_ability_specialization(
    env: &mut InferenceEnv,
    rank: Rank,
    abilities_store: &mut AbilitiesStore,
    obligation_cache: &mut ObligationCache,
    awaiting_specializations: &mut AwaitingSpecializations,
    problems: &mut Vec<TypeError>,
    symbol: Symbol,
    symbol_loc_var: Loc<Variable>,
) {
    // If the symbol specializes an ability member, we need to make sure that the
    // inferred type for the specialization actually aligns with the expected
    // implementation.
    if let Some((impl_key, root_data)) = abilities_store.impl_key_and_def(symbol) {
        let ability_member = impl_key.ability_member;
        let root_signature_var = root_data.signature_var();
        let parent_ability = root_data.parent_ability;

        // Check if they unify - if they don't, then the claimed specialization isn't really one,
        // and that's a type error!
        // This also fixes any latent type variables that need to be specialized to exactly what
        // the ability signature expects.

        // We need to freshly instantiate the root signature so that all unifications are reflected
        // in the specialization type, but not the original signature type.
        let root_signature_var = {
            let mut solve_env = env.as_solve_env();
            let solve_env = &mut solve_env;
            deep_copy_var_in(
                solve_env,
                Rank::toplevel(),
                root_signature_var,
                solve_env.arena,
            )
        };
        let snapshot = env.subs.snapshot();
        let unified = unify_introduced_ability_specialization(
            &mut env.uenv(),
            root_signature_var,
            symbol_loc_var.value,
            UnificationMode::EQ,
        );

        let resolved_mark = match unified {
            Success {
                vars,
                must_implement_ability,
                lambda_sets_to_specialize,
                extra_metadata: SpecializationLsetCollector(specialization_lambda_sets),
            } => {
                let specialization_type =
                    type_implementing_specialization(&must_implement_ability, parent_ability);

                match specialization_type {
                    Some(Obligated::Opaque(opaque)) => {
                        // This is a specialization for an opaque - but is it the opaque the
                        // specialization was claimed to be for?
                        if opaque == impl_key.opaque {
                            // It was! All is good.

                            env.subs.commit_snapshot(snapshot);
                            env.introduce(rank, &vars);

                            let specialization_lambda_sets = specialization_lambda_sets
                                .into_iter()
                                .map(|((symbol, region), var)| {
                                    debug_assert_eq!(symbol, ability_member);
                                    (region, var)
                                })
                                .collect();

                            compact_lambdas_and_check_obligations(
                                env,
                                problems,
                                abilities_store,
                                obligation_cache,
                                awaiting_specializations,
                                lambda_sets_to_specialize,
                            );

                            let specialization =
                                MemberSpecializationInfo::new(symbol, specialization_lambda_sets);

                            Ok(specialization)
                        } else {
                            // This def is not specialized for the claimed opaque type, that's an
                            // error.

                            // Commit so that the bad signature and its error persists in subs.
                            env.subs.commit_snapshot(snapshot);

                            let _typ = env
                                .subs
                                .var_to_error_type(symbol_loc_var.value, Polarity::OF_VALUE);

                            let problem = TypeError::WrongSpecialization {
                                region: symbol_loc_var.region,
                                ability_member: impl_key.ability_member,
                                expected_opaque: impl_key.opaque,
                                found_opaque: opaque,
                            };

                            problems.push(problem);

                            Err(())
                        }
                    }
                    Some(Obligated::Adhoc(var)) => {
                        // This is a specialization of a structural type - never allowed.

                        // Commit so that `var` persists in subs.
                        env.subs.commit_snapshot(snapshot);

                        let typ = env.subs.var_to_error_type(var, Polarity::OF_VALUE);

                        let problem = TypeError::StructuralSpecialization {
                            region: symbol_loc_var.region,
                            typ,
                            ability: parent_ability,
                            member: ability_member,
                        };

                        problems.push(problem);

                        Err(())
                    }
                    None => {
                        // This can happen when every ability constriant on a type variable went
                        // through only another type variable. That means this def is not specialized
                        // for one concrete type, and especially not our opaque - we won't admit this currently.

                        // Rollback the snapshot so we unlink the root signature with the specialization,
                        // so we can have two separate error types.
                        env.subs.rollback_to(snapshot);

                        let expected_type = env
                            .subs
                            .var_to_error_type(root_signature_var, Polarity::OF_VALUE);
                        let actual_type = env
                            .subs
                            .var_to_error_type(symbol_loc_var.value, Polarity::OF_VALUE);

                        let reason = Reason::GeneralizedAbilityMemberSpecialization {
                            member_name: ability_member,
                            def_region: root_data.region,
                        };

                        let problem = TypeError::BadExpr(
                            symbol_loc_var.region,
                            Category::AbilityMemberSpecialization(ability_member),
                            actual_type,
                            Expected::ForReason(reason, expected_type, symbol_loc_var.region),
                        );

                        problems.push(problem);

                        Err(())
                    }
                }
            }

            Failure(vars, expected_type, actual_type, unimplemented_abilities) => {
                env.subs.commit_snapshot(snapshot);
                env.introduce(rank, &vars);

                let reason = Reason::InvalidAbilityMemberSpecialization {
                    member_name: ability_member,
                    def_region: root_data.region,
                    unimplemented_abilities,
                };

                let problem = TypeError::BadExpr(
                    symbol_loc_var.region,
                    Category::AbilityMemberSpecialization(ability_member),
                    actual_type,
                    Expected::ForReason(reason, expected_type, symbol_loc_var.region),
                );

                problems.push(problem);

                Err(())
            }
        };

        abilities_store
            .mark_implementation(impl_key, resolved_mark)
            .expect("marked as a custom implementation, but not recorded as such");

        // Get the lambda sets that are ready for specialization because this ability member
        // specialization was resolved, and compact them.
        let new_lambda_sets_to_specialize =
            awaiting_specializations.remove_for_specialized(env.subs, impl_key);
        compact_lambdas_and_check_obligations(
            env,
            problems,
            abilities_store,
            obligation_cache,
            awaiting_specializations,
            new_lambda_sets_to_specialize,
        );
        debug_assert!(
            !awaiting_specializations.waiting_for(impl_key),
            "still have lambda sets waiting for {impl_key:?}, but it was just resolved"
        );
    }
}

#[derive(Debug, Clone)]
enum LocalDefVarsVec<T> {
    Stack(arrayvec::ArrayVec<T, 32>),
    Heap(Vec<T>),
}

impl<T> LocalDefVarsVec<T> {
    #[inline(always)]
    fn with_length(length: usize) -> Self {
        if length <= 32 {
            Self::Stack(Default::default())
        } else {
            Self::Heap(Default::default())
        }
    }

    fn push(&mut self, element: T) {
        match self {
            LocalDefVarsVec::Stack(vec) => vec.push(element),
            LocalDefVarsVec::Heap(vec) => vec.push(element),
        }
    }

    fn iter(&self) -> impl Iterator<Item = &T> {
        match self {
            LocalDefVarsVec::Stack(vec) => vec.iter(),
            LocalDefVarsVec::Heap(vec) => vec.iter(),
        }
    }
}

impl LocalDefVarsVec<(Symbol, Loc<Variable>)> {
    fn from_def_types(
        env: &mut InferenceEnv,
        rank: Rank,
        problems: &mut Vec<TypeError>,
        abilities_store: &mut AbilitiesStore,
        obligation_cache: &mut ObligationCache,
        types: &mut Types,
        aliases: &mut Aliases,
        def_types_slice: roc_can::constraint::DefTypes,
    ) -> Self {
        let type_indices_slice = &env.constraints.type_slices[def_types_slice.types.indices()];
        let loc_symbols_slice = &env.constraints.loc_symbols[def_types_slice.loc_symbols.indices()];

        let mut local_def_vars = Self::with_length(type_indices_slice.len());

        for (&(symbol, region), typ_index) in (loc_symbols_slice.iter()).zip(type_indices_slice) {
            let var = either_type_index_to_var(
                env,
                rank,
                problems,
                abilities_store,
                obligation_cache,
                types,
                aliases,
                *typ_index,
            );

            local_def_vars.push((symbol, Loc { value: var, region }));
        }

        local_def_vars
    }
}

fn check_for_infinite_type(
    env: &mut InferenceEnv,
    problems: &mut Vec<TypeError>,
    symbol: Symbol,
    loc_var: Loc<Variable>,
) {
    let var = loc_var.value;

    'next_occurs_check: while let Err((_, chain)) = env.subs.occurs(var) {
        // walk the chain till we find a tag union or lambda set, starting from the variable that
        // occurred recursively, which is always at the end of the chain.
        for &var in chain.iter().rev() {
            match *env.subs.get_content_without_compacting(var) {
                Content::Structure(FlatType::TagUnion(tags, ext_var)) => {
                    let rec_var = env.subs.mark_tag_union_recursive(var, tags, ext_var);
                    env.register_existing_var(rec_var);

                    continue 'next_occurs_check;
                }
                Content::LambdaSet(subs::LambdaSet {
                    solved,
                    recursion_var: OptVariable::NONE,
                    unspecialized,
                    ambient_function: ambient_function_var,
                }) => {
                    let rec_var = env.subs.mark_lambda_set_recursive(
                        var,
                        solved,
                        unspecialized,
                        ambient_function_var,
                    );
                    env.register_existing_var(rec_var);

                    continue 'next_occurs_check;
                }
                _ => { /* fall through */ }
            }
        }

        circular_error(env.subs, problems, symbol, &loc_var);
    }
}

fn circular_error(
    subs: &mut Subs,
    problems: &mut Vec<TypeError>,
    symbol: Symbol,
    loc_var: &Loc<Variable>,
) {
    let var = loc_var.value;
    let error_type = subs.var_to_error_type(var, Polarity::OF_VALUE);
    let problem = TypeError::CircularType(loc_var.region, symbol, error_type);

    subs.set_content(var, Content::Error);

    problems.push(problem);
}

/// Generalizes variables at the `young_rank`, which did not escape a let-binding
/// into a lower scope.
///
/// Ensures that variables introduced at the `young_rank`, but that should be
/// stuck at a lower level, are marked at that level and not generalized at the
/// present `young_rank`. See [adjust_rank].
fn generalize(env: &mut InferenceEnv, young_mark: Mark, visit_mark: Mark, young_rank: Rank) {
    let subs = &mut env.subs;
    let pools = &mut env.pools;

    let young_vars = std::mem::take(pools.get_mut(young_rank));
    let rank_table = pool_to_rank_table(subs, young_mark, young_rank, young_vars);

    // Get the ranks right for each entry.
    // Start at low ranks so we only have to pass over the information once.
    for (index, table) in rank_table.iter().enumerate() {
        for &var in table.iter() {
            adjust_rank(subs, young_mark, visit_mark, Rank::from(index), var);
        }
    }

    let (mut last_pool, all_but_last_pool) = rank_table.split_last();

    // For variables that have rank lowerer than young_rank, register them in
    // the appropriate old pool if they are not redundant.
    for vars in all_but_last_pool {
        for var in vars {
            let rank = subs.get_rank(var);

            pools.get_mut(rank).push(var);
        }
    }

    // For variables with rank young_rank, if rank < young_rank: register in old pool,
    // otherwise generalize
    for var in last_pool.drain(..) {
        let desc_rank = subs.get_rank(var);

        if desc_rank < young_rank {
            pools.get_mut(desc_rank).push(var);
        } else {
            subs.set_rank(var, Rank::GENERALIZED);
        }
    }

    // re-use the last_vector (which likely has a good capacity for future runs)
    debug_assert!(last_pool.is_empty());
    *pools.get_mut(young_rank) = last_pool;
}

/// Sort the variables into buckets by rank.
#[inline]
fn pool_to_rank_table(
    subs: &mut Subs,
    young_mark: Mark,
    young_rank: Rank,
    mut young_vars: Vec<Variable>,
) -> Pools {
    let mut pools = Pools::new(young_rank.into_usize() + 1);

    // the vast majority of young variables have young_rank
    let mut i = 0;
    while i < young_vars.len() {
        let var = subs.get_root_key(young_vars[i]);

        subs.set_mark_unchecked(var, young_mark);
        let rank = subs.get_rank_unchecked(var);

        if rank != young_rank {
            debug_assert!(rank.into_usize() < young_rank.into_usize() + 1);

            pools.get_mut(rank).push(var);

            // swap an element in; don't increment i
            young_vars.swap_remove(i);
        } else {
            i += 1;
        }
    }

    std::mem::swap(pools.get_mut(young_rank), &mut young_vars);

    pools
}

/// Adjust variable ranks such that ranks never increase as you move deeper.
/// This way the outermost rank is representative of the entire structure.
///
/// This procedure also catches type variables at a given rank that contain types at a higher rank.
/// In such cases, the contained types must be lowered to the rank of the outer type. This is
/// critical for soundness of the type inference; for example consider
///
/// ```ignore(illustrative)
/// \f ->              # rank=1
///     g = \x -> f x  # rank=2
///     g
/// ```
///
/// say that during the solving of the outer body at rank 1 we conditionally give `f` the type
/// `a -> b (rank=1)`. Without rank-adjustment, the type of `g` would be solved as `c -> d (rank=2)` for
/// some `c ~ a`, `d ~ b`, and hence would be generalized to the function `c -> d`, even though `c`
/// and `d` are individually at rank 1 after unfication with `a` and `b` respectively.
/// This is incorrect; the whole of `c -> d` must lie at rank 1, and only be generalized at the
/// level that `f` is introduced.
fn adjust_rank(
    subs: &mut Subs,
    young_mark: Mark,
    visit_mark: Mark,
    group_rank: Rank,
    var: Variable,
) -> Rank {
    let var = subs.get_root_key(var);

    let desc_rank = subs.get_rank_unchecked(var);
    let desc_mark = subs.get_mark_unchecked(var);

    if desc_mark == young_mark {
        let content = *subs.get_content_unchecked(var);

        // Mark the variable as visited before adjusting content, as it may be cyclic.
        subs.set_mark_unchecked(var, visit_mark);

        // Adjust the nested types' ranks, making sure that no nested unbound type variable is at a
        // higher rank than the group rank this `var` is at
        let max_rank = adjust_rank_content(subs, young_mark, visit_mark, group_rank, &content);

        subs.set_rank_unchecked(var, max_rank);
        subs.set_mark_unchecked(var, visit_mark);

        max_rank
    } else if desc_mark == visit_mark {
        // we have already visited this variable
        // (probably two variables had the same root)
        desc_rank
    } else {
        let min_rank = group_rank.min(desc_rank);

        // TODO from elm-compiler: how can min_rank ever be group_rank?
        subs.set_rank_unchecked(var, min_rank);
        subs.set_mark_unchecked(var, visit_mark);

        min_rank
    }
}

fn adjust_rank_content(
    subs: &mut Subs,
    young_mark: Mark,
    visit_mark: Mark,
    group_rank: Rank,
    content: &Content,
) -> Rank {
    use roc_types::subs::Content::*;
    use roc_types::subs::FlatType::*;

    match content {
        FlexVar(_) | RigidVar(_) | FlexAbleVar(_, _) | RigidAbleVar(_, _) | Error => group_rank,

        RecursionVar { .. } => group_rank,

        Structure(flat_type) => {
            match flat_type {
                Apply(_, args) => {
                    let mut rank = Rank::toplevel();

                    for var_index in args.into_iter() {
                        let var = subs[var_index];
                        rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
                    }

                    rank
                }

                Func(arg_vars, closure_var, ret_var, _fx_var) => {
                    let mut rank = adjust_rank(subs, young_mark, visit_mark, group_rank, *ret_var);

                    // TODO investigate further.
                    //
                    // My theory is that because the closure_var contains variables already
                    // contained in the signature only, it does not need to be part of the rank
                    // calculuation
                    if true {
                        rank = rank.max(adjust_rank(
                            subs,
                            young_mark,
                            visit_mark,
                            group_rank,
                            *closure_var,
                        ));
                    }

                    for index in arg_vars.into_iter() {
                        let var = subs[index];
                        rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
                    }

                    rank
                }

                EmptyRecord => {
                    // from elm-compiler: THEORY: an empty record never needs to get generalized
                    //
                    // But for us, that theory does not hold, because there might be type variables hidden
                    // inside a lambda set but not on the left or right of an arrow, and records should not
                    // force de-generalization in such cases.
                    //
                    // See https://github.com/roc-lang/roc/issues/3641 for a longer discussion and
                    // example.
                    group_rank
                }

                // THEORY: an empty tag never needs to get generalized
                EmptyTagUnion => Rank::toplevel(),

                EffectfulFunc => Rank::toplevel(),

                Record(fields, ext_var) => {
                    let mut rank = adjust_rank(subs, young_mark, visit_mark, group_rank, *ext_var);

                    for (_, var_index, field_index) in fields.iter_all() {
                        let var = subs[var_index];
                        rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));

                        // When generalizing annotations with rigid optional/required fields,
                        // we want to promote them to non-rigid, so that usages at
                        // specialized sites don't have to exactly include the optional/required field.
                        match subs[field_index] {
                            RecordField::RigidOptional(()) => {
                                subs[field_index] = RecordField::Optional(());
                            }
                            RecordField::RigidRequired(()) => {
                                subs[field_index] = RecordField::Required(());
                            }
                            _ => {}
                        }
                    }

                    rank
                }

                Tuple(elems, ext_var) => {
                    let mut rank = adjust_rank(subs, young_mark, visit_mark, group_rank, *ext_var);

                    for (_, var_index) in elems.iter_all() {
                        let var = subs[var_index];
                        rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
                    }

                    rank
                }

                TagUnion(tags, ext_var) => {
                    let mut rank =
                        adjust_rank(subs, young_mark, visit_mark, group_rank, ext_var.var());
                    // For performance reasons, we only keep one representation of empty tag unions
                    // in subs. That representation exists at rank 0, which we don't always want to
                    // reflect the whole tag union as, because doing so may over-generalize free
                    // type variables.
                    // Normally this is not a problem because of the loop below that maximizes the
                    // rank from nested types in the union. But suppose we have the simple tag
                    // union
                    //   [Z]{}
                    // there are no nested types in the tags, and the empty tag union is at rank 0,
                    // so we promote the tag union to rank 0. Now if we introduce the presence
                    // constraint
                    //   [Z]{} += [S a]
                    // we'll wind up with [Z, S a]{}, but it will be at rank 0, and "a" will get
                    // over-generalized. Really, the empty tag union should be introduced at
                    // whatever current group rank we're at, and so that's how we encode it here.
                    if ext_var.var() == Variable::EMPTY_TAG_UNION && rank.is_generalized() {
                        rank = group_rank;
                    }

                    for (_, index) in tags.iter_all() {
                        let slice = subs[index];
                        for var_index in slice {
                            let var = subs[var_index];
                            rank = rank
                                .max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
                        }
                    }

                    rank
                }

                FunctionOrTagUnion(_, _, ext_var) => {
                    adjust_rank(subs, young_mark, visit_mark, group_rank, ext_var.var())
                }

                RecursiveTagUnion(rec_var, tags, ext_var) => {
                    let mut rank =
                        adjust_rank(subs, young_mark, visit_mark, group_rank, ext_var.var());

                    for (_, index) in tags.iter_all() {
                        let slice = subs[index];
                        for var_index in slice {
                            let var = subs[var_index];
                            rank = rank
                                .max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
                        }
                    }

                    // The recursion var may have a higher rank than the tag union itself, if it is
                    // erroneous and escapes into a region where it is let-generalized before it is
                    // constrained back down to the rank it originated from.
                    //
                    // For example, see the `recursion_var_specialization_error` reporting test -
                    // there, we have
                    //
                    //      Job a : [Job (List (Job a)) a]
                    //
                    //      job : Job Str
                    //
                    //      when job is
                    //          Job lst _ -> lst == ""
                    //
                    // In this case, `lst` is generalized and has a higher rank for the type
                    // `(List (Job a)) as a` - notice that only the recursion var `a` is active
                    // here, not the entire recursive tag union. In the body of this branch, `lst`
                    // becomes a type error, but the nested recursion var `a` is left untouched,
                    // because it is nested under the of `lst`, not the surface type that becomes
                    // an error.
                    //
                    // Had this not become a type error, `lst` would then be constrained against
                    // `job`, and its rank would get pulled back down. So, this can only happen in
                    // the presence of type errors.
                    //
                    // In all other cases, the recursion var has the same rank as the tag union itself
                    // all types it uses are also in the tags already, so it cannot influence the
                    // rank.
                    if cfg!(debug_assertions)
                        && !matches!(
                            subs.get_content_without_compacting(*rec_var),
                            Content::Error | Content::FlexVar(..)
                        )
                    {
                        let rec_var_rank =
                            adjust_rank(subs, young_mark, visit_mark, group_rank, *rec_var);

                        debug_assert!(
                            rank >= rec_var_rank,
                            "rank was {:?} but recursion var <{:?}>{:?} has higher rank {:?}",
                            rank,
                            rec_var,
                            subs.get_content_without_compacting(*rec_var),
                            rec_var_rank
                        );
                    }

                    rank
                }
            }
        }

        Alias(_, args, real_var, _) => {
            let mut rank = Rank::toplevel();

            // Avoid visiting lambda set variables stored in the type variables of the alias
            // independently.
            //
            // Why? Lambda set variables on the alias are not truly type arguments to the alias,
            // and instead are links to the lambda sets that appear in functions under the real
            // type of the alias. If their ranks are adjusted independently, we end up looking at
            // function types "inside-out" - when the whole point of rank-adjustment is to look
            // from the outside-in to determine at what rank a type lies!
            //
            // So, just wait to adjust their ranks until we visit the function types that contain
            // them. If they should be generalized (or pulled to a lower rank) that will happen
            // then; otherwise, we risk generalizing a lambda set too early, when its enclosing
            // function type should not be.
            let adjustable_variables =
                (args.type_variables().into_iter()).chain(args.infer_ext_in_output_variables());

            for var_index in adjustable_variables {
                let var = subs[var_index];
                rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
            }

            // from elm-compiler: THEORY: anything in the real_var would be Rank::toplevel()
            // this theory is not true in Roc! aliases of function types capture the closure var
            rank = rank.max(adjust_rank(
                subs, young_mark, visit_mark, group_rank, *real_var,
            ));

            rank
        }

        LambdaSet(subs::LambdaSet {
            solved,
            recursion_var,
            unspecialized,
            ambient_function: ambient_function_var,
        }) => {
            let mut rank = group_rank;

            for (_, index) in solved.iter_all() {
                let slice = subs[index];
                for var_index in slice {
                    let var = subs[var_index];
                    rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
                }
            }

            for uls_index in *unspecialized {
                let Uls(var, _, _) = subs[uls_index];
                rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
            }

            if let (true, Some(rec_var)) = (cfg!(debug_assertions), recursion_var.into_variable()) {
                // THEORY: unlike the situation for recursion vars under recursive tag unions,
                // recursive vars inside lambda sets can't escape into higher let-generalized regions
                // because lambda sets aren't user-facing.
                //
                // So the recursion var should be fully accounted by everything else in the lambda set
                // (since it appears in the lambda set), and if the rank is higher, it's either a
                // bug or our theory is wrong and indeed they can escape into higher regions.
                let rec_var_rank = adjust_rank(subs, young_mark, visit_mark, group_rank, rec_var);

                debug_assert!(
                    rank >= rec_var_rank,
                    "rank was {:?} but recursion var <{:?}>{:?} has higher rank {:?}",
                    rank,
                    rec_var,
                    subs.get_content_without_compacting(rec_var),
                    rec_var_rank
                );
            }

            // NEVER TOUCH the ambient function var, it would already have been passed through.
            {
                let _ = ambient_function_var;
            }

            rank
        }

        ErasedLambda => group_rank,

        Pure | Effectful => group_rank,

        RangedNumber(_) => group_rank,
    }
}
