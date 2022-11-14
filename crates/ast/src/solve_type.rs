#![allow(clippy::all)]
#![allow(dead_code)]
use bumpalo::Bump;
use roc_can::expected::{Expected, PExpected};
use roc_collections::all::{BumpMap, BumpMapDefault, MutMap};
use roc_error_macros::internal_error;
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_solve::module::Solved;
use roc_types::subs::{
    self, AliasVariables, Content, Descriptor, FlatType, Mark, OptVariable, Rank, RecordFields,
    Subs, SubsSlice, UnionLambdas, UnionTags, Variable, VariableSubsSlice,
};
use roc_types::types::{
    gather_fields_unsorted_iter, Alias, AliasKind, Category, ErrorType, PatternCategory, Polarity,
    RecordField,
};
use roc_unify::unify::unify;
use roc_unify::unify::Env as UEnv;
use roc_unify::unify::Mode;
use roc_unify::unify::Unified::*;

use crate::constrain::{Constraint, PresenceConstraint};
use crate::lang::core::types::Type2;
use crate::mem_pool::pool::Pool;
use crate::mem_pool::pool_vec::PoolVec;
use crate::mem_pool::shallow_clone::ShallowClone;

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

#[derive(Debug, Clone)]
pub enum TypeError {
    BadExpr(Region, Category, ErrorType, Expected<ErrorType>),
    BadPattern(Region, PatternCategory, ErrorType, PExpected<ErrorType>),
    CircularType(Region, Symbol, ErrorType),
    UnexposedLookup(Symbol),
}

#[derive(Clone, Debug, Default)]
pub struct Env {
    pub vars_by_symbol: MutMap<Symbol, Variable>,
    pub aliases: MutMap<Symbol, Alias>,
}

const DEFAULT_POOLS: usize = 8;

#[derive(Clone, Debug)]
struct Pools(Vec<Vec<Variable>>);

impl Default for Pools {
    fn default() -> Self {
        Pools::new(DEFAULT_POOLS)
    }
}

impl Pools {
    pub fn new(num_pools: usize) -> Self {
        Pools(vec![Vec::new(); num_pools])
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get_mut(&mut self, rank: Rank) -> &mut Vec<Variable> {
        self.0
            .get_mut(rank.into_usize())
            .unwrap_or_else(|| panic!("Compiler bug: could not find pool at rank {}", rank))
    }

    pub fn get(&self, rank: Rank) -> &Vec<Variable> {
        self.0
            .get(rank.into_usize())
            .unwrap_or_else(|| panic!("Compiler bug: could not find pool at rank {}", rank))
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Vec<Variable>> {
        self.0.iter()
    }

    pub fn split_last(&self) -> (&Vec<Variable>, &[Vec<Variable>]) {
        self.0
            .split_last()
            .unwrap_or_else(|| panic!("Attempted to split_last() on non-empty Pools"))
    }

    pub fn extend_to(&mut self, n: usize) {
        for _ in self.len()..n {
            self.0.push(Vec::new());
        }
    }
}

#[derive(Clone)]
struct State {
    env: Env,
    mark: Mark,
}

pub fn run<'a>(
    arena: &'a Bump,
    mempool: &mut Pool,
    env: &Env,
    problems: &mut Vec<TypeError>,
    mut subs: Subs,
    constraint: &Constraint,
) -> (Solved<Subs>, Env) {
    let env = run_in_place(arena, mempool, env, problems, &mut subs, constraint);

    (Solved(subs), env)
}

/// Modify an existing subs in-place instead
pub fn run_in_place<'a>(
    arena: &'a Bump,
    mempool: &mut Pool,
    env: &Env,
    problems: &mut Vec<TypeError>,
    subs: &mut Subs,
    constraint: &Constraint,
) -> Env {
    let mut pools = Pools::default();
    let state = State {
        env: env.clone(),
        mark: Mark::NONE.next(),
    };
    let rank = Rank::toplevel();
    let state = solve(
        arena,
        mempool,
        env,
        state,
        rank,
        &mut pools,
        problems,
        &mut MutMap::default(),
        subs,
        constraint,
    );

    state.env
}

#[allow(clippy::too_many_arguments)]
fn solve<'a>(
    arena: &'a Bump,
    mempool: &mut Pool,
    env: &Env,
    state: State,
    rank: Rank,
    pools: &mut Pools,
    problems: &mut Vec<TypeError>,
    cached_aliases: &mut MutMap<Symbol, Variable>,
    subs: &mut Subs,
    constraint: &Constraint,
) -> State {
    use crate::solve_type::Constraint::*;

    match constraint {
        True => state,
        //        SaveTheEnvironment => {
        //            // NOTE deviation: elm only copies the env into the state on SaveTheEnvironment
        //            let mut copy = state;
        //
        //            copy.env = env.clone();
        //
        //            copy
        //        }
        Eq(typ, expectation, category, region) => {
            let actual = type_to_var(arena, mempool, subs, rank, pools, cached_aliases, typ);
            let expected = type_to_var(
                arena,
                mempool,
                subs,
                rank,
                pools,
                cached_aliases,
                expectation.get_type_ref(),
            );

            match unify(
                &mut UEnv::new(subs),
                actual,
                expected,
                Mode::EQ,
                Polarity::OF_VALUE,
            ) {
                Success {
                    vars,
                    must_implement_ability: _,
                    lambda_sets_to_specialize: _, // TODO ignored
                    extra_metadata: _,
                } => {
                    // TODO(abilities) record deferred ability checks
                    introduce(subs, rank, pools, &vars);

                    state
                }
                Failure(vars, actual_type, expected_type, _bad_impl) => {
                    introduce(subs, rank, pools, &vars);

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
        //        Store(source, target, _filename, _linenr) => {
        //            // a special version of Eq that is used to store types in the AST.
        //            // IT DOES NOT REPORT ERRORS!
        //            let actual = type_to_var(subs, rank, pools, cached_aliases, source);
        //            let target = *target;
        //
        //            match unify(subs, actual, target) {
        //                Success(vars) => {
        //                    introduce(subs, rank, pools, &vars);
        //
        //                    state
        //                }
        //                Failure(vars, _actual_type, _expected_type, _bad_impl) => {
        //                    introduce(subs, rank, pools, &vars);
        //
        //                    // ERROR NOT REPORTED
        //
        //                    state
        //                }
        //                BadType(vars, _problem) => {
        //                    introduce(subs, rank, pools, &vars);
        //
        //                    // ERROR NOT REPORTED
        //
        //                    state
        //                }
        //            }
        //        }
        Lookup(symbol, expectation, region) => {
            match env.vars_by_symbol.get(&symbol) {
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
                    let actual = deep_copy_var(subs, rank, pools, *var);

                    let expected = type_to_var(
                        arena,
                        mempool,
                        subs,
                        rank,
                        pools,
                        cached_aliases,
                        expectation.get_type_ref(),
                    );

                    match unify(
                        &mut UEnv::new(subs),
                        actual,
                        expected,
                        Mode::EQ,
                        Polarity::OF_VALUE,
                    ) {
                        Success {
                            vars,
                            must_implement_ability: _,
                            lambda_sets_to_specialize: _, // TODO ignored
                            extra_metadata: _,
                        } => {
                            // TODO(abilities) record deferred ability checks
                            introduce(subs, rank, pools, &vars);

                            state
                        }

                        Failure(vars, actual_type, expected_type, _bad_impl) => {
                            introduce(subs, rank, pools, &vars);

                            let problem = TypeError::BadExpr(
                                *region,
                                Category::Lookup(*symbol),
                                actual_type,
                                expectation.shallow_clone().replace(expected_type),
                            );

                            problems.push(problem);

                            state
                        }
                    }
                }
                None => {
                    problems.push(TypeError::UnexposedLookup(*symbol));

                    state
                }
            }
        }
        And(sub_constraints) => {
            let mut state = state;

            for sub_constraint in sub_constraints.iter() {
                state = solve(
                    arena,
                    mempool,
                    env,
                    state,
                    rank,
                    pools,
                    problems,
                    cached_aliases,
                    subs,
                    sub_constraint,
                );
            }

            state
        }
        Pattern(region, category, typ, expectation)
        | Present(typ, PresenceConstraint::Pattern(region, category, expectation)) => {
            let actual = type_to_var(arena, mempool, subs, rank, pools, cached_aliases, typ);
            let expected = type_to_var(
                arena,
                mempool,
                subs,
                rank,
                pools,
                cached_aliases,
                expectation.get_type_ref(),
            );

            // TODO(ayazhafiz): presence constraints for Expr2/Type2
            match unify(
                &mut UEnv::new(subs),
                actual,
                expected,
                Mode::EQ,
                Polarity::OF_PATTERN,
            ) {
                Success {
                    vars,
                    must_implement_ability: _,
                    lambda_sets_to_specialize: _, // TODO ignored
                    extra_metadata: _,
                } => {
                    // TODO(abilities) record deferred ability checks
                    introduce(subs, rank, pools, &vars);

                    state
                }
                Failure(vars, actual_type, expected_type, _bad_impl) => {
                    introduce(subs, rank, pools, &vars);

                    let problem = TypeError::BadPattern(
                        *region,
                        category.clone(),
                        actual_type,
                        expectation.shallow_clone().replace(expected_type),
                    );

                    problems.push(problem);

                    state
                }
            }
        }
        Let(let_con) => {
            match &let_con.ret_constraint {
                True if let_con.rigid_vars.is_empty() => {
                    introduce(subs, rank, pools, &let_con.flex_vars);

                    // If the return expression is guaranteed to solve,
                    // solve the assignments themselves and move on.
                    solve(
                        arena,
                        mempool,
                        &env,
                        state,
                        rank,
                        pools,
                        problems,
                        cached_aliases,
                        subs,
                        &let_con.defs_constraint,
                    )
                }
                ret_con if let_con.rigid_vars.is_empty() && let_con.flex_vars.is_empty() => {
                    let state = solve(
                        arena,
                        mempool,
                        env,
                        state,
                        rank,
                        pools,
                        problems,
                        cached_aliases,
                        subs,
                        &let_con.defs_constraint,
                    );

                    // Add a variable for each def to new_vars_by_env.
                    let mut local_def_vars = BumpMap::new_in(arena);

                    for (symbol, typ) in let_con.def_types.iter() {
                        let var =
                            type_to_var(arena, mempool, subs, rank, pools, cached_aliases, typ);

                        // TODO: region should come from typ
                        local_def_vars.insert(
                            *symbol,
                            Loc {
                                value: var,
                                region: Region::zero(),
                            },
                        );
                    }

                    let mut new_env = env.clone();
                    for (symbol, loc_var) in local_def_vars.iter() {
                        if !new_env.vars_by_symbol.contains_key(&symbol) {
                            new_env.vars_by_symbol.insert(*symbol, loc_var.value);
                        }
                    }

                    let new_state = solve(
                        arena,
                        mempool,
                        &new_env,
                        state,
                        rank,
                        pools,
                        problems,
                        cached_aliases,
                        subs,
                        ret_con,
                    );

                    for (symbol, loc_var) in local_def_vars {
                        check_for_infinite_type(subs, problems, symbol, loc_var);
                    }

                    new_state
                }
                ret_con => {
                    let rigid_vars = &let_con.rigid_vars;
                    let flex_vars = &let_con.flex_vars;

                    // work in the next pool to localize header
                    let next_rank = rank.next();

                    // introduce variables
                    for &var in rigid_vars.iter().chain(flex_vars.iter()) {
                        subs.set_rank(var, next_rank);
                    }

                    // determine the next pool
                    let next_pools;
                    if next_rank.into_usize() < pools.len() {
                        next_pools = pools
                    } else {
                        // we should be off by one at this point
                        debug_assert_eq!(next_rank.into_usize(), 1 + pools.len());
                        pools.extend_to(next_rank.into_usize());
                        next_pools = pools;
                    }

                    let pool: &mut Vec<Variable> = next_pools.get_mut(next_rank);

                    // Replace the contents of this pool with rigid_vars and flex_vars
                    pool.clear();
                    pool.reserve(rigid_vars.len() + flex_vars.len());
                    pool.extend(rigid_vars.iter());
                    pool.extend(flex_vars.iter());

                    // run solver in next pool

                    // Add a variable for each def to local_def_vars.
                    let mut local_def_vars = BumpMap::new_in(arena);

                    for (symbol, typ) in let_con.def_types.iter() {
                        let var = type_to_var(
                            arena,
                            mempool,
                            subs,
                            next_rank,
                            next_pools,
                            cached_aliases,
                            typ,
                        );

                        // TODO: region should come from type
                        local_def_vars.insert(
                            *symbol,
                            Loc {
                                value: var,
                                region: Region::zero(),
                            },
                        );
                    }

                    // Solve the assignments' constraints first.
                    let State {
                        env: saved_env,
                        mark,
                    } = solve(
                        arena,
                        mempool,
                        &env,
                        state,
                        next_rank,
                        next_pools,
                        problems,
                        cached_aliases,
                        subs,
                        &let_con.defs_constraint,
                    );

                    let young_mark = mark;
                    let visit_mark = young_mark.next();
                    let final_mark = visit_mark.next();

                    debug_assert_eq!(
                        {
                            let offenders = next_pools
                                .get(next_rank)
                                .iter()
                                .filter(|var| {
                                    let current_rank =
                                        subs.get_rank(roc_types::subs::Variable::clone(var));

                                    current_rank.into_usize() > next_rank.into_usize()
                                })
                                .collect::<Vec<_>>();

                            let result = offenders.len();

                            if result > 0 {
                                dbg!(&subs, &offenders, &let_con.def_types);
                            }

                            result
                        },
                        0
                    );

                    // pop pool
                    generalize(subs, young_mark, visit_mark, next_rank, next_pools);

                    next_pools.get_mut(next_rank).clear();

                    // check that things went well
                    debug_assert!({
                        // NOTE the `subs.redundant` check is added for the uniqueness
                        // inference, and does not come from elm. It's unclear whether this is
                        // a bug with uniqueness inference (something is redundant that
                        // shouldn't be) or that it just never came up in elm.
                        let failing: Vec<_> = rigid_vars
                            .iter()
                            .filter(|&var| {
                                !subs.redundant(*var) && subs.get_rank(*var) != Rank::NONE
                            })
                            .collect();

                        if !failing.is_empty() {
                            println!("Rigids {:?}", &rigid_vars);
                            println!("Failing {:?}", failing);
                        }

                        failing.is_empty()
                    });

                    let mut new_env = env.clone();
                    for (symbol, loc_var) in local_def_vars.iter() {
                        // when there are duplicates, keep the one from `env`
                        if !new_env.vars_by_symbol.contains_key(&symbol) {
                            new_env.vars_by_symbol.insert(*symbol, loc_var.value);
                        }
                    }

                    // Note that this vars_by_symbol is the one returned by the
                    // previous call to solve()
                    let temp_state = State {
                        env: saved_env,
                        mark: final_mark,
                    };

                    // Now solve the body, using the new vars_by_symbol which includes
                    // the assignments' name-to-variable mappings.
                    let new_state = solve(
                        arena,
                        mempool,
                        &new_env,
                        temp_state,
                        rank,
                        next_pools,
                        problems,
                        cached_aliases,
                        subs,
                        &ret_con,
                    );

                    for (symbol, loc_var) in local_def_vars {
                        check_for_infinite_type(subs, problems, symbol, loc_var);
                    }

                    new_state
                }
            }
        }
        Present(typ, PresenceConstraint::IsOpen) => {
            let actual = type_to_var(arena, mempool, subs, rank, pools, cached_aliases, typ);
            let mut new_desc = subs.get(actual);
            match new_desc.content {
                Content::Structure(FlatType::TagUnion(tags, _)) => {
                    let new_ext = subs.fresh_unnamed_flex_var();
                    let new_union = Content::Structure(FlatType::TagUnion(tags, new_ext));
                    new_desc.content = new_union;
                    subs.set(actual, new_desc);
                    state
                }
                _ => {
                    // Today, an "open" constraint doesn't affect any types
                    // other than tag unions. Recursive tag unions are constructed
                    // at a later time (during occurs checks after tag unions are
                    // resolved), so that's not handled here either.
                    // NB: Handle record types here if we add presence constraints
                    // to their type inference as well.
                    state
                }
            }
        }
        Present(typ, PresenceConstraint::IncludesTag(tag_name, tys)) => {
            let actual = type_to_var(arena, mempool, subs, rank, pools, cached_aliases, typ);
            let tag_ty = Type2::TagUnion(
                PoolVec::new(
                    std::iter::once((
                        tag_name.clone(),
                        PoolVec::new(tys.into_iter().map(ShallowClone::shallow_clone), mempool),
                    )),
                    mempool,
                ),
                mempool.add(Type2::EmptyTagUnion),
            );
            let includes = type_to_var(arena, mempool, subs, rank, pools, cached_aliases, &tag_ty);

            match unify(
                &mut UEnv::new(subs),
                actual,
                includes,
                Mode::PRESENT,
                Polarity::OF_PATTERN,
            ) {
                Success {
                    vars,
                    must_implement_ability: _,
                    lambda_sets_to_specialize: _, // TODO ignored
                    extra_metadata: _,
                } => {
                    // TODO(abilities) record deferred ability checks
                    introduce(subs, rank, pools, &vars);

                    state
                }
                Failure(vars, actual_type, expected_type, _bad_impl) => {
                    introduce(subs, rank, pools, &vars);

                    // TODO: do we need a better error type here?
                    let problem = TypeError::BadExpr(
                        Region::zero(),
                        Category::When,
                        actual_type,
                        Expected::NoExpectation(expected_type),
                    );

                    problems.push(problem);

                    state
                }
            }
        }
    }
}

fn type_to_var<'a>(
    arena: &'a Bump,
    mempool: &mut Pool,
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    cached: &mut MutMap<Symbol, Variable>,
    typ: &Type2,
) -> Variable {
    type_to_variable(arena, mempool, subs, rank, pools, cached, typ)
}

/// Abusing existing functions for our purposes
/// this is to put a solved type back into subs
pub fn insert_type_into_subs<'a>(
    arena: &'a Bump,
    mempool: &mut Pool,
    subs: &mut Subs,
    typ: &Type2,
) -> Variable {
    let rank = Rank::NONE;
    let mut pools = Pools::default();
    let mut cached = MutMap::default();

    type_to_variable(arena, mempool, subs, rank, &mut pools, &mut cached, typ)
}

fn type_to_variable<'a>(
    arena: &'a Bump,
    mempool: &Pool,
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    cached: &mut MutMap<Symbol, Variable>,
    typ: &Type2,
) -> Variable {
    use Type2::*;

    match typ {
        Variable(var) => *var,
        Apply(symbol, args) => {
            let mut arg_vars = Vec::with_capacity(args.len());
            for var_id in args.iter_node_ids() {
                let arg = mempool.get(var_id);
                arg_vars.push(type_to_variable(
                    arena, mempool, subs, rank, pools, cached, arg,
                ))
            }

            let arg_vars = VariableSubsSlice::insert_into_subs(subs, arg_vars);
            let flat_type = FlatType::Apply(*symbol, arg_vars);
            let content = Content::Structure(flat_type);

            register(subs, rank, pools, content)
        }

        EmptyRec => roc_types::subs::Variable::EMPTY_RECORD,
        EmptyTagUnion => roc_types::subs::Variable::EMPTY_TAG_UNION,

        Record(fields, ext_id) => {
            let mut field_vars = Vec::new();

            for node_id in fields.iter_node_ids() {
                use RecordField::*;

                let (field, field_type) = mempool.get(node_id);

                let field_var = match field_type {
                    Required(type_id) => Required(type_to_variable(
                        arena,
                        mempool,
                        subs,
                        rank,
                        pools,
                        cached,
                        mempool.get(*type_id),
                    )),
                    RigidRequired(type_id) => RigidRequired(type_to_variable(
                        arena,
                        mempool,
                        subs,
                        rank,
                        pools,
                        cached,
                        mempool.get(*type_id),
                    )),
                    Optional(type_id) => Optional(type_to_variable(
                        arena,
                        mempool,
                        subs,
                        rank,
                        pools,
                        cached,
                        mempool.get(*type_id),
                    )),
                    RigidOptional(type_id) => RigidOptional(type_to_variable(
                        arena,
                        mempool,
                        subs,
                        rank,
                        pools,
                        cached,
                        mempool.get(*type_id),
                    )),
                    Demanded(type_id) => Demanded(type_to_variable(
                        arena,
                        mempool,
                        subs,
                        rank,
                        pools,
                        cached,
                        mempool.get(*type_id),
                    )),
                };

                field_vars.push((field.as_str(mempool).into(), field_var));
            }

            let ext = mempool.get(*ext_id);
            let temp_ext_var = type_to_variable(arena, mempool, subs, rank, pools, cached, ext);

            let (it, new_ext_var) =
                gather_fields_unsorted_iter(subs, RecordFields::empty(), temp_ext_var)
                    .expect("Something ended up weird in this record type");

            let it = it
                .into_iter()
                .map(|(field, field_type)| (field.clone(), field_type));

            field_vars.extend(it);
            field_vars.sort_unstable_by(RecordFields::compare);

            let record_fields = RecordFields::insert_into_subs(subs, field_vars);

            let content = Content::Structure(FlatType::Record(record_fields, new_ext_var));

            register(subs, rank, pools, content)
        }

        Alias(symbol, args, alias_type_id) | Opaque(symbol, args, alias_type_id) => {
            // TODO cache in uniqueness inference gives problems! all Int's get the same uniqueness var!
            // Cache aliases without type arguments. Commonly used aliases like `Int` would otherwise get O(n)
            // different variables (once for each occurrence). The recursion restriction is required
            // for uniqueness types only: recursive aliases "introduce" an unbound uniqueness
            // attribute in the body, when
            //
            // Peano : [S Peano, Z]
            //
            // becomes
            //
            // Peano : [S (Attr u Peano), Z]
            //
            // This `u` variable can be different between lists, so giving just one variable to
            // this type is incorrect.
            // TODO does caching work at all with uniqueness types? even Int then hides a uniqueness variable

            let alias_type = mempool.get(*alias_type_id);
            let is_recursive = false; // alias_type.is_recursive();
            let no_args = args.is_empty();
            /*
            if no_args && !is_recursive {
                if let Some(var) = cached.get(symbol) {
                    return *var;
                }
            }
            */

            let mut arg_vars = Vec::with_capacity(args.len());

            for arg_type_id in args.iter(mempool) {
                let arg_type = mempool.get(*arg_type_id);

                let arg_var = type_to_variable(arena, mempool, subs, rank, pools, cached, arg_type);

                arg_vars.push(arg_var);
            }

            let arg_vars = AliasVariables::insert_into_subs(subs, arg_vars, [], []);

            let alias_var = type_to_variable(arena, mempool, subs, rank, pools, cached, alias_type);

            let kind = match typ {
                Alias(..) => AliasKind::Structural,
                Opaque(..) => AliasKind::Opaque,
                _ => internal_error!(),
            };
            let content = Content::Alias(*symbol, arg_vars, alias_var, kind);

            let result = register(subs, rank, pools, content);

            if no_args && !is_recursive {
                // cached.insert(*symbol, result);
            }

            result
        }
        TagUnion(tags, ext_id) => {
            let ext = mempool.get(*ext_id);

            let (union_tags, ext) =
                type_to_union_tags(arena, mempool, subs, rank, pools, cached, tags, ext);
            let content = Content::Structure(FlatType::TagUnion(union_tags, ext));

            register(subs, rank, pools, content)
        }
        // This case is important for the rank of boolean variables
        Function(arg_vars, closure_type_id, ret_type_id) => {
            let closure_type = mempool.get(*closure_type_id);
            let ret_type = mempool.get(*ret_type_id);

            let mut new_arg_vars = Vec::with_capacity(arg_vars.len());

            for var_id in arg_vars.iter_node_ids() {
                let arg = mempool.get(var_id);
                let var = type_to_variable(arena, mempool, subs, rank, pools, cached, arg);
                new_arg_vars.push(var)
            }

            let arg_vars = VariableSubsSlice::insert_into_subs(subs, new_arg_vars);

            let ret_var = type_to_variable(arena, mempool, subs, rank, pools, cached, ret_type);
            let closure_var =
                type_to_variable(arena, mempool, subs, rank, pools, cached, closure_type);

            let content = Content::Structure(FlatType::Func(arg_vars, closure_var, ret_var));

            register(subs, rank, pools, content)
        }
        other => todo!("not implemented {:?}", &other),
        //        RecursiveTagUnion(rec_var, tags, ext) => {
        //            let mut tag_vars = MutMap::default();
        //
        //            for (tag, tag_argument_types) in tags {
        //                let mut tag_argument_vars = Vec::with_capacity(tag_argument_types.len());
        //
        //                for arg_type in tag_argument_types {
        //                    tag_argument_vars.push(type_to_variable(subs, rank, pools, cached, arg_type));
        //                }
        //
        //                tag_vars.insert(tag.clone(), tag_argument_vars);
        //            }
        //
        //            let temp_ext_var = type_to_variable(subs, rank, pools, cached, ext);
        //            let mut ext_tag_vec = Vec::new();
        //            let new_ext_var = match roc_types::pretty_print::chase_ext_tag_union(
        //                subs,
        //                temp_ext_var,
        //                &mut ext_tag_vec,
        //            ) {
        //                Ok(()) => Variable::EMPTY_TAG_UNION,
        //                Err((new, _)) => new,
        //            };
        //            tag_vars.extend(ext_tag_vec.into_iter());
        //
        //            let content =
        //                Content::Structure(FlatType::RecursiveTagUnion(*rec_var, tag_vars, new_ext_var));
        //
        //            let tag_union_var = register(subs, rank, pools, content);
        //
        //            subs.set_content(
        //                *rec_var,
        //                Content::RecursionVar {
        //                    opt_name: None,
        //                    structure: tag_union_var,
        //                },
        //            );
        //
        //            tag_union_var
        //        }
        //        HostExposedAlias {
        //            name: symbol,
        //            arguments: args,
        //            actual: alias_type,
        //            actual_var,
        //            ..
        //        } => {
        //            let mut arg_vars = Vec::with_capacity(args.len());
        //            let mut new_aliases = ImMap::default();
        //
        //            for (arg, arg_type) in args {
        //                let arg_var = type_to_variable(subs, rank, pools, cached, arg_type);
        //
        //                arg_vars.push((arg.clone(), arg_var));
        //                new_aliases.insert(arg.clone(), arg_var);
        //            }
        //
        //            let alias_var = type_to_variable(subs, rank, pools, cached, alias_type);
        //
        //            // unify the actual_var with the result var
        //            // this can be used to access the type of the actual_var
        //            // to determine its layout later
        //            // subs.set_content(*actual_var, descriptor.content);
        //
        //            //subs.set(*actual_var, descriptor.clone());
        //            let content = Content::Alias(*symbol, arg_vars, alias_var);
        //
        //            let result = register(subs, rank, pools, content);
        //
        //            // We only want to unify the actual_var with the alias once
        //            // if it's already redirected (and therefore, redundant)
        //            // don't do it again
        //            if !subs.redundant(*actual_var) {
        //                let descriptor = subs.get(result);
        //                subs.union(result, *actual_var, descriptor);
        //            }
        //
        //            result
        //        }
        //        Erroneous(problem) => {
        //            let content = Content::Structure(FlatType::Erroneous(problem.clone()));
        //
        //            register(subs, rank, pools, content)
        //        }
    }
}

fn type_to_union_tags<'a>(
    arena: &'a Bump,
    mempool: &Pool,
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    cached: &mut MutMap<Symbol, Variable>,
    tags: &PoolVec<(TagName, PoolVec<Type2>)>,
    ext: &Type2,
) -> (UnionTags, Variable) {
    let mut tag_vars = Vec::with_capacity(tags.len());

    let mut tag_argument_vars = Vec::new();
    for id in tags.iter_node_ids() {
        let (tag, tag_argument_types) = mempool.get(id);
        for arg_id in tag_argument_types.iter_node_ids() {
            let arg_type = mempool.get(arg_id);
            let new_var = type_to_variable(arena, mempool, subs, rank, pools, cached, arg_type);
            tag_argument_vars.push(new_var);
        }

        let new_slice = VariableSubsSlice::insert_into_subs(subs, tag_argument_vars.drain(..));

        tag_vars.push((tag.clone(), new_slice));
    }

    let temp_ext_var = type_to_variable(arena, mempool, subs, rank, pools, cached, ext);

    let ext = {
        let (it, ext) =
            roc_types::types::gather_tags_unsorted_iter(subs, UnionTags::default(), temp_ext_var)
                .expect("not a tag union");

        tag_vars.extend(it.map(|(n, v)| (n.clone(), v)));
        tag_vars.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));

        // deduplicate, keeping the right-most occurrence of a tag name
        let mut i = 0;

        while i < tag_vars.len() {
            match (tag_vars.get(i), tag_vars.get(i + 1)) {
                (Some((t1, _)), Some((t2, _))) => {
                    if t1 == t2 {
                        tag_vars.remove(i);
                    } else {
                        i += 1;
                    }
                }
                _ => break,
            }
        }

        ext
    };

    (UnionTags::insert_slices_into_subs(subs, tag_vars), ext)
}

fn check_for_infinite_type(
    subs: &mut Subs,
    problems: &mut Vec<TypeError>,
    symbol: Symbol,
    loc_var: Loc<Variable>,
) {
    let var = loc_var.value;

    while let Err((recursive, _chain)) = subs.occurs(var) {
        let description = subs.get(recursive);
        let content = description.content;

        // try to make a tag union recursive, see if that helps
        match content {
            Content::Structure(FlatType::TagUnion(tags, ext_var)) => {
                let rec_var = subs.fresh_unnamed_flex_var();
                subs.set_rank(rec_var, description.rank);
                subs.set_content(
                    rec_var,
                    Content::RecursionVar {
                        opt_name: None,
                        structure: recursive,
                    },
                );

                let mut new_tags = Vec::with_capacity(tags.len());

                for (name_index, slice_index) in tags.iter_all() {
                    let slice = subs[slice_index];

                    let mut new_vars = Vec::new();
                    for var_index in slice {
                        let var = subs[var_index];
                        new_vars.push(subs.explicit_substitute(recursive, rec_var, var));
                    }

                    new_tags.push((subs[name_index].clone(), new_vars));
                }

                let new_ext_var = subs.explicit_substitute(recursive, rec_var, ext_var);

                let new_tags = UnionTags::insert_into_subs(subs, new_tags);

                let flat_type = FlatType::RecursiveTagUnion(rec_var, new_tags, new_ext_var);

                subs.set_content(recursive, Content::Structure(flat_type));
            }

            _other => circular_error(subs, problems, symbol, &loc_var),
        }
    }
}

fn circular_error(
    subs: &mut Subs,
    problems: &mut Vec<TypeError>,
    symbol: Symbol,
    loc_var: &Loc<Variable>,
) {
    let var = loc_var.value;
    let error_type = subs.var_to_error_type(var, Polarity::Pos);
    let problem = TypeError::CircularType(loc_var.region, symbol, error_type);

    subs.set_content(var, Content::Error);

    problems.push(problem);
}

fn generalize(
    subs: &mut Subs,
    young_mark: Mark,
    visit_mark: Mark,
    young_rank: Rank,
    pools: &mut Pools,
) {
    let young_vars = pools.get(young_rank);
    let rank_table = pool_to_rank_table(subs, young_mark, young_rank, young_vars);

    // Get the ranks right for each entry.
    // Start at low ranks so we only have to pass over the information once.
    for (index, table) in rank_table.iter().enumerate() {
        for &var in table.iter() {
            adjust_rank(subs, young_mark, visit_mark, Rank::from(index), var);
        }
    }

    let (last_pool, all_but_last_pool) = rank_table.split_last();

    // For variables that have rank lowerer than young_rank, register them in
    // the appropriate old pool if they are not redundant.
    for vars in all_but_last_pool {
        for &var in vars {
            if !subs.redundant(var) {
                let rank = subs.get_rank(var);

                pools.get_mut(rank).push(var);
            }
        }
    }

    // For variables with rank young_rank, if rank < young_rank: register in old pool,
    // otherwise generalize
    for &var in last_pool {
        if !subs.redundant(var) {
            let desc_rank = subs.get_rank(var);

            if desc_rank < young_rank {
                pools.get_mut(desc_rank).push(var);
            } else {
                subs.set_rank(var, Rank::NONE);
            }
        }
    }
}

fn pool_to_rank_table(
    subs: &mut Subs,
    young_mark: Mark,
    young_rank: Rank,
    young_vars: &[Variable],
) -> Pools {
    let mut pools = Pools::new(young_rank.into_usize() + 1);

    // Sort the variables into buckets by rank.
    for &var in young_vars.iter() {
        let rank = subs.get_rank(var);
        subs.set_mark(var, young_mark);

        debug_assert!(rank.into_usize() < young_rank.into_usize() + 1);
        pools.get_mut(rank).push(var);
    }

    pools
}

/// Adjust variable ranks such that ranks never increase as you move deeper.
/// This way the outermost rank is representative of the entire structure.
fn adjust_rank(
    subs: &mut Subs,
    young_mark: Mark,
    visit_mark: Mark,
    group_rank: Rank,
    var: Variable,
) -> Rank {
    let desc = subs.get(var);

    if desc.mark == young_mark {
        let Descriptor {
            content,
            rank: _,
            mark: _,
            copy,
        } = desc;

        // Mark the variable as visited before adjusting content, as it may be cyclic.
        subs.set_mark(var, visit_mark);

        let max_rank = adjust_rank_content(subs, young_mark, visit_mark, group_rank, &content);

        subs.set(
            var,
            Descriptor {
                content,
                rank: max_rank,
                mark: visit_mark,
                copy,
            },
        );

        max_rank
    } else if desc.mark == visit_mark {
        // nothing changes
        desc.rank
    } else {
        let mut desc = desc;

        let min_rank = group_rank.min(desc.rank);

        // TODO from elm-compiler: how can min_rank ever be group_rank?
        desc.rank = min_rank;
        desc.mark = visit_mark;

        subs.set(var, desc);

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
        FlexVar(_) | RigidVar(_) | FlexAbleVar(..) | RigidAbleVar(..) | Error => group_rank,

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

                Func(arg_vars, closure_var, ret_var) => {
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
                    Rank::toplevel()
                }

                EmptyTagUnion => Rank::toplevel(),

                Record(fields, ext_var) => {
                    let mut rank = adjust_rank(subs, young_mark, visit_mark, group_rank, *ext_var);

                    for index in fields.iter_variables() {
                        let var = subs[index];
                        rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
                    }

                    rank
                }

                TagUnion(tags, ext_var) => {
                    let mut rank = adjust_rank(subs, young_mark, visit_mark, group_rank, *ext_var);

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
                    adjust_rank(subs, young_mark, visit_mark, group_rank, *ext_var)
                }

                RecursiveTagUnion(rec_var, tags, ext_var) => {
                    let mut rank = adjust_rank(subs, young_mark, visit_mark, group_rank, *ext_var);

                    for (_, index) in tags.iter_all() {
                        let slice = subs[index];
                        for var_index in slice {
                            let var = subs[var_index];
                            rank = rank
                                .max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
                        }
                    }

                    // THEORY: the recursion var has the same rank as the tag union itself
                    // all types it uses are also in the tags already, so it cannot influence the
                    // rank
                    debug_assert!(
                        rank >= adjust_rank(subs, young_mark, visit_mark, group_rank, *rec_var)
                    );

                    rank
                }
            }
        }

        LambdaSet(subs::LambdaSet {
            solved,
            recursion_var,
            // TODO: handle unspecialized
            unspecialized: _,
            ambient_function: _,
        }) => {
            let mut rank = group_rank;

            for (_, index) in solved.iter_all() {
                let slice = subs[index];
                for var_index in slice {
                    let var = subs[var_index];
                    rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
                }
            }

            if let Some(rec_var) = recursion_var.into_variable() {
                // THEORY: the recursion var has the same rank as the tag union itself
                // all types it uses are also in the tags already, so it cannot influence the
                // rank
                debug_assert!(
                    rank >= adjust_rank(subs, young_mark, visit_mark, group_rank, rec_var)
                );
            }

            rank
        }

        Alias(_, args, real_var, _) => {
            let mut rank = Rank::toplevel();

            for var_index in args.all_variables() {
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

        RangedNumber(_vars) => group_rank,
    }
}

/// Introduce some variables to Pools at the given rank.
/// Also, set each of their ranks in Subs to be the given rank.
fn introduce(subs: &mut Subs, rank: Rank, pools: &mut Pools, vars: &[Variable]) {
    let pool: &mut Vec<Variable> = pools.get_mut(rank);

    for &var in vars.iter() {
        subs.set_rank(var, rank);
    }

    pool.extend(vars);
}

/// Function that converts rigids variables to flex variables
/// this is used during the monomorphization process
pub fn instantiate_rigids(subs: &mut Subs, var: Variable) {
    let rank = Rank::NONE;
    let mut pools = Pools::default();

    instantiate_rigids_help(subs, rank, &mut pools, var);
}

fn instantiate_rigids_help(
    subs: &mut Subs,
    max_rank: Rank,
    pools: &mut Pools,
    var: Variable,
) -> Variable {
    use roc_types::subs::Content::*;
    use roc_types::subs::FlatType::*;

    let desc = subs.get_without_compacting(var);

    if let Some(copy) = desc.copy.into_variable() {
        return copy;
    }

    let make_descriptor = |content| Descriptor {
        content,
        rank: max_rank,
        mark: Mark::NONE,
        copy: OptVariable::NONE,
    };

    let content = desc.content;
    let copy = var;

    pools.get_mut(max_rank).push(copy);

    // Link the original variable to the new variable. This lets us
    // avoid making multiple copies of the variable we are instantiating.
    //
    // Need to do this before recursively copying to avoid looping.
    subs.set(
        var,
        Descriptor {
            content: content.clone(),
            rank: desc.rank,
            mark: Mark::NONE,
            copy: copy.into(),
        },
    );

    // Now we recursively copy the content of the variable.
    // We have already marked the variable as copied, so we
    // will not repeat this work or crawl this variable again.
    match content {
        Structure(flat_type) => {
            match flat_type {
                Apply(_, args) => {
                    for var_index in args.into_iter() {
                        let var = subs[var_index];
                        instantiate_rigids_help(subs, max_rank, pools, var);
                    }
                }

                Func(arg_vars, closure_var, ret_var) => {
                    instantiate_rigids_help(subs, max_rank, pools, ret_var);
                    instantiate_rigids_help(subs, max_rank, pools, closure_var);

                    for index in arg_vars.into_iter() {
                        let var = subs[index];
                        instantiate_rigids_help(subs, max_rank, pools, var);
                    }
                }

                EmptyRecord | EmptyTagUnion => {}

                Record(fields, ext_var) => {
                    for index in fields.iter_variables() {
                        let var = subs[index];
                        instantiate_rigids_help(subs, max_rank, pools, var);
                    }

                    instantiate_rigids_help(subs, max_rank, pools, ext_var);
                }

                TagUnion(tags, ext_var) => {
                    for (_, index) in tags.iter_all() {
                        let slice = subs[index];
                        for var_index in slice {
                            let var = subs[var_index];
                            instantiate_rigids_help(subs, max_rank, pools, var);
                        }
                    }

                    instantiate_rigids_help(subs, max_rank, pools, ext_var);
                }

                FunctionOrTagUnion(_tag_name, _symbol, ext_var) => {
                    instantiate_rigids_help(subs, max_rank, pools, ext_var);
                }

                RecursiveTagUnion(rec_var, tags, ext_var) => {
                    instantiate_rigids_help(subs, max_rank, pools, rec_var);

                    for (_, index) in tags.iter_all() {
                        let slice = subs[index];
                        for var_index in slice {
                            let var = subs[var_index];
                            instantiate_rigids_help(subs, max_rank, pools, var);
                        }
                    }

                    instantiate_rigids_help(subs, max_rank, pools, ext_var);
                }
            };
        }

        FlexVar(_) | FlexAbleVar(_, _) | Error => {}

        RecursionVar { structure, .. } => {
            instantiate_rigids_help(subs, max_rank, pools, structure);
        }

        RigidVar(name) => {
            // what it's all about: convert the rigid var into a flex var
            subs.set(copy, make_descriptor(FlexVar(Some(name))));
        }

        RigidAbleVar(name, ability) => {
            // what it's all about: convert the rigid var into a flex var
            subs.set(copy, make_descriptor(FlexAbleVar(Some(name), ability)));
        }

        Alias(_, args, real_type_var, _) => {
            for var_index in args.all_variables() {
                let var = subs[var_index];
                instantiate_rigids_help(subs, max_rank, pools, var);
            }

            instantiate_rigids_help(subs, max_rank, pools, real_type_var);
        }

        LambdaSet(subs::LambdaSet {
            solved,
            recursion_var,
            // TODO: handle unspecialized
            unspecialized: _,
            ambient_function: _,
        }) => {
            if let Some(rec_var) = recursion_var.into_variable() {
                instantiate_rigids_help(subs, max_rank, pools, rec_var);
            }

            for (_, index) in solved.iter_all() {
                let slice = subs[index];
                for var_index in slice {
                    let var = subs[var_index];
                    instantiate_rigids_help(subs, max_rank, pools, var);
                }
            }
        }

        RangedNumber(_vars) => {}
    }

    var
}

fn deep_copy_var(subs: &mut Subs, rank: Rank, pools: &mut Pools, var: Variable) -> Variable {
    let copy = deep_copy_var_help(subs, rank, pools, var);

    copy
}

fn deep_copy_var_help(
    subs: &mut Subs,
    max_rank: Rank,
    pools: &mut Pools,
    var: Variable,
) -> Variable {
    use roc_types::subs::Content::*;
    use roc_types::subs::FlatType::*;

    let desc = subs.get_without_compacting(var);

    if let Some(copy) = desc.copy.into_variable() {
        return copy;
    } else if desc.rank != Rank::NONE {
        return var;
    }

    let make_descriptor = |content| Descriptor {
        content,
        rank: max_rank,
        mark: Mark::NONE,
        copy: OptVariable::NONE,
    };

    let content = desc.content;
    let copy = subs.fresh(make_descriptor(content.clone()));

    pools.get_mut(max_rank).push(copy);

    // Link the original variable to the new variable. This lets us
    // avoid making multiple copies of the variable we are instantiating.
    //
    // Need to do this before recursively copying to avoid looping.
    subs.set(
        var,
        Descriptor {
            content: content.clone(),
            rank: desc.rank,
            mark: Mark::NONE,
            copy: copy.into(),
        },
    );

    // Now we recursively copy the content of the variable.
    // We have already marked the variable as copied, so we
    // will not repeat this work or crawl this variable again.
    match content {
        Structure(flat_type) => {
            let new_flat_type = match flat_type {
                Apply(symbol, args) => {
                    let mut new_arg_vars = Vec::with_capacity(args.len());

                    for index in args.into_iter() {
                        let var = subs[index];
                        let copy_var = deep_copy_var_help(subs, max_rank, pools, var);
                        new_arg_vars.push(copy_var);
                    }

                    let arg_vars = VariableSubsSlice::insert_into_subs(subs, new_arg_vars);

                    Apply(symbol, arg_vars)
                }

                Func(arg_vars, closure_var, ret_var) => {
                    let new_ret_var = deep_copy_var_help(subs, max_rank, pools, ret_var);
                    let new_closure_var = deep_copy_var_help(subs, max_rank, pools, closure_var);

                    let mut new_arg_vars = Vec::with_capacity(arg_vars.len());

                    for index in arg_vars.into_iter() {
                        let var = subs[index];
                        let copy_var = deep_copy_var_help(subs, max_rank, pools, var);
                        new_arg_vars.push(copy_var);
                    }

                    let arg_vars = VariableSubsSlice::insert_into_subs(subs, new_arg_vars);

                    Func(arg_vars, new_closure_var, new_ret_var)
                }

                same @ EmptyRecord | same @ EmptyTagUnion => same,

                Record(fields, ext_var) => {
                    let record_fields = {
                        let mut new_vars = Vec::with_capacity(fields.len());

                        for index in fields.iter_variables() {
                            let var = subs[index];
                            let copy_var = deep_copy_var_help(subs, max_rank, pools, var);

                            new_vars.push(copy_var);
                        }

                        let field_names_start = subs.field_names.len() as u32;
                        let variables_start = subs.variables.len() as u32;
                        let field_types_start = subs.record_fields.len() as u32;

                        let mut length = 0;

                        for ((i1, _, i3), var) in fields.iter_all().zip(new_vars) {
                            let record_field = subs[i3].map(|_| var);

                            subs.field_names.push(subs[i1].clone());
                            subs.record_fields.push(record_field.map(|_| ()));
                            subs.variables.push(*record_field.as_inner());

                            length += 1;
                        }

                        RecordFields {
                            length,
                            field_names_start,
                            variables_start,
                            field_types_start,
                        }
                    };

                    Record(
                        record_fields,
                        deep_copy_var_help(subs, max_rank, pools, ext_var),
                    )
                }

                TagUnion(tags, ext_var) => {
                    let mut new_variable_slices = Vec::with_capacity(tags.len());

                    let mut new_variables = Vec::new();
                    for index in tags.variables() {
                        let slice = subs[index];
                        for var_index in slice {
                            let var = subs[var_index];
                            let new_var = deep_copy_var_help(subs, max_rank, pools, var);
                            new_variables.push(new_var);
                        }

                        let new_slice =
                            VariableSubsSlice::insert_into_subs(subs, new_variables.drain(..));

                        new_variable_slices.push(new_slice);
                    }

                    let new_variables = {
                        let start = subs.variable_slices.len() as u32;
                        let length = new_variable_slices.len() as u16;
                        subs.variable_slices.extend(new_variable_slices);

                        SubsSlice::new(start, length)
                    };

                    let union_tags = UnionTags::from_slices(tags.labels(), new_variables);

                    let new_ext = deep_copy_var_help(subs, max_rank, pools, ext_var);
                    TagUnion(union_tags, new_ext)
                }

                FunctionOrTagUnion(tag_name, symbol, ext_var) => FunctionOrTagUnion(
                    tag_name,
                    symbol,
                    deep_copy_var_help(subs, max_rank, pools, ext_var),
                ),

                RecursiveTagUnion(rec_var, tags, ext_var) => {
                    let mut new_variable_slices = Vec::with_capacity(tags.len());

                    let mut new_variables = Vec::new();
                    for index in tags.variables() {
                        let slice = subs[index];
                        for var_index in slice {
                            let var = subs[var_index];
                            let new_var = deep_copy_var_help(subs, max_rank, pools, var);
                            new_variables.push(new_var);
                        }

                        let new_slice =
                            VariableSubsSlice::insert_into_subs(subs, new_variables.drain(..));

                        new_variable_slices.push(new_slice);
                    }

                    let new_variables = {
                        let start = subs.variable_slices.len() as u32;
                        let length = new_variable_slices.len() as u16;
                        subs.variable_slices.extend(new_variable_slices);

                        SubsSlice::new(start, length)
                    };

                    let union_tags = UnionTags::from_slices(tags.labels(), new_variables);

                    let new_ext = deep_copy_var_help(subs, max_rank, pools, ext_var);
                    let new_rec_var = deep_copy_var_help(subs, max_rank, pools, rec_var);
                    FlatType::RecursiveTagUnion(new_rec_var, union_tags, new_ext)
                }
            };

            subs.set(copy, make_descriptor(Structure(new_flat_type)));

            copy
        }

        FlexVar(_) | FlexAbleVar(_, _) | Error => copy,

        RecursionVar {
            opt_name,
            structure,
        } => {
            let new_structure = deep_copy_var_help(subs, max_rank, pools, structure);

            subs.set(
                copy,
                make_descriptor(RecursionVar {
                    opt_name,
                    structure: new_structure,
                }),
            );

            copy
        }

        RigidVar(name) => {
            subs.set(copy, make_descriptor(FlexVar(Some(name))));

            copy
        }

        RigidAbleVar(name, ability) => {
            subs.set(copy, make_descriptor(FlexAbleVar(Some(name), ability)));

            copy
        }

        Alias(symbol, mut args, real_type_var, kind) => {
            let mut new_args = Vec::with_capacity(args.all_variables().len());

            for var_index in args.all_variables() {
                let var = subs[var_index];
                let new_var = deep_copy_var_help(subs, max_rank, pools, var);
                new_args.push(new_var);
            }

            args.replace_variables(subs, new_args);

            let new_real_type_var = deep_copy_var_help(subs, max_rank, pools, real_type_var);
            let new_content = Alias(symbol, args, new_real_type_var, kind);

            subs.set(copy, make_descriptor(new_content));

            copy
        }

        LambdaSet(subs::LambdaSet {
            solved,
            recursion_var,
            unspecialized,
            ambient_function,
        }) => {
            let mut new_variable_slices = Vec::with_capacity(solved.len());

            let mut new_variables = Vec::new();
            for index in solved.variables() {
                let slice = subs[index];
                for var_index in slice {
                    let var = subs[var_index];
                    let new_var = deep_copy_var_help(subs, max_rank, pools, var);
                    new_variables.push(new_var);
                }

                let new_slice = VariableSubsSlice::insert_into_subs(subs, new_variables.drain(..));

                new_variable_slices.push(new_slice);
            }

            let new_variables = {
                let start = subs.variable_slices.len() as u32;
                let length = new_variable_slices.len() as u16;
                subs.variable_slices.extend(new_variable_slices);

                SubsSlice::new(start, length)
            };

            let new_solved = UnionLambdas::from_slices(solved.labels(), new_variables);
            let new_rec_var =
                recursion_var.map(|rec_var| deep_copy_var_help(subs, max_rank, pools, rec_var));

            let new_content = LambdaSet(subs::LambdaSet {
                solved: new_solved,
                recursion_var: new_rec_var,
                // TODO: actually copy
                unspecialized,
                ambient_function,
            });

            subs.set(copy, make_descriptor(new_content));

            copy
        }

        RangedNumber(vars) => {
            let new_content = RangedNumber(vars);

            subs.set(copy, make_descriptor(new_content));

            copy
        }
    }
}

fn register(subs: &mut Subs, rank: Rank, pools: &mut Pools, content: Content) -> Variable {
    let var = subs.fresh(Descriptor {
        content,
        rank,
        mark: Mark::NONE,
        copy: OptVariable::NONE,
    });

    pools.get_mut(rank).push(var);

    var
}
