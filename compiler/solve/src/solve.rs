use roc_can::constraint::Constraint::{self, *};
use roc_can::expected::{Expected, PExpected};
use roc_collections::all::{ImMap, MutMap, SendMap};
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::boolean_algebra::{self, Bool};
use roc_types::solved_types::Solved;
use roc_types::subs::{Content, Descriptor, FlatType, Mark, OptVariable, Rank, Subs, Variable};
use roc_types::types::Type::{self, *};
use roc_types::types::{Alias, Category, ErrorType, PatternCategory, RecordField};
use roc_unify::unify::unify;
use roc_unify::unify::Unified::*;

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

#[derive(PartialEq, Debug, Clone)]
pub enum TypeError {
    BadExpr(Region, Category, ErrorType, Expected<ErrorType>),
    BadPattern(Region, PatternCategory, ErrorType, PExpected<ErrorType>),
    CircularType(Region, Symbol, ErrorType),
    BadType(roc_types::types::Problem),
}

#[derive(Clone, Debug, Default)]
pub struct Env {
    pub vars_by_symbol: SendMap<Symbol, Variable>,
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

    pub fn iter<'a>(&'a self) -> std::slice::Iter<'a, Vec<Variable>> {
        self.0.iter()
    }

    pub fn split_last(&self) -> (&Vec<Variable>, &[Vec<Variable>]) {
        self.0
            .split_last()
            .unwrap_or_else(|| panic!("Attempted to split_last() on non-empy Pools"))
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

pub fn run(
    env: &Env,
    problems: &mut Vec<TypeError>,
    mut subs: Subs,
    constraint: &Constraint,
) -> (Solved<Subs>, Env) {
    let mut pools = Pools::default();
    let state = State {
        env: env.clone(),
        mark: Mark::NONE.next(),
    };
    let rank = Rank::toplevel();
    let state = solve(
        env,
        state,
        rank,
        &mut pools,
        problems,
        &mut MutMap::default(),
        &mut subs,
        constraint,
    );

    (Solved(subs), state.env)
}

/// Modify an existing subs in-place instead
pub fn run_in_place(
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
fn solve(
    env: &Env,
    state: State,
    rank: Rank,
    pools: &mut Pools,
    problems: &mut Vec<TypeError>,
    cached_aliases: &mut MutMap<Symbol, Variable>,
    subs: &mut Subs,
    constraint: &Constraint,
) -> State {
    match constraint {
        True => state,
        SaveTheEnvironment => {
            // NOTE deviation: elm only copies the env into the state on SaveTheEnvironment
            let mut copy = state;

            copy.env = env.clone();

            copy
        }
        Eq(typ, expectation, category, region) => {
            let actual = type_to_var(subs, rank, pools, cached_aliases, typ);
            let expected = type_to_var(
                subs,
                rank,
                pools,
                cached_aliases,
                expectation.get_type_ref(),
            );

            match unify(subs, actual, expected) {
                Success(vars) => {
                    introduce(subs, rank, pools, &vars);

                    state
                }
                Failure(vars, actual_type, expected_type) => {
                    introduce(subs, rank, pools, &vars);

                    let problem = TypeError::BadExpr(
                        *region,
                        category.clone(),
                        actual_type,
                        expectation.clone().replace(expected_type),
                    );

                    problems.push(problem);

                    state
                }
                BadType(vars, problem) => {
                    introduce(subs, rank, pools, &vars);

                    problems.push(TypeError::BadType(problem));

                    state
                }
            }
        }
        Lookup(symbol, expectation, region) => {
            let var = *env.vars_by_symbol.get(&symbol).unwrap_or_else(|| {
                // TODO Instead of panicking, solve this as True and record
                // a TypeError ("module Foo does not expose `bar`") for later.
                panic!(
                    "Could not find symbol {:?} in vars_by_symbol {:?}",
                    symbol, env.vars_by_symbol
                )
            });

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
            let actual = deep_copy_var(subs, rank, pools, var);
            let expected = type_to_var(
                subs,
                rank,
                pools,
                cached_aliases,
                expectation.get_type_ref(),
            );
            match unify(subs, actual, expected) {
                Success(vars) => {
                    introduce(subs, rank, pools, &vars);

                    state
                }

                Failure(vars, actual_type, expected_type) => {
                    introduce(subs, rank, pools, &vars);

                    let problem = TypeError::BadExpr(
                        *region,
                        Category::Lookup(*symbol),
                        actual_type,
                        expectation.clone().replace(expected_type),
                    );

                    problems.push(problem);

                    state
                }
                BadType(vars, problem) => {
                    introduce(subs, rank, pools, &vars);

                    problems.push(TypeError::BadType(problem));

                    state
                }
            }
        }
        And(sub_constraints) => {
            let mut state = state;

            for sub_constraint in sub_constraints.iter() {
                state = solve(
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
        Pattern(region, category, typ, expectation) => {
            let actual = type_to_var(subs, rank, pools, cached_aliases, typ);
            let expected = type_to_var(
                subs,
                rank,
                pools,
                cached_aliases,
                expectation.get_type_ref(),
            );

            match unify(subs, actual, expected) {
                Success(vars) => {
                    introduce(subs, rank, pools, &vars);

                    state
                }
                Failure(vars, actual_type, expected_type) => {
                    introduce(subs, rank, pools, &vars);

                    let problem = TypeError::BadPattern(
                        *region,
                        category.clone(),
                        actual_type,
                        expectation.clone().replace(expected_type),
                    );

                    problems.push(problem);

                    state
                }
                BadType(vars, problem) => {
                    introduce(subs, rank, pools, &vars);

                    problems.push(TypeError::BadType(problem));

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
                    let mut local_def_vars = ImMap::default();

                    for (symbol, loc_type) in let_con.def_types.iter() {
                        let var = type_to_var(subs, rank, pools, cached_aliases, &loc_type.value);

                        local_def_vars.insert(
                            *symbol,
                            Located {
                                value: var,
                                region: loc_type.region,
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
                    let mut local_def_vars = ImMap::default();

                    for (symbol, loc_type) in let_con.def_types.iter() {
                        let def_type = &loc_type.value;

                        let var =
                            type_to_var(subs, next_rank, next_pools, cached_aliases, def_type);

                        local_def_vars.insert(
                            *symbol,
                            Located {
                                value: var,
                                region: loc_type.region,
                            },
                        );
                    }

                    // Solve the assignments' constraints first.
                    let State {
                        env: saved_env,
                        mark,
                    } = solve(
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
                                    let current = subs.get_without_compacting(
                                        roc_types::subs::Variable::clone(var),
                                    );

                                    current.rank.into_usize() > next_rank.into_usize()
                                })
                                .collect::<Vec<_>>();

                            let result = offenders.len();

                            if result > 0 {
                                dbg!(&subs, &offenders, &let_con.def_types, &let_con.def_aliases);
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
                                !subs.redundant(*var)
                                    && subs.get_without_compacting(*var).rank != Rank::NONE
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
    }
}

fn type_to_var(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    cached: &mut MutMap<Symbol, Variable>,
    typ: &Type,
) -> Variable {
    type_to_variable(subs, rank, pools, cached, typ)
}

/// Abusing existing functions for our purposes
/// this is to put a solved type back into subs
pub fn insert_type_into_subs(subs: &mut Subs, typ: &Type) -> Variable {
    let rank = Rank::NONE;
    let mut pools = Pools::default();
    let mut cached = MutMap::default();

    type_to_variable(subs, rank, &mut pools, &mut cached, typ)
}

fn type_to_variable(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    cached: &mut MutMap<Symbol, Variable>,
    typ: &Type,
) -> Variable {
    match typ {
        Variable(var) => *var,
        Apply(symbol, args) => {
            let mut arg_vars = Vec::with_capacity(args.len());

            for arg in args {
                arg_vars.push(type_to_variable(subs, rank, pools, cached, arg))
            }

            let flat_type = FlatType::Apply(*symbol, arg_vars);
            let content = Content::Structure(flat_type);

            register(subs, rank, pools, content)
        }
        EmptyRec => Variable::EMPTY_RECORD,
        EmptyTagUnion => Variable::EMPTY_TAG_UNION,

        // This case is important for the rank of boolean variables
        Boolean(boolean_algebra::Bool::Container(cvar, mvars)) if mvars.is_empty() => *cvar,
        Boolean(b) => {
            let content = Content::Structure(FlatType::Boolean(b.clone()));

            register(subs, rank, pools, content)
        }
        Function(args, closure_type, ret_type) => {
            let mut arg_vars = Vec::with_capacity(args.len());

            for arg in args {
                arg_vars.push(type_to_variable(subs, rank, pools, cached, arg))
            }

            let ret_var = type_to_variable(subs, rank, pools, cached, ret_type);
            let closure_var = type_to_variable(subs, rank, pools, cached, closure_type);
            let content = Content::Structure(FlatType::Func(arg_vars, closure_var, ret_var));

            register(subs, rank, pools, content)
        }
        Record(fields, ext) => {
            let mut field_vars = MutMap::default();

            for (field, field_type) in fields {
                use RecordField::*;

                let field_var = match field_type {
                    Required(typ) => Required(type_to_variable(subs, rank, pools, cached, typ)),
                    Optional(typ) => Optional(type_to_variable(subs, rank, pools, cached, typ)),
                    Demanded(typ) => Demanded(type_to_variable(subs, rank, pools, cached, typ)),
                };

                field_vars.insert(field.clone(), field_var);
            }

            let temp_ext_var = type_to_variable(subs, rank, pools, cached, ext);
            let new_ext_var = match roc_types::pretty_print::chase_ext_record(
                subs,
                temp_ext_var,
                &mut field_vars,
            ) {
                Ok(()) => Variable::EMPTY_RECORD,
                Err((new, _)) => new,
            };

            let content = Content::Structure(FlatType::Record(field_vars, new_ext_var));

            register(subs, rank, pools, content)
        }
        TagUnion(tags, ext) => {
            let mut tag_vars = MutMap::default();

            for (tag, tag_argument_types) in tags {
                let mut tag_argument_vars = Vec::with_capacity(tag_argument_types.len());

                for arg_type in tag_argument_types {
                    tag_argument_vars.push(type_to_variable(subs, rank, pools, cached, arg_type));
                }

                tag_vars.insert(tag.clone(), tag_argument_vars);
            }

            let temp_ext_var = type_to_variable(subs, rank, pools, cached, ext);
            let mut ext_tag_vec = Vec::new();
            let new_ext_var = match roc_types::pretty_print::chase_ext_tag_union(
                subs,
                temp_ext_var,
                &mut ext_tag_vec,
            ) {
                Ok(()) => Variable::EMPTY_TAG_UNION,
                Err((new, _)) => new,
            };
            tag_vars.extend(ext_tag_vec.into_iter());

            let content = Content::Structure(FlatType::TagUnion(tag_vars, new_ext_var));

            register(subs, rank, pools, content)
        }
        RecursiveTagUnion(rec_var, tags, ext) => {
            let mut tag_vars = MutMap::default();

            for (tag, tag_argument_types) in tags {
                let mut tag_argument_vars = Vec::with_capacity(tag_argument_types.len());

                for arg_type in tag_argument_types {
                    tag_argument_vars.push(type_to_variable(subs, rank, pools, cached, arg_type));
                }

                tag_vars.insert(tag.clone(), tag_argument_vars);
            }

            let temp_ext_var = type_to_variable(subs, rank, pools, cached, ext);
            let mut ext_tag_vec = Vec::new();
            let new_ext_var = match roc_types::pretty_print::chase_ext_tag_union(
                subs,
                temp_ext_var,
                &mut ext_tag_vec,
            ) {
                Ok(()) => Variable::EMPTY_TAG_UNION,
                Err((new, _)) => new,
            };
            tag_vars.extend(ext_tag_vec.into_iter());

            subs.set_content(*rec_var, Content::RecursionVar(None));

            let content =
                Content::Structure(FlatType::RecursiveTagUnion(*rec_var, tag_vars, new_ext_var));

            register(subs, rank, pools, content)
        }
        Alias(Symbol::BOOL_BOOL, _, _) => Variable::BOOL,
        Alias(symbol, args, alias_type) => {
            // TODO cache in uniqueness inference gives problems! all Int's get the same uniqueness var!
            // Cache aliases without type arguments. Commonly used aliases like `Int` would otherwise get O(n)
            // different variables (once for each occurence). The recursion restriction is required
            // for uniqueness types only: recursive aliases "introduce" an unbound uniqueness
            // attribute in the body, when
            //
            // Peano : [ S Peano, Z ]
            //
            // becomes
            //
            // Peano : [ S (Attr u Peano), Z ]
            //
            // This `u` variable can be different between lists, so giving just one variable to
            // this type is incorrect.
            // TODO does caching work at all with uniqueness types? even Int then hides a uniqueness variable
            let is_recursive = alias_type.is_recursive();
            let no_args = args.is_empty();
            /*
            if no_args && !is_recursive {
                if let Some(var) = cached.get(symbol) {
                    return *var;
                }
            }
            */

            let mut arg_vars = Vec::with_capacity(args.len());
            let mut new_aliases = ImMap::default();

            for (arg, arg_type) in args {
                let arg_var = type_to_variable(subs, rank, pools, cached, arg_type);

                arg_vars.push((arg.clone(), arg_var));
                new_aliases.insert(arg.clone(), arg_var);
            }

            let alias_var = type_to_variable(subs, rank, pools, cached, alias_type);
            let content = Content::Alias(*symbol, arg_vars, alias_var);

            let result = register(subs, rank, pools, content);

            if no_args && !is_recursive {
                // cached.insert(*symbol, result);
            }

            result
        }
        Erroneous(problem) => {
            let content = Content::Structure(FlatType::Erroneous(problem.clone()));

            register(subs, rank, pools, content)
        }
    }
}

fn check_for_infinite_type(
    subs: &mut Subs,
    problems: &mut Vec<TypeError>,
    symbol: Symbol,
    loc_var: Located<Variable>,
) {
    let var = loc_var.value;

    let is_uniq_infer = matches!(
        subs.get(var).content,
        Content::Alias(Symbol::ATTR_ATTR, _, _)
    );

    while let Some((recursive, chain)) = subs.occurs(var) {
        let description = subs.get(recursive);
        let content = description.content;

        // try to make a tag union recursive, see if that helps
        match content {
            Content::Structure(FlatType::TagUnion(tags, ext_var)) => {
                if !is_uniq_infer {
                    let rec_var = subs.fresh_unnamed_flex_var();
                    subs.set_rank(rec_var, description.rank);

                    let mut new_tags = MutMap::default();

                    for (label, args) in &tags {
                        let new_args: Vec<_> = args
                            .iter()
                            .map(|var| subs.explicit_substitute(recursive, rec_var, *var))
                            .collect();

                        new_tags.insert(label.clone(), new_args);
                    }

                    let new_ext_var = subs.explicit_substitute(recursive, rec_var, ext_var);

                    let flat_type = FlatType::RecursiveTagUnion(rec_var, new_tags, new_ext_var);

                    subs.set_content(recursive, Content::Structure(flat_type));
                } else {
                    // Sometimes, the recursion "starts" at the tag-union, not an `Attr`. Here we
                    // We use the path that `occurs` took to find the recursion to go one step
                    // forward in the recursion and find the `Attr` there.
                    let index = 0;
                    match subs.get(chain[index]).content {
                        Content::Structure(FlatType::Apply(Symbol::ATTR_ATTR, args)) => {
                            debug_assert!(args.len() == 2);
                            debug_assert!(
                                subs.get_root_key_without_compacting(recursive)
                                    == subs.get_root_key_without_compacting(args[1])
                            );

                            // NOTE this ensures we use the same uniqueness var for the whole spine
                            // that might add too much uniqueness restriction.
                            // using `subs.fresh_unnamed_flex_var()` loosens it.
                            let uniq_var = args[0];
                            let tag_union_var = recursive;
                            let recursive = chain[index];

                            correct_recursive_attr(
                                subs,
                                recursive,
                                uniq_var,
                                tag_union_var,
                                ext_var,
                                &tags,
                            );
                        }
                        _ => circular_error(subs, problems, symbol, &loc_var),
                    }
                }
            }
            Content::Structure(FlatType::Apply(Symbol::ATTR_ATTR, args)) => {
                debug_assert!(args.len() == 2);
                let uniq_var = args[0];
                let tag_union_var = args[1];
                let nested_description = subs.get(tag_union_var);
                match nested_description.content {
                    Content::Structure(FlatType::TagUnion(tags, ext_var)) => {
                        correct_recursive_attr(
                            subs,
                            recursive,
                            uniq_var,
                            tag_union_var,
                            ext_var,
                            &tags,
                        );
                    }
                    _ => circular_error(subs, problems, symbol, &loc_var),
                }
            }
            Content::Structure(FlatType::Boolean(Bool::Container(_cvar, _mvars))) => {
                // We have a loop in boolean attributes. The attributes can be seen as constraints
                // too, so if we have
                //
                // Container( u1, { u2, u3 } )
                //
                // That means u1 >= u2 and u1 >= u3
                //
                // Now if u1 occurs in the definition of u2, then that's like saying u1 >= u2 >= u1,
                // which can only be true if u1 == u2. So that's what we do with unify.
                for var in chain {
                    if let Content::Structure(FlatType::Boolean(_)) =
                        subs.get_without_compacting(var).content
                    {
                        // this unify just makes new pools. is that bad?
                        let outcome = unify(subs, recursive, var);
                        debug_assert!(matches!(outcome, roc_unify::unify::Unified::Success(_)));
                    }
                }

                boolean_algebra::flatten(subs, recursive);
            }

            _other => circular_error(subs, problems, symbol, &loc_var),
        }
    }
}

fn content_attr(u: Variable, a: Variable) -> Content {
    Content::Structure(FlatType::Apply(Symbol::ATTR_ATTR, vec![u, a]))
}

fn correct_recursive_attr(
    subs: &mut Subs,
    recursive: Variable,
    uniq_var: Variable,
    tag_union_var: Variable,
    ext_var: Variable,
    tags: &MutMap<TagName, Vec<Variable>>,
) {
    let rec_var = subs.fresh_unnamed_flex_var();
    let attr_var = subs.fresh_unnamed_flex_var();

    let content = content_attr(uniq_var, rec_var);
    subs.set_content(attr_var, content);

    let mut new_tags = MutMap::default();

    let new_ext_var = subs.explicit_substitute(recursive, attr_var, ext_var);
    for (label, args) in tags {
        let new_args: Vec<_> = args
            .iter()
            .map(|var| subs.explicit_substitute(recursive, attr_var, *var))
            .collect();

        new_tags.insert(label.clone(), new_args);
    }

    let new_tag_type = FlatType::RecursiveTagUnion(rec_var, new_tags, new_ext_var);
    subs.set_content(tag_union_var, Content::Structure(new_tag_type));

    let new_recursive = content_attr(uniq_var, tag_union_var);

    subs.set_content(recursive, new_recursive);
}

fn circular_error(
    subs: &mut Subs,
    problems: &mut Vec<TypeError>,
    symbol: Symbol,
    loc_var: &Located<Variable>,
) {
    let var = loc_var.value;
    let (error_type, _) = subs.var_to_error_type(var);
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
                let rank = subs.get(var).rank;

                pools.get_mut(rank).push(var);
            }
        }
    }

    // For variables with rank young_rank, if rank < young_rank: register in old pool,
    // otherwise generalize
    for &var in last_pool {
        if !subs.redundant(var) {
            let mut desc = subs.get(var);

            if desc.rank < young_rank {
                pools.get_mut(desc.rank).push(var);
            } else {
                desc.rank = Rank::NONE;
                subs.set(var, desc);
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
        let desc = subs.get(var);
        let rank = desc.rank;

        subs.set(
            var,
            Descriptor {
                rank,
                mark: young_mark,
                content: desc.content,
                copy: desc.copy,
            },
        );

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
    let mut desc = subs.get(var);
    let mark = desc.mark;

    if mark == young_mark {
        desc.mark = visit_mark;

        let content = desc.content.clone();
        let mut marked_desc = desc.clone();

        // Mark the variable as visited before adjusting content, as it may be cyclic.
        subs.set(var, desc);

        let max_rank = adjust_rank_content(subs, young_mark, visit_mark, group_rank, content);
        marked_desc.rank = max_rank;

        debug_assert_eq!(marked_desc.mark, visit_mark);

        subs.set(var, marked_desc);

        max_rank
    } else if mark == visit_mark {
        desc.rank
    } else {
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
    content: Content,
) -> Rank {
    use roc_types::subs::Content::*;
    use roc_types::subs::FlatType::*;

    match content {
        FlexVar(_) | RigidVar(_) | RecursionVar(_) | Error => group_rank,

        Structure(flat_type) => {
            match flat_type {
                Apply(_, args) => {
                    let mut rank = Rank::toplevel();

                    for var in args {
                        rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
                    }

                    rank
                }

                Func(arg_vars, closure_var, ret_var) => {
                    let mut rank = adjust_rank(subs, young_mark, visit_mark, group_rank, ret_var);

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
                            closure_var,
                        ));
                    }

                    for var in arg_vars {
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
                    let mut rank = adjust_rank(subs, young_mark, visit_mark, group_rank, ext_var);

                    for (_, var) in fields {
                        rank = rank.max(adjust_rank(
                            subs,
                            young_mark,
                            visit_mark,
                            group_rank,
                            var.into_inner(),
                        ));
                    }

                    rank
                }

                TagUnion(tags, ext_var) => {
                    let mut rank = adjust_rank(subs, young_mark, visit_mark, group_rank, ext_var);

                    for var in tags.values().flatten() {
                        rank =
                            rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, *var));
                    }

                    rank
                }

                RecursiveTagUnion(rec_var, tags, ext_var) => {
                    let mut rank = adjust_rank(subs, young_mark, visit_mark, group_rank, rec_var);
                    rank = rank.max(adjust_rank(
                        subs, young_mark, visit_mark, group_rank, ext_var,
                    ));

                    for var in tags.values().flatten() {
                        rank =
                            rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, *var));
                    }

                    rank
                }

                Boolean(Bool::Shared) => Rank::toplevel(),
                Boolean(Bool::Container(cvar, mvars)) => {
                    let mut rank = adjust_rank(subs, young_mark, visit_mark, group_rank, cvar);

                    for var in mvars {
                        rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
                    }

                    rank
                }

                Erroneous(_) => group_rank,
            }
        }

        Alias(_, args, real_var) => {
            let mut rank = Rank::toplevel();

            for (_, var) in args {
                rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
            }

            // from elm-compiler: THEORY: anything in the real_var would be Rank::toplevel()
            // this theory is not true in Roc! aliases of function types capture the closure var
            rank = rank.max(adjust_rank(
                subs, young_mark, visit_mark, group_rank, real_var,
            ));

            rank
        }
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
            let new_flat_type = match flat_type {
                Apply(symbol, args) => {
                    let args = args
                        .into_iter()
                        .map(|var| instantiate_rigids_help(subs, max_rank, pools, var))
                        .collect();

                    Apply(symbol, args)
                }

                Func(arg_vars, closure_var, ret_var) => {
                    let new_ret_var = instantiate_rigids_help(subs, max_rank, pools, ret_var);
                    let new_closure_var =
                        instantiate_rigids_help(subs, max_rank, pools, closure_var);
                    let arg_vars = arg_vars
                        .into_iter()
                        .map(|var| instantiate_rigids_help(subs, max_rank, pools, var))
                        .collect();

                    Func(arg_vars, new_closure_var, new_ret_var)
                }

                same @ EmptyRecord | same @ EmptyTagUnion | same @ Erroneous(_) => same,

                Record(fields, ext_var) => {
                    let mut new_fields = MutMap::default();

                    for (label, field) in fields {
                        use RecordField::*;

                        let new_field = match field {
                            Demanded(var) => {
                                Demanded(instantiate_rigids_help(subs, max_rank, pools, var))
                            }
                            Required(var) => {
                                Required(instantiate_rigids_help(subs, max_rank, pools, var))
                            }
                            Optional(var) => {
                                Optional(instantiate_rigids_help(subs, max_rank, pools, var))
                            }
                        };

                        new_fields.insert(label, new_field);
                    }

                    Record(
                        new_fields,
                        instantiate_rigids_help(subs, max_rank, pools, ext_var),
                    )
                }

                TagUnion(tags, ext_var) => {
                    let mut new_tags = MutMap::default();

                    for (tag, vars) in tags {
                        let new_vars: Vec<Variable> = vars
                            .into_iter()
                            .map(|var| instantiate_rigids_help(subs, max_rank, pools, var))
                            .collect();
                        new_tags.insert(tag, new_vars);
                    }

                    TagUnion(
                        new_tags,
                        instantiate_rigids_help(subs, max_rank, pools, ext_var),
                    )
                }

                RecursiveTagUnion(rec_var, tags, ext_var) => {
                    let mut new_tags = MutMap::default();

                    let new_rec_var = instantiate_rigids_help(subs, max_rank, pools, rec_var);

                    for (tag, vars) in tags {
                        let new_vars: Vec<Variable> = vars
                            .into_iter()
                            .map(|var| instantiate_rigids_help(subs, max_rank, pools, var))
                            .collect();
                        new_tags.insert(tag, new_vars);
                    }

                    RecursiveTagUnion(
                        new_rec_var,
                        new_tags,
                        instantiate_rigids_help(subs, max_rank, pools, ext_var),
                    )
                }

                Boolean(b) => {
                    let mut mapper = |var| instantiate_rigids_help(subs, max_rank, pools, var);

                    Boolean(b.map_variables(&mut mapper))
                }
            };

            subs.set(copy, make_descriptor(Structure(new_flat_type)));

            copy
        }

        FlexVar(_) | RecursionVar(_) | Error => copy,

        RigidVar(name) => {
            subs.set(copy, make_descriptor(FlexVar(Some(name))));

            copy
        }

        Alias(symbol, args, real_type_var) => {
            let new_args = args
                .into_iter()
                .map(|(name, var)| (name, instantiate_rigids_help(subs, max_rank, pools, var)))
                .collect();
            let new_real_type_var = instantiate_rigids_help(subs, max_rank, pools, real_type_var);
            let new_content = Alias(symbol, new_args, new_real_type_var);

            subs.set(copy, make_descriptor(new_content));

            copy
        }
    }
}

fn deep_copy_var(subs: &mut Subs, rank: Rank, pools: &mut Pools, var: Variable) -> Variable {
    let copy = deep_copy_var_help(subs, rank, pools, var);

    subs.restore(var);

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
                    let args = args
                        .into_iter()
                        .map(|var| deep_copy_var_help(subs, max_rank, pools, var))
                        .collect();

                    Apply(symbol, args)
                }

                Func(arg_vars, closure_var, ret_var) => {
                    let new_ret_var = deep_copy_var_help(subs, max_rank, pools, ret_var);
                    let new_closure_var = deep_copy_var_help(subs, max_rank, pools, closure_var);
                    let arg_vars = arg_vars
                        .into_iter()
                        .map(|var| deep_copy_var_help(subs, max_rank, pools, var))
                        .collect();

                    Func(arg_vars, new_closure_var, new_ret_var)
                }

                same @ EmptyRecord | same @ EmptyTagUnion | same @ Erroneous(_) => same,

                Record(fields, ext_var) => {
                    let mut new_fields = MutMap::default();

                    for (label, field) in fields {
                        use RecordField::*;

                        let new_field = match field {
                            Demanded(var) => {
                                Demanded(deep_copy_var_help(subs, max_rank, pools, var))
                            }
                            Required(var) => {
                                Required(deep_copy_var_help(subs, max_rank, pools, var))
                            }
                            Optional(var) => {
                                Optional(deep_copy_var_help(subs, max_rank, pools, var))
                            }
                        };

                        new_fields.insert(label, new_field);
                    }

                    Record(
                        new_fields,
                        deep_copy_var_help(subs, max_rank, pools, ext_var),
                    )
                }

                TagUnion(tags, ext_var) => {
                    let mut new_tags = MutMap::default();

                    for (tag, vars) in tags {
                        let new_vars: Vec<Variable> = vars
                            .into_iter()
                            .map(|var| deep_copy_var_help(subs, max_rank, pools, var))
                            .collect();
                        new_tags.insert(tag, new_vars);
                    }

                    TagUnion(new_tags, deep_copy_var_help(subs, max_rank, pools, ext_var))
                }

                RecursiveTagUnion(rec_var, tags, ext_var) => {
                    let mut new_tags = MutMap::default();

                    let new_rec_var = deep_copy_var_help(subs, max_rank, pools, rec_var);

                    for (tag, vars) in tags {
                        let new_vars: Vec<Variable> = vars
                            .into_iter()
                            .map(|var| deep_copy_var_help(subs, max_rank, pools, var))
                            .collect();
                        new_tags.insert(tag, new_vars);
                    }

                    RecursiveTagUnion(
                        new_rec_var,
                        new_tags,
                        deep_copy_var_help(subs, max_rank, pools, ext_var),
                    )
                }

                Boolean(b) => {
                    let mut mapper = |var| deep_copy_var_help(subs, max_rank, pools, var);

                    Boolean(b.map_variables(&mut mapper))
                }
            };

            subs.set(copy, make_descriptor(Structure(new_flat_type)));

            copy
        }

        FlexVar(_) | RecursionVar(_) | Error => copy,

        RigidVar(name) => {
            subs.set(copy, make_descriptor(FlexVar(Some(name))));

            copy
        }

        Alias(symbol, args, real_type_var) => {
            let new_args = args
                .into_iter()
                .map(|(name, var)| (name, deep_copy_var_help(subs, max_rank, pools, var)))
                .collect();
            let new_real_type_var = deep_copy_var_help(subs, max_rank, pools, real_type_var);
            let new_content = Alias(symbol, new_args, new_real_type_var);

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
