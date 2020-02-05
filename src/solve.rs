use crate::can::ident::Lowercase;
use crate::collections::{ImMap, MutMap, SendMap};
use crate::module::symbol::{ModuleId, Symbol};
use crate::region::Located;
use crate::subs::{Content, Descriptor, FlatType, Mark, OptVariable, Rank, Subs, Variable};
use crate::types::Constraint::{self, *};
use crate::types::Problem;
use crate::types::Type::{self, *};
use crate::unify::{unify, Unified};
use crate::uniqueness::boolean_algebra::{self, Atom};
use std::sync::Arc;

// Type checking system adapted from Elm by Evan Czaplicki, BSD-3-Clause Licensed
// https://github.com/elm/compiler
// Thank you, Evan!

pub type SubsByModule = MutMap<ModuleId, ModuleSubs>;

#[derive(Clone, Debug)]
pub enum ModuleSubs {
    Invalid,
    Valid(Arc<Solved<Subs>>),
}

type Env = SendMap<Symbol, Variable>;

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
        let mut pools = Vec::with_capacity(num_pools);

        for _ in 0..num_pools {
            pools.push(Vec::new());
        }

        Pools(pools)
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
}

#[derive(Clone)]
struct State {
    vars_by_symbol: Env,
    mark: Mark,
}

/// A marker that a given Subs has been solved.
/// The only way to obtain a Solved<Subs> is by running the solver on it.
#[derive(Clone, Debug)]
pub struct Solved<T>(T);

impl<T> Solved<T> {
    pub fn inner(&self) -> &'_ T {
        &self.0
    }

    pub fn into_inner(self) -> T {
        self.0
    }
}

pub fn run(
    vars_by_symbol: &Env,
    problems: &mut Vec<Problem>,
    mut subs: Subs,
    constraint: &Constraint,
) -> (Solved<Subs>, Env) {
    let mut pools = Pools::default();
    let state = State {
        vars_by_symbol: vars_by_symbol.clone(),
        mark: Mark::NONE.next(),
    };
    let rank = Rank::toplevel();
    let state = solve(
        vars_by_symbol,
        state,
        rank,
        &mut pools,
        problems,
        &mut subs,
        constraint,
    );

    (Solved(subs), state.vars_by_symbol)
}

fn solve(
    vars_by_symbol: &Env,
    state: State,
    rank: Rank,
    pools: &mut Pools,
    problems: &mut Vec<Problem>,
    subs: &mut Subs,
    constraint: &Constraint,
) -> State {
    match constraint {
        True => state,
        SaveTheEnvironment => {
            let mut copy = state;

            copy.vars_by_symbol = vars_by_symbol.clone();

            copy
        }
        Eq(typ, expected_type, _region) => {
            let actual = type_to_var(subs, rank, pools, typ);
            let expected = type_to_var(subs, rank, pools, expected_type.get_type_ref());
            let Unified { vars, mismatches } = unify(subs, actual, expected);

            // TODO use region when reporting a problem
            problems.extend(mismatches);

            introduce(subs, rank, pools, &vars);

            state
        }
        Lookup(symbol, expected_type, _region) => {
            let var = *vars_by_symbol.get(&symbol).unwrap_or_else(|| {
                // TODO Instead of panicking, solve this as True and record
                // a Problem ("module Foo does not expose `bar`") for later.
                panic!(
                    "Could not find symbol {:?} in vars_by_symbol {:?}",
                    symbol, vars_by_symbol
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
            let expected = type_to_var(subs, rank, pools, expected_type.get_type_ref());
            let Unified { vars, mismatches } = unify(subs, actual, expected);

            // TODO use region when reporting a problem
            problems.extend(mismatches);

            introduce(subs, rank, pools, &vars);

            state
        }
        And(sub_constraints) => {
            let mut state = state;

            for sub_constraint in sub_constraints.iter() {
                state = solve(
                    vars_by_symbol,
                    state,
                    rank,
                    pools,
                    problems,
                    subs,
                    sub_constraint,
                );
            }

            state
        }
        Pattern(_region, _category, typ, expected) => {
            let actual = type_to_var(subs, rank, pools, typ);
            let expected = type_to_var(subs, rank, pools, expected.get_type_ref());
            let Unified { vars, mismatches } = unify(subs, actual, expected);

            // TODO use region when reporting a problem
            problems.extend(mismatches);

            introduce(subs, rank, pools, &vars);

            state
        }
        Let(let_con) => {
            match &let_con.ret_constraint {
                True if let_con.rigid_vars.is_empty() => {
                    introduce(subs, rank, pools, &let_con.flex_vars);

                    // If the return expression is guaranteed to solve,
                    // solve the assignments themselves and move on.
                    solve(
                        vars_by_symbol,
                        state,
                        rank,
                        pools,
                        problems,
                        subs,
                        &let_con.defs_constraint,
                    )
                }
                ret_con if let_con.rigid_vars.is_empty() && let_con.flex_vars.is_empty() => {
                    let state = solve(
                        vars_by_symbol,
                        state,
                        rank,
                        pools,
                        problems,
                        subs,
                        &let_con.defs_constraint,
                    );

                    // Add a variable for each def to new_vars_by_env.
                    let mut local_def_vars = ImMap::default();

                    for (symbol, loc_type) in let_con.def_types.iter() {
                        let var = type_to_var(subs, rank, pools, &loc_type.value);

                        local_def_vars.insert(
                            symbol.clone(),
                            Located {
                                value: var,
                                region: loc_type.region,
                            },
                        );
                    }

                    let mut new_vars_by_symbol = vars_by_symbol.clone();

                    for (symbol, loc_var) in local_def_vars.iter() {
                        if !new_vars_by_symbol.contains_key(&symbol) {
                            new_vars_by_symbol.insert(symbol.clone(), loc_var.value);
                        }
                    }

                    let new_state = solve(
                        &new_vars_by_symbol,
                        state,
                        rank,
                        pools,
                        problems,
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

                    let work_in_next_pools = |next_pools: &mut Pools| {
                        let pool: &mut Vec<Variable> = next_pools.get_mut(next_rank);

                        // Replace the contents of this pool with rigid_vars and flex_vars
                        pool.clear();
                        pool.reserve(rigid_vars.len() + flex_vars.len());
                        pool.extend(rigid_vars.iter());
                        pool.extend(flex_vars.iter());

                        // Add a variable for each def to local_def_vars.
                        let mut local_def_vars = ImMap::default();

                        for (symbol, loc_type) in let_con.def_types.iter() {
                            let def_type = loc_type.value.clone();
                            let var = type_to_var(subs, next_rank, next_pools, &def_type);

                            local_def_vars.insert(
                                symbol.clone(),
                                Located {
                                    value: var,
                                    region: loc_type.region,
                                },
                            );
                        }

                        // run solver in next pool

                        // Solve the assignments' constraints first.
                        let new_state = solve(
                            vars_by_symbol,
                            state,
                            next_rank,
                            next_pools,
                            problems,
                            subs,
                            &let_con.defs_constraint,
                        );
                        let young_mark = new_state.mark;
                        let visit_mark = young_mark.next();
                        let final_mark = visit_mark.next();

                        debug_assert!({
                            let offenders = next_pools
                                .get(next_rank)
                                .iter()
                                .filter(|var| {
                                    subs.get_without_compacting(crate::subs::Variable::clone(var))
                                        .rank
                                        .into_usize()
                                        > next_rank.into_usize()
                                })
                                .collect::<Vec<&crate::subs::Variable>>();

                            offenders.is_empty()
                        });

                        // pop pool
                        generalize(subs, young_mark, visit_mark, next_rank, next_pools);

                        next_pools.get_mut(next_rank).clear();

                        // check that things went well
                        debug_assert!(rigid_vars
                            .iter()
                            .all(|&var| subs.get_without_compacting(var).rank == Rank::NONE));
                        let mut new_vars_by_symbol = vars_by_symbol.clone();

                        for (symbol, loc_var) in local_def_vars.iter() {
                            if !new_vars_by_symbol.contains_key(&symbol) {
                                new_vars_by_symbol.insert(symbol.clone(), loc_var.value);
                            }
                        }

                        // Note that this vars_by_symbol is the one returned by the
                        // previous call to solve()
                        let temp_state = State {
                            vars_by_symbol: new_state.vars_by_symbol,
                            mark: final_mark,
                        };

                        // Now solve the body, using the new vars_by_symbol which includes
                        // the assignments' name-to-variable mappings.
                        let new_state = solve(
                            &new_vars_by_symbol,
                            temp_state,
                            rank,
                            next_pools,
                            problems,
                            subs,
                            &ret_con,
                        );

                        for (symbol, loc_var) in local_def_vars {
                            check_for_infinite_type(subs, problems, symbol, loc_var);
                        }

                        new_state
                    };

                    if next_rank.into_usize() < pools.len() {
                        work_in_next_pools(pools)
                    } else {
                        // TODO shouldn't this grow the pool, it does in the elm source
                        work_in_next_pools(&mut pools.clone())
                    }
                }
            }
        }
    }
}

fn type_to_var(subs: &mut Subs, rank: Rank, pools: &mut Pools, typ: &Type) -> Variable {
    type_to_variable(subs, rank, pools, &ImMap::default(), typ)
}

fn type_to_variable(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    aliases: &ImMap<Lowercase, Variable>,
    typ: &Type,
) -> Variable {
    match typ {
        Variable(var) => *var,
        Apply(symbol, args) => {
            let mut arg_vars = Vec::with_capacity(args.len());

            for arg in args {
                arg_vars.push(type_to_variable(subs, rank, pools, aliases, arg))
            }

            let flat_type = FlatType::Apply(*symbol, arg_vars);
            let content = Content::Structure(flat_type);

            register(subs, rank, pools, content)
        }
        EmptyRec => {
            let content = Content::Structure(FlatType::EmptyRecord);

            register(subs, rank, pools, content)
        }
        EmptyTagUnion => {
            let content = Content::Structure(FlatType::EmptyTagUnion);

            register(subs, rank, pools, content)
        }

        // This case is important for the rank of boolean variables
        Boolean(boolean_algebra::Bool(Atom::Variable(var), rest)) if rest.is_empty() => *var,
        Boolean(b) => {
            let content = Content::Structure(FlatType::Boolean(b.clone()));

            register(subs, rank, pools, content)
        }
        Function(args, ret_type) => {
            let mut arg_vars = Vec::with_capacity(args.len());

            for arg in args {
                arg_vars.push(type_to_variable(subs, rank, pools, aliases, arg))
            }

            let ret_var = type_to_variable(subs, rank, pools, aliases, ret_type);
            let content = Content::Structure(FlatType::Func(arg_vars, ret_var));

            register(subs, rank, pools, content)
        }
        Record(fields, ext) => {
            let mut field_vars = MutMap::default();

            for (field, field_type) in fields {
                field_vars.insert(
                    field.clone(),
                    type_to_variable(subs, rank, pools, aliases, field_type),
                );
            }

            let ext_var = type_to_variable(subs, rank, pools, aliases, ext);
            let content = Content::Structure(FlatType::Record(field_vars, ext_var));

            register(subs, rank, pools, content)
        }
        TagUnion(tags, ext) => {
            let mut tag_vars = MutMap::default();

            for (tag, tag_argument_types) in tags {
                let mut tag_argument_vars = Vec::with_capacity(tag_argument_types.len());

                for arg_type in tag_argument_types {
                    tag_argument_vars.push(type_to_variable(subs, rank, pools, aliases, arg_type));
                }

                tag_vars.insert(tag.clone(), tag_argument_vars);
            }

            let ext_var = type_to_variable(subs, rank, pools, aliases, ext);
            let content = Content::Structure(FlatType::TagUnion(tag_vars, ext_var));

            register(subs, rank, pools, content)
        }
        RecursiveTagUnion(rec_var, tags, ext) => {
            let mut tag_vars = MutMap::default();

            for (tag, tag_argument_types) in tags {
                let mut tag_argument_vars = Vec::with_capacity(tag_argument_types.len());

                for arg_type in tag_argument_types {
                    tag_argument_vars.push(type_to_variable(subs, rank, pools, aliases, arg_type));
                }

                tag_vars.insert(tag.clone(), tag_argument_vars);
            }

            let ext_var = type_to_variable(subs, rank, pools, aliases, ext);
            let content =
                Content::Structure(FlatType::RecursiveTagUnion(*rec_var, tag_vars, ext_var));

            register(subs, rank, pools, content)
        }
        Alias(symbol, args, alias_type) => {
            let mut arg_vars = Vec::with_capacity(args.len());
            let mut new_aliases = ImMap::default();

            for (arg, arg_type) in args {
                let arg_var = type_to_variable(subs, rank, pools, &ImMap::default(), arg_type);

                arg_vars.push((arg.clone(), arg_var));
                new_aliases.insert(arg.clone(), arg_var);
            }

            let alias_var = type_to_variable(subs, rank, pools, &new_aliases, alias_type);
            let content = Content::Alias(*symbol, arg_vars, alias_var);

            register(subs, rank, pools, content)
        }
        As(_, _) => panic!("TODO turn As into a variable"),
        Erroneous(problem) => {
            let content = Content::Structure(FlatType::Erroneous(problem.clone()));

            register(subs, rank, pools, content)
        }
    }
}

fn check_for_infinite_type(
    subs: &mut Subs,
    problems: &mut Vec<Problem>,
    symbol: Symbol,
    loc_var: Located<Variable>,
) {
    let var = loc_var.value;

    while let Some(recursive) = subs.occurs(var) {
        // try to make a tag union recursive, see if that helps
        if let Content::Structure(FlatType::TagUnion(tags, ext_var)) = subs.get(recursive).content {
            let rec_var = subs.fresh_unnamed_flex_var();

            let mut new_tags = MutMap::default();

            for (label, args) in tags {
                let new_args = args
                    .clone()
                    .into_iter()
                    .map(|var| if var == recursive { rec_var } else { var })
                    .collect();

                new_tags.insert(label.clone(), new_args);
            }

            let flat_type = FlatType::RecursiveTagUnion(rec_var, new_tags, ext_var);
            subs.set_content(recursive, Content::Structure(flat_type));
        } else {
            let error_type = subs.var_to_error_type(var);
            let problem = Problem::CircularType(symbol, error_type, loc_var.region);

            subs.set_content(var, Content::Error);

            problems.push(problem);
        }
    }
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
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

    match content {
        FlexVar(_) | RigidVar(_) | Error => group_rank,

        Structure(flat_type) => {
            match flat_type {
                Apply(_, args) => {
                    let mut rank = Rank::toplevel();

                    for var in args {
                        rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
                    }

                    rank
                }

                Func(arg_vars, ret_var) => {
                    let mut rank = adjust_rank(subs, young_mark, visit_mark, group_rank, ret_var);

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
                        rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
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

                Boolean(b) => {
                    let mut rank = Rank::toplevel();
                    for var in b.variables() {
                        rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
                    }

                    rank
                }

                Erroneous(_) => group_rank,
            }
        }

        Alias(_, args, _) => {
            let mut rank = Rank::toplevel();

            // from elm-compiler: THEORY: anything in the real_var would be Rank::toplevel()
            for (_, var) in args {
                rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
            }

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
    use crate::subs::Content::*;
    use crate::subs::FlatType::*;

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

                Func(arg_vars, ret_var) => {
                    let new_ret_var = deep_copy_var_help(subs, max_rank, pools, ret_var);
                    let arg_vars = arg_vars
                        .into_iter()
                        .map(|var| deep_copy_var_help(subs, max_rank, pools, var))
                        .collect();

                    Func(arg_vars, new_ret_var)
                }

                same @ EmptyRecord | same @ EmptyTagUnion | same @ Erroneous(_) => same,

                Record(fields, ext_var) => {
                    let mut new_fields = MutMap::default();

                    for (label, var) in fields {
                        new_fields.insert(label, deep_copy_var_help(subs, max_rank, pools, var));
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

                    for (tag, vars) in tags {
                        let new_vars: Vec<Variable> = vars
                            .into_iter()
                            .map(|var| deep_copy_var_help(subs, max_rank, pools, var))
                            .collect();
                        new_tags.insert(tag, new_vars);
                    }

                    RecursiveTagUnion(
                        deep_copy_var_help(subs, max_rank, pools, rec_var),
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

        FlexVar(_) | Error => copy,

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
