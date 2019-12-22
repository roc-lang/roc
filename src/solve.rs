use crate::can::ident::Lowercase;
use crate::can::symbol::Symbol;
use crate::collections::ImMap;
use crate::region::Located;
use crate::subs::{Content, Descriptor, FlatType, Mark, Rank, Subs, Variable};
use crate::types::Constraint::{self, *};
use crate::types::Problem;
use crate::types::Type::{self, *};
use crate::unify::{unify, Problems};

type Env = ImMap<Symbol, Variable>;

const DEFAULT_POOLS: usize = 8;

#[derive(Clone)]
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

struct State {
    vars_by_symbol: Env,
    mark: Mark,
}

pub fn run(
    vars_by_symbol: &Env,
    problems: &mut Problems,
    subs: &mut Subs,
    constraint: &Constraint,
) {
    let mut pools = Pools::default();
    let state = State {
        vars_by_symbol: vars_by_symbol.clone(),
        mark: Mark::none().next(),
    };
    let rank = Rank::toplevel();

    solve(
        vars_by_symbol,
        state,
        rank,
        &mut pools,
        problems,
        subs,
        constraint,
    );
}

fn solve(
    vars_by_symbol: &Env,
    state: State,
    rank: Rank,
    pools: &mut Pools,
    problems: &mut Problems,
    subs: &mut Subs,
    constraint: &Constraint,
) -> State {
    match constraint {
        True => state,
        Eq(typ, expected_type, _region) => {
            let actual = type_to_var(subs, rank, pools, typ);
            let expected = type_to_var(subs, rank, pools, expected_type.get_type_ref());

            // TODO use region when reporting a problem
            let vars = unify(subs, problems, actual, expected);

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
            let actual = var; // TODO deep copy this var
            let expected = type_to_var(subs, rank, pools, expected_type.get_type_ref());

            // TODO use region when reporting a problem
            let vars = unify(subs, problems, actual, expected);

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
            // TODO use region?
            let actual = type_to_var(subs, rank, pools, typ);
            let expected = type_to_var(subs, rank, pools, expected.get_type_ref());
            let vars = unify(subs, problems, actual, expected);

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
                    // Add a variable for each assignment to the vars_by_symbol.
                    let mut locals = ImMap::default();

                    for (symbol, loc_type) in let_con.def_types.iter() {
                        let var = type_to_var(subs, rank, pools, &loc_type.value);

                        locals.insert(
                            symbol.clone(),
                            Located {
                                value: var,
                                region: loc_type.region,
                            },
                        );
                    }
                    let mut new_vars_by_symbol = vars_by_symbol.clone();

                    for (symbol, loc_var) in locals.iter() {
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

                    for (symbol, loc_var) in locals {
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
                    for &var in rigid_vars.iter() {
                        subs.set_rank(var, next_rank);
                    }

                    for &var in flex_vars.iter() {
                        subs.set_rank(var, next_rank);
                    }

                    let work_in_next_pools = |next_pools: &mut Pools| {
                        let pool: &mut Vec<Variable> = next_pools.get_mut(next_rank);

                        // Replace the contents of this pool with rigid_vars and flex_vars
                        pool.clear();
                        pool.reserve(rigid_vars.len() + flex_vars.len());
                        pool.extend(rigid_vars.iter());
                        pool.extend(flex_vars.iter());

                        // Add a variable for each assignment to the vars_by_symbol.
                        let mut locals = ImMap::default();

                        for (symbol, loc_type) in let_con.def_types.iter() {
                            let def_type = loc_type.value.clone();
                            let var = type_to_var(subs, next_rank, next_pools, &def_type);

                            locals.insert(
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

                        // pop pool
                        generalize(subs, young_mark, visit_mark, next_rank, next_pools);

                        next_pools.get_mut(next_rank).clear();

                        // check that things went well
                        debug_assert!(rigid_vars
                            .iter()
                            .all(|&var| subs.get_without_compacting(var).rank == Rank::none()));

                        let mut new_vars_by_symbol = vars_by_symbol.clone();

                        for (symbol, loc_var) in locals.iter() {
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

                        for (symbol, loc_var) in locals {
                            check_for_infinite_type(subs, problems, symbol, loc_var);
                        }

                        new_state
                    };

                    if next_rank.into_usize() < pools.len() {
                        work_in_next_pools(pools)
                    } else {
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
        Apply {
            module_name,
            name,
            args,
        } => {
            let mut arg_vars = Vec::with_capacity(args.len());

            for arg in args {
                arg_vars.push(type_to_variable(subs, rank, pools, aliases, arg))
            }

            let flat_type = FlatType::Apply {
                module_name: module_name.clone(),
                name: name.clone(),
                args: arg_vars,
            };
            let content = Content::Structure(flat_type);

            register(subs, rank, pools, content)
        }
        EmptyRec => {
            let content = Content::Structure(FlatType::EmptyRecord);

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
            let mut field_vars = ImMap::default();

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
        Alias(home, name, args, alias_type) => {
            let mut arg_vars = Vec::with_capacity(args.len());
            let mut new_aliases = ImMap::default();

            for (arg, arg_type) in args {
                let arg_var = type_to_variable(subs, rank, pools, aliases, arg_type);

                arg_vars.push((arg.clone(), arg_var));
                new_aliases.insert(arg.clone(), arg_var);
            }

            let alias_var = type_to_variable(subs, rank, pools, &new_aliases, alias_type);
            let content = Content::Alias(home.clone(), name.clone(), arg_vars, alias_var);

            register(subs, rank, pools, content)
        }
        Erroneous(problem) => {
            let content = Content::Structure(FlatType::Erroneous(problem.clone()));

            register(subs, rank, pools, content)
        }
    }
}

fn check_for_infinite_type(
    subs: &mut Subs,
    problems: &mut Problems,
    symbol: Symbol,
    loc_var: Located<Variable>,
) {
    let var = loc_var.value;

    if subs.occurs(var) {
        let error_type = subs.var_to_error_type(var);
        let problem = Problem::CircularType(symbol, error_type, loc_var.region);

        subs.set_content(var, Content::Error(problem.clone()));

        problems.push(problem);
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
                desc.rank = Rank::none();

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

        subs.set(var, marked_desc);

        max_rank
    } else if mark == visit_mark {
        desc.rank
    } else {
        let min_rank = desc.rank.min(group_rank);

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
        FlexVar(_) | RigidVar(_) | Error(_) => group_rank,

        Structure(flat_type) => {
            match flat_type {
                Apply { args, .. } => {
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

                Record(fields, ext_var) => {
                    let mut rank = adjust_rank(subs, young_mark, visit_mark, group_rank, ext_var);

                    for (_, var) in fields {
                        rank = rank.max(adjust_rank(subs, young_mark, visit_mark, group_rank, var));
                    }

                    rank
                }
                Erroneous(_) => group_rank,
            }
        }

        Alias(_, _, args, _) => {
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

fn register(subs: &mut Subs, rank: Rank, pools: &mut Pools, content: Content) -> Variable {
    let var = subs.fresh(Descriptor {
        content,
        rank,
        mark: Mark::none(),
        copy: None,
    });

    pools.get_mut(rank).push(var);

    var
}
