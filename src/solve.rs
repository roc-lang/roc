use crate::can::ident::{Lowercase, TagName};
use crate::collections::{ImMap, MutMap, SendMap};
use crate::module::symbol::{ModuleId, Symbol};
use crate::region::{Located, Region};
use crate::subs::{Content, Descriptor, FlatType, Mark, OptVariable, Rank, Subs, VarId, Variable};
use crate::types::Alias;
use crate::types::Constraint::{self, *};
use crate::types::Problem;
use crate::types::Type::{self, *};
use crate::unify::{unify, Unified};
use crate::uniqueness::boolean_algebra::{self, Atom};

// Type checking system adapted from Elm by Evan Czaplicki, BSD-3-Clause Licensed
// https://github.com/elm/compiler
// Thank you, Evan!

pub type SubsByModule = MutMap<ModuleId, ExposedModuleTypes>;

#[derive(Clone, Debug)]
pub enum ExposedModuleTypes {
    Invalid,
    Valid(MutMap<Symbol, SolvedType>, MutMap<Symbol, Alias>),
}

/// This is a fully solved type, with no Variables remaining in it.
#[derive(Debug, Clone, PartialEq)]
pub enum SolvedType {
    /// A function. The types of its arguments, then the type of its return value.
    Func(Vec<SolvedType>, Box<SolvedType>),
    /// Applying a type to some arguments (e.g. Map.Map String Int)
    Apply(Symbol, Vec<SolvedType>),
    /// A bound type variable, e.g. `a` in `(a -> a)`
    Rigid(Lowercase),
    Flex(VarId),
    Wildcard,
    /// Inline type alias, e.g. `as List a` in `[ Cons a (List a), Nil ] as List a`
    Record {
        fields: Vec<(Lowercase, SolvedType)>,
        /// The row type variable in an open record, e.g. the `r` in `{ name: Str }r`.
        /// This is None if it's a closed record annotation like `{ name: Str }`.
        ext: Box<SolvedType>,
    },
    EmptyRecord,
    TagUnion(Vec<(TagName, Vec<SolvedType>)>, Box<SolvedType>),
    RecursiveTagUnion(VarId, Vec<(TagName, Vec<SolvedType>)>, Box<SolvedType>),
    EmptyTagUnion,
    /// A type from an Invalid module
    Erroneous(Problem),

    /// A type alias
    Alias(Symbol, Vec<(Lowercase, SolvedType)>, Box<SolvedType>),

    /// a boolean algebra Bool
    Boolean(SolvedAtom, Vec<SolvedAtom>),

    /// A type error
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SolvedAtom {
    Zero,
    One,
    Variable(VarId),
}

impl SolvedAtom {
    pub fn from_atom(atom: boolean_algebra::Atom) -> Self {
        use boolean_algebra::Atom::*;

        // NOTE we blindly trust that `var` is a root and has a FlexVar as content
        match atom {
            Zero => SolvedAtom::Zero,
            One => SolvedAtom::One,
            Variable(var) => SolvedAtom::Variable(VarId::from_var(var)),
        }
    }
}

impl SolvedType {
    pub fn new(solved_subs: &Solved<Subs>, var: Variable) -> Self {
        Self::from_var(solved_subs.inner(), var)
    }

    pub fn from_type(solved_subs: &Solved<Subs>, typ: Type) -> Self {
        use crate::types::Type::*;

        match typ {
            EmptyRec => SolvedType::EmptyRecord,
            EmptyTagUnion => SolvedType::EmptyTagUnion,
            Apply(symbol, types) => {
                let mut solved_types = Vec::with_capacity(types.len());

                for typ in types {
                    let solved_type = Self::from_type(solved_subs, typ);

                    solved_types.push(solved_type);
                }

                SolvedType::Apply(symbol, solved_types)
            }
            Function(args, box_ret) => {
                let solved_ret = Self::from_type(solved_subs, *box_ret);
                let mut solved_args = Vec::with_capacity(args.len());

                for arg in args.into_iter() {
                    let solved_arg = Self::from_type(solved_subs, arg);

                    solved_args.push(solved_arg);
                }

                SolvedType::Func(solved_args, Box::new(solved_ret))
            }
            Record(fields, box_ext) => {
                let solved_ext = Self::from_type(solved_subs, *box_ext);
                let mut solved_fields = Vec::with_capacity(fields.len());
                for (label, typ) in fields {
                    let solved_type = Self::from_type(solved_subs, typ);

                    solved_fields.push((label.clone(), solved_type));
                }

                SolvedType::Record {
                    fields: solved_fields,
                    ext: Box::new(solved_ext),
                }
            }
            TagUnion(tags, box_ext) => {
                let solved_ext = Self::from_type(solved_subs, *box_ext);
                let mut solved_tags = Vec::with_capacity(tags.len());
                for (tag_name, types) in tags {
                    let mut solved_types = Vec::with_capacity(types.len());

                    for typ in types {
                        let solved_type = Self::from_type(solved_subs, typ);
                        solved_types.push(solved_type);
                    }

                    solved_tags.push((tag_name.clone(), solved_types));
                }

                SolvedType::TagUnion(solved_tags, Box::new(solved_ext))
            }
            RecursiveTagUnion(rec_var, tags, box_ext) => {
                let solved_ext = Self::from_type(solved_subs, *box_ext);
                let mut solved_tags = Vec::with_capacity(tags.len());
                for (tag_name, types) in tags {
                    let mut solved_types = Vec::with_capacity(types.len());

                    for typ in types {
                        let solved_type = Self::from_type(solved_subs, typ);
                        solved_types.push(solved_type);
                    }

                    solved_tags.push((tag_name.clone(), solved_types));
                }

                SolvedType::RecursiveTagUnion(
                    VarId::from_var(rec_var),
                    solved_tags,
                    Box::new(solved_ext),
                )
            }
            Erroneous(problem) => SolvedType::Erroneous(problem),
            Alias(symbol, args, box_type) => {
                let solved_type = Self::from_type(solved_subs, *box_type);
                let mut solved_args = Vec::with_capacity(args.len());

                for (name, var) in args {
                    solved_args.push((name.clone(), Self::from_type(solved_subs, var)));
                }

                SolvedType::Alias(symbol, solved_args, Box::new(solved_type))
            }
            Boolean(val) => {
                let free = SolvedAtom::from_atom(val.0);

                let mut rest = Vec::with_capacity(val.1.len());
                for atom in val.1 {
                    rest.push(SolvedAtom::from_atom(atom));
                }
                SolvedType::Boolean(free, rest)
            }
            Variable(var) => Self::from_var(solved_subs.inner(), var),
        }
    }

    fn from_var(subs: &Subs, var: Variable) -> Self {
        use Content::*;
        match subs.get_without_compacting(var).content {
            FlexVar(_) => SolvedType::Flex(VarId::from_var(var)),
            RigidVar(name) => SolvedType::Rigid(name),
            Structure(flat_type) => Self::from_flat_type(subs, flat_type),
            Alias(symbol, args, actual_var) => {
                let mut new_args = Vec::with_capacity(args.len());

                for (arg_name, arg_var) in args {
                    new_args.push((arg_name, Self::from_var(subs, arg_var)));
                }

                let aliased_to = Self::from_var(subs, actual_var);

                SolvedType::Alias(symbol, new_args, Box::new(aliased_to))
            }
            Error => SolvedType::Error,
        }
    }

    fn from_flat_type(subs: &Subs, flat_type: FlatType) -> Self {
        use crate::subs::FlatType::*;

        match flat_type {
            Apply(symbol, args) => {
                let mut new_args = Vec::with_capacity(args.len());

                for var in args {
                    new_args.push(Self::from_var(subs, var));
                }

                SolvedType::Apply(symbol, new_args)
            }
            Func(args, ret) => {
                let mut new_args = Vec::with_capacity(args.len());

                for var in args {
                    new_args.push(Self::from_var(subs, var));
                }

                let ret = Self::from_var(subs, ret);

                SolvedType::Func(new_args, Box::new(ret))
            }
            Record(fields, ext_var) => {
                let mut new_fields = Vec::with_capacity(fields.len());

                for (label, var) in fields {
                    let field = Self::from_var(subs, var);

                    new_fields.push((label, field));
                }

                let ext = Self::from_var(subs, ext_var);

                SolvedType::Record {
                    fields: new_fields,
                    ext: Box::new(ext),
                }
            }
            TagUnion(tags, ext_var) => {
                let mut new_tags = Vec::with_capacity(tags.len());

                for (tag_name, args) in tags {
                    let mut new_args = Vec::with_capacity(args.len());

                    for var in args {
                        new_args.push(Self::from_var(subs, var));
                    }

                    new_tags.push((tag_name, new_args));
                }

                let ext = Self::from_var(subs, ext_var);

                SolvedType::TagUnion(new_tags, Box::new(ext))
            }
            RecursiveTagUnion(rec_var, tags, ext_var) => {
                let mut new_tags = Vec::with_capacity(tags.len());

                for (tag_name, args) in tags {
                    let mut new_args = Vec::with_capacity(args.len());

                    for var in args {
                        new_args.push(Self::from_var(subs, var));
                    }

                    new_tags.push((tag_name, new_args));
                }

                let ext = Self::from_var(subs, ext_var);

                SolvedType::RecursiveTagUnion(VarId::from_var(rec_var), new_tags, Box::new(ext))
            }
            EmptyRecord => SolvedType::EmptyRecord,
            EmptyTagUnion => SolvedType::EmptyTagUnion,
            Boolean(val) => {
                let free = SolvedAtom::from_atom(val.0);

                let mut rest = Vec::with_capacity(val.1.len());
                for atom in val.1 {
                    rest.push(SolvedAtom::from_atom(atom));
                }
                SolvedType::Boolean(free, rest)
            }
            Erroneous(problem) => SolvedType::Erroneous(problem),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinAlias {
    pub region: Region,
    pub vars: Vec<Located<Lowercase>>,
    pub typ: SolvedType,
}

#[derive(Clone, Debug)]
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
    env: Env,
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
    env: &Env,
    problems: &mut Vec<Problem>,
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

#[allow(clippy::too_many_arguments)]
fn solve(
    env: &Env,
    state: State,
    rank: Rank,
    pools: &mut Pools,
    problems: &mut Vec<Problem>,
    cached_aliases: &mut MutMap<Symbol, Variable>,
    subs: &mut Subs,
    constraint: &Constraint,
) -> State {
    match constraint {
        True => state,
        SaveTheEnvironment => {
            let mut copy = state;

            copy.env = env.clone();

            copy
        }
        Eq(typ, expected_type, _region) => {
            let actual = type_to_var(subs, rank, pools, cached_aliases, typ);
            let expected = type_to_var(
                subs,
                rank,
                pools,
                cached_aliases,
                expected_type.get_type_ref(),
            );
            let Unified { vars, mismatches } = unify(subs, actual, expected);

            // TODO use region when reporting a problem
            problems.extend(mismatches);

            introduce(subs, rank, pools, &vars);

            state
        }
        Lookup(symbol, expected_type, _region) => {
            let var = *env.vars_by_symbol.get(&symbol).unwrap_or_else(|| {
                // TODO Instead of panicking, solve this as True and record
                // a Problem ("module Foo does not expose `bar`") for later.
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
                expected_type.get_type_ref(),
            );
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
        Pattern(_region, _category, typ, expected) => {
            let actual = type_to_var(subs, rank, pools, cached_aliases, typ);
            let expected = type_to_var(subs, rank, pools, cached_aliases, expected.get_type_ref());
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
                            symbol.clone(),
                            Located {
                                value: var,
                                region: loc_type.region,
                            },
                        );
                    }

                    let mut new_env = env.clone();
                    for (symbol, loc_var) in local_def_vars.iter() {
                        if !new_env.vars_by_symbol.contains_key(&symbol) {
                            new_env.vars_by_symbol.insert(symbol.clone(), loc_var.value);
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

                    let work_in_next_pools = |next_pools: &mut Pools| {
                        let pool: &mut Vec<Variable> = next_pools.get_mut(next_rank);

                        // Replace the contents of this pool with rigid_vars and flex_vars
                        pool.clear();
                        pool.reserve(rigid_vars.len() + flex_vars.len());
                        pool.extend(rigid_vars.iter());
                        pool.extend(flex_vars.iter());

                        let mut new_env = env.clone();

                        // Add a variable for each def to local_def_vars.
                        let mut local_def_vars = ImMap::default();

                        for (symbol, loc_type) in let_con.def_types.iter() {
                            let def_type = loc_type.value.clone();

                            let var =
                                type_to_var(subs, next_rank, next_pools, cached_aliases, &def_type);

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
                            &new_env,
                            state,
                            next_rank,
                            next_pools,
                            problems,
                            cached_aliases,
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

                        for (symbol, loc_var) in local_def_vars.iter() {
                            if !new_env.vars_by_symbol.contains_key(&symbol) {
                                new_env.vars_by_symbol.insert(symbol.clone(), loc_var.value);
                            }
                        }

                        // Note that this vars_by_symbol is the one returned by the
                        // previous call to solve()
                        let temp_state = State {
                            env: new_state.env,
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

fn type_to_var(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    cached: &mut MutMap<Symbol, Variable>,
    typ: &Type,
) -> Variable {
    type_to_variable(subs, rank, pools, cached, typ)
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
                arg_vars.push(type_to_variable(subs, rank, pools, cached, arg))
            }

            let ret_var = type_to_variable(subs, rank, pools, cached, ret_type);
            let content = Content::Structure(FlatType::Func(arg_vars, ret_var));

            register(subs, rank, pools, content)
        }
        Record(fields, ext) => {
            let mut field_vars = MutMap::default();

            for (field, field_type) in fields {
                field_vars.insert(
                    field.clone(),
                    type_to_variable(subs, rank, pools, cached, field_type),
                );
            }

            let ext_var = type_to_variable(subs, rank, pools, cached, ext);
            let content = Content::Structure(FlatType::Record(field_vars, ext_var));

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

            let ext_var = type_to_variable(subs, rank, pools, cached, ext);
            let content = Content::Structure(FlatType::TagUnion(tag_vars, ext_var));

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

            let ext_var = type_to_variable(subs, rank, pools, cached, ext);
            let content =
                Content::Structure(FlatType::RecursiveTagUnion(*rec_var, tag_vars, ext_var));

            register(subs, rank, pools, content)
        }
        Alias(symbol, args, alias_type) => {
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
            let is_recursive = alias_type.is_recursive();
            let no_args = args.is_empty();
            if no_args && !is_recursive {
                if let Some(var) = cached.get(symbol) {
                    return *var;
                }
            }

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
                cached.insert(*symbol, result);
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
    problems: &mut Vec<Problem>,
    symbol: Symbol,
    loc_var: Located<Variable>,
) {
    let var = loc_var.value;

    let is_uniqueness_infer = match subs.get(var).content {
        Content::Alias(Symbol::ATTR_ATTR, _, _) => true,
        _ => false,
    };

    while let Some((recursive, chain)) = subs.occurs(var) {
        let description = subs.get(recursive);
        let content = description.content;

        // try to make a tag union recursive, see if that helps
        match content {
            Content::Structure(FlatType::TagUnion(tags, ext_var)) => {
                if !is_uniqueness_infer {
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
                        Content::Alias(Symbol::ATTR_ATTR, args, _actual) => {
                            debug_assert!(args.len() == 2);
                            debug_assert!(
                                subs.get_root_key_without_compacting(recursive)
                                    == subs.get_root_key_without_compacting(args[1].1)
                            );

                            // NOTE this ensures we use the same uniqueness var for the whole spine
                            // that might add too much uniqueness restriction.
                            // using `subs.fresh_unnamed_flex_var()` loosens it.
                            let uniq_var = args[0].1;
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
            Content::Alias(Symbol::ATTR_ATTR, args, _actual) => {
                debug_assert!(args.len() == 2);
                let uniq_var = args[0].1;
                let tag_union_var = args[1].1;
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
            _ => circular_error(subs, problems, symbol, &loc_var),
        }
    }
}

fn content_attr_alias(subs: &mut Subs, u: Variable, a: Variable) -> Content {
    let actual = subs.fresh_unnamed_flex_var();
    let ext_var = subs.fresh_unnamed_flex_var();

    let mut attr_at_attr = MutMap::default();
    attr_at_attr.insert(TagName::Private(Symbol::ATTR_AT_ATTR), vec![u, a]);
    let attr_tag = FlatType::TagUnion(attr_at_attr, ext_var);

    subs.set_content(actual, Content::Structure(attr_tag));

    Content::Alias(
        Symbol::ATTR_ATTR,
        vec![("u".into(), u), ("a".into(), a)],
        actual,
    )
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

    let content = content_attr_alias(subs, uniq_var, rec_var);
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

    let new_recursive = content_attr_alias(subs, uniq_var, tag_union_var);

    subs.set_content(recursive, new_recursive);
}

fn circular_error(
    subs: &mut Subs,
    problems: &mut Vec<Problem>,
    symbol: Symbol,
    loc_var: &Located<Variable>,
) {
    let var = loc_var.value;
    let error_type = subs.var_to_error_type(var);
    let problem = Problem::CircularType(symbol, error_type, loc_var.region);

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
