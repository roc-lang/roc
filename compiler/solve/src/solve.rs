use roc_can::constraint::Constraint::{self, *};
use roc_can::constraint::PresenceConstraint;
use roc_can::expected::{Expected, PExpected};
use roc_collections::all::MutMap;
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::solved_types::Solved;
use roc_types::subs::{
    AliasVariables, Content, Descriptor, FlatType, Mark, OptVariable, Rank, RecordFields, Subs,
    SubsIndex, SubsSlice, UnionTags, Variable, VariableSubsSlice,
};
use roc_types::types::Type::{self, *};
use roc_types::types::{gather_fields_unsorted_iter, Alias, Category, ErrorType, PatternCategory};
use roc_unify::unify::{unify, Mode, Unified::*};
use std::collections::hash_map::Entry;

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
        match self.0.get_mut(rank.into_usize()) {
            Some(reference) => reference,
            None => panic!("Compiler bug: could not find pool at rank {}", rank),
        }
    }

    pub fn get(&self, rank: Rank) -> &Vec<Variable> {
        match self.0.get(rank.into_usize()) {
            Some(reference) => reference,
            None => panic!("Compiler bug: could not find pool at rank {}", rank),
        }
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

pub fn run(
    env: &Env,
    problems: &mut Vec<TypeError>,
    mut subs: Subs,
    constraint: &Constraint,
) -> (Solved<Subs>, Env) {
    let env = run_in_place(env, problems, &mut subs, constraint);

    (Solved(subs), env)
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

            match unify(subs, actual, expected, Mode::EQ) {
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
        Store(source, target, _filename, _linenr) => {
            // a special version of Eq that is used to store types in the AST.
            // IT DOES NOT REPORT ERRORS!
            let actual = type_to_var(subs, rank, pools, cached_aliases, source);
            let target = *target;

            match unify(subs, actual, target, Mode::EQ) {
                Success(vars) => {
                    introduce(subs, rank, pools, &vars);

                    state
                }
                Failure(vars, _actual_type, _expected_type) => {
                    introduce(subs, rank, pools, &vars);

                    // ERROR NOT REPORTED

                    state
                }
                BadType(vars, _) => {
                    introduce(subs, rank, pools, &vars);

                    // ERROR NOT REPORTED

                    state
                }
            }
        }
        Lookup(symbol, expectation, region) => {
            match env.vars_by_symbol.get(symbol) {
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
                        subs,
                        rank,
                        pools,
                        cached_aliases,
                        expectation.get_type_ref(),
                    );
                    match unify(subs, actual, expected, Mode::EQ) {
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
            let actual = type_to_var(subs, rank, pools, cached_aliases, typ);
            let expected = type_to_var(
                subs,
                rank,
                pools,
                cached_aliases,
                expectation.get_type_ref(),
            );

            let mode = match constraint {
                Present(_, _) => Mode::PRESENT,
                _ => Mode::EQ,
            };

            match unify(subs, actual, expected, mode) {
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
                        env,
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
                    let mut local_def_vars = LocalDefVarsVec::with_length(let_con.def_types.len());

                    for (symbol, loc_type) in let_con.def_types.iter() {
                        let var = type_to_var(subs, rank, pools, cached_aliases, &loc_type.value);

                        local_def_vars.push((
                            *symbol,
                            Loc {
                                value: var,
                                region: loc_type.region,
                            },
                        ));
                    }

                    let mut new_env = env.clone();
                    for (symbol, loc_var) in local_def_vars.iter() {
                        match new_env.vars_by_symbol.entry(*symbol) {
                            Entry::Occupied(_) => {
                                // keep the existing value
                            }
                            Entry::Vacant(vacant) => {
                                vacant.insert(loc_var.value);
                            }
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

                    for (symbol, loc_var) in local_def_vars.iter() {
                        check_for_infinite_type(subs, problems, *symbol, *loc_var);
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
                    let mut local_def_vars = LocalDefVarsVec::with_length(let_con.def_types.len());

                    for (symbol, loc_type) in let_con.def_types.iter() {
                        let def_type = &loc_type.value;

                        let var =
                            type_to_var(subs, next_rank, next_pools, cached_aliases, def_type);

                        local_def_vars.push((
                            *symbol,
                            Loc {
                                value: var,
                                region: loc_type.region,
                            },
                        ));
                    }

                    // Solve the assignments' constraints first.
                    let State {
                        env: saved_env,
                        mark,
                    } = solve(
                        env,
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
                        match new_env.vars_by_symbol.entry(*symbol) {
                            Entry::Occupied(_) => {
                                // keep the existing value
                            }
                            Entry::Vacant(vacant) => {
                                vacant.insert(loc_var.value);
                            }
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
                        ret_con,
                    );

                    for (symbol, loc_var) in local_def_vars.iter() {
                        check_for_infinite_type(subs, problems, *symbol, *loc_var);
                    }

                    new_state
                }
            }
        }
        Present(typ, PresenceConstraint::IsOpen) => {
            let actual = type_to_var(subs, rank, pools, cached_aliases, typ);
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
            let actual = type_to_var(subs, rank, pools, cached_aliases, typ);
            let tag_ty = Type::TagUnion(
                vec![(tag_name.clone(), tys.clone())],
                Box::new(Type::EmptyTagUnion),
            );
            let includes = type_to_var(subs, rank, pools, cached_aliases, &tag_ty);

            match unify(subs, actual, includes, Mode::PRESENT) {
                Success(vars) => {
                    introduce(subs, rank, pools, &vars);

                    state
                }
                Failure(vars, actual_type, expected_type) => {
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
                BadType(vars, problem) => {
                    introduce(subs, rank, pools, &vars);

                    problems.push(TypeError::BadType(problem));

                    state
                }
            }
        }
    }
}

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

use std::cell::RefCell;
std::thread_local! {
    /// Scratchpad arena so we don't need to allocate a new one all the time
    static SCRATCHPAD: RefCell<bumpalo::Bump> = RefCell::new(bumpalo::Bump::with_capacity(4 * 1024));
}

fn take_scratchpad() -> bumpalo::Bump {
    let mut result = bumpalo::Bump::new();
    SCRATCHPAD.with(|f| {
        result = f.replace(bumpalo::Bump::new());
    });

    result
}

fn put_scratchpad(scratchpad: bumpalo::Bump) {
    SCRATCHPAD.with(|f| {
        f.replace(scratchpad);
    });
}

fn type_to_var(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    _: &mut MutMap<Symbol, Variable>,
    typ: &Type,
) -> Variable {
    let mut arena = take_scratchpad();

    let var = type_to_variable(subs, rank, pools, &arena, typ);

    arena.reset();
    put_scratchpad(arena);

    var
}

fn type_to_variable<'a>(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    arena: &'a bumpalo::Bump,
    typ: &Type,
) -> Variable {
    use bumpalo::collections::Vec;

    match typ {
        Variable(var) => *var,
        RangedNumber(typ, vars) => {
            let ty_var = type_to_variable(subs, rank, pools, arena, typ);
            let vars = VariableSubsSlice::insert_into_subs(subs, vars.iter().copied());
            let content = Content::RangedNumber(ty_var, vars);

            register(subs, rank, pools, content)
        }
        Apply(symbol, arguments, _) => {
            let new_arguments = VariableSubsSlice::reserve_into_subs(subs, arguments.len());
            for (target_index, var_index) in (new_arguments.indices()).zip(arguments) {
                let var = type_to_variable(subs, rank, pools, arena, var_index);
                subs.variables[target_index] = var;
            }

            let flat_type = FlatType::Apply(*symbol, new_arguments);
            let content = Content::Structure(flat_type);

            register(subs, rank, pools, content)
        }
        EmptyRec => Variable::EMPTY_RECORD,
        EmptyTagUnion => Variable::EMPTY_TAG_UNION,

        ClosureTag { name, ext } => {
            let tag_name = TagName::Closure(*name);
            let tag_names = SubsSlice::new(subs.tag_names.len() as u32, 1);

            subs.tag_names.push(tag_name);

            // the first VariableSubsSlice in the array is a zero-length slice
            let union_tags = UnionTags::from_slices(tag_names, SubsSlice::new(0, 1));

            let content = Content::Structure(FlatType::TagUnion(union_tags, *ext));

            register(subs, rank, pools, content)
        }

        // This case is important for the rank of boolean variables
        Function(arguments, closure_type, ret_type) => {
            let new_arguments = VariableSubsSlice::reserve_into_subs(subs, arguments.len());
            for (target_index, var_index) in (new_arguments.indices()).zip(arguments) {
                let var = type_to_variable(subs, rank, pools, arena, var_index);
                subs.variables[target_index] = var;
            }

            let ret_var = type_to_variable(subs, rank, pools, arena, ret_type);
            let closure_var = type_to_variable(subs, rank, pools, arena, closure_type);
            let content = Content::Structure(FlatType::Func(new_arguments, closure_var, ret_var));

            register(subs, rank, pools, content)
        }
        Record(fields, ext) => {
            // An empty fields is inefficient (but would be correct)
            // If hit, try to turn the value into an EmptyRecord in canonicalization
            debug_assert!(!fields.is_empty() || !ext.is_empty_record());

            let mut field_vars = Vec::with_capacity_in(fields.len(), arena);

            for (field, field_type) in fields {
                let field_var =
                    field_type.map(|typ| type_to_variable(subs, rank, pools, arena, typ));

                field_vars.push((field.clone(), field_var));
            }

            let temp_ext_var = type_to_variable(subs, rank, pools, arena, ext);

            let (it, new_ext_var) =
                gather_fields_unsorted_iter(subs, RecordFields::empty(), temp_ext_var)
                    .expect("Something ended up weird in this record type");

            let it = it
                .into_iter()
                .map(|(field, field_type)| (field.clone(), field_type));

            field_vars.extend(it);
            insertion_sort_by(&mut field_vars, RecordFields::compare);

            let record_fields = RecordFields::insert_into_subs(subs, field_vars);

            let content = Content::Structure(FlatType::Record(record_fields, new_ext_var));

            register(subs, rank, pools, content)
        }
        TagUnion(tags, ext) => {
            // An empty tags is inefficient (but would be correct)
            // If hit, try to turn the value into an EmptyTagUnion in canonicalization
            debug_assert!(!tags.is_empty() || !ext.is_empty_tag_union());

            let (union_tags, ext) = type_to_union_tags(subs, rank, pools, arena, tags, ext);
            let content = Content::Structure(FlatType::TagUnion(union_tags, ext));

            register(subs, rank, pools, content)
        }
        FunctionOrTagUnion(tag_name, symbol, ext) => {
            let temp_ext_var = type_to_variable(subs, rank, pools, arena, ext);

            let (it, ext) = roc_types::types::gather_tags_unsorted_iter(
                subs,
                UnionTags::default(),
                temp_ext_var,
            );

            for _ in it {
                unreachable!("we assert that the ext var is empty; otherwise we'd already know it was a tag union!");
            }

            let slice = SubsIndex::new(subs.tag_names.len() as u32);
            subs.tag_names.push(tag_name.clone());

            let content = Content::Structure(FlatType::FunctionOrTagUnion(slice, *symbol, ext));

            register(subs, rank, pools, content)
        }
        RecursiveTagUnion(rec_var, tags, ext) => {
            // An empty tags is inefficient (but would be correct)
            // If hit, try to turn the value into an EmptyTagUnion in canonicalization
            debug_assert!(!tags.is_empty() || !ext.is_empty_tag_union());

            let (union_tags, ext) = type_to_union_tags(subs, rank, pools, arena, tags, ext);
            let content =
                Content::Structure(FlatType::RecursiveTagUnion(*rec_var, union_tags, ext));

            let tag_union_var = register(subs, rank, pools, content);

            register_with_known_var(
                subs,
                *rec_var,
                rank,
                pools,
                Content::RecursionVar {
                    opt_name: None,
                    structure: tag_union_var,
                },
            );

            tag_union_var
        }

        Type::Alias {
            symbol,
            type_arguments,
            actual,
            lambda_set_variables,
            // TODO(opaques): revisit kind
            kind: _,
        } => {
            if let Some(reserved) = Variable::get_reserved(*symbol) {
                if rank.is_none() {
                    // reserved variables are stored with rank NONE
                    return reserved;
                } else {
                    // for any other rank, we need to copy; it takes care of adjusting the rank
                    return deep_copy_var(subs, rank, pools, reserved);
                }
            }

            let alias_variables = alias_to_var(
                subs,
                rank,
                pools,
                arena,
                type_arguments,
                lambda_set_variables,
            );

            let alias_variable = if let Symbol::RESULT_RESULT = *symbol {
                roc_result_to_var(subs, rank, pools, arena, actual)
            } else {
                type_to_variable(subs, rank, pools, arena, actual)
            };
            let content = Content::Alias(*symbol, alias_variables, alias_variable);

            register(subs, rank, pools, content)
        }
        HostExposedAlias {
            name: symbol,
            type_arguments,
            actual: alias_type,
            actual_var,
            lambda_set_variables,
            ..
        } => {
            let alias_variables = alias_to_var(
                subs,
                rank,
                pools,
                arena,
                type_arguments,
                lambda_set_variables,
            );

            let alias_variable = type_to_variable(subs, rank, pools, arena, alias_type);
            let content = Content::Alias(*symbol, alias_variables, alias_variable);
            let result = register(subs, rank, pools, content);

            // We only want to unify the actual_var with the alias once
            // if it's already redirected (and therefore, redundant)
            // don't do it again
            if !subs.redundant(*actual_var) {
                let descriptor = subs.get(result);
                subs.union(result, *actual_var, descriptor);
            }

            result
        }
        Erroneous(problem) => {
            let content = Content::Structure(FlatType::Erroneous(Box::new(problem.clone())));

            register(subs, rank, pools, content)
        }
    }
}

fn alias_to_var<'a>(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    arena: &'a bumpalo::Bump,
    type_arguments: &[(roc_module::ident::Lowercase, Type)],
    lambda_set_variables: &[roc_types::types::LambdaSet],
) -> AliasVariables {
    let length = type_arguments.len() + lambda_set_variables.len();
    let new_variables = VariableSubsSlice::reserve_into_subs(subs, length);

    for (target_index, (_, arg_type)) in (new_variables.indices()).zip(type_arguments) {
        let copy_var = type_to_variable(subs, rank, pools, arena, arg_type);
        subs.variables[target_index] = copy_var;
    }

    let it = (new_variables.indices().skip(type_arguments.len())).zip(lambda_set_variables);
    for (target_index, ls) in it {
        let copy_var = type_to_variable(subs, rank, pools, arena, &ls.0);
        subs.variables[target_index] = copy_var;
    }

    AliasVariables {
        variables_start: new_variables.start,
        type_variables_len: type_arguments.len() as _,
        all_variables_len: length as _,
    }
}

fn roc_result_to_var<'a>(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    arena: &'a bumpalo::Bump,
    result_type: &Type,
) -> Variable {
    match result_type {
        Type::TagUnion(tags, ext) => {
            debug_assert!(ext.is_empty_tag_union());
            debug_assert!(tags.len() == 2);

            if let [(err, err_args), (ok, ok_args)] = &tags[..] {
                debug_assert_eq!(err, &subs.tag_names[0]);
                debug_assert_eq!(ok, &subs.tag_names[1]);

                if let ([err_type], [ok_type]) = (err_args.as_slice(), ok_args.as_slice()) {
                    let err_var = type_to_variable(subs, rank, pools, arena, err_type);
                    let ok_var = type_to_variable(subs, rank, pools, arena, ok_type);

                    let start = subs.variables.len() as u32;
                    let err_slice = SubsSlice::new(start, 1);
                    let ok_slice = SubsSlice::new(start + 1, 1);

                    subs.variables.push(err_var);
                    subs.variables.push(ok_var);

                    let variables = SubsSlice::new(subs.variable_slices.len() as _, 2);
                    subs.variable_slices.push(err_slice);
                    subs.variable_slices.push(ok_slice);

                    let union_tags = UnionTags::from_slices(Subs::RESULT_TAG_NAMES, variables);
                    let ext = Variable::EMPTY_TAG_UNION;

                    let content = Content::Structure(FlatType::TagUnion(union_tags, ext));

                    return register(subs, rank, pools, content);
                }
            }

            unreachable!("invalid arguments to Result.Result; canonicalization should catch this!")
        }
        _ => unreachable!("not a valid type inside a Result.Result alias"),
    }
}

fn insertion_sort_by<T, F>(arr: &mut [T], mut compare: F)
where
    F: FnMut(&T, &T) -> std::cmp::Ordering,
{
    for i in 1..arr.len() {
        let val = &arr[i];
        let mut j = i;
        let pos = arr[..i]
            .binary_search_by(|x| compare(x, val))
            .unwrap_or_else(|pos| pos);
        // Swap all elements until specific position.
        while j > pos {
            arr.swap(j - 1, j);
            j -= 1;
        }
    }
}

fn sorted_no_duplicates<T>(slice: &[(TagName, T)]) -> bool {
    match slice.split_first() {
        None => true,
        Some(((first, _), rest)) => {
            let mut current = first;

            for (next, _) in rest {
                if current >= next {
                    return false;
                } else {
                    current = next;
                }
            }

            true
        }
    }
}

fn sort_and_deduplicate<T>(tag_vars: &mut bumpalo::collections::Vec<(TagName, T)>) {
    insertion_sort_by(tag_vars, |(a, _), (b, _)| a.cmp(b));

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
}

/// Find whether the current run of tag names is in the subs.tag_names array already. If so,
/// we take a SubsSlice to the existing tag names, so we don't have to add/clone those tag names
/// and keep subs memory consumption low
fn find_tag_name_run<T>(slice: &[(TagName, T)], subs: &mut Subs) -> Option<SubsSlice<TagName>> {
    use std::cmp::Ordering;

    let tag_name = slice.get(0)?.0.clone();

    let mut result = None;

    // the `SubsSlice<TagName>` that inserting `slice` into subs would give
    let bigger_slice = SubsSlice::new(subs.tag_names.len() as _, slice.len() as _);

    match subs.tag_name_cache.entry(tag_name) {
        Entry::Occupied(mut occupied) => {
            let subs_slice = *occupied.get();

            let prefix_slice = SubsSlice::new(subs_slice.start, slice.len() as _);

            if slice.len() == 1 {
                return Some(prefix_slice);
            }

            match slice.len().cmp(&subs_slice.len()) {
                Ordering::Less => {
                    // we might have a prefix
                    let tag_names = &subs.tag_names[subs_slice.start as usize..];

                    for (from_subs, (from_slice, _)) in tag_names.iter().zip(slice.iter()) {
                        if from_subs != from_slice {
                            return None;
                        }
                    }

                    result = Some(prefix_slice);
                }
                Ordering::Equal => {
                    let tag_names = &subs.tag_names[subs_slice.indices()];

                    for (from_subs, (from_slice, _)) in tag_names.iter().zip(slice.iter()) {
                        if from_subs != from_slice {
                            return None;
                        }
                    }

                    result = Some(subs_slice);
                }
                Ordering::Greater => {
                    // switch to the bigger slice that is not inserted yet, but will be soon
                    occupied.insert(bigger_slice);
                }
            }
        }
        Entry::Vacant(vacant) => {
            vacant.insert(bigger_slice);
        }
    }

    result
}

/// Assumes that the tags are sorted and there are no duplicates!
fn insert_tags_fast_path<'a>(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    arena: &'a bumpalo::Bump,
    tags: &[(TagName, Vec<Type>)],
) -> UnionTags {
    let new_variable_slices = SubsSlice::reserve_variable_slices(subs, tags.len());

    match find_tag_name_run(tags, subs) {
        Some(new_tag_names) => {
            let it = (new_variable_slices.indices()).zip(tags);

            for (variable_slice_index, (_, arguments)) in it {
                // turn the arguments into variables
                let new_variables = VariableSubsSlice::reserve_into_subs(subs, arguments.len());
                let it = (new_variables.indices()).zip(arguments);
                for (target_index, argument) in it {
                    let var = type_to_variable(subs, rank, pools, arena, argument);
                    subs.variables[target_index] = var;
                }

                subs.variable_slices[variable_slice_index] = new_variables;
            }

            UnionTags::from_slices(new_tag_names, new_variable_slices)
        }
        None => {
            let new_tag_names = SubsSlice::reserve_tag_names(subs, tags.len());

            let it = (new_variable_slices.indices())
                .zip(new_tag_names.indices())
                .zip(tags);

            for ((variable_slice_index, tag_name_index), (tag_name, arguments)) in it {
                // turn the arguments into variables
                let new_variables = VariableSubsSlice::reserve_into_subs(subs, arguments.len());
                let it = (new_variables.indices()).zip(arguments);
                for (target_index, argument) in it {
                    let var = type_to_variable(subs, rank, pools, arena, argument);
                    subs.variables[target_index] = var;
                }

                subs.variable_slices[variable_slice_index] = new_variables;
                subs.tag_names[tag_name_index] = tag_name.clone();
            }

            UnionTags::from_slices(new_tag_names, new_variable_slices)
        }
    }
}

fn insert_tags_slow_path<'a>(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    arena: &'a bumpalo::Bump,
    tags: &[(TagName, Vec<Type>)],
    mut tag_vars: bumpalo::collections::Vec<(TagName, VariableSubsSlice)>,
) -> UnionTags {
    for (tag, tag_argument_types) in tags {
        let new_slice = VariableSubsSlice::reserve_into_subs(subs, tag_argument_types.len());

        for (i, arg) in (new_slice.indices()).zip(tag_argument_types) {
            let var = type_to_variable(subs, rank, pools, arena, arg);
            subs.variables[i] = var;
        }

        tag_vars.push((tag.clone(), new_slice));
    }

    sort_and_deduplicate(&mut tag_vars);

    UnionTags::insert_slices_into_subs(subs, tag_vars)
}

fn type_to_union_tags<'a>(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    arena: &'a bumpalo::Bump,
    tags: &[(TagName, Vec<Type>)],
    ext: &Type,
) -> (UnionTags, Variable) {
    use bumpalo::collections::Vec;

    let sorted = tags.len() == 1 || sorted_no_duplicates(tags);

    if ext.is_empty_tag_union() {
        let ext = type_to_variable(subs, rank, pools, arena, &Type::EmptyTagUnion);
        // let ext = Variable::EMPTY_TAG_UNION;

        let union_tags = if sorted {
            insert_tags_fast_path(subs, rank, pools, arena, tags)
        } else {
            let tag_vars = Vec::with_capacity_in(tags.len(), arena);
            insert_tags_slow_path(subs, rank, pools, arena, tags, tag_vars)
        };

        (union_tags, ext)
    } else {
        let mut tag_vars = Vec::with_capacity_in(tags.len(), arena);

        let temp_ext_var = type_to_variable(subs, rank, pools, arena, ext);
        let (it, ext) =
            roc_types::types::gather_tags_unsorted_iter(subs, UnionTags::default(), temp_ext_var);

        tag_vars.extend(it.map(|(n, v)| (n.clone(), v)));

        let union_tags = if tag_vars.is_empty() && sorted {
            insert_tags_fast_path(subs, rank, pools, arena, tags)
        } else {
            insert_tags_slow_path(subs, rank, pools, arena, tags, tag_vars)
        };

        (union_tags, ext)
    }
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

        // try to make a tag union recursive, see if that helps
        match description.content {
            Content::Structure(FlatType::TagUnion(tags, ext_var)) => {
                subs.mark_tag_union_recursive(recursive, tags, ext_var);
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
        let rank = subs.get_rank_set_mark(var, young_mark);

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
    let (desc_rank, desc_mark) = subs.get_rank_mark(var);

    if desc_mark == young_mark {
        // Mark the variable as visited before adjusting content, as it may be cyclic.
        subs.set_mark(var, visit_mark);

        // SAFETY: in this function (and functions it calls, we ONLY modify rank and mark, never content!
        // hence, we can have an immutable reference to it even though we also have a mutable
        // reference to the Subs as a whole. This prevents a clone of the content, which turns out
        // to be quite expensive.
        let content = {
            let ptr = &subs.get_ref(var).content as *const _;
            unsafe { &*ptr }
        };

        let max_rank = adjust_rank_content(subs, young_mark, visit_mark, group_rank, content);

        subs.set_rank_mark(var, max_rank, visit_mark);

        max_rank
    } else if desc_mark == visit_mark {
        // nothing changes
        desc_rank
    } else {
        let min_rank = group_rank.min(desc_rank);

        // TODO from elm-compiler: how can min_rank ever be group_rank?
        subs.set_rank_mark(var, min_rank, visit_mark);

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
        FlexVar(_) | RigidVar(_) | Error => group_rank,

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
                    // For performance reasons, we only keep one representation of empty tag unions
                    // in subs. That representation exists at rank 0, which we don't always want to
                    // reflect the whole tag union as, because doing so may over-generalize free
                    // type variables.
                    // Normally this is not a problem because of the loop below that maximizes the
                    // rank from nested types in the union. But suppose we have the simple tag
                    // union
                    //   [ Z ]{}
                    // there are no nested types in the tags, and the empty tag union is at rank 0,
                    // so we promote the tag union to rank 0. Now if we introduce the presence
                    // constraint
                    //   [ Z ]{} += [ S a ]
                    // we'll wind up with [ Z, S a ]{}, but it will be at rank 0, and "a" will get
                    // over-generalized. Really, the empty tag union should be introduced at
                    // whatever current group rank we're at, and so that's how we encode it here.
                    if *ext_var == Variable::EMPTY_TAG_UNION && rank.is_none() {
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

                    if cfg!(debug_assertions) {
                        let rec_var_rank =
                            adjust_rank(subs, young_mark, visit_mark, group_rank, *rec_var);

                        debug_assert!(
                            rank >= rec_var_rank,
                            "rank was {:?} but recursion var {:?} has higher rank {:?}",
                            rank,
                            rec_var,
                            rec_var_rank
                        );
                    }

                    rank
                }

                Erroneous(_) => group_rank,
            }
        }

        Alias(_, args, real_var) => {
            let mut rank = Rank::toplevel();

            for var_index in args.variables() {
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

        RangedNumber(typ, _) => adjust_rank(subs, young_mark, visit_mark, group_rank, *typ),
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

    instantiate_rigids_help(subs, rank, var);

    // NOTE subs.restore(var) is done at the end of instantiate_rigids_help
}

fn instantiate_rigids_help(subs: &mut Subs, max_rank: Rank, initial: Variable) {
    let mut visited = vec![];
    let mut stack = vec![initial];

    macro_rules! var_slice {
        ($variable_subs_slice:expr) => {{
            let slice = $variable_subs_slice;
            &subs.variables[slice.indices()]
        }};
    }

    while let Some(var) = stack.pop() {
        visited.push(var);

        let desc = subs.get_ref_mut(var);
        if desc.copy.is_some() {
            continue;
        }

        desc.rank = Rank::NONE;
        desc.mark = Mark::NONE;
        desc.copy = OptVariable::from(var);

        use Content::*;
        use FlatType::*;

        match &desc.content {
            RigidVar(name) => {
                // what it's all about: convert the rigid var into a flex var
                let name = name.clone();

                // NOTE: we must write to the mutually borrowed `desc` value here
                // using `subs.set` does not work (unclear why, really)
                // but get_ref_mut approach saves a lookup, so the weirdness is worth it
                desc.content = FlexVar(Some(name));
                desc.rank = max_rank;
                desc.mark = Mark::NONE;
                desc.copy = OptVariable::NONE;
            }
            FlexVar(_) | Error => (),

            RecursionVar { structure, .. } => {
                stack.push(*structure);
            }

            Structure(flat_type) => match flat_type {
                Apply(_, args) => {
                    stack.extend(var_slice!(*args));
                }

                Func(arg_vars, closure_var, ret_var) => {
                    let arg_vars = *arg_vars;
                    let ret_var = *ret_var;
                    let closure_var = *closure_var;

                    stack.extend(var_slice!(arg_vars));

                    stack.push(ret_var);
                    stack.push(closure_var);
                }

                EmptyRecord => (),
                EmptyTagUnion => (),

                Record(fields, ext_var) => {
                    let fields = *fields;
                    let ext_var = *ext_var;
                    stack.extend(var_slice!(fields.variables()));

                    stack.push(ext_var);
                }
                TagUnion(tags, ext_var) => {
                    let tags = *tags;
                    let ext_var = *ext_var;

                    for slice_index in tags.variables() {
                        let slice = subs.variable_slices[slice_index.index as usize];
                        stack.extend(var_slice!(slice));
                    }

                    stack.push(ext_var);
                }
                FunctionOrTagUnion(_, _, ext_var) => {
                    stack.push(*ext_var);
                }

                RecursiveTagUnion(rec_var, tags, ext_var) => {
                    let tags = *tags;
                    let ext_var = *ext_var;
                    let rec_var = *rec_var;

                    for slice_index in tags.variables() {
                        let slice = subs.variable_slices[slice_index.index as usize];
                        stack.extend(var_slice!(slice));
                    }

                    stack.push(ext_var);
                    stack.push(rec_var);
                }

                Erroneous(_) => (),
            },
            Alias(_, args, var) => {
                let var = *var;
                let args = *args;

                stack.extend(var_slice!(args.variables()));

                stack.push(var);
            }
            &RangedNumber(typ, vars) => {
                stack.push(typ);

                stack.extend(var_slice!(vars));
            }
        }
    }

    // we have tracked all visited variables, and can now traverse them
    // in one go (without looking at the UnificationTable) and clear the copy field
    for var in visited {
        let descriptor = subs.get_ref_mut(var);

        if descriptor.copy.is_some() {
            descriptor.rank = Rank::NONE;
            descriptor.mark = Mark::NONE;
            descriptor.copy = OptVariable::NONE;
        }
    }
}

fn deep_copy_var(subs: &mut Subs, rank: Rank, pools: &mut Pools, var: Variable) -> Variable {
    let mut arena = take_scratchpad();

    let mut visited = bumpalo::collections::Vec::with_capacity_in(4 * 1024, &arena);

    let copy = deep_copy_var_help(subs, rank, pools, &mut visited, var);

    // we have tracked all visited variables, and can now traverse them
    // in one go (without looking at the UnificationTable) and clear the copy field
    for var in visited {
        let descriptor = subs.get_ref_mut(var);

        if descriptor.copy.is_some() {
            descriptor.rank = Rank::NONE;
            descriptor.mark = Mark::NONE;
            descriptor.copy = OptVariable::NONE;
        }
    }

    arena.reset();
    put_scratchpad(arena);

    copy
}

fn deep_copy_var_help(
    subs: &mut Subs,
    max_rank: Rank,
    pools: &mut Pools,
    visited: &mut bumpalo::collections::Vec<'_, Variable>,
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

    visited.push(var);

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
                Apply(symbol, arguments) => {
                    let new_arguments = VariableSubsSlice::reserve_into_subs(subs, arguments.len());
                    for (target_index, var_index) in (new_arguments.indices()).zip(arguments) {
                        let var = subs[var_index];
                        let copy_var = deep_copy_var_help(subs, max_rank, pools, visited, var);
                        subs.variables[target_index] = copy_var;
                    }

                    Apply(symbol, new_arguments)
                }

                Func(arguments, closure_var, ret_var) => {
                    let new_ret_var = deep_copy_var_help(subs, max_rank, pools, visited, ret_var);
                    let new_closure_var =
                        deep_copy_var_help(subs, max_rank, pools, visited, closure_var);

                    let new_arguments = VariableSubsSlice::reserve_into_subs(subs, arguments.len());
                    for (target_index, var_index) in (new_arguments.indices()).zip(arguments) {
                        let var = subs[var_index];
                        let copy_var = deep_copy_var_help(subs, max_rank, pools, visited, var);
                        subs.variables[target_index] = copy_var;
                    }

                    Func(new_arguments, new_closure_var, new_ret_var)
                }

                same @ EmptyRecord | same @ EmptyTagUnion | same @ Erroneous(_) => same,

                Record(fields, ext_var) => {
                    let record_fields = {
                        let new_variables =
                            VariableSubsSlice::reserve_into_subs(subs, fields.len());

                        let it = (new_variables.indices()).zip(fields.iter_variables());
                        for (target_index, var_index) in it {
                            let var = subs[var_index];
                            let copy_var = deep_copy_var_help(subs, max_rank, pools, visited, var);
                            subs.variables[target_index] = copy_var;
                        }

                        RecordFields {
                            length: fields.length,
                            field_names_start: fields.field_names_start,
                            variables_start: new_variables.start,
                            field_types_start: fields.field_types_start,
                        }
                    };

                    Record(
                        record_fields,
                        deep_copy_var_help(subs, max_rank, pools, visited, ext_var),
                    )
                }

                TagUnion(tags, ext_var) => {
                    let new_variable_slices = SubsSlice::reserve_variable_slices(subs, tags.len());

                    let it = (new_variable_slices.indices()).zip(tags.variables());
                    for (target_index, index) in it {
                        let slice = subs[index];

                        let new_variables = VariableSubsSlice::reserve_into_subs(subs, slice.len());
                        let it = (new_variables.indices()).zip(slice);
                        for (target_index, var_index) in it {
                            let var = subs[var_index];
                            let copy_var = deep_copy_var_help(subs, max_rank, pools, visited, var);
                            subs.variables[target_index] = copy_var;
                        }

                        subs.variable_slices[target_index] = new_variables;
                    }

                    let union_tags = UnionTags::from_slices(tags.tag_names(), new_variable_slices);

                    let new_ext = deep_copy_var_help(subs, max_rank, pools, visited, ext_var);
                    TagUnion(union_tags, new_ext)
                }

                FunctionOrTagUnion(tag_name, symbol, ext_var) => FunctionOrTagUnion(
                    tag_name,
                    symbol,
                    deep_copy_var_help(subs, max_rank, pools, visited, ext_var),
                ),

                RecursiveTagUnion(rec_var, tags, ext_var) => {
                    let new_variable_slices = SubsSlice::reserve_variable_slices(subs, tags.len());

                    let it = (new_variable_slices.indices()).zip(tags.variables());
                    for (target_index, index) in it {
                        let slice = subs[index];

                        let new_variables = VariableSubsSlice::reserve_into_subs(subs, slice.len());
                        let it = (new_variables.indices()).zip(slice);
                        for (target_index, var_index) in it {
                            let var = subs[var_index];
                            let copy_var = deep_copy_var_help(subs, max_rank, pools, visited, var);
                            subs.variables[target_index] = copy_var;
                        }

                        subs.variable_slices[target_index] = new_variables;
                    }

                    let union_tags = UnionTags::from_slices(tags.tag_names(), new_variable_slices);

                    let new_ext = deep_copy_var_help(subs, max_rank, pools, visited, ext_var);
                    let new_rec_var = deep_copy_var_help(subs, max_rank, pools, visited, rec_var);

                    RecursiveTagUnion(new_rec_var, union_tags, new_ext)
                }
            };

            subs.set(copy, make_descriptor(Structure(new_flat_type)));

            copy
        }

        FlexVar(_) | Error => copy,

        RecursionVar {
            opt_name,
            structure,
        } => {
            let new_structure = deep_copy_var_help(subs, max_rank, pools, visited, structure);

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

        Alias(symbol, arguments, real_type_var) => {
            let new_variables =
                SubsSlice::reserve_into_subs(subs, arguments.all_variables_len as _);
            for (target_index, var_index) in (new_variables.indices()).zip(arguments.variables()) {
                let var = subs[var_index];
                let copy_var = deep_copy_var_help(subs, max_rank, pools, visited, var);
                subs.variables[target_index] = copy_var;
            }

            let new_arguments = AliasVariables {
                variables_start: new_variables.start,
                ..arguments
            };

            let new_real_type_var =
                deep_copy_var_help(subs, max_rank, pools, visited, real_type_var);
            let new_content = Alias(symbol, new_arguments, new_real_type_var);

            subs.set(copy, make_descriptor(new_content));

            copy
        }

        RangedNumber(typ, range_vars) => {
            let new_type_var = deep_copy_var_help(subs, max_rank, pools, visited, typ);

            let new_vars = SubsSlice::reserve_into_subs(subs, range_vars.len());
            for (target_index, var_index) in (new_vars.indices()).zip(range_vars) {
                let var = subs[var_index];
                let copy_var = deep_copy_var_help(subs, max_rank, pools, visited, var);
                subs.variables[target_index] = copy_var;
            }

            let new_content = RangedNumber(new_type_var, new_vars);

            subs.set(copy, make_descriptor(new_content));

            copy
        }
    }
}

fn register(subs: &mut Subs, rank: Rank, pools: &mut Pools, content: Content) -> Variable {
    let descriptor = Descriptor {
        content,
        rank,
        mark: Mark::NONE,
        copy: OptVariable::NONE,
    };

    let var = subs.fresh(descriptor);

    pools.get_mut(rank).push(var);

    var
}

fn register_with_known_var(
    subs: &mut Subs,
    var: Variable,
    rank: Rank,
    pools: &mut Pools,
    content: Content,
) {
    let descriptor = Descriptor {
        content,
        rank,
        mark: Mark::NONE,
        copy: OptVariable::NONE,
    };

    subs.set(var, descriptor);

    pools.get_mut(rank).push(var);
}
