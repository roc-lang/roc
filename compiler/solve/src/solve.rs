use crate::ability::{type_implementing_member, AbilityImplError, DeferredMustImplementAbility};
use bumpalo::Bump;
use roc_can::abilities::{AbilitiesStore, MemberSpecialization};
use roc_can::constraint::Constraint::{self, *};
use roc_can::constraint::{Constraints, LetConstraint};
use roc_can::expected::{Expected, PExpected};
use roc_collections::all::MutMap;
use roc_debug_flags::{dbg_do, ROC_VERIFY_RIGID_LET_GENERALIZED};
use roc_error_macros::internal_error;
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::solved_types::Solved;
use roc_types::subs::{
    AliasVariables, Content, Descriptor, FlatType, Mark, OptVariable, Rank, RecordFields, Subs,
    SubsIndex, SubsSlice, UnionTags, Variable, VariableSubsSlice,
};
use roc_types::types::Type::{self, *};
use roc_types::types::{
    gather_fields_unsorted_iter, AliasCommon, AliasKind, Category, ErrorType, LambdaSet,
    OptAbleType, OptAbleVar, PatternCategory, Reason, TypeExtension,
};
use roc_unify::unify::{unify, Mode, Unified::*};

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
pub struct IncompleteAbilityImplementation {
    // TODO(abilities): have general types here, not just opaques
    pub typ: Symbol,
    pub ability: Symbol,
    pub specialized_members: Vec<Loc<Symbol>>,
    pub missing_members: Vec<Loc<Symbol>>,
}

#[derive(Debug, Clone)]
pub enum TypeError {
    BadExpr(Region, Category, ErrorType, Expected<ErrorType>),
    BadPattern(Region, PatternCategory, ErrorType, PExpected<ErrorType>),
    CircularType(Region, Symbol, ErrorType),
    BadType(roc_types::types::Problem),
    UnexposedLookup(Symbol),
    IncompleteAbilityImplementation(IncompleteAbilityImplementation),
    BadExprMissingAbility(
        Region,
        Category,
        ErrorType,
        Vec<IncompleteAbilityImplementation>,
    ),
    BadPatternMissingAbility(
        Region,
        PatternCategory,
        ErrorType,
        Vec<IncompleteAbilityImplementation>,
    ),
    Exhaustive(roc_exhaustive::Error),
}

use roc_types::types::Alias;

#[derive(Debug, Clone, Copy)]
struct DelayedAliasVariables {
    start: u32,
    type_variables_len: u8,
    lambda_set_variables_len: u8,
    recursion_variables_len: u8,
}

impl DelayedAliasVariables {
    fn recursion_variables(self, variables: &mut [OptAbleVar]) -> &mut [OptAbleVar] {
        let start = self.start as usize
            + (self.type_variables_len + self.lambda_set_variables_len) as usize;
        let length = self.recursion_variables_len as usize;

        &mut variables[start..][..length]
    }

    fn lambda_set_variables(self, variables: &mut [OptAbleVar]) -> &mut [OptAbleVar] {
        let start = self.start as usize + self.type_variables_len as usize;
        let length = self.lambda_set_variables_len as usize;

        &mut variables[start..][..length]
    }

    fn type_variables(self, variables: &mut [OptAbleVar]) -> &mut [OptAbleVar] {
        let start = self.start as usize;
        let length = self.type_variables_len as usize;

        &mut variables[start..][..length]
    }
}

#[derive(Debug, Default)]
pub struct Aliases {
    aliases: Vec<(Symbol, Type, DelayedAliasVariables, AliasKind)>,
    variables: Vec<OptAbleVar>,
}

impl Aliases {
    pub fn insert(&mut self, symbol: Symbol, alias: Alias) {
        let alias_variables =
            {
                let start = self.variables.len() as _;

                self.variables.extend(
                    alias
                        .type_variables
                        .iter()
                        .map(|x| OptAbleVar::from(&x.value)),
                );

                self.variables.extend(alias.lambda_set_variables.iter().map(
                    |x| match x.as_inner() {
                        Type::Variable(v) => OptAbleVar::unbound(*v),
                        _ => unreachable!("lambda set type is not a variable"),
                    },
                ));

                let recursion_variables_len = alias.recursion_variables.len() as _;
                self.variables.extend(
                    alias
                        .recursion_variables
                        .iter()
                        .copied()
                        .map(OptAbleVar::unbound),
                );

                DelayedAliasVariables {
                    start,
                    type_variables_len: alias.type_variables.len() as _,
                    lambda_set_variables_len: alias.lambda_set_variables.len() as _,
                    recursion_variables_len,
                }
            };

        self.aliases
            .push((symbol, alias.typ, alias_variables, alias.kind));
    }

    fn instantiate_result_result(
        subs: &mut Subs,
        rank: Rank,
        pools: &mut Pools,
        alias_variables: AliasVariables,
    ) -> Variable {
        let tag_names_slice = Subs::RESULT_TAG_NAMES;

        let err_slice = SubsSlice::new(alias_variables.variables_start + 1, 1);
        let ok_slice = SubsSlice::new(alias_variables.variables_start, 1);

        let variable_slices =
            SubsSlice::extend_new(&mut subs.variable_slices, [err_slice, ok_slice]);

        let union_tags = UnionTags::from_slices(tag_names_slice, variable_slices);
        let ext_var = Variable::EMPTY_TAG_UNION;
        let flat_type = FlatType::TagUnion(union_tags, ext_var);
        let content = Content::Structure(flat_type);

        register(subs, rank, pools, content)
    }

    /// Build an alias of the form `Num range := range`
    fn build_num_opaque(
        subs: &mut Subs,
        rank: Rank,
        pools: &mut Pools,
        symbol: Symbol,
        range_var: Variable,
    ) -> Variable {
        let content = Content::Alias(
            symbol,
            AliasVariables::insert_into_subs(subs, [range_var], []),
            range_var,
            AliasKind::Opaque,
        );

        register(subs, rank, pools, content)
    }

    fn instantiate_builtin_aliases_real_var(
        &mut self,
        subs: &mut Subs,
        rank: Rank,
        pools: &mut Pools,
        symbol: Symbol,
        alias_variables: AliasVariables,
    ) -> Option<(Variable, AliasKind)> {
        match symbol {
            Symbol::RESULT_RESULT => {
                let var = Self::instantiate_result_result(subs, rank, pools, alias_variables);

                Some((var, AliasKind::Structural))
            }
            Symbol::NUM_NUM | Symbol::NUM_INTEGER | Symbol::NUM_FLOATINGPOINT => {
                // Num range := range | Integer range := range | FloatingPoint range := range
                let range_var = subs.variables[alias_variables.variables_start as usize];
                Some((range_var, AliasKind::Opaque))
            }
            Symbol::NUM_INT => {
                // Int range : Num (Integer range)
                //
                // build `Integer range := range`
                let integer_content_var = Self::build_num_opaque(
                    subs,
                    rank,
                    pools,
                    Symbol::NUM_INTEGER,
                    subs.variables[alias_variables.variables_start as usize],
                );

                // build `Num (Integer range) := Integer range`
                let num_content_var =
                    Self::build_num_opaque(subs, rank, pools, Symbol::NUM_NUM, integer_content_var);

                Some((num_content_var, AliasKind::Structural))
            }
            Symbol::NUM_FLOAT => {
                // Float range : Num (FloatingPoint range)
                //
                // build `FloatingPoint range := range`
                let fpoint_content_var = Self::build_num_opaque(
                    subs,
                    rank,
                    pools,
                    Symbol::NUM_FLOATINGPOINT,
                    subs.variables[alias_variables.variables_start as usize],
                );

                // build `Num (FloatingPoint range) := FloatingPoint range`
                let num_content_var =
                    Self::build_num_opaque(subs, rank, pools, Symbol::NUM_NUM, fpoint_content_var);

                Some((num_content_var, AliasKind::Structural))
            }
            _ => None,
        }
    }

    fn instantiate_real_var(
        &mut self,
        subs: &mut Subs,
        rank: Rank,
        pools: &mut Pools,
        arena: &bumpalo::Bump,
        symbol: Symbol,
        alias_variables: AliasVariables,
    ) -> Result<(Variable, AliasKind), ()> {
        // hardcoded instantiations for builtin aliases
        if let Some((var, kind)) = Self::instantiate_builtin_aliases_real_var(
            self,
            subs,
            rank,
            pools,
            symbol,
            alias_variables,
        ) {
            return Ok((var, kind));
        }

        let (typ, delayed_variables, &mut kind) =
            match self.aliases.iter_mut().find(|(s, _, _, _)| *s == symbol) {
                None => return Err(()),
                Some((_, typ, delayed_variables, kind)) => (typ, delayed_variables, kind),
            };

        let mut substitutions: MutMap<_, _> = Default::default();

        for OptAbleVar {
            var: rec_var,
            opt_ability,
        } in delayed_variables
            .recursion_variables(&mut self.variables)
            .iter_mut()
        {
            debug_assert!(opt_ability.is_none());
            let new_var = subs.fresh_unnamed_flex_var();
            substitutions.insert(*rec_var, new_var);
            *rec_var = new_var;
        }

        let old_type_variables = delayed_variables.type_variables(&mut self.variables);
        #[allow(clippy::needless_collect)] // otherwise we borrow self.variables immutably twice
        let variable_opt_abilities: Vec<_> =
            old_type_variables.iter().map(|ov| ov.opt_ability).collect();
        let new_type_variables = &subs.variables[alias_variables.type_variables().indices()];

        for (old, new) in old_type_variables.iter_mut().zip(new_type_variables) {
            // if constraint gen duplicated a type these variables could be the same
            // (happens very often in practice)
            if old.var != *new {
                substitutions.insert(old.var, *new);

                old.var = *new;
            }
        }

        let old_lambda_set_variables = delayed_variables.lambda_set_variables(&mut self.variables);
        let new_lambda_set_variables =
            &subs.variables[alias_variables.lambda_set_variables().indices()];

        for (old, new) in old_lambda_set_variables
            .iter_mut()
            .zip(new_lambda_set_variables)
        {
            debug_assert!(old.opt_ability.is_none());
            if old.var != *new {
                substitutions.insert(old.var, *new);
                old.var = *new;
            }
        }

        if !substitutions.is_empty() {
            typ.substitute_variables(&substitutions);
        }

        let mut t = Type::EmptyRec;

        std::mem::swap(typ, &mut t);

        // assumption: an alias does not (transitively) syntactically contain itself
        // (if it did it would have to be a recursive tag union, which we should have fixed up
        // during canonicalization)
        let alias_variable = type_to_variable(subs, rank, pools, arena, self, &t);

        {
            match self.aliases.iter_mut().find(|(s, _, _, _)| *s == symbol) {
                None => unreachable!(),
                Some((_, typ, _, _)) => {
                    // swap typ back
                    std::mem::swap(typ, &mut t);
                }
            }
        }

        Ok((alias_variable, kind))
    }
}

#[derive(Clone, Debug, Default)]
pub struct Env {
    symbols: Vec<Symbol>,
    variables: Vec<Variable>,
}

impl Env {
    pub fn vars_by_symbol(&self) -> impl Iterator<Item = (Symbol, Variable)> + '_ {
        let it1 = self.symbols.iter().copied();
        let it2 = self.variables.iter().copied();

        it1.zip(it2)
    }

    #[inline(always)]
    fn get_var_by_symbol(&self, symbol: &Symbol) -> Option<Variable> {
        self.symbols
            .iter()
            .position(|s| s == symbol)
            .map(|index| self.variables[index])
    }

    #[inline(always)]
    fn insert_symbol_var_if_vacant(&mut self, symbol: Symbol, var: Variable) {
        match self.symbols.iter().position(|s| *s == symbol) {
            None => {
                // symbol is not in vars_by_symbol yet; insert it
                self.symbols.push(symbol);
                self.variables.push(var);
            }
            Some(_) => {
                // do nothing
            }
        }
    }
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

    pub fn split_last(mut self) -> (Vec<Variable>, Vec<Vec<Variable>>) {
        let last = self
            .0
            .pop()
            .unwrap_or_else(|| panic!("Attempted to split_last() on non-empty Pools"));

        (last, self.0)
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
    constraints: &Constraints,
    problems: &mut Vec<TypeError>,
    mut subs: Subs,
    aliases: &mut Aliases,
    constraint: &Constraint,
    abilities_store: &mut AbilitiesStore,
) -> (Solved<Subs>, Env) {
    let env = run_in_place(
        constraints,
        problems,
        &mut subs,
        aliases,
        constraint,
        abilities_store,
    );

    (Solved(subs), env)
}

/// Modify an existing subs in-place instead
fn run_in_place(
    constraints: &Constraints,
    problems: &mut Vec<TypeError>,
    subs: &mut Subs,
    aliases: &mut Aliases,
    constraint: &Constraint,
    abilities_store: &mut AbilitiesStore,
) -> Env {
    let mut pools = Pools::default();

    let state = State {
        env: Env::default(),
        mark: Mark::NONE.next(),
    };
    let rank = Rank::toplevel();
    let arena = Bump::new();

    let mut deferred_must_implement_abilities = DeferredMustImplementAbility::default();

    let state = solve(
        &arena,
        constraints,
        state,
        rank,
        &mut pools,
        problems,
        aliases,
        subs,
        constraint,
        abilities_store,
        &mut deferred_must_implement_abilities,
    );

    // Now that the module has been solved, we can run through and check all
    // types claimed to implement abilities.
    problems.extend(deferred_must_implement_abilities.check(subs, abilities_store));

    state.env
}

enum Work<'a> {
    Constraint {
        env: &'a Env,
        rank: Rank,
        constraint: &'a Constraint,
    },
    CheckForInfiniteTypes(LocalDefVarsVec<(Symbol, Loc<Variable>)>),
    /// The ret_con part of a let constraint that does NOT introduces rigid and/or flex variables
    LetConNoVariables {
        env: &'a Env,
        rank: Rank,
        let_con: &'a LetConstraint,

        /// The variables used to store imported types in the Subs.
        /// The `Contents` are copied from the source module, but to
        /// mimic `type_to_var`, we must add these variables to `Pools`
        /// at the correct rank
        pool_variables: &'a [Variable],
    },
    /// The ret_con part of a let constraint that introduces rigid and/or flex variables
    ///
    /// These introduced variables must be generalized, hence this variant
    /// is more complex than `LetConNoVariables`.
    LetConIntroducesVariables {
        env: &'a Env,
        rank: Rank,
        let_con: &'a LetConstraint,

        /// The variables used to store imported types in the Subs.
        /// The `Contents` are copied from the source module, but to
        /// mimic `type_to_var`, we must add these variables to `Pools`
        /// at the correct rank
        pool_variables: &'a [Variable],
    },
}

#[allow(clippy::too_many_arguments)]
fn solve(
    arena: &Bump,
    constraints: &Constraints,
    mut state: State,
    rank: Rank,
    pools: &mut Pools,
    problems: &mut Vec<TypeError>,
    aliases: &mut Aliases,
    subs: &mut Subs,
    constraint: &Constraint,
    abilities_store: &mut AbilitiesStore,
    deferred_must_implement_abilities: &mut DeferredMustImplementAbility,
) -> State {
    let initial = Work::Constraint {
        env: &Env::default(),
        rank,
        constraint,
    };

    let mut stack = vec![initial];

    while let Some(work_item) = stack.pop() {
        let (env, rank, constraint) = match work_item {
            Work::Constraint {
                env,
                rank,
                constraint,
            } => {
                // the default case; actually solve this constraint
                (env, rank, constraint)
            }
            Work::CheckForInfiniteTypes(def_vars) => {
                // after a LetCon, we must check if any of the variables that we introduced
                // loop back to themselves after solving the ret_constraint
                for (symbol, loc_var) in def_vars.iter() {
                    check_for_infinite_type(subs, problems, *symbol, *loc_var);
                }

                continue;
            }
            Work::LetConNoVariables {
                env,
                rank,
                let_con,
                pool_variables,
            } => {
                // NOTE be extremely careful with shadowing here
                let offset = let_con.defs_and_ret_constraint.index();
                let ret_constraint = &constraints.constraints[offset + 1];

                // Add a variable for each def to new_vars_by_env.
                let local_def_vars = LocalDefVarsVec::from_def_types(
                    constraints,
                    rank,
                    pools,
                    aliases,
                    subs,
                    let_con.def_types,
                );

                pools.get_mut(rank).extend(pool_variables);

                let mut new_env = env.clone();
                for (symbol, loc_var) in local_def_vars.iter() {
                    check_ability_specialization(
                        arena,
                        subs,
                        pools,
                        rank,
                        abilities_store,
                        problems,
                        deferred_must_implement_abilities,
                        *symbol,
                        *loc_var,
                    );

                    new_env.insert_symbol_var_if_vacant(*symbol, loc_var.value);
                }

                stack.push(Work::CheckForInfiniteTypes(local_def_vars));
                stack.push(Work::Constraint {
                    env: arena.alloc(new_env),
                    rank,
                    constraint: ret_constraint,
                });

                continue;
            }
            Work::LetConIntroducesVariables {
                env,
                rank,
                let_con,
                pool_variables,
            } => {
                // NOTE be extremely careful with shadowing here
                let offset = let_con.defs_and_ret_constraint.index();
                let ret_constraint = &constraints.constraints[offset + 1];

                let next_rank = rank.next();

                let mark = state.mark;
                let saved_env = state.env;

                let young_mark = mark;
                let visit_mark = young_mark.next();
                let final_mark = visit_mark.next();

                // Add a variable for each def to local_def_vars.
                let local_def_vars = LocalDefVarsVec::from_def_types(
                    constraints,
                    next_rank,
                    pools,
                    aliases,
                    subs,
                    let_con.def_types,
                );

                pools.get_mut(next_rank).extend(pool_variables);

                debug_assert_eq!(
                    // Check that no variable ended up in a higher rank than the next rank.. that
                    // would mean we generalized one level more than we need to!
                    {
                        let offenders = pools
                            .get(next_rank)
                            .iter()
                            .filter(|var| {
                                subs.get_rank(**var).into_usize() > next_rank.into_usize()
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
                generalize(subs, young_mark, visit_mark, next_rank, pools);
                debug_assert!(pools.get(next_rank).is_empty());

                // check that things went well
                dbg_do!(ROC_VERIFY_RIGID_LET_GENERALIZED, {
                    let rigid_vars = &constraints.variables[let_con.rigid_vars.indices()];

                    // NOTE the `subs.redundant` check does not come from elm.
                    // It's unclear whether this is a bug with our implementation
                    // (something is redundant that shouldn't be)
                    // or that it just never came up in elm.
                    let mut it = rigid_vars
                        .iter()
                        .filter(|&var| !subs.redundant(*var) && subs.get_rank(*var) != Rank::NONE)
                        .peekable();

                    if it.peek().is_some() {
                        let failing: Vec<_> = it.collect();
                        println!("Rigids {:?}", &rigid_vars);
                        println!("Failing {:?}", failing);
                        debug_assert!(false);
                    }
                });

                let mut new_env = env.clone();
                for (symbol, loc_var) in local_def_vars.iter() {
                    check_ability_specialization(
                        arena,
                        subs,
                        pools,
                        rank,
                        abilities_store,
                        problems,
                        deferred_must_implement_abilities,
                        *symbol,
                        *loc_var,
                    );

                    new_env.insert_symbol_var_if_vacant(*symbol, loc_var.value);
                }

                // Note that this vars_by_symbol is the one returned by the
                // previous call to solve()
                let state_for_ret_con = State {
                    env: saved_env,
                    mark: final_mark,
                };

                // Now solve the body, using the new vars_by_symbol which includes
                // the assignments' name-to-variable mappings.
                stack.push(Work::CheckForInfiniteTypes(local_def_vars));
                stack.push(Work::Constraint {
                    env: arena.alloc(new_env),
                    rank,
                    constraint: ret_constraint,
                });

                state = state_for_ret_con;

                continue;
            }
        };

        state = match constraint {
            True => state,
            SaveTheEnvironment => {
                let mut copy = state;

                copy.env = env.clone();

                copy
            }
            Eq(roc_can::constraint::Eq(type_index, expectation_index, category_index, region)) => {
                let category = &constraints.categories[category_index.index()];

                let actual =
                    either_type_index_to_var(constraints, subs, rank, pools, aliases, *type_index);

                let expectation = &constraints.expectations[expectation_index.index()];
                let expected = type_to_var(subs, rank, pools, aliases, expectation.get_type_ref());

                match unify(subs, actual, expected, Mode::EQ) {
                    Success {
                        vars,
                        must_implement_ability,
                    } => {
                        introduce(subs, rank, pools, &vars);
                        if !must_implement_ability.is_empty() {
                            deferred_must_implement_abilities.add(
                                must_implement_ability,
                                AbilityImplError::BadExpr(*region, category.clone(), actual),
                            );
                        }

                        state
                    }
                    Failure(vars, actual_type, expected_type, _bad_impls) => {
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
            Store(source_index, target, _filename, _linenr) => {
                // a special version of Eq that is used to store types in the AST.
                // IT DOES NOT REPORT ERRORS!
                let actual = either_type_index_to_var(
                    constraints,
                    subs,
                    rank,
                    pools,
                    aliases,
                    *source_index,
                );
                let target = *target;

                match unify(subs, actual, target, Mode::EQ) {
                    Success {
                        vars,
                        // ERROR NOT REPORTED
                        must_implement_ability: _,
                    } => {
                        introduce(subs, rank, pools, &vars);

                        state
                    }
                    Failure(vars, _actual_type, _expected_type, _bad_impls) => {
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
            Lookup(symbol, expectation_index, region) => {
                match env.get_var_by_symbol(symbol) {
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
                        let actual = deep_copy_var_in(subs, rank, pools, var, arena);
                        let expectation = &constraints.expectations[expectation_index.index()];

                        let expected =
                            type_to_var(subs, rank, pools, aliases, expectation.get_type_ref());

                        match unify(subs, actual, expected, Mode::EQ) {
                            Success {
                                vars,
                                must_implement_ability,
                            } => {
                                introduce(subs, rank, pools, &vars);
                                if !must_implement_ability.is_empty() {
                                    deferred_must_implement_abilities.add(
                                        must_implement_ability,
                                        AbilityImplError::BadExpr(
                                            *region,
                                            Category::Lookup(*symbol),
                                            actual,
                                        ),
                                    );
                                }

                                state
                            }

                            Failure(vars, actual_type, expected_type, _bad_impls) => {
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
            And(slice) => {
                let it = constraints.constraints[slice.indices()].iter().rev();
                for sub_constraint in it {
                    stack.push(Work::Constraint {
                        env,
                        rank,
                        constraint: sub_constraint,
                    })
                }

                state
            }
            Pattern(type_index, expectation_index, category_index, region)
            | PatternPresence(type_index, expectation_index, category_index, region) => {
                let category = &constraints.pattern_categories[category_index.index()];

                let actual =
                    either_type_index_to_var(constraints, subs, rank, pools, aliases, *type_index);

                let expectation = &constraints.pattern_expectations[expectation_index.index()];
                let expected = type_to_var(subs, rank, pools, aliases, expectation.get_type_ref());

                let mode = match constraint {
                    PatternPresence(..) => Mode::PRESENT,
                    _ => Mode::EQ,
                };

                match unify(subs, actual, expected, mode) {
                    Success {
                        vars,
                        must_implement_ability,
                    } => {
                        introduce(subs, rank, pools, &vars);
                        if !must_implement_ability.is_empty() {
                            deferred_must_implement_abilities.add(
                                must_implement_ability,
                                AbilityImplError::BadPattern(*region, category.clone(), actual),
                            );
                        }

                        state
                    }
                    Failure(vars, actual_type, expected_type, _bad_impls) => {
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
            Let(index, pool_slice) => {
                let let_con = &constraints.let_constraints[index.index()];

                let offset = let_con.defs_and_ret_constraint.index();
                let defs_constraint = &constraints.constraints[offset];
                let ret_constraint = &constraints.constraints[offset + 1];

                let flex_vars = &constraints.variables[let_con.flex_vars.indices()];
                let rigid_vars = &constraints.variables[let_con.rigid_vars.indices()];

                let pool_variables = &constraints.variables[pool_slice.indices()];

                if matches!(&ret_constraint, True) && let_con.rigid_vars.is_empty() {
                    debug_assert!(pool_variables.is_empty());

                    introduce(subs, rank, pools, flex_vars);

                    // If the return expression is guaranteed to solve,
                    // solve the assignments themselves and move on.
                    stack.push(Work::Constraint {
                        env,
                        rank,
                        constraint: defs_constraint,
                    });

                    state
                } else if let_con.rigid_vars.is_empty() && let_con.flex_vars.is_empty() {
                    // items are popped from the stack in reverse order. That means that we'll
                    // first solve then defs_constraint, and then (eventually) the ret_constraint.
                    //
                    // Note that the LetConSimple gets the current env and rank,
                    // and not the env/rank from after solving the defs_constraint
                    stack.push(Work::LetConNoVariables {
                        env,
                        rank,
                        let_con,
                        pool_variables,
                    });
                    stack.push(Work::Constraint {
                        env,
                        rank,
                        constraint: defs_constraint,
                    });

                    state
                } else {
                    // work in the next pool to localize header
                    let next_rank = rank.next();

                    // introduce variables
                    for &var in rigid_vars.iter().chain(flex_vars.iter()) {
                        subs.set_rank(var, next_rank);
                    }

                    // determine the next pool
                    if next_rank.into_usize() < pools.len() {
                        // Nothing to do, we already accounted for the next rank, no need to
                        // adjust the pools
                    } else {
                        // we should be off by one at this point
                        debug_assert_eq!(next_rank.into_usize(), 1 + pools.len());
                        pools.extend_to(next_rank.into_usize());
                    }

                    let pool: &mut Vec<Variable> = pools.get_mut(next_rank);

                    // Replace the contents of this pool with rigid_vars and flex_vars
                    pool.clear();
                    pool.reserve(rigid_vars.len() + flex_vars.len());
                    pool.extend(rigid_vars.iter());
                    pool.extend(flex_vars.iter());

                    // run solver in next pool

                    // items are popped from the stack in reverse order. That means that we'll
                    // first solve then defs_constraint, and then (eventually) the ret_constraint.
                    //
                    // Note that the LetConSimple gets the current env and rank,
                    // and not the env/rank from after solving the defs_constraint
                    stack.push(Work::LetConIntroducesVariables {
                        env,
                        rank,
                        let_con,
                        pool_variables,
                    });
                    stack.push(Work::Constraint {
                        env,
                        rank: next_rank,
                        constraint: defs_constraint,
                    });

                    state
                }
            }
            IsOpenType(type_index) => {
                let actual =
                    either_type_index_to_var(constraints, subs, rank, pools, aliases, *type_index);

                open_tag_union(subs, actual);

                state
            }
            IncludesTag(index) => {
                let includes_tag = &constraints.includes_tags[index.index()];

                let roc_can::constraint::IncludesTag {
                    type_index,
                    tag_name,
                    types,
                    pattern_category,
                    region,
                } = includes_tag;

                let typ = &constraints.types[type_index.index()];
                let tys = &constraints.types[types.indices()];
                let pattern_category = &constraints.pattern_categories[pattern_category.index()];

                let actual = type_to_var(subs, rank, pools, aliases, typ);
                let tag_ty = Type::TagUnion(
                    vec![(tag_name.clone(), tys.to_vec())],
                    TypeExtension::Closed,
                );
                let includes = type_to_var(subs, rank, pools, aliases, &tag_ty);

                match unify(subs, actual, includes, Mode::PRESENT) {
                    Success {
                        vars,
                        must_implement_ability,
                    } => {
                        introduce(subs, rank, pools, &vars);
                        if !must_implement_ability.is_empty() {
                            deferred_must_implement_abilities.add(
                                must_implement_ability,
                                AbilityImplError::BadPattern(
                                    *region,
                                    pattern_category.clone(),
                                    actual,
                                ),
                            );
                        }

                        state
                    }
                    Failure(vars, actual_type, expected_to_include_type, _bad_impls) => {
                        introduce(subs, rank, pools, &vars);

                        let problem = TypeError::BadPattern(
                            *region,
                            pattern_category.clone(),
                            expected_to_include_type,
                            PExpected::NoExpectation(actual_type),
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

                let (real_var, real_region, expected_type, category_and_expected) = match eq {
                    Ok(eq) => {
                        let roc_can::constraint::Eq(real_var, expected, category, real_region) =
                            constraints.eq[eq.index()];
                        let expected = &constraints.expectations[expected.index()];
                        (
                            real_var,
                            real_region,
                            expected.get_type_ref(),
                            Ok((category, expected)),
                        )
                    }
                    Err(peq) => {
                        let roc_can::constraint::PatternEq(
                            real_var,
                            expected,
                            category,
                            real_region,
                        ) = constraints.pattern_eq[peq.index()];
                        let expected = &constraints.pattern_expectations[expected.index()];
                        (
                            real_var,
                            real_region,
                            expected.get_type_ref(),
                            Err((category, expected)),
                        )
                    }
                };

                let real_var =
                    either_type_index_to_var(constraints, subs, rank, pools, aliases, real_var);

                let branches_var = type_to_var(subs, rank, pools, aliases, expected_type);

                let real_content = subs.get_content_without_compacting(real_var);
                let branches_content = subs.get_content_without_compacting(branches_var);
                let already_have_error = matches!(
                    (real_content, branches_content),
                    (
                        Content::Error | Content::Structure(FlatType::Erroneous(_)),
                        _
                    ) | (
                        _,
                        Content::Error | Content::Structure(FlatType::Erroneous(_))
                    )
                );

                let snapshot = subs.snapshot();
                let outcome = unify(subs, real_var, branches_var, Mode::EQ);

                let should_check_exhaustiveness;
                match outcome {
                    Success {
                        vars,
                        must_implement_ability,
                    } => {
                        subs.commit_snapshot(snapshot);

                        introduce(subs, rank, pools, &vars);
                        if !must_implement_ability.is_empty() {
                            internal_error!("Didn't expect ability vars to land here");
                        }

                        // Case 1: unify error types, but don't check exhaustiveness.
                        // Case 2: run exhaustiveness to check for redundant branches.
                        should_check_exhaustiveness = !already_have_error;
                    }
                    Failure(..) => {
                        // Rollback and check for almost-equality.
                        subs.rollback_to(snapshot);

                        let almost_eq_snapshot = subs.snapshot();
                        // TODO: turn this on for bidirectional exhaustiveness checking
                        // open_tag_union(subs, real_var);
                        open_tag_union(subs, branches_var);
                        let almost_eq = matches!(
                            unify(subs, real_var, branches_var, Mode::EQ),
                            Success { .. }
                        );

                        subs.rollback_to(almost_eq_snapshot);

                        if almost_eq {
                            // Case 3: almost equal, check exhaustiveness.
                            should_check_exhaustiveness = true;
                        } else {
                            // Case 4: incompatible types, report type error.
                            // Re-run first failed unification to get the type diff.
                            match unify(subs, real_var, branches_var, Mode::EQ) {
                                Failure(vars, actual_type, expected_type, _bad_impls) => {
                                    introduce(subs, rank, pools, &vars);

                                    // Figure out the problem - it might be pattern or value
                                    // related.
                                    let problem = match category_and_expected {
                                        Ok((category, expected)) => {
                                            let real_category =
                                                constraints.categories[category.index()].clone();
                                            TypeError::BadExpr(
                                                real_region,
                                                real_category,
                                                actual_type,
                                                expected.replace_ref(expected_type),
                                            )
                                        }

                                        Err((category, expected)) => {
                                            let real_category = constraints.pattern_categories
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
                    BadType(vars, problem) => {
                        subs.commit_snapshot(snapshot);

                        introduce(subs, rank, pools, &vars);

                        problems.push(TypeError::BadType(problem));

                        should_check_exhaustiveness = false;
                    }
                }

                let sketched_rows = constraints.sketched_rows[sketched_rows.index()].clone();

                if should_check_exhaustiveness {
                    use roc_can::exhaustive::{check, ExhaustiveSummary};

                    let ExhaustiveSummary {
                        errors,
                        exhaustive,
                        redundancies,
                    } = check(subs, sketched_rows, context);

                    // Store information about whether the "when" is exhaustive, and
                    // which (if any) of its branches are redundant. Codegen may use
                    // this for branch-fixing and redundant elimination.
                    if !exhaustive {
                        exhaustive_mark.set_non_exhaustive(subs);
                    }
                    for redundant_mark in redundancies {
                        redundant_mark.set_redundant(subs);
                    }

                    // Store the errors.
                    problems.extend(errors.into_iter().map(TypeError::Exhaustive));
                }

                state
            }
        };
    }

    state
}

fn open_tag_union(subs: &mut Subs, var: Variable) {
    let mut stack = vec![var];
    while let Some(var) = stack.pop() {
        use {Content::*, FlatType::*};

        let mut desc = subs.get(var);
        if let Structure(TagUnion(tags, ext)) = desc.content {
            if let Structure(EmptyTagUnion) = subs.get_content_without_compacting(ext) {
                let new_ext = subs.fresh_unnamed_flex_var();
                subs.set_rank(new_ext, desc.rank);
                let new_union = Structure(TagUnion(tags, new_ext));
                desc.content = new_union;
                subs.set(var, desc);
            }

            // Also open up all nested tag unions.
            let all_vars = tags.variables().into_iter();
            stack.extend(all_vars.flat_map(|slice| subs[slice]).map(|var| subs[var]));
        }

        // Today, an "open" constraint doesn't affect any types
        // other than tag unions. Recursive tag unions are constructed
        // at a later time (during occurs checks after tag unions are
        // resolved), so that's not handled here either.
        // NB: Handle record types here if we add presence constraints
        // to their type inference as well.
    }
}

/// If a symbol claims to specialize an ability member, check that its solved type in fact
/// does specialize the ability, and record the specialization.
#[allow(clippy::too_many_arguments)]
// Aggressive but necessary - there aren't many usages.
#[inline(always)]
fn check_ability_specialization(
    arena: &Bump,
    subs: &mut Subs,
    pools: &mut Pools,
    rank: Rank,
    abilities_store: &mut AbilitiesStore,
    problems: &mut Vec<TypeError>,
    deferred_must_implement_abilities: &mut DeferredMustImplementAbility,
    symbol: Symbol,
    symbol_loc_var: Loc<Variable>,
) {
    // If the symbol specializes an ability member, we need to make sure that the
    // inferred type for the specialization actually aligns with the expected
    // implementation.
    if let Some((root_symbol, root_data)) = abilities_store.root_name_and_def(symbol) {
        let root_signature_var = root_data.signature_var;

        // Check if they unify - if they don't, then the claimed specialization isn't really one,
        // and that's a type error!
        // This also fixes any latent type variables that need to be specialized to exactly what
        // the ability signature expects.

        // We need to freshly instantiate the root signature so that all unifications are reflected
        // in the specialization type, but not the original signature type.
        let root_signature_var =
            deep_copy_var_in(subs, Rank::toplevel(), pools, root_signature_var, arena);
        let snapshot = subs.snapshot();
        let unified = unify(subs, symbol_loc_var.value, root_signature_var, Mode::EQ);

        match unified {
            Success {
                vars: _,
                must_implement_ability,
            } if must_implement_ability.is_empty() => {
                // This can happen when every ability constriant on a type variable went
                // through only another type variable. That means this def is not specialized
                // for one type - for now, we won't admit this.

                // Rollback the snapshot so we unlink the root signature with the specialization,
                // so we can have two separate error types.
                subs.rollback_to(snapshot);

                let (expected_type, _problems) = subs.var_to_error_type(root_signature_var);
                let (actual_type, _problems) = subs.var_to_error_type(symbol_loc_var.value);

                let reason = Reason::GeneralizedAbilityMemberSpecialization {
                    member_name: root_symbol,
                    def_region: root_data.region,
                };

                let problem = TypeError::BadExpr(
                    symbol_loc_var.region,
                    Category::AbilityMemberSpecialization(root_symbol),
                    actual_type,
                    Expected::ForReason(reason, expected_type, symbol_loc_var.region),
                );

                problems.push(problem);
            }

            Success {
                vars,
                must_implement_ability,
            } => {
                subs.commit_snapshot(snapshot);
                introduce(subs, rank, pools, &vars);

                // First, figure out and register for what type does this symbol specialize
                // the ability member.
                let specialization_type =
                    type_implementing_member(&must_implement_ability, root_data.parent_ability);
                let specialization = MemberSpecialization {
                    symbol,
                    region: symbol_loc_var.region,
                };
                abilities_store.register_specialization_for_type(
                    root_symbol,
                    specialization_type,
                    specialization,
                );

                // Store the checks for what abilities must be implemented to be checked after the
                // whole module is complete.
                deferred_must_implement_abilities
                    .add(must_implement_ability, AbilityImplError::IncompleteAbility);
            }
            Failure(vars, actual_type, expected_type, unimplemented_abilities) => {
                subs.commit_snapshot(snapshot);
                introduce(subs, rank, pools, &vars);

                let reason = Reason::InvalidAbilityMemberSpecialization {
                    member_name: root_symbol,
                    def_region: root_data.region,
                    unimplemented_abilities,
                };

                let problem = TypeError::BadExpr(
                    symbol_loc_var.region,
                    Category::AbilityMemberSpecialization(root_symbol),
                    actual_type,
                    Expected::ForReason(reason, expected_type, symbol_loc_var.region),
                );

                problems.push(problem);
            }
            BadType(vars, problem) => {
                subs.commit_snapshot(snapshot);
                introduce(subs, rank, pools, &vars);

                problems.push(TypeError::BadType(problem));
            }
        }
    }
}

#[derive(Debug)]
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
        constraints: &Constraints,
        rank: Rank,
        pools: &mut Pools,
        aliases: &mut Aliases,
        subs: &mut Subs,
        def_types_slice: roc_can::constraint::DefTypes,
    ) -> Self {
        let types_slice = &constraints.types[def_types_slice.types.indices()];
        let loc_symbols_slice = &constraints.loc_symbols[def_types_slice.loc_symbols.indices()];

        let mut local_def_vars = Self::with_length(types_slice.len());

        for ((symbol, region), typ) in loc_symbols_slice.iter().copied().zip(types_slice) {
            let var = type_to_var(subs, rank, pools, aliases, typ);

            local_def_vars.push((symbol, Loc { value: var, region }));
        }

        local_def_vars
    }
}

use std::cell::RefCell;
std::thread_local! {
    /// Scratchpad arena so we don't need to allocate a new one all the time
    static SCRATCHPAD: RefCell<Option<bumpalo::Bump>> = RefCell::new(Some(bumpalo::Bump::with_capacity(4 * 1024)));
}

fn take_scratchpad() -> bumpalo::Bump {
    SCRATCHPAD.with(|f| f.take().unwrap())
}

fn put_scratchpad(scratchpad: bumpalo::Bump) {
    SCRATCHPAD.with(|f| {
        f.replace(Some(scratchpad));
    });
}

fn either_type_index_to_var(
    constraints: &Constraints,
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    aliases: &mut Aliases,
    either_type_index: roc_collections::soa::EitherIndex<Type, Variable>,
) -> Variable {
    match either_type_index.split() {
        Ok(type_index) => {
            let typ = &constraints.types[type_index.index()];

            type_to_var(subs, rank, pools, aliases, typ)
        }
        Err(var_index) => {
            // we cheat, and  store the variable directly in the index
            unsafe { Variable::from_index(var_index.index() as _) }
        }
    }
}

fn type_to_var(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    aliases: &mut Aliases,
    typ: &Type,
) -> Variable {
    if let Type::Variable(var) = typ {
        *var
    } else {
        let mut arena = take_scratchpad();

        let var = type_to_variable(subs, rank, pools, &arena, aliases, typ);

        arena.reset();
        put_scratchpad(arena);

        var
    }
}

enum RegisterVariable {
    /// Based on the Type, we already know what variable this will be
    Direct(Variable),
    /// This Type needs more complicated Content. We reserve a Variable
    /// for it, but put a placeholder Content in subs
    Deferred,
}

impl RegisterVariable {
    fn from_type(
        subs: &mut Subs,
        rank: Rank,
        pools: &mut Pools,
        arena: &'_ bumpalo::Bump,
        typ: &Type,
    ) -> Self {
        use RegisterVariable::*;

        match typ {
            Type::Variable(var) => Direct(*var),
            EmptyRec => Direct(Variable::EMPTY_RECORD),
            EmptyTagUnion => Direct(Variable::EMPTY_TAG_UNION),
            Type::DelayedAlias(AliasCommon { symbol, .. }) => {
                if let Some(reserved) = Variable::get_reserved(*symbol) {
                    if rank.is_none() {
                        // reserved variables are stored with rank NONE
                        return Direct(reserved);
                    } else {
                        // for any other rank, we need to copy; it takes care of adjusting the rank
                        let copied = deep_copy_var_in(subs, rank, pools, reserved, arena);
                        return Direct(copied);
                    }
                }

                Deferred
            }
            Type::Alias { symbol, .. } => {
                if let Some(reserved) = Variable::get_reserved(*symbol) {
                    if rank.is_none() {
                        // reserved variables are stored with rank NONE
                        return Direct(reserved);
                    } else {
                        // for any other rank, we need to copy; it takes care of adjusting the rank
                        let copied = deep_copy_var_in(subs, rank, pools, reserved, arena);
                        return Direct(copied);
                    }
                }

                Deferred
            }
            _ => Deferred,
        }
    }

    #[inline(always)]
    fn with_stack<'a>(
        subs: &mut Subs,
        rank: Rank,
        pools: &mut Pools,
        arena: &'_ bumpalo::Bump,
        typ: &'a Type,
        stack: &mut bumpalo::collections::Vec<'_, TypeToVar<'a>>,
    ) -> Variable {
        match Self::from_type(subs, rank, pools, arena, typ) {
            Self::Direct(var) => var,
            Self::Deferred => {
                let var = subs.fresh_unnamed_flex_var();
                stack.push(TypeToVar::Defer(typ, var));
                var
            }
        }
    }
}

#[derive(Debug)]
enum TypeToVar<'a> {
    Defer(&'a Type, Variable),
}

fn type_to_variable<'a>(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    arena: &'a bumpalo::Bump,
    aliases: &mut Aliases,
    typ: &Type,
) -> Variable {
    use bumpalo::collections::Vec;

    let mut stack = Vec::with_capacity_in(8, arena);

    macro_rules! helper {
        ($typ:expr) => {{
            match RegisterVariable::from_type(subs, rank, pools, arena, $typ) {
                RegisterVariable::Direct(var) => var,
                RegisterVariable::Deferred => {
                    let var = subs.fresh_unnamed_flex_var();
                    stack.push(TypeToVar::Defer($typ, var));
                    var
                }
            }
        }};
    }

    let result = helper!(typ);

    while let Some(TypeToVar::Defer(typ, destination)) = stack.pop() {
        match typ {
            Variable(_) | EmptyRec | EmptyTagUnion => {
                unreachable!("This variant should never be deferred!")
            }
            RangedNumber(typ, vars) => {
                let ty_var = helper!(typ);
                let vars = VariableSubsSlice::insert_into_subs(subs, vars.iter().copied());
                let content = Content::RangedNumber(ty_var, vars);

                register_with_known_var(subs, destination, rank, pools, content)
            }
            Apply(symbol, arguments, _) => {
                let new_arguments = VariableSubsSlice::reserve_into_subs(subs, arguments.len());
                for (target_index, var_index) in (new_arguments.indices()).zip(arguments) {
                    let var = helper!(var_index);
                    subs.variables[target_index] = var;
                }

                let flat_type = FlatType::Apply(*symbol, new_arguments);
                let content = Content::Structure(flat_type);

                register_with_known_var(subs, destination, rank, pools, content)
            }

            ClosureTag { name, ext } => {
                let tag_name = TagName::Closure(*name);
                let tag_names = SubsSlice::new(subs.tag_names.len() as u32, 1);

                subs.tag_names.push(tag_name);

                // the first VariableSubsSlice in the array is a zero-length slice
                let union_tags = UnionTags::from_slices(tag_names, SubsSlice::new(0, 1));

                let content = Content::Structure(FlatType::TagUnion(union_tags, *ext));

                register_with_known_var(subs, destination, rank, pools, content)
            }
            // This case is important for the rank of boolean variables
            Function(arguments, closure_type, ret_type) => {
                let new_arguments = VariableSubsSlice::reserve_into_subs(subs, arguments.len());
                for (target_index, var_index) in (new_arguments.indices()).zip(arguments) {
                    let var = helper!(var_index);
                    subs.variables[target_index] = var;
                }

                let ret_var = helper!(ret_type);
                let closure_var = helper!(closure_type);
                let content =
                    Content::Structure(FlatType::Func(new_arguments, closure_var, ret_var));

                register_with_known_var(subs, destination, rank, pools, content)
            }
            Record(fields, ext) => {
                // An empty fields is inefficient (but would be correct)
                // If hit, try to turn the value into an EmptyRecord in canonicalization
                debug_assert!(!fields.is_empty() || !ext.is_closed());

                let mut field_vars = Vec::with_capacity_in(fields.len(), arena);

                for (field, field_type) in fields {
                    let field_var = {
                        use roc_types::types::RecordField::*;
                        match &field_type {
                            Optional(t) => Optional(helper!(t)),
                            Required(t) => Required(helper!(t)),
                            Demanded(t) => Demanded(helper!(t)),
                        }
                    };

                    field_vars.push((field.clone(), field_var));
                }

                let temp_ext_var = match ext {
                    TypeExtension::Open(ext) => helper!(ext),
                    TypeExtension::Closed => Variable::EMPTY_RECORD,
                };

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

                register_with_known_var(subs, destination, rank, pools, content)
            }

            TagUnion(tags, ext) => {
                // An empty tags is inefficient (but would be correct)
                // If hit, try to turn the value into an EmptyTagUnion in canonicalization
                debug_assert!(!tags.is_empty() || !ext.is_closed());

                let (union_tags, ext) =
                    type_to_union_tags(subs, rank, pools, arena, tags, ext, &mut stack);
                let content = Content::Structure(FlatType::TagUnion(union_tags, ext));

                register_with_known_var(subs, destination, rank, pools, content)
            }
            FunctionOrTagUnion(tag_name, symbol, ext) => {
                let temp_ext_var = match ext {
                    TypeExtension::Open(ext) => helper!(ext),
                    TypeExtension::Closed => Variable::EMPTY_TAG_UNION,
                };

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

                register_with_known_var(subs, destination, rank, pools, content)
            }
            RecursiveTagUnion(rec_var, tags, ext) => {
                // An empty tags is inefficient (but would be correct)
                // If hit, try to turn the value into an EmptyTagUnion in canonicalization
                debug_assert!(!tags.is_empty() || !ext.is_closed());

                let (union_tags, ext) =
                    type_to_union_tags(subs, rank, pools, arena, tags, ext, &mut stack);
                let content =
                    Content::Structure(FlatType::RecursiveTagUnion(*rec_var, union_tags, ext));

                let tag_union_var = destination;
                register_with_known_var(subs, tag_union_var, rank, pools, content);

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

            Type::DelayedAlias(AliasCommon {
                symbol,
                type_arguments,
                lambda_set_variables,
            }) => {
                let alias_variables = {
                    let length = type_arguments.len() + lambda_set_variables.len();
                    let new_variables = VariableSubsSlice::reserve_into_subs(subs, length);

                    for (target_index, arg_type) in (new_variables.indices()).zip(type_arguments) {
                        let copy_var = helper!(arg_type);
                        subs.variables[target_index] = copy_var;
                    }

                    let it = (new_variables.indices().skip(type_arguments.len()))
                        .zip(lambda_set_variables);
                    for (target_index, ls) in it {
                        let copy_var = helper!(&ls.0);
                        subs.variables[target_index] = copy_var;
                    }

                    AliasVariables {
                        variables_start: new_variables.start,
                        type_variables_len: type_arguments.len() as _,
                        all_variables_len: length as _,
                    }
                };

                let instantiated = aliases.instantiate_real_var(
                    subs,
                    rank,
                    pools,
                    arena,
                    *symbol,
                    alias_variables,
                );

                let (alias_variable, kind) = instantiated
                    .unwrap_or_else(|_| unreachable!("Alias {:?} is not available", symbol));

                let content = Content::Alias(*symbol, alias_variables, alias_variable, kind);

                register_with_known_var(subs, destination, rank, pools, content)
            }

            Type::Alias {
                symbol,
                type_arguments,
                actual,
                lambda_set_variables,
                kind,
            } => {
                debug_assert!(Variable::get_reserved(*symbol).is_none());

                let alias_variables = {
                    let length = type_arguments.len() + lambda_set_variables.len();
                    let new_variables = VariableSubsSlice::reserve_into_subs(subs, length);

                    for (target_index, OptAbleType { typ, opt_ability }) in
                        (new_variables.indices()).zip(type_arguments)
                    {
                        let copy_var = match opt_ability {
                            None => helper!(typ),
                            Some(ability) => {
                                match RegisterVariable::from_type(subs, rank, pools, arena, typ) {
                                    RegisterVariable::Direct(var) => {
                                        use Content::*;
                                        match *subs.get_content_without_compacting(var) {
                                            FlexVar(opt_name) => subs
                                                .set_content(var, FlexAbleVar(opt_name, *ability)),
                                            RigidVar(name) => subs.set_content(
                                                var,
                                                FlexAbleVar(Some(name), *ability),
                                            ),
                                            RigidAbleVar(name, existing_ability) => {
                                                debug_assert_eq!(existing_ability, *ability);
                                                subs.set_content(
                                                    var,
                                                    FlexAbleVar(Some(name), existing_ability),
                                                );
                                            }
                                            _ => {
                                                // TODO associate the type to the bound ability, and check
                                                // that it correctly implements the ability.
                                            }
                                        }
                                        var
                                    }
                                    RegisterVariable::Deferred => {
                                        // TODO associate the type to the bound ability, and check
                                        // that it correctly implements the ability.
                                        let var = subs.fresh_unnamed_flex_var();
                                        stack.push(TypeToVar::Defer(typ, var));
                                        var
                                    }
                                }
                            }
                        };
                        subs.variables[target_index] = copy_var;
                    }

                    let it = (new_variables.indices().skip(type_arguments.len()))
                        .zip(lambda_set_variables);
                    for (target_index, ls) in it {
                        let copy_var = helper!(&ls.0);
                        subs.variables[target_index] = copy_var;
                    }

                    AliasVariables {
                        variables_start: new_variables.start,
                        type_variables_len: type_arguments.len() as _,
                        all_variables_len: length as _,
                    }
                };

                let alias_variable = if let Symbol::RESULT_RESULT = *symbol {
                    roc_result_to_var(subs, rank, pools, arena, actual, &mut stack)
                } else {
                    helper!(actual)
                };
                let content = Content::Alias(*symbol, alias_variables, alias_variable, *kind);

                register_with_known_var(subs, destination, rank, pools, content)
            }
            HostExposedAlias {
                name: symbol,
                type_arguments,
                actual: alias_type,
                actual_var,
                lambda_set_variables,
                ..
            } => {
                let alias_variables = {
                    let length = type_arguments.len() + lambda_set_variables.len();
                    let new_variables = VariableSubsSlice::reserve_into_subs(subs, length);

                    for (target_index, arg_type) in (new_variables.indices()).zip(type_arguments) {
                        let copy_var = helper!(arg_type);
                        subs.variables[target_index] = copy_var;
                    }

                    AliasVariables {
                        variables_start: new_variables.start,
                        type_variables_len: type_arguments.len() as _,
                        all_variables_len: length as _,
                    }
                };

                // cannot use helper! here because this variable may be involved in unification below
                let alias_variable =
                    type_to_variable(subs, rank, pools, arena, aliases, alias_type);
                // TODO(opaques): I think host-exposed aliases should always be structural
                // (when does it make sense to give a host an opaque type?)
                let content = Content::Alias(
                    *symbol,
                    alias_variables,
                    alias_variable,
                    AliasKind::Structural,
                );
                // let result = register(subs, rank, pools, content);
                let result = register_with_known_var(subs, destination, rank, pools, content);

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
                let problem_index = SubsIndex::push_new(&mut subs.problems, problem.clone());
                let content = Content::Structure(FlatType::Erroneous(problem_index));

                register_with_known_var(subs, destination, rank, pools, content)
            }
        };
    }

    result
}

#[inline(always)]
fn roc_result_to_var<'a>(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    arena: &'_ bumpalo::Bump,
    result_type: &'a Type,
    stack: &mut bumpalo::collections::Vec<'_, TypeToVar<'a>>,
) -> Variable {
    match result_type {
        Type::TagUnion(tags, ext) => {
            debug_assert!(ext.is_closed());
            debug_assert!(tags.len() == 2);

            if let [(err, err_args), (ok, ok_args)] = &tags[..] {
                debug_assert_eq!(err, &subs.tag_names[0]);
                debug_assert_eq!(ok, &subs.tag_names[1]);

                if let ([err_type], [ok_type]) = (err_args.as_slice(), ok_args.as_slice()) {
                    let err_var =
                        RegisterVariable::with_stack(subs, rank, pools, arena, err_type, stack);
                    let ok_var =
                        RegisterVariable::with_stack(subs, rank, pools, arena, ok_type, stack);

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

    let tag_name = &slice.get(0)?.0;

    let mut result = None;

    // the `SubsSlice<TagName>` that inserting `slice` into subs would give
    let bigger_slice = SubsSlice::new(subs.tag_names.len() as _, slice.len() as _);

    match subs.tag_name_cache.get_mut(tag_name) {
        Some(occupied) => {
            let subs_slice = *occupied;

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
                    *occupied = bigger_slice;
                }
            }
        }
        None => {
            subs.tag_name_cache.push(tag_name, bigger_slice);
        }
    }

    result
}

#[inline(always)]
fn register_tag_arguments<'a>(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    arena: &'_ bumpalo::Bump,
    stack: &mut bumpalo::collections::Vec<'_, TypeToVar<'a>>,
    arguments: &'a [Type],
) -> VariableSubsSlice {
    if arguments.is_empty() {
        VariableSubsSlice::default()
    } else {
        let new_variables = VariableSubsSlice::reserve_into_subs(subs, arguments.len());
        let it = (new_variables.indices()).zip(arguments);
        for (target_index, argument) in it {
            let var = RegisterVariable::with_stack(subs, rank, pools, arena, argument, stack);
            subs.variables[target_index] = var;
        }

        new_variables
    }
}

/// Assumes that the tags are sorted and there are no duplicates!
fn insert_tags_fast_path<'a>(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    arena: &'_ bumpalo::Bump,
    tags: &'a [(TagName, Vec<Type>)],
    stack: &mut bumpalo::collections::Vec<'_, TypeToVar<'a>>,
) -> UnionTags {
    if let [(TagName::Tag(tag_name), arguments)] = tags {
        let variable_slice = register_tag_arguments(subs, rank, pools, arena, stack, arguments);
        let new_variable_slices =
            SubsSlice::extend_new(&mut subs.variable_slices, [variable_slice]);

        macro_rules! subs_tag_name {
            ($tag_name_slice:expr) => {
                return UnionTags::from_slices($tag_name_slice, new_variable_slices)
            };
        }

        match tag_name.as_str() {
            "Ok" => subs_tag_name!(Subs::TAG_NAME_OK.as_slice()),
            "Err" => subs_tag_name!(Subs::TAG_NAME_ERR.as_slice()),
            "InvalidNumStr" => subs_tag_name!(Subs::TAG_NAME_INVALID_NUM_STR.as_slice()),
            "BadUtf8" => subs_tag_name!(Subs::TAG_NAME_BAD_UTF_8.as_slice()),
            "OutOfBounds" => subs_tag_name!(Subs::TAG_NAME_OUT_OF_BOUNDS.as_slice()),
            _other => {}
        }
    }

    let new_variable_slices = SubsSlice::reserve_variable_slices(subs, tags.len());
    match find_tag_name_run(tags, subs) {
        Some(new_tag_names) => {
            let it = (new_variable_slices.indices()).zip(tags);

            for (variable_slice_index, (_, arguments)) in it {
                subs.variable_slices[variable_slice_index] =
                    register_tag_arguments(subs, rank, pools, arena, stack, arguments);
            }

            UnionTags::from_slices(new_tag_names, new_variable_slices)
        }
        None => {
            let new_tag_names = SubsSlice::reserve_tag_names(subs, tags.len());

            let it = (new_variable_slices.indices())
                .zip(new_tag_names.indices())
                .zip(tags);

            for ((variable_slice_index, tag_name_index), (tag_name, arguments)) in it {
                subs.variable_slices[variable_slice_index] =
                    register_tag_arguments(subs, rank, pools, arena, stack, arguments);

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
    arena: &'_ bumpalo::Bump,
    tags: &'a [(TagName, Vec<Type>)],
    mut tag_vars: bumpalo::collections::Vec<(TagName, VariableSubsSlice)>,
    stack: &mut bumpalo::collections::Vec<'_, TypeToVar<'a>>,
) -> UnionTags {
    for (tag, tag_argument_types) in tags {
        let new_slice = VariableSubsSlice::reserve_into_subs(subs, tag_argument_types.len());

        for (i, arg) in (new_slice.indices()).zip(tag_argument_types) {
            let var = RegisterVariable::with_stack(subs, rank, pools, arena, arg, stack);
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
    arena: &'_ bumpalo::Bump,
    tags: &'a [(TagName, Vec<Type>)],
    ext: &'a TypeExtension,
    stack: &mut bumpalo::collections::Vec<'_, TypeToVar<'a>>,
) -> (UnionTags, Variable) {
    use bumpalo::collections::Vec;

    let sorted = tags.len() == 1 || sorted_no_duplicates(tags);

    match ext {
        TypeExtension::Closed => {
            let ext = Variable::EMPTY_TAG_UNION;

            let union_tags = if sorted {
                insert_tags_fast_path(subs, rank, pools, arena, tags, stack)
            } else {
                let tag_vars = Vec::with_capacity_in(tags.len(), arena);
                insert_tags_slow_path(subs, rank, pools, arena, tags, tag_vars, stack)
            };

            (union_tags, ext)
        }
        TypeExtension::Open(ext) => {
            let mut tag_vars = Vec::with_capacity_in(tags.len(), arena);

            let temp_ext_var = RegisterVariable::with_stack(subs, rank, pools, arena, ext, stack);
            let (it, ext) = roc_types::types::gather_tags_unsorted_iter(
                subs,
                UnionTags::default(),
                temp_ext_var,
            );

            tag_vars.extend(it.map(|(n, v)| (n.clone(), v)));

            let union_tags = if tag_vars.is_empty() && sorted {
                insert_tags_fast_path(subs, rank, pools, arena, tags, stack)
            } else {
                insert_tags_slow_path(subs, rank, pools, arena, tags, tag_vars, stack)
            };

            (union_tags, ext)
        }
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
            if !subs.redundant(var) {
                let rank = subs.get_rank(var);

                pools.get_mut(rank).push(var);
            }
        }
    }

    // For variables with rank young_rank, if rank < young_rank: register in old pool,
    // otherwise generalize
    for var in last_pool.drain(..) {
        if !subs.redundant(var) {
            let desc_rank = subs.get_rank(var);

            if desc_rank < young_rank {
                pools.get_mut(desc_rank).push(var);
            } else {
                subs.set_rank(var, Rank::NONE);
            }
        }
    }

    // re-use the last_vector (which likely has a good capacity for future runs
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
        let var = young_vars[i];
        let rank = subs.get_rank_set_mark(var, young_mark);

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
fn adjust_rank(
    subs: &mut Subs,
    young_mark: Mark,
    visit_mark: Mark,
    group_rank: Rank,
    var: Variable,
) -> Rank {
    let desc = subs.get_ref_mut(var);

    let desc_rank = desc.rank;
    let desc_mark = desc.mark;

    if desc_mark == young_mark {
        // SAFETY: in this function (and functions it calls, we ONLY modify rank and mark, never content!
        // hence, we can have an immutable reference to it even though we also have a mutable
        // reference to the Subs as a whole. This prevents a clone of the content, which turns out
        // to be quite expensive.
        let content = {
            let ptr = &desc.content as *const _;
            unsafe { &*ptr }
        };

        // Mark the variable as visited before adjusting content, as it may be cyclic.
        desc.mark = visit_mark;

        let max_rank = adjust_rank_content(subs, young_mark, visit_mark, group_rank, content);

        subs.set_rank_mark(var, max_rank, visit_mark);

        max_rank
    } else if desc_mark == visit_mark {
        // we have already visited this variable
        // (probably two variables had the same root)
        desc_rank
    } else {
        let min_rank = group_rank.min(desc_rank);

        // TODO from elm-compiler: how can min_rank ever be group_rank?
        desc.rank = min_rank;
        desc.mark = visit_mark;

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
                let name = *name;

                // NOTE: we must write to the mutually borrowed `desc` value here
                // using `subs.set` does not work (unclear why, really)
                // but get_ref_mut approach saves a lookup, so the weirdness is worth it
                desc.content = FlexVar(Some(name));
                desc.rank = max_rank;
                desc.mark = Mark::NONE;
                desc.copy = OptVariable::NONE;
            }
            &RigidAbleVar(name, ability) => {
                // Same as `RigidVar` above
                desc.content = FlexAbleVar(Some(name), ability);
                desc.rank = max_rank;
                desc.mark = Mark::NONE;
                desc.copy = OptVariable::NONE;
            }
            FlexVar(_) | FlexAbleVar(_, _) | Error => (),

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
            Alias(_, args, var, _) => {
                let var = *var;
                let args = *args;

                stack.extend(var_slice!(args.all_variables()));

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

fn deep_copy_var_in(
    subs: &mut Subs,
    rank: Rank,
    pools: &mut Pools,
    var: Variable,
    arena: &Bump,
) -> Variable {
    let mut visited = bumpalo::collections::Vec::with_capacity_in(256, arena);

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

    let subs_len = subs.len();
    let desc = subs.get_ref_mut(var);

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

    // Safety: Here we make a variable that is 1 position out of bounds.
    // The reason is that we can now keep the mutable reference to `desc`
    // Below, we actually push a new variable onto subs meaning the `copy`
    // variable is in-bounds before it is ever used.
    let copy = unsafe { Variable::from_index(subs_len as u32) };

    pools.get_mut(max_rank).push(copy);

    // Link the original variable to the new variable. This lets us
    // avoid making multiple copies of the variable we are instantiating.
    //
    // Need to do this before recursively copying to avoid looping.
    desc.mark = Mark::NONE;
    desc.copy = copy.into();

    let actual_copy = subs.fresh(make_descriptor(content));
    debug_assert_eq!(copy, actual_copy);

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

        FlexVar(_) | FlexAbleVar(_, _) | Error => copy,

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

        RigidAbleVar(name, ability) => {
            subs.set(copy, make_descriptor(FlexAbleVar(Some(name), ability)));

            copy
        }

        Alias(symbol, arguments, real_type_var, kind) => {
            let new_variables =
                SubsSlice::reserve_into_subs(subs, arguments.all_variables_len as _);
            for (target_index, var_index) in
                (new_variables.indices()).zip(arguments.all_variables())
            {
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
            let new_content = Alias(symbol, new_arguments, new_real_type_var, kind);

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

#[inline(always)]
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
) -> Variable {
    let descriptor = Descriptor {
        content,
        rank,
        mark: Mark::NONE,
        copy: OptVariable::NONE,
    };

    subs.set(var, descriptor);

    pools.get_mut(rank).push(var);

    var
}
