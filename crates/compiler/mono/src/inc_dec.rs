// This program was written by Jelle Teeuwissen within a final
// thesis project of the Computing Science master program at Utrecht
// University under supervision of Wouter Swierstra (w.s.swierstra@uu.nl).

use std::{collections::HashMap, hash::BuildHasherDefault, iter};

use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::{all::WyHash, MutMap, MutSet};
use roc_module::{low_level::LowLevel, symbol::Symbol};

use crate::{
    borrow::Ownership,
    ir::{
        BranchInfo, Call, CallType, Expr, HigherOrderLowLevel, JoinPointId, ListLiteralElement,
        ModifyRc, Param, Proc, ProcLayout, Stmt,
    },
    layout::{InLayout, LayoutInterner, STLayoutInterner},
    low_level::HigherOrder,
};

/**
Insert the reference count operations for procedures.
*/
pub fn insert_inc_dec_operations<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i STLayoutInterner<'a>,
    procedures: &mut HashMap<(Symbol, ProcLayout), Proc<'a>, BuildHasherDefault<WyHash>>,
) -> () {
    // Create a VariableRcTypesEnv for the procedures as they get referenced but should be marked as non reference counted.
    let mut variable_rc_types_env = VariableRcTypesEnv::from_layout_interner(layout_interner);
    variable_rc_types_env.insert_proc_symbols(procedures.keys().map(|(symbol, _layout)| *symbol));
    for (_, proc) in procedures.iter_mut() {
        // Clone the variable_rc_types_env and insert the variables in the current procedure.
        // As the variables should be limited in scope for the current proc.
        let variable_rc_types_env = variable_rc_types_env.clone();

        insert_inc_dec_operations_proc(arena, variable_rc_types_env, proc);
    }
}

/**
Enum indicating whether a variable (symbol) should be reference counted or not.
This includes layouts that themselves can be stack allocated but that contain a heap allocated item.
*/
#[derive(Clone)]
enum VarRcType {
    ReferenceCounted,
    NotReferenceCounted,
}

type VariableRcTypes = MutMap<Symbol, VarRcType>;

/**
Environment to keep track which of the variables should be reference counted and which ones should not.
 */
struct VariableRcTypesEnv<'a, 'i> {
    // A map keeping track of which variables are reference counted and which are not.
    variables_rc_type: VariableRcTypes,

    layout_interner: &'i STLayoutInterner<'a>,
}

impl<'a, 'i> VariableRcTypesEnv<'a, 'i> {
    /**
    Create a new VariableRcTypesEnv from a layout interner.
    */
    fn from_layout_interner(
        layout_interner: &'i STLayoutInterner<'a>,
    ) -> VariableRcTypesEnv<'a, 'i> {
        VariableRcTypesEnv {
            variables_rc_type: VariableRcTypes::default(),
            layout_interner,
        }
    }
    /**
    Insert the reference count type of top level functions.
    As functions are not reference counted, they can be marked as such.
    */
    fn insert_proc_symbols(
        self: &mut VariableRcTypesEnv<'a, 'i>,
        proc_symbols: impl Iterator<Item = Symbol>,
    ) {
        for proc_symbol in proc_symbols {
            self.variables_rc_type
                .insert(proc_symbol, VarRcType::NotReferenceCounted);
        }
    }

    /**
    Insert the reference count types of all variables in a procedure.
    */
    fn insert_variables_rc_type_proc(self: &mut VariableRcTypesEnv<'a, 'i>, proc: &Proc<'a>) {
        // First collect the argument types.
        for (layout, symbol) in proc.args.iter() {
            self.insert_symbol_layout_rc_type(symbol, layout);
        }

        // Then collect the types of the variables in the body.
        self.insert_variables_rc_type_stmt(&proc.body);
    }

    /**
    Insert the reference count types of all variables in a statement.
    */
    fn insert_variables_rc_type_stmt(self: &mut VariableRcTypesEnv<'a, 'i>, stmt: &Stmt<'a>) {
        match stmt {
            Stmt::Let(
                binding,
                // Expressions can be omitted, as they won't create new variables.
                _expr,
                layout,
                continuation,
            ) => {
                self.insert_symbol_layout_rc_type(&binding, layout);
                self.insert_variables_rc_type_stmt(continuation);
            }
            Stmt::Switch {
                // The switch condition is an integer and thus not reference counted.
                cond_symbol: _,
                cond_layout: _,
                branches,
                default_branch,
                ret_layout: _,
            } => {
                // Collect the types of the variables in all the branches, including the default one.
                for (info, stmt) in branches
                    .iter()
                    .map(|(_branch, info, stmt)| (info, stmt))
                    .chain(iter::once((&default_branch.0, default_branch.1)))
                {
                    match info {
                        BranchInfo::None => (),
                        BranchInfo::Constructor {
                            scrutinee,
                            layout,
                            tag_id: _,
                        } => {
                            self.insert_symbol_layout_rc_type(scrutinee, layout);
                        }
                    }

                    self.insert_variables_rc_type_stmt(stmt);
                }
            }
            Stmt::Ret(_symbol) => {
                // The return does not introduce new variables.
            }
            Stmt::Refcounting(_, _) => unreachable!(
                "Refcounting operations should not be present in the AST at this point."
            ),
            Stmt::Expect { remainder, .. }
            | Stmt::ExpectFx { remainder, .. }
            | Stmt::Dbg { remainder, .. } => {
                self.insert_variables_rc_type_stmt(remainder);
            }
            Stmt::Join {
                id: _,
                parameters,
                body,
                remainder: continuation,
            } => {
                for parameter in parameters.iter() {
                    self.insert_symbol_layout_rc_type(&parameter.symbol, &parameter.layout);
                }

                self.insert_variables_rc_type_stmt(body);
                self.insert_variables_rc_type_stmt(continuation);
            }
            Stmt::Jump(_, _) => {
                // A join point does not introduce new variables.
            }
            Stmt::Crash(_, _) => {
                // A crash does not introduce new variables.
            }
        }
    }

    /*
    Insert the reference count type of a symbol given its layout.
    */
    fn insert_symbol_layout_rc_type(
        self: &mut VariableRcTypesEnv<'a, 'i>,
        symbol: &Symbol,
        layout: &InLayout,
    ) {
        // This will reference count the entire struct, even if only one field is reference counted.
        // In another pass we can inline these operations, potentially improving reuse.
        let contains_refcounted = self.layout_interner.contains_refcounted(*layout);
        if contains_refcounted {
            self.variables_rc_type
                .insert(*symbol, VarRcType::ReferenceCounted);
        } else {
            self.variables_rc_type
                .insert(*symbol, VarRcType::NotReferenceCounted);
        }
    }
}

impl Clone for VariableRcTypesEnv<'_, '_> {
    fn clone(&self) -> Self {
        VariableRcTypesEnv {
            variables_rc_type: self.variables_rc_type.clone(),
            layout_interner: self.layout_interner,
        }
    }
}

type VariablesOwnership = MutMap<Symbol, Ownership>;

/**
Type containing data about the variable consumption of a join point.
*/
type JoinPointConsumption = MutSet<Symbol>;

/**
The environment for the reference counting pass.
Contains the variable rc types and the ownership.
*/
struct RefcountEnvironment<'v> {
    // Keep track which variables are reference counted and which are not.
    variables_rc_types: &'v VariableRcTypes,
    // The Koka implementation assumes everything that is not owned to be borrowed.
    variables_ownership: VariablesOwnership,
    jointpoint_closures: MutMap<JoinPointId, JoinPointConsumption>,
}

impl<'v, 'a> Clone for RefcountEnvironment<'v> {
    fn clone(&self) -> Self {
        RefcountEnvironment {
            variables_rc_types: self.variables_rc_types,
            variables_ownership: self.variables_ownership.clone(),
            jointpoint_closures: self.jointpoint_closures.clone(),
        }
    }
}

impl<'v> RefcountEnvironment<'v> {
    /**
    Retrieve the rc type of a variable.
    */
    fn get_variable_rc_type(self: &mut RefcountEnvironment<'v>, variable: &Symbol) -> &VarRcType {
        self.variables_rc_types
            .get(variable)
            .expect("variable should have rc type")
    }

    /*
    Retrieve whether the variable is owned or borrowed.
    If it was owned, set it to borrowed (as it is consumed/the variable can be used as owned only once without incrementing).
    If the variable is not reference counted, do nothing and return None.
    */
    fn consume_variable(
        self: &mut RefcountEnvironment<'v>,
        variable: &Symbol,
    ) -> Option<Ownership> {
        if !self.variables_ownership.contains_key(variable) {
            return None;
        }

        // Consume the variable and return the previous ownership.
        Some(self.consume_rc_variable(variable))
    }

    /*
    Retrieve whether the variable is owned or borrowed.
    If it was owned, set it to borrowed (as it is consumed/the variable can be used as owned only once without incrementing).
    */
    fn consume_rc_variable(self: &mut RefcountEnvironment<'v>, variable: &Symbol) -> Ownership {
        // Consume the variable by setting it to borrowed (if it was owned before), and return the previous ownership.
        self.variables_ownership
            .insert(*variable, Ownership::Borrowed)
            .expect("Expected variable to be in environment")
    }

    /**
       Retrieve the ownership of a variable.
       If the variable is not reference counted, it will None.
    */
    fn get_variable_ownership(
        self: &RefcountEnvironment<'v>,
        variable: &Symbol,
    ) -> Option<&Ownership> {
        self.variables_ownership.get(variable)
    }

    /**
    Remove non reference counted variables from an iterator.
    Useful when we want to insert reference count operations for symbols that might not be rc.
    */
    fn filter_rc_variables<'a>(
        self: &RefcountEnvironment<'v>,
        variables: impl Iterator<Item = &'a Symbol>,
    ) -> std::vec::Vec<&'a Symbol> {
        variables
            .filter(|variable| self.variables_ownership.contains_key(variable))
            .collect::<std::vec::Vec<_>>()
    }

    /**
    Add a variables to the environment if they are reference counted.
    */
    fn add_variables(
        self: &mut RefcountEnvironment<'v>,
        variables: impl IntoIterator<Item = Symbol>,
    ) {
        variables
            .into_iter()
            .for_each(|variable| self.add_variable(variable))
    }

    /**
    Add a variable to the environment if it is reference counted.
    */
    fn add_variable(self: &mut RefcountEnvironment<'v>, variable: Symbol) {
        match self.get_variable_rc_type(&variable) {
            VarRcType::ReferenceCounted => {
                self.variables_ownership.insert(variable, Ownership::Owned);
            }
            VarRcType::NotReferenceCounted => {
                // If this variable is not reference counted, we don't need to do anything.
            }
        }
    }

    /**
    Remove a variable from the environment.
    Is used when a variable is no longer in scope (before a let binding).
     */
    fn remove_variable(self: &mut RefcountEnvironment<'v>, variable: Symbol) {
        self.variables_ownership.remove(&variable);
    }

    /**
    Add a joinpoint id and the consumed closure to the environment.
    Used when analyzing a join point. So that a jump can update the environment on call.
    */
    fn add_joinpoint_consumption(
        self: &mut RefcountEnvironment<'v>,
        joinpoint_id: JoinPointId,
        consumption: JoinPointConsumption,
    ) {
        self.jointpoint_closures.insert(joinpoint_id, consumption);
    }

    /**
    Get the consumed closure from a join point id.
    */
    fn get_joinpoint_consumption(
        self: &RefcountEnvironment<'v>,
        joinpoint_id: JoinPointId,
    ) -> JoinPointConsumption {
        self.jointpoint_closures
            .get(&joinpoint_id)
            .expect("Expected closure to be in environment")
            .clone()
    }

    /**
    Remove a joinpoint id and the consumed closure from the environment.
    Used after analyzing the continuation of a join point.
    */
    fn remove_joinpoint_consumption(self: &mut RefcountEnvironment<'v>, joinpoint_id: JoinPointId) {
        let closure = self.jointpoint_closures.remove(&joinpoint_id);
        debug_assert!(
            matches!(closure, Some(_)),
            "Expected closure to be in environment"
        );
    }

    /**
    Return owned usages.
    Collect the usage of all the reference counted symbols in the iterator and return as a map.
    */
    fn owned_usages(
        self: &RefcountEnvironment<'v>,
        symbols: impl IntoIterator<Item = Symbol>,
    ) -> MutMap<Symbol, u64> {
        // A groupby or something similar would be nice here.
        let mut variable_usage = MutMap::default();
        for symbol in symbols {
            match {
                self.variables_rc_types
                    .get(&symbol)
                    .expect("Expected variable to be in the map")
            } {
                // If the variable is reference counted, we need to increment the usage count.
                VarRcType::ReferenceCounted => {
                    match variable_usage.get(&symbol) {
                        Some(count) => variable_usage.insert(symbol, count + 1),
                        None => variable_usage.insert(symbol, 1),
                    };
                }
                // If the variable is not reference counted, we don't need to do anything.
                VarRcType::NotReferenceCounted => continue,
            }
        }
        variable_usage
    }

    /**
    Filter the given symbols to only contain reference counted symbols.
    */
    fn borrowed_usages(
        self: &RefcountEnvironment<'v>,
        symbols: impl IntoIterator<Item = Symbol>,
    ) -> MutSet<Symbol> {
        symbols
            .into_iter()
            .filter_map(|symbol| {
                match {
                    self.variables_rc_types
                        .get(&symbol)
                        .expect("Expected variable to be in the map")
                } {
                    // If the variable is reference counted, we need to increment the usage count.
                    VarRcType::ReferenceCounted => Some(symbol),
                    // If the variable is not reference counted, we don't need to do anything.
                    VarRcType::NotReferenceCounted => None,
                }
            })
            .collect()
    }
}

/**
 Insert the reference counting operations into a statement.
*/
fn insert_inc_dec_operations_proc<'a, 'i>(
    arena: &'a Bump,
    mut variable_rc_types_env: VariableRcTypesEnv<'a, 'i>,
    proc: &mut Proc<'a>,
) {
    // Clone the variable_rc_types_env and insert the variables in the current procedure.
    // As the variables should be limited in scope for the current proc.
    variable_rc_types_env.insert_variables_rc_type_proc(&proc);

    let mut environment = RefcountEnvironment {
        variables_rc_types: &variable_rc_types_env.variables_rc_type,
        variables_ownership: MutMap::default(),
        jointpoint_closures: MutMap::default(),
    };

    // Add all arguments to the environment (if they are reference counted)
    let proc_symbols = proc.args.iter().map(|(_layout, symbol)| symbol);
    for symbol in proc_symbols.clone() {
        environment.add_variable(*symbol);
    }

    // Update the body with reference count statements.
    let new_body = insert_refcount_operations_stmt(arena, &mut environment, &proc.body);

    // Insert decrement statements for unused parameters (which are still marked as owned).
    let rc_proc_symbols = environment.filter_rc_variables(proc_symbols);
    let newer_body = consume_and_insert_dec_stmts(
        arena,
        &mut environment,
        rc_proc_symbols.iter().copied().copied(),
        new_body,
    );

    // Assert that just the arguments are in the environment. And (after decrementing the unused ones) that they are all borrowed.
    debug_assert!(environment
        .variables_ownership
        .iter()
        .all(|(symbol, ownership)| {
            // All variables should be borrowed.
            *ownership == Ownership::Borrowed
                && proc
                    .args
                    .iter()
                    .map(|(_layout, symbol)| symbol)
                    .any(|s| s == symbol)
        }));

    proc.body = newer_body.clone();
}

/**
Given an environment, insert the reference counting operations for a statement.
Assuming that a symbol can only be defined once (no binding to the same variable multiple times).
*/
fn insert_refcount_operations_stmt<'v, 'a>(
    arena: &'a Bump,
    environment: &mut RefcountEnvironment<'v>,
    stmt: &Stmt<'a>,
) -> &'a Stmt<'a> {
    match &stmt {
        // The expression borrows the values owned (used) by the continuation.
        Stmt::Let(_, _, _, _) => {
            // Collect all the subsequent let bindings (including the current one).
            // To prevent the stack from overflowing when there are many let bindings.
            let mut triples = vec![];
            let mut current_stmt = stmt;
            while let Stmt::Let(binding, expr, layout, next_stmt) = current_stmt {
                triples.push((binding, expr, layout));
                current_stmt = next_stmt
            }

            debug_assert!(
                !triples.is_empty(),
                "Expected at least one let binding in the vector"
            );
            debug_assert!(
                !matches!(current_stmt, Stmt::Let(_, _, _, _)),
                "All let bindings should be in the vector"
            );

            for (binding, _, _) in triples.iter() {
                environment.add_variable(**binding); // Add the bound variable to the environment. As it can be used in the continuation.
            }

            triples
                .into_iter()
                .rev()
                // First evaluate the continuation and let it consume it's free variables.
                .fold(
                    insert_refcount_operations_stmt(arena, environment, current_stmt),
                    |new_stmt, (binding, expr, layout)| {
                        // If the binding is still owned in the environment, it is not used in the continuation and we can drop it right away.
                        let new_stmt_without_unused = match matches!(
                            environment.get_variable_ownership(binding),
                            Some(Ownership::Owned)
                        ) {
                            true => insert_dec_stmt(arena, *binding, new_stmt),
                            false => new_stmt,
                        };

                        // And as the variable should not be in scope before this let binding, remove it from the environment.
                        environment.remove_variable(*binding);

                        insert_refcount_operations_binding(
                            arena,
                            environment,
                            binding,
                            expr,
                            layout,
                            new_stmt_without_unused,
                        )
                    },
                )
        }
        Stmt::Switch {
            cond_symbol,
            cond_layout,
            branches,
            default_branch,
            ret_layout,
        } => {
            let new_branches = branches
                .iter()
                .map(|(label, info, branch)| {
                    let mut branch_env = environment.clone();

                    let new_branch =
                        insert_refcount_operations_stmt(arena, &mut branch_env, branch);

                    (*label, info.clone(), new_branch, branch_env)
                })
                .collect::<std::vec::Vec<_>>();

            let new_default_branch = {
                let (info, branch) = default_branch;

                let mut branch_env = environment.clone();
                let new_branch = insert_refcount_operations_stmt(arena, &mut branch_env, branch);

                (info.clone(), new_branch, branch_env)
            };

            // Determine what variables are consumed in some of the branches.
            // So we can make sure they are consumed in the current environment and all branches.
            let consume_variables = {
                let branch_envs = {
                    let mut branch_environments =
                        Vec::with_capacity_in(new_branches.len() + 1, arena);

                    for (_, _, _, branch_env) in new_branches.iter() {
                        branch_environments.push(branch_env);
                    }

                    branch_environments.push(&new_default_branch.2);

                    branch_environments
                };

                {
                    let mut consume_variables = MutSet::default();

                    for (symbol, ownership) in environment.variables_ownership.iter() {
                        match ownership {
                            Ownership::Owned => {
                                if branch_envs
                                .iter()
                                .any(|branch_env| matches!(branch_env.get_variable_ownership(symbol).expect("All symbols defined in the current environment should be in the environment of the branches."), Ownership::Borrowed))
                            {
                                // If the variable is currently owned, and not in a some branches, it must be consumed in all branches
                                consume_variables.insert(*symbol);
                            }
                                // Otherwise it can stay owned.
                            }
                            Ownership::Borrowed => {
                                // If the variable is currently borrowed, it must be borrowed in all branches and we don't have to do anything.
                            }
                        }
                    }

                    consume_variables
                }
            };

            // Given the consume_variables we can determine what additional variables should be dropped in each branch.
            let newer_branches = Vec::from_iter_in(
                new_branches
                    .into_iter()
                    .map(|(label, info, branch, branch_env)| {
                        // If the variable is owned in the branch, it is not used in the branch and we can drop it.
                        let consume_variables_branch = consume_variables.iter().copied().filter(|consume_variable| {
                             matches!(branch_env.get_variable_ownership(consume_variable).expect("All symbols defined in the current environment should be in the environment of the branches."), Ownership::Owned) 
                        });

                        let newer_branch = insert_dec_stmts(arena,  consume_variables_branch, branch);
                        (label, info, newer_branch.clone())
                    }),
                arena,
            )
            .into_bump_slice();

            let newer_default_branch = {
                let (info, branch, branch_env) = new_default_branch;
                // If the variable is owned in the branch, it is not used in the branch and we can drop it.
                let consume_variables_branch = consume_variables.iter().copied().filter(|consume_variable| {
                    matches!(branch_env.get_variable_ownership(consume_variable).expect("All symbols defined in the current environment should be in the environment of the branches."), Ownership::Owned) 
                 });

                let newer_branch = insert_dec_stmts(arena, consume_variables_branch, branch);

                (info, newer_branch)
            };

            // In addition to updating the branches, we need to update the current environment.
            for consume_variable in consume_variables.iter() {
                environment.consume_variable(consume_variable);
            }

            arena.alloc(Stmt::Switch {
                cond_symbol: *cond_symbol,
                cond_layout: *cond_layout,
                branches: newer_branches,
                default_branch: newer_default_branch,
                ret_layout: *ret_layout,
            })
        }
        Stmt::Ret(s) => {
            let ownership = environment.consume_variable(s);
            debug_assert!(matches!(ownership, None | Some(Ownership::Owned))); // the return value should be owned or not reference counted at the return.
            return arena.alloc(Stmt::Ret(*s));
        }
        Stmt::Refcounting(_, _) => unreachable!("refcounting should not be in the AST yet"),
        Stmt::Expect {
            condition,
            region,
            lookups,
            variables,
            remainder,
        } => {
            let new_remainder = insert_refcount_operations_stmt(arena, environment, remainder);

            let new_expect = arena.alloc(Stmt::Expect {
                condition: *condition,
                region: *region,
                lookups,
                variables,
                remainder: new_remainder,
            });

            consume_and_insert_inc_stmts(
                arena,
                environment,
                environment.owned_usages(lookups.iter().copied()),
                new_expect,
            )
        }
        Stmt::ExpectFx {
            condition,
            region,
            lookups,
            variables,
            remainder,
        } => {
            let new_remainder = insert_refcount_operations_stmt(arena, environment, remainder);

            let new_expectfx = arena.alloc(Stmt::ExpectFx {
                condition: *condition,
                region: *region,
                lookups,
                variables,
                remainder: new_remainder,
            });

            consume_and_insert_inc_stmts(
                arena,
                environment,
                environment.owned_usages(lookups.iter().copied()),
                new_expectfx,
            )
        }
        Stmt::Dbg {
            symbol,
            variable,
            remainder,
        } => {
            let new_remainder = insert_refcount_operations_stmt(arena, environment, remainder);

            let new_debug = arena.alloc(Stmt::Dbg {
                symbol: *symbol,
                variable: *variable,
                remainder: new_remainder,
            });

            // TODO this assumes the debug statement to consume the variable. I'm not sure if that is (always) the case.
            // But the old inc_dec pass passes variables
            consume_and_insert_inc_stmts(
                arena,
                environment,
                environment.owned_usages(std::iter::once(symbol).copied()),
                new_debug,
            )
        }
        Stmt::Join {
            id: joinpoint_id,
            parameters,
            body,
            remainder,
        } => {
            // Assuming that the values in the closure of the body of this jointpoint are already bound.

            // Assuming that all variables are still owned. (So that we can determine what variables got consumed in the join point.)
            debug_assert!(environment
                .variables_ownership
                .iter()
                .all(|(_, ownership)| matches!(ownership, Ownership::Owned)));

            let mut body_env = environment.clone();

            let parameter_variables = parameters.iter().map(|Param { symbol, .. }| *symbol);
            let parameter_variables_set = parameter_variables.clone().collect::<MutSet<_>>();
            body_env.add_variables(parameter_variables);

            /*
            We use a fixed point iteration to determine what variables are consumed in the join point.
            We need to do this because the join point might be called recursively.
            If we consume variables in the closure, like y in the example below:

            add = \x, y ->
                jp 1 \acc, count ->
                    if count == 0
                    then acc + y
                    else jump 1 (acc+1) (count-1)
                jump 1 0 x

            If we were to just use an empty consumption without iteration,
            the analysis will not know that y is still used in the jump and thus will drop if after the else.
            */

            let mut joinpoint_consumption = MutSet::default();

            let new_body = loop {
                // Copy the env to make sure each iteration has a fresh environment.
                let mut current_body_env = body_env.clone();

                current_body_env
                    .add_joinpoint_consumption(*joinpoint_id, joinpoint_consumption.clone());
                let new_body = insert_refcount_operations_stmt(arena, &mut current_body_env, body);
                current_body_env.remove_joinpoint_consumption(*joinpoint_id);

                // We save the parameters consumed by this join point. So we can do the same when we jump to this joinpoint.
                // This includes parameter variables, this might help with unused closure variables.
                let current_joinpoint_consumption = {
                    let consumed_variables = current_body_env
                        .variables_ownership
                        .iter()
                        .filter_map(|(symbol, ownership)| match ownership {
                            Ownership::Borrowed => Some(*symbol),
                            _ => None,
                        })
                        .collect::<MutSet<_>>();

                    consumed_variables
                        .difference(&parameter_variables_set)
                        .copied()
                        .collect::<MutSet<Symbol>>()
                };

                if joinpoint_consumption == current_joinpoint_consumption {
                    break new_body;
                } else {
                    debug_assert!(
                        current_joinpoint_consumption.is_superset(&joinpoint_consumption),
                        "The current consumption should be a superset of the previous consumption.
                        As the consumption should only ever increase.
                        Otherwise we will be looping forever."
                    );
                    joinpoint_consumption = current_joinpoint_consumption;
                }
            };

            environment.add_joinpoint_consumption(*joinpoint_id, joinpoint_consumption);
            let new_remainder = insert_refcount_operations_stmt(arena, environment, remainder);
            environment.remove_joinpoint_consumption(*joinpoint_id);

            arena.alloc(Stmt::Join {
                id: *joinpoint_id,
                parameters: parameters.clone(),
                body: new_body,
                remainder: new_remainder,
            })
        }
        Stmt::Jump(joinpoint_id, arguments) => {
            let consumed_variables = environment.get_joinpoint_consumption(*joinpoint_id);
            for consumed_variable in consumed_variables.iter() {
                environment.consume_variable(consumed_variable);
            }

            let new_jump = arena.alloc(Stmt::Jump(*joinpoint_id, arguments.clone()));

            // Note that this should only insert increments if a later join point has a current parameter as consumed closure.
            consume_and_insert_inc_stmts(
                arena,
                environment,
                environment.owned_usages(arguments.iter().copied()),
                new_jump,
            )
        }
        Stmt::Crash(symbol, crash_tag) => {
            // We don't have to worry about reference counting *after* the crash.
            // But we do need to make sure the symbol of the crash is live until the crash.
            // So we insert increment statements for the symbol (if it is reference counted)
            let new_crash = arena.alloc(Stmt::Crash(*symbol, *crash_tag));

            consume_and_insert_inc_stmts(
                arena,
                environment,
                environment.owned_usages(std::iter::once(symbol).copied()),
                new_crash,
            )
        }
    }
}

fn insert_refcount_operations_binding<'a>(
    arena: &'a Bump,
    environment: &mut RefcountEnvironment,
    binding: &Symbol,
    expr: &Expr<'a>,
    layout: &InLayout<'a>,
    stmt: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    macro_rules! dec_borrowed {
        ($symbols:expr,$stmt:expr) => {
            // Insert decrement operations for borrowed variables if they are currently owned.
            consume_and_insert_dec_stmts(
                arena,
                environment,
                environment.borrowed_usages($symbols),
                stmt,
            )
        };
    }

    macro_rules! new_let {
        ($stmt:expr) => {
            arena.alloc(Stmt::Let(*binding, expr.clone(), *layout, $stmt))
        };
    }

    macro_rules! inc_owned {
        ($symbols:expr, $stmt:expr) => {
            // Insert increment operations for the owned variables used in the expression.
            consume_and_insert_inc_stmts(
                arena,
                environment,
                environment.owned_usages($symbols),
                $stmt,
            )
        };
    }

    match expr {
        Expr::Literal(_) | Expr::NullPointer | Expr::EmptyArray | Expr::RuntimeErrorFunction(_) => {
            // Literals, empty arrays, and runtime errors are not (and have nothing) reference counted.
            new_let!(stmt)
        }
        Expr::Call(Call {
            arguments,
            call_type,
        }) => match call_type {
            // A by name call refers to a normal function call.
            // Normal functions take all their parameters as owned, so we can mark them all as such.
            CallType::ByName { .. } => {
                let new_let = new_let!(stmt);

                inc_owned!(arguments.iter().copied(), new_let)
            }
            // A foreign function call is responsible for managing the reference counts of its arguments.
            // TODO make sure this is true.
            CallType::Foreign { .. } => {
                let new_let = new_let!(stmt);

                inc_owned!(arguments.iter().copied(), new_let)
            }
            // Doesn't include higher order
            CallType::LowLevel {
                op: operator,
                update_mode: _,
            } => {
                let borrow_signature = lowlevel_borrow_signature(arena, operator);
                let arguments_with_borrow_signature = arguments
                    .iter()
                    .copied()
                    .zip(borrow_signature.iter().copied());
                let (owned_arguments, borrowed_arguments) = arguments_with_borrow_signature
                    .partition::<std::vec::Vec<_>, _>(
                    |(_, ownership)| matches!(ownership, Ownership::Owned),
                );

                let new_stmt = dec_borrowed!(
                    borrowed_arguments.iter().map(|(symbol, _)| symbol).copied(),
                    stmt
                );

                let new_let = new_let!(new_stmt);

                inc_owned!(
                    owned_arguments.iter().map(|(symbol, _)| symbol).copied(),
                    new_let
                )
            }
            CallType::HigherOrder(HigherOrderLowLevel {
                op: operator,

                closure_env_layout: _,

                /// update mode of the higher order lowlevel itself
                    update_mode: _,

                passed_function,
            }) => {
                // Functions always take their arguments as owned.
                // (Except lowlevels, but those are wrapped in functions that take their arguments as owned and perform rc.)

                // This should always be true, not sure where this could be set to false.
                debug_assert!(passed_function.owns_captured_environment);

                // define macro that inserts a decref statement for a variable amount of symbols
                macro_rules! decref_lists {
                    ($stmt:expr, $symbol:expr) => {
                        arena.alloc(Stmt::Refcounting(ModifyRc::DecRef($symbol), $stmt))
                    };

                    ($stmt:expr, $symbol:expr, $($symbols:expr),+) => {{
                        decref_lists!(decref_lists!($stmt, $symbol), $($symbols),+)
                    }};
                }

                match operator {
                    HigherOrder::ListMap { xs } => {
                        if let [_xs_symbol, _function_symbol, closure_symbol] = &arguments {
                            let new_stmt = decref_lists!(stmt, *xs);

                            let new_let = new_let!(new_stmt);

                            inc_owned!([*xs, *closure_symbol].into_iter(), new_let)
                        } else {
                            panic!("ListMap should have 3 arguments");
                        }
                    }
                    HigherOrder::ListMap2 { xs, ys } => {
                        if let [_xs_symbol, _ys_symbol, _function_symbol, closure_symbol] =
                            &arguments
                        {
                            let new_stmt = decref_lists!(stmt, *xs, *ys);

                            let new_let = new_let!(new_stmt);

                            inc_owned!([*xs, *ys, *closure_symbol].into_iter(), new_let)
                        } else {
                            panic!("ListMap2 should have 4 arguments");
                        }
                    }
                    HigherOrder::ListMap3 { xs, ys, zs } => {
                        if let [_xs_symbol, _ys_symbol, _zs_symbol, _function_symbol, closure_symbol] =
                            &arguments
                        {
                            let new_stmt = decref_lists!(stmt, *xs, *ys, *zs);

                            let new_let = new_let!(new_stmt);

                            inc_owned!([*xs, *ys, *zs, *closure_symbol].into_iter(), new_let)
                        } else {
                            panic!("ListMap3 should have 5 arguments");
                        }
                    }
                    HigherOrder::ListMap4 { xs, ys, zs, ws } => {
                        if let [_xs_symbol, _ys_symbol, _zs_symbol, _ws_symbol, _function_symbol, closure_symbol] =
                            &arguments
                        {
                            let new_stmt = decref_lists!(stmt, *xs, *ys, *zs, *ws);

                            let new_let = new_let!(new_stmt);

                            inc_owned!([*xs, *ys, *zs, *ws, *closure_symbol].into_iter(), new_let)
                        } else {
                            panic!("ListMap4 should have 6 arguments");
                        }
                    }
                    HigherOrder::ListSortWith { xs } => {
                        // TODO if non-unique, elements have been consumed, must still consume the list itself
                        if let [_xs_symbol, _function_symbol, closure_symbol] = &arguments {
                            let new_let = new_let!(stmt);

                            inc_owned!([*xs, *closure_symbol].into_iter(), new_let)
                        } else {
                            panic!("ListSortWith should have 3 arguments");
                        }
                    }
                }
            }
        },
        Expr::Tag { arguments, .. } | Expr::Struct(arguments) => {
            let new_let = new_let!(stmt);

            inc_owned!(arguments.iter().copied(), new_let)
        }
        Expr::ExprBox { symbol } => {
            let new_let = new_let!(stmt);

            inc_owned!(iter::once(*symbol), new_let)
        }
        Expr::GetTagId { structure, .. }
        | Expr::StructAtIndex { structure, .. }
        | Expr::UnionAtIndex { structure, .. }
        | Expr::ExprUnbox { symbol: structure } => {
            // All structures are alive at this point and don't have to be copied in order to take an index out/get tag id/copy values to the stack.
            // But we do want to make sure to decrement this item if it is the last reference.

            let new_stmt = dec_borrowed!(iter::once(*structure), stmt);

            // Add an increment operation for the binding if it is reference counted and if the expression creates a new reference to a value.
            let newer_stmt = if matches!(
                environment.get_variable_rc_type(binding),
                VarRcType::ReferenceCounted
            ) {
                match expr {
                    Expr::StructAtIndex { .. }
                    | Expr::UnionAtIndex { .. }
                    | Expr::ExprUnbox { .. } => insert_inc_stmt(arena, *binding, 1, new_stmt),
                    // No usage of an element of a reference counted variable. No need to increment.
                    Expr::GetTagId { .. } => new_stmt,
                    _ => unreachable!("Unexpected expression type"),
                }
            } else {
                // If the variable is not reference counted, we don't need to increment it.
                new_stmt
            };

            new_let!(newer_stmt)
        }
        Expr::Array {
            elem_layout: _,
            elems,
        } => {
            // For an array creation, we insert all the used elements.
            let new_let = new_let!(stmt);

            inc_owned!(
                elems.iter().filter_map(|element| match element {
                    // Literal elements are not reference counted.
                    ListLiteralElement::Literal(_) => None,
                    // Symbol elements might be reference counted.
                    ListLiteralElement::Symbol(symbol) => Some(*symbol),
                }),
                new_let
            )
        }
        Expr::Reuse { .. } | Expr::Reset { .. } | Expr::ResetRef { .. } => {
            unreachable!("Reset(ref) and reuse should not exist at this point")
        }
    }
}

/**
Insert increment statements for the given symbols compensating for the ownership.
*/
fn consume_and_insert_inc_stmts<'a>(
    arena: &'a Bump,
    environment: &mut RefcountEnvironment,
    usage: impl IntoIterator<Item = (Symbol, u64)>,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    usage
        .into_iter()
        .fold(continuation, |continuation, (symbol, usage_count)| {
            consume_and_insert_inc_stmt(arena, environment, &symbol, usage_count, continuation)
        })
}

/**
Insert an increment statement for the given symbol compensating for the ownership.
*/
fn consume_and_insert_inc_stmt<'a>(
    arena: &'a Bump,
    environment: &mut RefcountEnvironment,
    symbol: &Symbol,
    usage_count: u64,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    let new_count = match environment.consume_rc_variable(symbol) {
        // If the variable is borrowed, we need to increment the reference count for each usage.
        Ownership::Borrowed => usage_count,
        // If the variable is owned, we need to increment the reference count for each usage except one.
        Ownership::Owned => usage_count - 1,
    };

    insert_inc_stmt(arena, *symbol, new_count, continuation)
}

/**
Insert a increment statement for the given symbol.
*/
fn insert_inc_stmt<'a, 's>(
    arena: &'a Bump,
    symbol: Symbol,
    count: u64,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    match count {
        0 => continuation,
        positive_count => arena.alloc(Stmt::Refcounting(
            ModifyRc::Inc(symbol, positive_count),
            continuation,
        )),
    }
}

/**
Insert decrement statements for the given symbols if they are owned.
*/
fn consume_and_insert_dec_stmts<'a>(
    arena: &'a Bump,
    environment: &mut RefcountEnvironment,
    symbols: impl IntoIterator<Item = Symbol>,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    symbols
        .into_iter()
        .fold(continuation, |continuation, symbol| {
            consume_and_insert_dec_stmt(arena, environment, &symbol, continuation)
        })
}

/**
Insert a decrement statement for the given symbol if it is owned.
*/
fn consume_and_insert_dec_stmt<'a>(
    arena: &'a Bump,
    environment: &mut RefcountEnvironment,
    symbol: &Symbol,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    match environment.consume_rc_variable(symbol) {
        // If the variable is borrowed, don't have to decrement the reference count.
        Ownership::Borrowed => continuation,
        // If the variable is owned, we do need to decrement the reference count.
        Ownership::Owned => insert_dec_stmt(arena, *symbol, continuation),
    }
}

/**
Insert decrement statements for the given symbols.
*/
fn insert_dec_stmts<'a, 's>(
    arena: &'a Bump,
    symbols: impl Iterator<Item = Symbol>,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    symbols.fold(continuation, |continuation, symbol| {
        insert_dec_stmt(arena, symbol, continuation)
    })
}

/**
Insert a decrement statement for the given symbol.
*/
fn insert_dec_stmt<'a, 's>(
    arena: &'a Bump,
    symbol: Symbol,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    arena.alloc(Stmt::Refcounting(ModifyRc::Dec(symbol), continuation))
}

/**
Taken from the original inc_dec borrow implementation.
*/
fn lowlevel_borrow_signature<'a>(arena: &'a Bump, op: &LowLevel) -> &'a [Ownership] {
    use LowLevel::*;

    let irrelevant = Ownership::Owned;
    let function = irrelevant;
    let closure_data = irrelevant;
    let owned = Ownership::Owned;
    let borrowed = Ownership::Borrowed;

    // Here we define the borrow signature of low-level operations
    //
    // - arguments with non-refcounted layouts (ints, floats) are `irrelevant`
    // - arguments that we may want to update destructively must be Owned
    // - other refcounted arguments are Borrowed
    match op {
        Unreachable => arena.alloc_slice_copy(&[irrelevant]),
        ListLen | StrIsEmpty | StrToScalars | StrCountGraphemes | StrGraphemes
        | StrCountUtf8Bytes | StrGetCapacity | ListGetCapacity => {
            arena.alloc_slice_copy(&[borrowed])
        }
        ListWithCapacity | StrWithCapacity => arena.alloc_slice_copy(&[irrelevant]),
        ListReplaceUnsafe => arena.alloc_slice_copy(&[owned, irrelevant, irrelevant]),
        StrGetUnsafe | ListGetUnsafe => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        ListConcat => arena.alloc_slice_copy(&[owned, owned]),
        StrConcat => arena.alloc_slice_copy(&[owned, borrowed]),
        StrSubstringUnsafe => arena.alloc_slice_copy(&[borrowed, irrelevant, irrelevant]),
        StrReserve => arena.alloc_slice_copy(&[owned, irrelevant]),
        StrAppendScalar => arena.alloc_slice_copy(&[owned, irrelevant]),
        StrGetScalarUnsafe => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        StrTrim => arena.alloc_slice_copy(&[owned]),
        StrTrimLeft => arena.alloc_slice_copy(&[owned]),
        StrTrimRight => arena.alloc_slice_copy(&[owned]),
        StrSplit => arena.alloc_slice_copy(&[borrowed, borrowed]),
        StrToNum => arena.alloc_slice_copy(&[borrowed]),
        ListPrepend => arena.alloc_slice_copy(&[owned, owned]),
        StrJoinWith => arena.alloc_slice_copy(&[borrowed, borrowed]),
        ListMap => arena.alloc_slice_copy(&[owned, function, closure_data]),
        ListMap2 => arena.alloc_slice_copy(&[owned, owned, function, closure_data]),
        ListMap3 => arena.alloc_slice_copy(&[owned, owned, owned, function, closure_data]),
        ListMap4 => arena.alloc_slice_copy(&[owned, owned, owned, owned, function, closure_data]),
        ListSortWith => arena.alloc_slice_copy(&[owned, function, closure_data]),

        ListAppendUnsafe => arena.alloc_slice_copy(&[owned, owned]),
        ListReserve => arena.alloc_slice_copy(&[owned, irrelevant]),
        ListSublist => arena.alloc_slice_copy(&[owned, irrelevant, irrelevant]),
        ListDropAt => arena.alloc_slice_copy(&[owned, irrelevant]),
        ListSwap => arena.alloc_slice_copy(&[owned, irrelevant, irrelevant]),
        ListReleaseExcessCapacity => arena.alloc_slice_copy(&[owned]),
        StrReleaseExcessCapacity => arena.alloc_slice_copy(&[owned]),

        Eq | NotEq => arena.alloc_slice_copy(&[borrowed, borrowed]),

        And | Or | NumAdd | NumAddWrap | NumAddChecked | NumAddSaturated | NumSub | NumSubWrap
        | NumSubChecked | NumSubSaturated | NumMul | NumMulWrap | NumMulSaturated
        | NumMulChecked | NumGt | NumGte | NumLt | NumLte | NumCompare | NumDivFrac
        | NumDivTruncUnchecked | NumDivCeilUnchecked | NumRemUnchecked | NumIsMultipleOf
        | NumPow | NumPowInt | NumBitwiseAnd | NumBitwiseXor | NumBitwiseOr | NumShiftLeftBy
        | NumShiftRightBy | NumShiftRightZfBy => arena.alloc_slice_copy(&[irrelevant, irrelevant]),

        NumToStr
        | NumAbs
        | NumNeg
        | NumSin
        | NumCos
        | NumSqrtUnchecked
        | NumLogUnchecked
        | NumRound
        | NumCeiling
        | NumFloor
        | NumToFrac
        | Not
        | NumIsFinite
        | NumAtan
        | NumAcos
        | NumAsin
        | NumIntCast
        | NumToIntChecked
        | NumToFloatCast
        | NumToFloatChecked
        | NumCountLeadingZeroBits
        | NumCountTrailingZeroBits
        | NumCountOneBits => arena.alloc_slice_copy(&[irrelevant]),
        NumBytesToU16 => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        NumBytesToU32 => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        NumBytesToU64 => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        NumBytesToU128 => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        StrStartsWith | StrEndsWith => arena.alloc_slice_copy(&[borrowed, borrowed]),
        StrStartsWithScalar => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        StrFromUtf8Range => arena.alloc_slice_copy(&[owned, irrelevant, irrelevant]),
        StrToUtf8 => arena.alloc_slice_copy(&[owned]),
        StrRepeat => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        StrFromInt | StrFromFloat => arena.alloc_slice_copy(&[irrelevant]),
        Hash => arena.alloc_slice_copy(&[borrowed, irrelevant]),

        ListIsUnique => arena.alloc_slice_copy(&[borrowed]),

        BoxExpr | UnboxExpr => {
            unreachable!("These lowlevel operations are turned into mono Expr's")
        }

        PtrCast | RefCountInc | RefCountDec => {
            unreachable!("Only inserted *after* borrow checking: {:?}", op);
        }
    }
}
