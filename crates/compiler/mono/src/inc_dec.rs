// This program was written by Jelle Teeuwissen within a final
// thesis project of the Computing Science master program at Utrecht
// University under supervision of Wouter Swierstra (w.s.swierstra@uu.nl).

use std::{collections::HashMap, hash::BuildHasherDefault, iter};

use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::{all::WyHash, MutMap, MutSet};
use roc_module::{low_level::LowLevelWrapperType, symbol::Symbol};

use crate::{
    borrow::{lowlevel_borrow_signature, Ownership},
    ir::{
        BranchInfo, Call, CallType, Expr, HigherOrderLowLevel, JoinPointId, ListLiteralElement,
        ModifyRc, Param, Proc, ProcLayout, Stmt, UpdateModeIds,
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
    update_mode_ids: &'i mut UpdateModeIds,
    procedures: &mut HashMap<(Symbol, ProcLayout), Proc<'a>, BuildHasherDefault<WyHash>>,
) {
    // Create a SymbolRcTypesEnv for the procedures as they get referenced but should be marked as non reference counted.
    let mut symbol_rc_types_env = SymbolRcTypesEnv::from_layout_interner(layout_interner);
    symbol_rc_types_env.insert_proc_symbols(procedures.keys().map(|(symbol, _layout)| *symbol));

    // Later in this file all wrapped calls to lowlevel functions will be inlined,
    // meaning they are no longer needed and the proc can be removed.
    // TODO removing these currently causes errors as these functions as they are expected to be there later on.
    // procedures.retain(|(symbol, _layout), _proc| {
    //     matches!(
    //         LowLevelWrapperType::from_symbol(*symbol),
    //         LowLevelWrapperType::NotALowLevelWrapper
    //     )
    // });

    for (_, proc) in procedures.iter_mut() {
        // Clone the symbol_rc_types_env and insert the symbols in the current procedure.
        // As the symbols should be limited in scope for the current proc.
        let symbol_rc_types_env = symbol_rc_types_env.clone();
        insert_inc_dec_operations_proc(arena, update_mode_ids, symbol_rc_types_env, proc);
    }
}

/**
Enum indicating whether a symbol should be reference counted or not.
This includes layouts that themselves can be stack allocated but that contain a heap allocated item.
*/
#[derive(Clone)]
enum VarRcType {
    ReferenceCounted,
    NotReferenceCounted,
}

type SymbolRcTypes = MutMap<Symbol, VarRcType>;

/**
Environment to keep track which of the symbols should be reference counted and which ones should not.
 */
#[derive(Clone)]
struct SymbolRcTypesEnv<'a, 'i> {
    // A map keeping track of which symbols are reference counted and which are not.
    symbols_rc_type: SymbolRcTypes,

    layout_interner: &'i STLayoutInterner<'a>,
}

impl<'a, 'i> SymbolRcTypesEnv<'a, 'i> {
    /**
    Create a new SymbolRcTypesEnv from a layout interner.
    */
    fn from_layout_interner(layout_interner: &'i STLayoutInterner<'a>) -> SymbolRcTypesEnv<'a, 'i> {
        SymbolRcTypesEnv {
            symbols_rc_type: SymbolRcTypes::default(),
            layout_interner,
        }
    }
    /**
    Insert the reference count type of top level functions.
    As functions are not reference counted, they can be marked as such.
    */
    fn insert_proc_symbols(&mut self, proc_symbols: impl Iterator<Item = Symbol>) {
        for proc_symbol in proc_symbols {
            self.symbols_rc_type
                .insert(proc_symbol, VarRcType::NotReferenceCounted);
        }
    }

    /**
    Insert the reference count types of all symbols in a procedure.
    */
    fn insert_symbols_rc_type_proc(&mut self, proc: &Proc<'a>) {
        // First collect the argument types.
        for (layout, symbol) in proc.args.iter() {
            self.insert_symbol_layout_rc_type(symbol, layout);
        }

        // Then collect the types of the symbols in the body.
        self.insert_symbols_rc_type_stmt(&proc.body);
    }

    /**
    Insert the reference count types of all symbols in a statement.
    */
    fn insert_symbols_rc_type_stmt(&mut self, stmt: &Stmt<'a>) {
        match stmt {
            Stmt::Let(
                binding,
                // Expressions can be omitted, as they won't create new symbols.
                _expr,
                layout,
                continuation,
            ) => {
                self.insert_symbol_layout_rc_type(binding, layout);
                self.insert_symbols_rc_type_stmt(continuation);
            }
            Stmt::Switch {
                // The switch condition is an integer and thus not reference counted.
                cond_symbol: _,
                cond_layout: _,
                branches,
                default_branch,
                ret_layout: _,
            } => {
                // Collect the types of the symbols in all the branches, including the default one.
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

                    self.insert_symbols_rc_type_stmt(stmt);
                }
            }
            Stmt::Ret(_symbol) => {
                // The return does not introduce new symbols.
            }
            Stmt::Refcounting(_, _) => unreachable!(
                "Refcounting operations should not be present in the AST at this point."
            ),
            Stmt::Expect { remainder, .. }
            | Stmt::ExpectFx { remainder, .. }
            | Stmt::Dbg { remainder, .. } => {
                self.insert_symbols_rc_type_stmt(remainder);
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

                self.insert_symbols_rc_type_stmt(body);
                self.insert_symbols_rc_type_stmt(continuation);
            }
            Stmt::Jump(_, _) => {
                // A join point does not introduce new symbols.
            }
            Stmt::Crash(_, _) => {
                // A crash does not introduce new symbols.
            }
        }
    }

    /*
    Insert the reference count type of a symbol given its layout.
    */
    fn insert_symbol_layout_rc_type(&mut self, symbol: &Symbol, layout: &InLayout) {
        // This will reference count the entire struct, even if only one field is reference counted.
        // In another pass we can inline these operations, potentially improving reuse.
        let contains_refcounted = self.layout_interner.contains_refcounted(*layout);
        let rc_type = match contains_refcounted {
            true => VarRcType::ReferenceCounted,
            false => VarRcType::NotReferenceCounted,
        };
        self.symbols_rc_type.insert(*symbol, rc_type);
    }
}

type SymbolsOwnership = MutMap<Symbol, Ownership>;

/**
Type containing data about the symbols consumption of a join point.
*/
type JoinPointConsumption = MutSet<Symbol>;

/**
The environment for the reference counting pass.
Contains the symbols rc types and the ownership.
*/
#[derive(Clone)]
struct RefcountEnvironment<'v> {
    // Keep track which symbols are reference counted and which are not.
    symbols_rc_types: &'v SymbolRcTypes,
    // The Koka implementation assumes everything that is not owned to be borrowed.
    symbols_ownership: SymbolsOwnership,
    jointpoint_closures: MutMap<JoinPointId, JoinPointConsumption>,
}

impl<'v> RefcountEnvironment<'v> {
    /**
    Retrieve the rc type of a symbol.
    */
    fn get_symbol_rc_type(&mut self, symbol: &Symbol) -> &VarRcType {
        self.symbols_rc_types
            .get(symbol)
            .expect("symbol should have rc type")
    }

    /*
    Retrieve whether the symbol is owned or borrowed.
    If it was owned, set it to borrowed (as it is consumed/the symbol can be used as owned only once without incrementing).
    If the symbol is not reference counted, do nothing and return None.
    */
    fn consume_symbol(&mut self, symbol: &Symbol) -> Option<Ownership> {
        if !self.symbols_ownership.contains_key(symbol) {
            return None;
        }

        // Consume the symbol and return the previous ownership.
        Some(self.consume_rc_symbol(symbol))
    }

    /*
    Retrieve whether the symbol is owned or borrowed.
    If it was owned, set it to borrowed (as it is consumed/the symbol can be used as owned only once without incrementing).
    */
    fn consume_rc_symbol(&mut self, symbol: &Symbol) -> Ownership {
        // Consume the symbol by setting it to borrowed (if it was owned before), and return the previous ownership.
        self.symbols_ownership
            .insert(*symbol, Ownership::Borrowed)
            .expect("Expected symbol to be in environment")
    }

    /**
       Retrieve the ownership of a symbol.
       If the symbol is not reference counted, it will None.
    */
    fn get_symbol_ownership(&self, symbol: &Symbol) -> Option<&Ownership> {
        self.symbols_ownership.get(symbol)
    }

    /**
    Remove non reference counted symbols from an iterator.
    Useful when we want to insert reference count operations for symbols that might not be rc.
    */
    fn filter_rc_symbols<'a>(
        &self,
        symbols: impl Iterator<Item = &'a Symbol>,
    ) -> std::vec::Vec<&'a Symbol> {
        symbols
            .filter(|symbol| self.symbols_ownership.contains_key(symbol))
            .collect::<std::vec::Vec<_>>()
    }

    /**
    Add a symbols to the environment if they are reference counted.
    */
    fn add_symbols(&mut self, symbols: impl IntoIterator<Item = Symbol>) {
        symbols
            .into_iter()
            .for_each(|symbol| self.add_symbol(symbol))
    }

    /**
    Add a symbol to the environment if it is reference counted.
    */
    fn add_symbol(&mut self, symbol: Symbol) {
        match self.get_symbol_rc_type(&symbol) {
            VarRcType::ReferenceCounted => {
                self.symbols_ownership.insert(symbol, Ownership::Owned);
            }
            VarRcType::NotReferenceCounted => {
                // If this symbol is not reference counted, we don't need to do anything.
            }
        }
    }

    /**
    Remove a symbol from the environment.
    Is used when a symbol is no longer in scope (before a let binding).
     */
    fn remove_symbol(&mut self, symbol: Symbol) {
        self.symbols_ownership.remove(&symbol);
    }

    /**
    Add a joinpoint id and the consumed closure to the environment.
    Used when analyzing a join point. So that a jump can update the environment on call.
    */
    fn add_joinpoint_consumption(
        &mut self,
        joinpoint_id: JoinPointId,
        consumption: JoinPointConsumption,
    ) {
        self.jointpoint_closures.insert(joinpoint_id, consumption);
    }

    /**
    Get the consumed closure from a join point id.
    */
    fn get_joinpoint_consumption(&self, joinpoint_id: JoinPointId) -> JoinPointConsumption {
        self.jointpoint_closures
            .get(&joinpoint_id)
            .expect("Expected closure to be in environment")
            .clone()
    }

    /**
    Remove a joinpoint id and the consumed closure from the environment.
    Used after analyzing the continuation of a join point.
    */
    fn remove_joinpoint_consumption(&mut self, joinpoint_id: JoinPointId) {
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
    fn owned_usages(&self, symbols: impl IntoIterator<Item = Symbol>) -> MutMap<Symbol, u64> {
        // A groupby or something similar would be nice here.
        let mut symbol_usage = MutMap::default();
        for symbol in symbols {
            match {
                self.symbols_rc_types
                    .get(&symbol)
                    .expect("Expected symbol to be in the map")
            } {
                // If the symbol is reference counted, we need to increment the usage count.
                VarRcType::ReferenceCounted => {
                    *symbol_usage.entry(symbol).or_default() += 1;
                }
                // If the symbol is not reference counted, we don't need to do anything.
                VarRcType::NotReferenceCounted => continue,
            }
        }
        symbol_usage
    }

    /**
    Filter the given symbols to only contain reference counted symbols.
    */
    fn borrowed_usages(&self, symbols: impl IntoIterator<Item = Symbol>) -> MutSet<Symbol> {
        symbols
            .into_iter()
            .filter(|symbol| {
                // If the symbol is reference counted, we need to increment the usage count.
                // If the symbol is not reference counted, we don't need to do anything.
                matches!(
                    self.symbols_rc_types
                        .get(symbol)
                        .expect("Expected symbol to be in the map"),
                    VarRcType::ReferenceCounted
                )
            })
            .collect()
    }
}

/**
 Insert the reference counting operations into a statement.
*/
fn insert_inc_dec_operations_proc<'a, 'i>(
    arena: &'a Bump,
    update_mode_ids: &'i mut UpdateModeIds,

    mut symbol_rc_types_env: SymbolRcTypesEnv<'a, 'i>,
    proc: &mut Proc<'a>,
) {
    // Clone the symbol_rc_types_env and insert the symbols in the current procedure.
    // As the symbols should be limited in scope for the current proc.
    symbol_rc_types_env.insert_symbols_rc_type_proc(proc);

    let mut environment = RefcountEnvironment {
        symbols_rc_types: &symbol_rc_types_env.symbols_rc_type,
        symbols_ownership: MutMap::default(),
        jointpoint_closures: MutMap::default(),
    };

    // Add all arguments to the environment (if they are reference counted)
    let proc_symbols = proc.args.iter().map(|(_layout, symbol)| symbol);
    for symbol in proc_symbols.clone() {
        environment.add_symbol(*symbol);
    }

    // Update the body with reference count statements.
    let new_body =
        insert_refcount_operations_stmt(arena, update_mode_ids, &mut environment, &proc.body);

    // Insert decrement statements for unused parameters (which are still marked as owned).
    let rc_proc_symbols = environment.filter_rc_symbols(proc_symbols);
    let newer_body = consume_and_insert_dec_stmts(
        arena,
        &mut environment,
        rc_proc_symbols.iter().copied().copied(),
        new_body,
    );

    // Assert that just the arguments are in the environment. And (after decrementing the unused ones) that they are all borrowed.
    debug_assert!(environment
        .symbols_ownership
        .iter()
        .all(|(symbol, ownership)| {
            // All symbols should be borrowed.
            ownership.is_owned()
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
Assuming that a symbol can only be defined once (no binding to the same symbol multiple times).
*/
fn insert_refcount_operations_stmt<'v, 'a, 'i>(
    arena: &'a Bump,
    update_mode_ids: &'i mut UpdateModeIds,
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
                environment.add_symbol(**binding); // Add the bound symbol to the environment. As it can be used in the continuation.
            }

            triples
                .into_iter()
                .rev()
                // First evaluate the continuation and let it consume it's free symbols.
                .fold(
                    insert_refcount_operations_stmt(
                        arena,
                        update_mode_ids,
                        environment,
                        current_stmt,
                    ),
                    |new_stmt, (binding, expr, layout)| {
                        // If the binding is still owned in the environment, it is not used in the continuation and we can drop it right away.
                        let new_stmt_without_unused = match environment
                            .get_symbol_ownership(binding)
                        {
                            Some(Ownership::Owned) => insert_dec_stmt(arena, *binding, new_stmt),
                            _ => new_stmt,
                        };

                        // And as the symbol should not be in scope before this let binding, remove it from the environment.
                        environment.remove_symbol(*binding);

                        insert_refcount_operations_binding(
                            arena,
                            update_mode_ids,
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

                    let new_branch = insert_refcount_operations_stmt(
                        arena,
                        update_mode_ids,
                        &mut branch_env,
                        branch,
                    );

                    (*label, info.clone(), new_branch, branch_env)
                })
                .collect::<std::vec::Vec<_>>();

            let new_default_branch = {
                let (info, branch) = default_branch;

                let mut branch_env = environment.clone();
                let new_branch = insert_refcount_operations_stmt(
                    arena,
                    update_mode_ids,
                    &mut branch_env,
                    branch,
                );

                (info.clone(), new_branch, branch_env)
            };

            // Determine what symbols are consumed in some of the branches.
            // So we can make sure they are consumed in the current environment and all branches.
            let consume_symbols = {
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
                    let mut consume_symbols = MutSet::default();

                    // If the symbol is currently borrowed, it must be borrowed in all branches and we don't have to do anything.
                    for (symbol, _) in environment
                        .symbols_ownership
                        .iter()
                        .filter(|(_, o)| o.is_owned())
                    {
                        let error = "All symbols defined in the current environment should be in the environment of the branches.";
                        let consumed =
                            branch_envs
                                .iter()
                                .any(|branch_env: &&RefcountEnvironment<'v>| {
                                    matches!(
                                        branch_env.get_symbol_ownership(symbol).expect(error),
                                        Ownership::Borrowed
                                    )
                                });
                        if consumed {
                            // If the symbol is currently owned, and not in a some branches, it must be consumed in all branches
                            consume_symbols.insert(*symbol);
                        }
                        // Otherwise it can stay owned.
                    }

                    consume_symbols
                }
            };

            // Given the consume_symbols we can determine what additional symbols should be dropped in each branch.
            let newer_branches = Vec::from_iter_in(
                new_branches
                    .into_iter()
                    .map(|(label, info, branch, branch_env)| {
                        // If the symbol is owned in the branch, it is not used in the branch and we can drop it.
                        let consume_symbols_branch = consume_symbols.iter().copied().filter(|consume_symbol| {
                             matches!(branch_env.get_symbol_ownership(consume_symbol).expect("All symbols defined in the current environment should be in the environment of the branches."), Ownership::Owned) 
                        });

                        let newer_branch = insert_dec_stmts(arena,  consume_symbols_branch, branch);
                        (label, info, newer_branch.clone())
                    }),
                arena,
            )
            .into_bump_slice();

            let newer_default_branch = {
                let (info, branch, branch_env) = new_default_branch;
                // If the symbol is owned in the branch, it is not used in the branch and we can drop it.
                let consume_symbols_branch = consume_symbols.iter().copied().filter(|consume_symbol| {
                    matches!(branch_env.get_symbol_ownership(consume_symbol).expect("All symbols defined in the current environment should be in the environment of the branches."), Ownership::Owned) 
                 });

                let newer_branch = insert_dec_stmts(arena, consume_symbols_branch, branch);

                (info, newer_branch)
            };

            // In addition to updating the branches, we need to update the current environment.
            for consume_symbol in consume_symbols.iter() {
                environment.consume_symbol(consume_symbol);
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
            let ownership = environment.consume_symbol(s);
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
            let new_remainder =
                insert_refcount_operations_stmt(arena, update_mode_ids, environment, remainder);

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
            let new_remainder =
                insert_refcount_operations_stmt(arena, update_mode_ids, environment, remainder);

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
            let new_remainder =
                insert_refcount_operations_stmt(arena, update_mode_ids, environment, remainder);

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

            // Assuming that all symbols are still owned. (So that we can determine what symbols got consumed in the join point.)
            debug_assert!(environment
                .symbols_ownership
                .iter()
                .all(|(_, ownership)| ownership.is_owned()));

            let mut body_env = environment.clone();

            let parameter_symbols = parameters.iter().map(|Param { symbol, .. }| *symbol);
            let parameter_symbols_set = parameter_symbols.clone().collect::<MutSet<_>>();
            body_env.add_symbols(parameter_symbols);

            /*
            We use a fixed point iteration to determine what symbols are consumed in the join point.
            We need to do this because the join point might be called recursively.
            If we consume symbols in the closure, like y in the example below:

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
                let new_body = insert_refcount_operations_stmt(
                    arena,
                    update_mode_ids,
                    &mut current_body_env,
                    body,
                );
                current_body_env.remove_joinpoint_consumption(*joinpoint_id);

                // We save the parameters consumed by this join point. So we can do the same when we jump to this joinpoint.
                // This includes parameter symbols, this might help with unused closure symbols.
                let current_joinpoint_consumption = {
                    let consumed_symbols = current_body_env
                        .symbols_ownership
                        .iter()
                        .filter_map(|(symbol, ownership)| ownership.is_owned().then_some(*symbol))
                        .collect::<MutSet<_>>();

                    consumed_symbols
                        .difference(&parameter_symbols_set)
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
            let new_remainder =
                insert_refcount_operations_stmt(arena, update_mode_ids, environment, remainder);
            environment.remove_joinpoint_consumption(*joinpoint_id);

            arena.alloc(Stmt::Join {
                id: *joinpoint_id,
                parameters,
                body: new_body,
                remainder: new_remainder,
            })
        }
        Stmt::Jump(joinpoint_id, arguments) => {
            let consumed_symbols = environment.get_joinpoint_consumption(*joinpoint_id);
            for consumed_symbol in consumed_symbols.iter() {
                environment.consume_symbol(consumed_symbol);
            }

            let new_jump = arena.alloc(Stmt::Jump(*joinpoint_id, arguments));

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

fn insert_refcount_operations_binding<'a, 'i>(
    arena: &'a Bump,
    update_mode_ids: &'i mut UpdateModeIds,
    environment: &mut RefcountEnvironment,
    binding: &Symbol,
    expr: &Expr<'a>,
    layout: &InLayout<'a>,
    stmt: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    macro_rules! dec_borrowed {
        ($symbols:expr,$stmt:expr) => {
            // Insert decrement operations for borrowed symbols if they are currently owned.
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
            // Insert increment operations for the owned symbols used in the expression.
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
            CallType::ByName { name, .. } => {
                // Lowlevels are wrapped in another function in order to add type signatures which help with inference.
                // But the reference counting algorithm inserts reference counting operations in the wrapper function.
                // But in a later stage, calls to the wrapper function were replaced by calls to the lowlevel function.
                // Effectively removing the inserted reference counting operations.
                // Thus to prevent that, we inline the operations here already.
                if let LowLevelWrapperType::CanBeReplacedBy(op) =
                    LowLevelWrapperType::from_symbol(name.name())
                {
                    // The function called is a wrapper function.
                    // We can replace the wrapper function with the lowlevel function.
                    let new_lowlevel = Expr::Call(Call {
                        arguments,
                        call_type: CallType::LowLevel {
                            op,
                            update_mode: update_mode_ids.next_id(),
                        },
                    });

                    insert_refcount_operations_binding(
                        arena,
                        update_mode_ids,
                        environment,
                        binding,
                        &new_lowlevel,
                        layout,
                        stmt,
                    )
                } else {
                    let new_let = new_let!(stmt);

                    inc_owned!(arguments.iter().copied(), new_let)
                }
            }
            CallType::Foreign { .. } => {
                // Foreign functions should be responsible for their own memory management.
                // But previously they were assumed to be called with borrowed parameters, so we do the same now.
                let new_stmt = dec_borrowed!(arguments.iter().copied(), stmt);

                new_let!(new_stmt)
            }
            // Doesn't include higher order
            CallType::LowLevel {
                op: operator,
                update_mode: _,
            } => {
                let borrow_signature = lowlevel_borrow_signature(arena, *operator);
                let arguments_with_borrow_signature = arguments
                    .iter()
                    .copied()
                    .zip(borrow_signature.iter().copied());
                let owned_arguments = arguments_with_borrow_signature
                    .clone()
                    .filter_map(|(symbol, ownership)| ownership.is_owned().then_some(symbol));
                let borrowed_arguments = arguments_with_borrow_signature
                    .filter_map(|(symbol, ownership)| ownership.is_borrowed().then_some(symbol));

                let new_stmt = dec_borrowed!(borrowed_arguments, stmt);

                let new_let = new_let!(new_stmt);

                inc_owned!(owned_arguments, new_let)
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

                // define macro that inserts a decref statement for a symbol amount of symbols
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
                            let new_stmt = dec_borrowed!(iter::once(*closure_symbol), stmt);
                            let new_stmt = decref_lists!(new_stmt, *xs);

                            let new_let = new_let!(new_stmt);

                            inc_owned!([*xs].into_iter(), new_let)
                        } else {
                            panic!("ListMap should have 3 arguments");
                        }
                    }
                    HigherOrder::ListMap2 { xs, ys } => {
                        if let [_xs_symbol, _ys_symbol, _function_symbol, closure_symbol] =
                            &arguments
                        {
                            let new_stmt = dec_borrowed!(iter::once(*closure_symbol), stmt);
                            let new_stmt = decref_lists!(new_stmt, *xs, *ys);

                            let new_let = new_let!(new_stmt);

                            inc_owned!([*xs, *ys].into_iter(), new_let)
                        } else {
                            panic!("ListMap2 should have 4 arguments");
                        }
                    }
                    HigherOrder::ListMap3 { xs, ys, zs } => {
                        if let [_xs_symbol, _ys_symbol, _zs_symbol, _function_symbol, closure_symbol] =
                            &arguments
                        {
                            let new_stmt = dec_borrowed!(iter::once(*closure_symbol), stmt);
                            let new_stmt = decref_lists!(new_stmt, *xs, *ys, *zs);

                            let new_let = new_let!(new_stmt);

                            inc_owned!([*xs, *ys, *zs].into_iter(), new_let)
                        } else {
                            panic!("ListMap3 should have 5 arguments");
                        }
                    }
                    HigherOrder::ListMap4 { xs, ys, zs, ws } => {
                        if let [_xs_symbol, _ys_symbol, _zs_symbol, _ws_symbol, _function_symbol, closure_symbol] =
                            &arguments
                        {
                            let new_stmt = dec_borrowed!(iter::once(*closure_symbol), stmt);
                            let new_stmt = decref_lists!(new_stmt, *xs, *ys, *zs, *ws);

                            let new_let = new_let!(new_stmt);

                            inc_owned!([*xs, *ys, *zs, *ws].into_iter(), new_let)
                        } else {
                            panic!("ListMap4 should have 6 arguments");
                        }
                    }
                    HigherOrder::ListSortWith { xs } => {
                        // TODO if non-unique, elements have been consumed, must still consume the list itself
                        if let [_xs_symbol, _function_symbol, closure_symbol] = &arguments {
                            let new_stmt = dec_borrowed!(iter::once(*closure_symbol), stmt);
                            let new_let = new_let!(new_stmt);

                            inc_owned!([*xs].into_iter(), new_let)
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
                environment.get_symbol_rc_type(binding),
                VarRcType::ReferenceCounted
            ) {
                match expr {
                    Expr::StructAtIndex { .. }
                    | Expr::UnionAtIndex { .. }
                    | Expr::ExprUnbox { .. } => insert_inc_stmt(arena, *binding, 1, new_stmt),
                    // No usage of an element of a reference counted symbol. No need to increment.
                    Expr::GetTagId { .. } => new_stmt,
                    _ => unreachable!("Unexpected expression type"),
                }
            } else {
                // If the symbol is not reference counted, we don't need to increment it.
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
    let new_count = match environment.consume_rc_symbol(symbol) {
        // If the symbol is borrowed, we need to increment the reference count for each usage.
        Ownership::Borrowed => usage_count,
        // If the symbol is owned, we need to increment the reference count for each usage except one.
        Ownership::Owned => usage_count - 1,
    };

    insert_inc_stmt(arena, *symbol, new_count, continuation)
}

/**
Insert a increment statement for the given symbol.
*/
fn insert_inc_stmt<'a>(
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
    match environment.consume_rc_symbol(symbol) {
        // If the symbol is borrowed, don't have to decrement the reference count.
        Ownership::Borrowed => continuation,
        // If the symbol is owned, we do need to decrement the reference count.
        Ownership::Owned => insert_dec_stmt(arena, *symbol, continuation),
    }
}

/**
Insert decrement statements for the given symbols.
*/
fn insert_dec_stmts<'a>(
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
fn insert_dec_stmt<'a>(
    arena: &'a Bump,
    symbol: Symbol,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    arena.alloc(Stmt::Refcounting(ModifyRc::Dec(symbol), continuation))
}
