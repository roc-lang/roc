use std::{collections::HashMap, hash::BuildHasherDefault, iter};

use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::{all::WyHash, MutMap, MutSet};
use roc_module::symbol::{IdentIds, ModuleId, Symbol};

use crate::{
    borrow::Ownership,
    ir::{
        BranchInfo, Call, Expr, JoinPointId, ListLiteralElement, ModifyRc, Param, Proc, ProcLayout,
        Stmt, UpdateModeIds,
    },
    layout::{InLayout, LayoutInterner, STLayoutInterner},
};

/**
Insert the reference count operations for procedures.
*/
pub fn insert_refcount_operations<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i STLayoutInterner<'a>,
    home: ModuleId,
    ident_ids: &mut IdentIds,
    update_mode_ids: &mut UpdateModeIds,
    procedures: &mut HashMap<(Symbol, ProcLayout), Proc<'a>, BuildHasherDefault<WyHash>>,
) -> () {
    // Create a VariableRcTypesEnv for the procedures as they get referenced but should be marked as non reference counted.
    let mut variable_rc_types_env = VariableRcTypesEnv::from_layout_interner(layout_interner);
    variable_rc_types_env.insert_proc_symbols(procedures.keys().map(|(symbol, _layout)| *symbol));

    for (_, proc) in procedures.iter_mut() {
        // Clone the variable_rc_types_env and insert the variables in the current procedure.
        // As the variables should be limited in scope for the current proc.
        let variable_rc_types_env = variable_rc_types_env.clone();

        insert_refcount_operations_proc(arena, variable_rc_types_env, proc);
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

// TODO what would be a good way to structure a similar pattern? creating env, evaluating multiple different objects and returning an element from the env.
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
            Stmt::Expect {
                condition,
                region,
                lookups,
                variables,
                remainder,
            } => {
                //TODO deal with other variables in the expect.
                self.insert_variables_rc_type_stmt(remainder);
            }
            Stmt::ExpectFx {
                condition,
                region,
                lookups,
                variables,
                remainder,
            } => {
                //TODO deal with other variables in the expect.
                self.insert_variables_rc_type_stmt(remainder);
            }
            Stmt::Dbg {
                symbol,
                variable,
                remainder,
            } => {
                //TODO deal with other variables in the expect.
                self.insert_variables_rc_type_stmt(remainder);
            }
            Stmt::Join {
                id: _,
                parameters,
                body,
                remainder: continuation,
            } => {
                // TODO do we need to do anything with the parameter ownership parameter?
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
        // TODO add more checks like *persistent*, consume, and reset.
        // TODO this currently marks anything that contains a reference counted type as reference counted.
        // This will reference count the entire struct, even if only one field is reference counted.
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
A map keeping track of how many times a variable is used as owned.
*/
type OwnedUsage = MutMap<Symbol, u64>;

/**
Structure to keep track of the borrowed variable usage of an expression.
For now a single symbol as struct/union indexing always happens on just a single item.
*/
type BorrowedUsage = Symbol;

/**
Enum to keep track of the variable usage of an expression.
*/
enum VariableUsage {
    None,
    Owned(OwnedUsage),
    Borrowed(BorrowedUsage),
}

impl VariableUsage {
    /**
    Retrieve how much a variable is used in an expression.
    */
    fn get_reference_counted_variable_usage_expr<'a>(
        variable_rc_types: &VariableRcTypes,
        expr: &Expr<'a>,
    ) -> VariableUsage {
        match expr {
            Expr::Literal(_) => {
                // Literals are not reference counted.
                VariableUsage::None
            }

            Expr::Call(Call { arguments, .. })
            | Expr::Tag { arguments, .. }
            | Expr::Struct(arguments) => {
                Self::owned_usages(variable_rc_types, arguments.iter().copied())
            }

            Expr::GetTagId { structure, .. }
            | Expr::StructAtIndex { structure, .. }
            | Expr::UnionAtIndex { structure, .. } => {
                // All structures are alive at this point and don't have to be copied in order to take an index out.
                // But we do want to make sure to decrement this item if it is the last reference.
                VariableUsage::Borrowed(*structure)
            }
            Expr::Array {
                elem_layout: _,
                elems,
            } => {
                // For an array creation, we insert all the used elements.
                Self::owned_usages(
                    variable_rc_types,
                    elems.iter().filter_map(|element| match element {
                        // Literal elements are not reference counted.
                        ListLiteralElement::Literal(_) => None,
                        // Symbol elements are reference counted.
                        ListLiteralElement::Symbol(symbol) => Some(*symbol),
                    }),
                )
            }
            Expr::EmptyArray => {
                // Empty arrays have no reference counted elements.
                VariableUsage::None
            }
            Expr::ExprBox { symbol } | Expr::ExprUnbox { symbol } => {
                Self::owned_usages(variable_rc_types, iter::once(*symbol))
            }
            Expr::Reuse { .. } | Expr::Reset { .. } => {
                unreachable!("Reset and reuse should not exist at this point")
            }
            Expr::RuntimeErrorFunction(_) => todo!(),
        }
    }

    /**
    Return owned usages.
    Collect the usage of all the reference counted symbols in the iterator and return as a map.
    */
    fn owned_usages(
        variable_rc_types: &VariableRcTypes,
        symbols: impl Iterator<Item = Symbol>,
    ) -> VariableUsage {
        // A groupby or something similar would be nice here.
        let mut variable_usage = OwnedUsage::default();
        symbols.for_each(|symbol| {
            match {
                variable_rc_types
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
                VarRcType::NotReferenceCounted => return,
            }
        });
        VariableUsage::Owned(variable_usage)
    }
}

/**
Enum to indicate whether or not a join point consumed a parameter.
*/
enum Consumption {
    Consumed,
    Unconsumed,
}

/**
Struct containing data about the variable consumption of a join point.
Contains data about consumed closure values and the consumption of the parameters.
*/
#[derive(Clone)]
struct JoinPointConsumption<'a> {
    closure: MutSet<Symbol>,
    parameters: &'a [Consumption],
}

/**
The environment for the reference counting pass.
Contains the variable rc types and the ownership.
*/
struct Environment<'v, 'a> {
    // Keep track which variables are reference counted and which are not.
    variables_rc_types: &'v VariableRcTypes,
    // The Koka implementation assumes everything that is not owned to be borrowed.
    variables_ownership: VariablesOwnership,
    jointpoint_closures: MutMap<JoinPointId, JoinPointConsumption<'a>>,
}

impl<'v, 'a> Clone for Environment<'v, 'a> {
    fn clone(&self) -> Self {
        Environment {
            variables_rc_types: self.variables_rc_types,
            variables_ownership: self.variables_ownership.clone(),
            jointpoint_closures: self.jointpoint_closures.clone(),
        }
    }
}

impl<'v, 'a> Environment<'v, 'a> {
    /**
    Retrieve the rc type of a variable.
    */
    fn get_variable_rc_type(self: &mut Environment<'v, 'a>, variable: &Symbol) -> &VarRcType {
        self.variables_rc_types
            .get(variable)
            .expect("variable should have rc type")
    }

    /*
    Retrieve whether the variable is owned or borrowed.
    If it was owned, set it to borrowed (as it is consumed/the variable can be used as owned only once without incrementing).
    If the variable is not reference counted, do nothing and return None.
    */
    fn consume_variable(self: &mut Environment<'v, 'a>, variable: &Symbol) -> Option<Ownership> {
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
    fn consume_rc_variable(self: &mut Environment<'v, 'a>, variable: &Symbol) -> Ownership {
        // Consume the variable by setting it to borrowed (if it was owned before), and return the previous ownership.
        self.variables_ownership
            .insert(*variable, Ownership::Borrowed)
            .expect("Expected variable to be in environment")
    }

    /**
       Retrieve the ownership of a variable.
       If the variable is not reference counted, it will None.
    */
    fn get_variable_ownership(self: &Environment<'v, 'a>, variable: &Symbol) -> Option<&Ownership> {
        self.variables_ownership.get(variable)
    }

    /**
    Add a variables to the environment if they are reference counted.
    */
    fn add_variables(self: &mut Environment<'v, 'a>, variables: impl Iterator<Item = Symbol>) {
        variables.for_each(|variable| self.add_variable(variable))
    }

    /**
    Add a variable to the environment if it is reference counted.
    */
    fn add_variable(self: &mut Environment<'v, 'a>, variable: Symbol) {
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
    Remove variables from the environment.
    Is used when a variable is no longer in scope (after checking a join point).
     */
    fn remove_variables(self: &mut Environment<'v, 'a>, variables: impl Iterator<Item = Symbol>) {
        variables.for_each(|variable| self.remove_variable(variable))
    }
    /**
    Remove a variable from the environment.
    Is used when a variable is no longer in scope (before a let binding).
     */
    fn remove_variable(self: &mut Environment<'v, 'a>, variable: Symbol) {
        self.variables_ownership.remove(&variable);
    }

    /**
    Add a joinpoint id and the consumed closure to the environment.
    Used when analyzing a join point. So that a jump can update the environment on call.
    */
    fn add_joinpoint_consumption(
        self: &mut Environment<'v, 'a>,
        joinpoint_id: JoinPointId,
        consumption: JoinPointConsumption<'a>,
    ) {
        self.jointpoint_closures.insert(joinpoint_id, consumption);
    }

    /**
    Get the consumed closure from a join point id.
    */
    fn get_joinpoint_consume(
        self: &Environment<'v, 'a>,
        joinpoint_id: JoinPointId,
    ) -> JoinPointConsumption<'a> {
        self.jointpoint_closures
            .get(&joinpoint_id)
            .expect("Expected closure to be in environment")
            .clone()
    }

    /**
    Remove a joinpoint id and the consumed closure from the environment.
    Used after analyzing the continuation of a join point.
    */
    fn remove_joinpoint_consumption(self: &mut Environment<'v, 'a>, joinpoint_id: JoinPointId) {
        let closure = self.jointpoint_closures.remove(&joinpoint_id);
        debug_assert!(
            matches!(closure, Some(_)),
            "Expected closure to be in environment"
        );
    }
}

/**
 Insert the reference counting operations into a statement.
*/
fn insert_refcount_operations_proc<'a, 'i>(
    arena: &'a Bump,
    mut variable_rc_types_env: VariableRcTypesEnv<'a, 'i>,
    proc: &mut Proc<'a>,
) {
    // Clone the variable_rc_types_env and insert the variables in the current procedure.
    // As the variables should be limited in scope for the current proc.
    variable_rc_types_env.insert_variables_rc_type_proc(&proc);

    let mut environment = Environment {
        variables_rc_types: &variable_rc_types_env.variables_rc_type,
        variables_ownership: MutMap::default(),
        jointpoint_closures: MutMap::default(),
    };

    // Add all arguments to the environment (if they are reference counted)
    let mut proc_symbols = proc.args.iter().map(|(_layout, symbol)| symbol);
    for symbol in proc_symbols.by_ref() {
        environment.add_variable(*symbol);
    }

    // Update the body with reference count statements.
    let new_body = insert_refcount_operations_stmt(arena, &mut environment, &proc.body);

    // Insert decrement statements for unused parameters (which are still marked as owned).
    let newer_body = consume_and_insert_dec_stmts(arena, &mut environment, proc_symbols, new_body);

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
Assuming that a symbol can only be defined once (no binding to the same variable mutliple times).
*/
fn insert_refcount_operations_stmt<'v, 'a>(
    arena: &'a Bump,
    environment: &mut Environment<'v, 'a>,
    stmt: &Stmt<'a>,
) -> &'a Stmt<'a> {
    // TODO: Deal with potentially stack overflowing let chains with an explicit loop.
    // Koka has a list for let bindings.

    match &stmt {
        // The expression borrows the values owned (used) by the continuation.
        Stmt::Let(binding, expr, layout, stmt) => {
            // INFO The Koka implementation (instead of calculating the owned environment beforehand)
            // First evaluates the continuation with the owned environment, setting the variables to dead (not alive).
            // And in the rest of the code, the dead variables are treated as borrowed (because not alive).

            // First evaluate the continuation and let it consume it's free variables.
            environment.add_variable(*binding); // Add the bound variable to the environment. As it can be used in the continuation.
            let mut new_stmt = insert_refcount_operations_stmt(arena, environment, stmt);

            // If the binding is still owned in the environment, it is not used in the continuation and we can drop it right away.
            if matches!(
                environment.get_variable_ownership(binding),
                Some(Ownership::Owned)
            ) {
                new_stmt = insert_dec_stmt(arena, *binding, new_stmt);
            }

            // And as the variable should not be in scope before this let binding, remove it from the environment.
            environment.remove_variable(*binding);

            // Then evaluate the bound expression. where the free variables from the continuation are borrowed.
            let variable_usage = VariableUsage::get_reference_counted_variable_usage_expr(
                &environment.variables_rc_types,
                expr,
            );

            match variable_usage {
                VariableUsage::None => {
                    arena.alloc(Stmt::Let(*binding, expr.clone(), *layout, new_stmt))
                }
                VariableUsage::Owned(owned_usage) => {
                    let new_let = arena.alloc(Stmt::Let(*binding, expr.clone(), *layout, new_stmt));

                    // Insert the reference count operations for the owned variables used in the expression.
                    consume_and_insert_inc_stmts(arena, environment, &owned_usage, new_let)
                }
                VariableUsage::Borrowed(symbol) => {
                    let newer_stmt =
                        consume_and_insert_dec_stmt(arena, environment, &symbol, new_stmt);

                    arena.alloc(Stmt::Let(*binding, expr.clone(), *layout, newer_stmt))
                }
            }
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
        } => todo!(),
        Stmt::ExpectFx {
            condition,
            region,
            lookups,
            variables,
            remainder,
        } => todo!(),
        Stmt::Dbg {
            symbol,
            variable,
            remainder,
        } => todo!(),
        Stmt::Join {
            id,
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

            let mut join_point_env = environment.clone();

            let mut parameter_variables = parameters.iter().map(|Param { symbol, .. }| *symbol);

            join_point_env.add_variables(parameter_variables.by_ref());

            let new_body = insert_refcount_operations_stmt(arena, &mut join_point_env, body);

            // We save the parameters consumed by this join point. So we can do the same when we jump to this joinpoint.
            // This includes parameter variables, this might help with unused closure variables.
            let consumed_variables = {
                let consumed_variables = join_point_env
                    .variables_ownership
                    .iter()
                    .filter_map(|(symbol, ownership)| match ownership {
                        Ownership::Borrowed => Some(*symbol),
                        _ => None,
                    })
                    .collect::<MutSet<_>>();

                let consumed_closure = consumed_variables
                    .difference(&parameter_variables.by_ref().collect::<MutSet<_>>())
                    .copied()
                    .collect::<MutSet<Symbol>>();

                let consumed_parameters = Vec::from_iter_in(
                    parameter_variables.map(|parameter| {
                        if consumed_closure.contains(&parameter) {
                            Consumption::Consumed
                        } else {
                            Consumption::Unconsumed
                        }
                    }),
                    arena,
                )
                .into_bump_slice();

                JoinPointConsumption {
                    closure: consumed_closure,
                    parameters: consumed_parameters,
                }
            };

            environment.add_joinpoint_consumption(*id, consumed_variables);
            let new_remainder = insert_refcount_operations_stmt(arena, environment, remainder);
            environment.remove_joinpoint_consumption(*id);

            arena.alloc(Stmt::Join {
                id: *id,
                parameters: parameters.clone(),
                body: new_body,
                remainder: new_remainder,
            })
        }
        Stmt::Jump(join_point_id, arguments) => {
            let JoinPointConsumption {
                closure: consumed_variables,
                parameters: consumed_parameters,
            } = environment.get_joinpoint_consume(*join_point_id);

            // the consumed variables contain the arguments, so we don't need ot
            for consumed_variable in consumed_variables.iter() {
                environment.consume_variable(consumed_variable);
            }

            // consume all arguments that are consumed by the join point.
            for (argument, _) in arguments
                .iter()
                .zip(consumed_parameters.iter())
                .filter(|(_, consumption)| matches!(consumption, Consumption::Consumed))
            {
                let ownership = environment.consume_variable(argument);
                // the argument should be owned or not reference counted at the return.
                debug_assert!(matches!(ownership, None | Some(Ownership::Owned)));
            }

            arena.alloc(Stmt::Jump(*join_point_id, arguments.clone()))
        }
        Stmt::Crash(_, _) => todo!(),
    }
}

/**
Insert increment statements for the given symbols compensating for the ownership.
*/
fn consume_and_insert_inc_stmts<'a>(
    arena: &'a Bump,
    environment: &mut Environment,
    usage: &OwnedUsage,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    usage
        .iter()
        .fold(continuation, |continuation, (symbol, usage_count)| {
            consume_and_insert_inc_stmt(arena, environment, symbol, *usage_count, continuation)
        })
}

/**
Insert an increment statement for the given symbol compensating for the ownership.
*/
fn consume_and_insert_inc_stmt<'a>(
    arena: &'a Bump,
    environment: &mut Environment,
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

    match new_count {
        0 => continuation,
        positive_count => arena.alloc(Stmt::Refcounting(
            ModifyRc::Inc(*symbol, positive_count),
            continuation,
        )),
    }
}

/**
Insert decrement statements for the given symbols if they are owned.
*/
fn consume_and_insert_dec_stmts<'a>(
    arena: &'a Bump,
    environment: &mut Environment,
    symbols: impl Iterator<Item = &'a Symbol>,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    symbols.fold(continuation, |continuation, symbol| {
        consume_and_insert_dec_stmt(arena, environment, symbol, continuation)
    })
}

/**
Insert a decrement statement for the given symbol if it is owned.
*/
fn consume_and_insert_dec_stmt<'a>(
    arena: &'a Bump,
    environment: &mut Environment,
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
