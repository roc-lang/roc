use std::{collections::HashMap, hash::BuildHasherDefault, iter};

use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::{all::WyHash, MutMap, MutSet};
use roc_module::symbol::{IdentIds, ModuleId, Symbol};

use crate::{
    borrow::Ownership,
    ir::{BranchInfo, Expr, ListLiteralElement, ModifyRc, Proc, ProcLayout, Stmt, UpdateModeIds},
    layout::{InLayout, LayoutInterner, STLayoutInterner},
};

/**
Insert the reference count operations for procedures.
*/
pub fn insert_refcount_operations<'a>(
    arena: &'a Bump,
    layout_interner: &STLayoutInterner,
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
        let mut variable_rc_types_env = variable_rc_types_env.clone();
        variable_rc_types_env.insert_variables_rc_type_proc(&proc);

        let mut initial_environment = Environment {
            variables_rc_types: &variable_rc_types_env.variables_rc_type,
            variables_ownership: MutMap::default(),
        };

        // Add all arguments to the environment (if they are reference counted)
        for (_layout, symbol) in proc.args.iter() {
            initial_environment.add_variable(symbol);
        }

        let (new_body, _) =
            insert_refcount_operations_stmt(arena, &mut initial_environment, &proc.body);
        proc.body = new_body.clone();
    }
}

#[derive(Clone)]
enum VarRcType {
    ReferenceCounted,
    NotReferenceCounted,
}

type VariableRcTypes = MutMap<Symbol, VarRcType>;

type FreeRcVariables = MutSet<Symbol>;

/**
Environment to keep track which of the variables should be reference counted and which ones should not.
 */
struct VariableRcTypesEnv<'a> {
    // A map keeping track of which variables are reference counted and which are not.
    variables_rc_type: VariableRcTypes,

    layout_interner: &'a STLayoutInterner<'a>,
}

// TODO what would be a good way to structure a similar pattern? creating env, evaluating multiple different objects and returning an element from the env.
impl<'a> VariableRcTypesEnv<'a> {
    /**
    Create a new VariableRcTypesEnv from a layout interner.
    */
    fn from_layout_interner(layout_interner: &'a STLayoutInterner) -> VariableRcTypesEnv<'a> {
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
        self: &mut VariableRcTypesEnv<'a>,
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
    fn insert_variables_rc_type_proc(self: &mut VariableRcTypesEnv<'a>, proc: &Proc<'a>) {
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
    fn insert_variables_rc_type_stmt(self: &mut VariableRcTypesEnv<'a>, stmt: &Stmt<'a>) {
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
        self: &mut VariableRcTypesEnv<'a>,
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

impl Clone for VariableRcTypesEnv<'_> {
    fn clone(&self) -> Self {
        VariableRcTypesEnv {
            variables_rc_type: self.variables_rc_type.clone(),
            layout_interner: self.layout_interner,
        }
    }
}

type VariablesOwnership = MutMap<Symbol, Ownership>;

// A map keeping track of how many times a variable is used.
type VariableUsage = MutMap<Symbol, u64>;

// Combine variable usage by summing the usage of each variable.
fn combine_variable_usage(
    variable_usage: &mut VariableUsage,
    other_variable_usage: &VariableUsage,
) {
    for (symbol, other_usage) in other_variable_usage.iter() {
        match variable_usage.get(symbol) {
            Some(old_usage) => variable_usage.insert(*symbol, old_usage + other_usage),
            None => variable_usage.insert(*symbol, *other_usage),
        };
    }
}

/**
A struct to determine the usage of variables in an expression.
 */
struct VariableUsageEnv<'a> {
    variable_usage: VariableUsage,

    variable_rc_types: &'a VariableRcTypes,
}

impl<'a> VariableUsageEnv<'a> {
    /**
    Retrieve how much a variable is used in an expression.
    */
    fn get_reference_counted_variable_usage_expr(
        variable_rc_types: &VariableRcTypes,
        expr: &Expr<'a>,
    ) -> VariableUsage {
        let mut usage_env = VariableUsageEnv {
            variable_usage: VariableUsage::default(),
            variable_rc_types,
        };

        match expr {
            Expr::Literal(_) => {
                // Literals are not reference counted.
            }
            Expr::Call(call) => {
                usage_env.insert_variable_usages(call.arguments.iter().copied());
            }
            Expr::Tag { arguments, .. } | Expr::Struct(arguments) => {
                usage_env.insert_variable_usages(arguments.iter().copied());
            }

            Expr::GetTagId { structure, .. }
            | Expr::StructAtIndex { structure, .. }
            | Expr::UnionAtIndex { structure, .. } => {
                // All structures are alive at this point and don't have to be copied in order to take an index out.
                // TODO perhaps inc if the index is a pointer to a heap value, assuming non pointers get copied.
            }
            Expr::Array {
                elem_layout: _,
                elems,
            } => {
                // For an array creation, we insert all the used elements.
                usage_env.insert_variable_usages(elems.iter().filter_map(
                    |element| match element {
                        // Literal elements are not reference counted.
                        ListLiteralElement::Literal(_) => None,
                        // Symbol elements are reference counted.
                        ListLiteralElement::Symbol(symbol) => Some(*symbol),
                    },
                ));
            }
            Expr::EmptyArray => {
                // Empty arrays have no reference counted elements.
            }
            Expr::ExprBox { symbol } | Expr::ExprUnbox { symbol } => {
                usage_env.insert_variable_usage(*symbol);
            }
            Expr::Reuse { .. } | Expr::Reset { .. } => {
                unreachable!("Reset and reuse should not exist at this point")
            }
            Expr::RuntimeErrorFunction(_) => todo!(),
        }

        usage_env.variable_usage
    }

    /**
    Insert the usage of a symbol into the env.
    If the symbol is not reference counted, it will be ignored.
    */
    fn insert_variable_usage(self: &mut VariableUsageEnv<'a>, symbol: Symbol) {
        match {
            // TODO convert this to a oneliner once panics do not occur anymore.
            let this = self.variable_rc_types.get(&symbol);
            match this {
                Some(val) => val,
                None => panic!("Expected variable to be in the map"),
            }
        } {
            // If the variable is reference counted, we need to increment the usage count.
            VarRcType::ReferenceCounted => {
                match self.variable_usage.get(&symbol) {
                    Some(count) => self.variable_usage.insert(symbol, count + 1),
                    None => self.variable_usage.insert(symbol, 1),
                };
            }
            // If the variable is not reference counted, we don't need to do anything.
            VarRcType::NotReferenceCounted => return,
        }
    }

    /**
    Call insert_variable_usage for multiple symbols.
    */
    fn insert_variable_usages(
        self: &mut VariableUsageEnv<'a>,
        symbols: impl Iterator<Item = Symbol>,
    ) {
        symbols.for_each(|symbol| self.insert_variable_usage(symbol));
    }
}

/**
The environment for the reference counting pass.
Contains the variable rc types and the ownership.
*/
struct Environment<'a> {
    // Keep track which variables are reference counted and which are not.
    variables_rc_types: &'a VariableRcTypes,
    // The Koka implementation assumes everything that is not owned to be borrowed.
    variables_ownership: VariablesOwnership,
}

impl<'a> Clone for Environment<'a> {
    fn clone(&self) -> Self {
        Environment {
            variables_rc_types: self.variables_rc_types,
            variables_ownership: self.variables_ownership.clone(),
        }
    }
}

impl<'a> Environment<'a> {
    /**
    Retrieve the rc type of a variable.
    */
    fn get_variable_rc_type(self: &mut Environment<'a>, variable: &Symbol) -> &VarRcType {
        self.variables_rc_types
            .get(variable)
            .expect("variable should have rc type")
    }

    /*
    Retrieve whether the variable is owned or borrowed.
    If it was owned, set it to borrowed (as it is consumed/the variable can be used as owned only once without incrementing).
    */
    fn consume_variable(self: &mut Environment<'a>, variable: &Symbol) -> Ownership {
        // This function should only be called on reference counted variables.
        debug_assert!(matches!(
            self.get_variable_rc_type(variable),
            VarRcType::ReferenceCounted
        ));
        // Consume the variable by setting it to borrowed (if it was owned before), and return the previous ownership.
        let this = self
            .variables_ownership
            .insert(*variable, Ownership::Borrowed);
        match this {
            Some(val) => val,
            None => panic!("Expected variable to be in environment"),
        }
    }

    /*
    Add a variable to the environment if it is reference counted.
    */
    fn add_variable(self: &mut Environment<'a>, variable: &Symbol) {
        match self.get_variable_rc_type(variable) {
            VarRcType::ReferenceCounted => {
                self.variables_ownership.insert(*variable, Ownership::Owned);
            }
            VarRcType::NotReferenceCounted => {
                // If this variable is not reference counted, we don't need to do anything.
            }
        }
    }

    /*
    Add a variable to the environment during a function call.
    Remove the variable afterwards to prevent it from being used outside the function call.
    */
    fn with_variable<F, R>(self: &mut Environment<'a>, variable: &Symbol, callback: F) -> R
    where
        F: Fn(&mut Environment) -> R,
    {
        match self.get_variable_rc_type(variable) {
            VarRcType::ReferenceCounted => {
                self.variables_ownership.insert(*variable, Ownership::Owned);
                let result = callback(self);
                self.variables_ownership.remove(&variable);
                result
            }
            VarRcType::NotReferenceCounted => callback(self),
        }
    }
}

/**
Given an environment, insert the reference counting operations for a statement.
*/
fn insert_refcount_operations_stmt<'a>(
    arena: &'a Bump,
    environment: &mut Environment,
    stmt: &Stmt<'a>,
) -> (&'a Stmt<'a>, FreeRcVariables) {
    // TODO: Deal with potentially stack overflowing let chains with an explicit loop.
    // Koka has a list for let bindings.

    match &stmt {
        // The expression borrows the values owned (used) by the continuation.
        Stmt::Let(binding, expr, layout, stmt) => {
            // TODO take into account that the bound variable might not be free in the continuation.
            // And as such, we can drop the value before continuing.

            // INFO The Koka implementation (instead of calculating the owned environment beforehand)
            // First evaluates the continuation with the owned environment, setting the variables to dead (not alive).
            // And in the rest of the code, the dead variables are treated as borrowed (because not alive).

            // First evalute the continuation and let it consume it's free variables.
            let (new_stmt, mut free_rc_vars_stmt) = environment.with_variable(binding, |env| {
                insert_refcount_operations_stmt(arena, env, stmt)
            });

            // Then evaluate the bound expression. where the free variables from the continuation are borrowed.
            let variable_usage = VariableUsageEnv::get_reference_counted_variable_usage_expr(
                &environment.variables_rc_types,
                expr,
            );

            let new_let = arena.alloc(Stmt::Let(*binding, expr.clone(), *layout, new_stmt));

            // Insert the reference count operations for the variables used in the expression.
            let new_let_with_refcount =
                insert_inc_stmts(arena, environment, &variable_usage, new_let);

            let free_rc_vars = {
                // Remove the bound variable, as it is no longer free.
                free_rc_vars_stmt.remove(binding);
                // Add the free variables from the expression.
                free_rc_vars_stmt.extend(variable_usage.keys());
                free_rc_vars_stmt
            };

            (new_let_with_refcount, free_rc_vars)
        }
        Stmt::Switch {
            cond_symbol,
            cond_layout,
            branches,
            default_branch,
            ret_layout,
        } => {
            let new_branches: std::vec::Vec<_> = branches
                .iter()
                .map(|(label, info, branch)| {
                    let (new_branch, free_variables_branch) =
                        insert_refcount_operations_stmt(arena, &mut environment.clone(), branch);

                    (*label, info.clone(), new_branch, free_variables_branch)
                })
                .collect();

            let new_default_branch = {
                let (info, branch) = default_branch;
                let (new_branch, free_variables_branch) =
                    insert_refcount_operations_stmt(arena, &mut environment.clone(), branch);

                (info.clone(), new_branch, free_variables_branch)
            };

            // Combine the free variables of the all branches.
            let free_rc_vars = {
                let mut free_rc_vars = FreeRcVariables::default();

                new_branches.iter().for_each(|(_, _, _, free_variables)| {
                    free_rc_vars.extend(free_variables);
                });

                free_rc_vars.extend(&new_default_branch.2);

                // The scrutinee is not reference counted as it is a number.
                // Thus, we don't need to insert it in free vars.
                // free_rc_vars.insert(*cond_symbol);

                free_rc_vars
            };

            // If we compare the free variables from a branch with the free variables from all branches.
            // We can determine which variables are not used in that branch.
            // And as such, we can drop them before continuing.
            let newer_branches = Vec::from_iter_in(
                new_branches
                    .into_iter()
                    .map(|(label, info, branch, free_variables)| {
                        let drop_vars = free_rc_vars.difference(&free_variables).collect();
                        let newer_branch = insert_dec_stmts(arena, environment, &drop_vars, branch);
                        (label, info, newer_branch.clone())
                    }),
                arena,
            )
            .into_bump_slice();

            let newer_default_branch = {
                let (info, branch, free_variables) = new_default_branch;
                let drop_vars = free_rc_vars.difference(&free_variables).collect();
                let newer_branch = insert_dec_stmts(arena, environment, &drop_vars, branch);

                (info, newer_branch)
            };

            let new_switch = arena.alloc(Stmt::Switch {
                cond_symbol: *cond_symbol,
                cond_layout: *cond_layout,
                branches: newer_branches,
                default_branch: newer_default_branch,
                ret_layout: *ret_layout,
            });

            (new_switch, free_rc_vars)
        }
        Stmt::Ret(s) => {
            // TODO use with_capacity if possible, or use an initializer to oneline this.
            let mut free_variables = FreeRcVariables::with_capacity_and_hasher(
                1,
                roc_collections::all::BuildHasher::default(),
            );
            free_variables.insert(*s);
            return (arena.alloc(Stmt::Ret(*s)), free_variables);
        }
        Stmt::Refcounting(_, _) => unreachable!("refcounting should not be in the AST already"),
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
        } => todo!(),
        Stmt::Jump(_, _) => todo!(),
        Stmt::Crash(_, _) => todo!(),
    }
}

/**
 * Insert increment statements for the given symbols compensating for the ownership.
 */
fn insert_inc_stmts<'a>(
    arena: &'a Bump,
    environment: &mut Environment,
    usage: &VariableUsage,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    usage
        .iter()
        .fold(continuation, |continuation, (symbol, usage_count)| {
            insert_inc_stmt(arena, environment, symbol, *usage_count, continuation)
        })
}

/**
Insert an increment statement for the given symbol compensating for the ownership.
*/
fn insert_inc_stmt<'a>(
    arena: &'a Bump,
    environment: &mut Environment,
    symbol: &Symbol,
    usage_count: u64,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    let new_count = match environment.consume_variable(symbol) {
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
fn insert_dec_stmts<'a>(
    arena: &'a Bump,
    environment: &mut Environment,
    symbols: &MutSet<&Symbol>,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    symbols.iter().fold(continuation, |continuation, symbol| {
        insert_dec_stmt(arena, environment, symbol, continuation)
    })
}

/**
Insert a decrement statement for the given symbol if it is owned.
*/
fn insert_dec_stmt<'a>(
    arena: &'a Bump,
    environment: &mut Environment,
    symbol: &Symbol,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    match environment.consume_variable(symbol) {
        // If the variable is borrowed, don't have to decrement the reference count.
        Ownership::Borrowed => continuation,
        // If the variable is owned, we do need to decrement the reference count.
        Ownership::Owned => arena.alloc(Stmt::Refcounting(ModifyRc::Dec(*symbol), continuation)),
    }
}
