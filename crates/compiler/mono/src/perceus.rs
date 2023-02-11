use std::{collections::HashMap, hash::BuildHasherDefault, iter};

use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::{all::WyHash, MutMap, MutSet};
use roc_module::symbol::{IdentIds, ModuleId, Symbol};

use crate::{
    borrow::Ownership,
    ir::{BranchInfo, Call, CallType, Expr, ModifyRc, Proc, ProcLayout, Stmt, UpdateModeIds},
    layout::{InLayout, LayoutInterner, STLayoutInterner},
};

pub fn insert_refcount_operations<'a>(
    arena: &'a Bump,
    layout_interner: &STLayoutInterner,
    home: ModuleId,
    ident_ids: &mut IdentIds,
    update_mode_ids: &mut UpdateModeIds,
    procedures: &mut HashMap<(Symbol, ProcLayout), Proc<'a>, BuildHasherDefault<WyHash>>,
) -> () {
    // Create a VariableRcTypesEnv for the procedures as they get referenced but should be marked as non reference counted.
    let proc_rc_type = procedures.keys().map(|(symbol, _layout)| *symbol);
    let variable_rc_types_env =
        VariableRcTypesEnv::from_proc_symbols(layout_interner, proc_rc_type);

    for (_, proc) in procedures.iter_mut() {
        // Clone the variable_rc_types_env and insert the variables in the current procedure.
        // As the variables should be limited in scope for the current proc.
        let variable_rc_types_env = &mut variable_rc_types_env.clone();
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

struct VariableRcTypesEnv<'a> {
    // A map keeping track of which variables are reference counted and which are not.
    variables_rc_type: VariableRcTypes,

    layout_interner: &'a STLayoutInterner<'a>,
}

// TODO what would be a good way to structure a similar pattern? creating env, evaluating multiple different objects and returning an element from the env.
impl<'a> VariableRcTypesEnv<'a> {
    fn from_proc_symbols(
        layout_interner: &'a STLayoutInterner,
        proc_symbols: impl Iterator<Item = Symbol>,
    ) -> VariableRcTypesEnv<'a> {
        VariableRcTypesEnv {
            variables_rc_type: proc_symbols
                .map(|symbol| (symbol, VarRcType::NotReferenceCounted))
                .collect(),
            layout_interner,
        }
    }

    // Get the reference count types of all variables in a procedure.
    fn insert_variables_rc_type_proc(self: &mut VariableRcTypesEnv<'a>, proc: &Proc<'a>) {
        for (layout, symbol) in proc.args.iter() {
            self.insert_symbol_layout_rc_type(symbol, layout);
        }

        self.insert_variables_rc_type_stmt(&proc.body);
    }

    fn insert_variables_rc_type_stmt(self: &mut VariableRcTypesEnv<'a>, stmt: &Stmt<'a>) {
        match stmt {
            Stmt::Let(binding, _expr, layout, stmt) => {
                self.insert_symbol_layout_rc_type(&binding, layout);
                self.insert_variables_rc_type_stmt(stmt);
            }
            Stmt::Switch {
                // TODO what is this symbol for and should it be handled?
                cond_symbol,
                cond_layout,
                branches,
                default_branch,
                ret_layout: _,
            } => {
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
            Stmt::Ret(_) => {}
            Stmt::Refcounting(_, _) => {}
            Stmt::Expect {
                condition,
                region,
                lookups,
                variables,
                remainder,
            } => {
                self.insert_variables_rc_type_stmt(remainder);
            }
            Stmt::ExpectFx {
                condition,
                region,
                lookups,
                variables,
                remainder,
            } => {
                self.insert_variables_rc_type_stmt(remainder);
            }
            Stmt::Dbg {
                symbol,
                variable,
                remainder,
            } => {
                self.insert_variables_rc_type_stmt(remainder);
            }
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

    fn insert_symbol_layout_rc_type(
        self: &mut VariableRcTypesEnv<'a>,
        symbol: &Symbol,
        layout: &InLayout,
    ) {
        // TODO add more checks like *persistent*, consume, and reset.
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

struct VariableUsageEnv<'a> {
    variable_usage: VariableUsage,

    variable_rc_types: &'a VariableRcTypes,
}

impl<'a> VariableUsageEnv<'a> {
    fn get_reference_counted_variable_usage_expr(
        variable_rc_types: &VariableRcTypes,
        expr: &Expr<'a>,
    ) -> HashMap<Symbol, u64, BuildHasherDefault<WyHash>> {
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
            Expr::StructAtIndex {
                index,
                field_layouts,
                structure,
            } => {
                usage_env.insert_variable_usage(*structure);
            }
            Expr::GetTagId {
                structure,
                union_layout,
            } => {
                // The arguments to get tag id are not reference counted.
            }
            Expr::UnionAtIndex {
                structure,
                tag_id,
                union_layout,
                index,
            } => todo!(),
            Expr::Array { elem_layout, elems } => todo!(),
            Expr::EmptyArray => todo!(),
            Expr::ExprBox { symbol } => todo!(),
            Expr::ExprUnbox { symbol } => todo!(),
            Expr::Reuse {
                symbol,
                update_tag_id,
                update_mode,
                tag_layout,
                tag_id,
                arguments,
            } => todo!(),
            Expr::Reset {
                symbol,
                update_mode,
            } => todo!(),
            Expr::RuntimeErrorFunction(_) => todo!(),
        }

        usage_env.variable_usage
    }

    fn insert_variable_usage(self: &mut VariableUsageEnv<'a>, symbol: Symbol) {
        match {
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

    fn insert_variable_usages(
        self: &mut VariableUsageEnv<'a>,
        symbols: impl Iterator<Item = Symbol>,
    ) {
        symbols.for_each(|symbol| self.insert_variable_usage(symbol));
    }
}

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
    fn get_variable_rc_type(self: &mut Environment<'a>, variable: &Symbol) -> &VarRcType {
        self.variables_rc_types
            .get(variable)
            .expect("variable should have rc type")
    }

    // Retrieve whether the variable is owned or borrowed.
    // If it was owned, set it to borrowed.
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

    // Add a variable to the environment.
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

    // Add a variable to the environment during a function call.
    // Remove the variable afterwards to prevent it from being used outside the function call.
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

// TODO take into account what values should and should not be reference counted.
// e.g. stack allocated values should not be reference counted.
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
            return (arena.alloc(Stmt::Ret(*s)), FreeRcVariables::default());
            todo!()
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
