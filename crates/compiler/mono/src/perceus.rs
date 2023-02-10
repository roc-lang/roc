use std::{collections::HashMap, hash::BuildHasherDefault, iter};

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
    for (_, proc) in procedures.iter_mut() {
        let mut initial_environment = Environment {
            variables_rc_type: VariableRcTypesEnv::get_variables_rc_type_proc(
                layout_interner,
                &proc,
            ),
            // All parameters are owned.
            variables_ownership: proc
                .args
                .iter()
                .map(|(_layout, symbol)| (*symbol, Ownership::Owned))
                .collect(),
        };
        let new_body = insert_refcount_operations_stmt(arena, &mut initial_environment, &proc.body);
        proc.body = new_body.clone();
    }
}

enum VarRcType {
    ReferenceCounted,
    NotReferenceCounted,
}

type VariableRcTypes = MutMap<Symbol, VarRcType>;

struct VariableRcTypesEnv<'a> {
    // A map keeping track of which variables are reference counted and which are not.
    variables_rc_type: VariableRcTypes,

    layout_interner: &'a STLayoutInterner<'a>,
}

impl<'a> VariableRcTypesEnv<'a> {
    // Get the reference count types of all variables in a procedure.
    fn get_variables_rc_type_proc(
        layout_interner: &STLayoutInterner,
        proc: &Proc<'a>,
    ) -> VariableRcTypes {
        let mut variable_rc_types = VariableRcTypesEnv {
            variables_rc_type: MutMap::default(),
            layout_interner,
        };

        for (layout, symbol) in proc.args.iter() {
            variable_rc_types.insert_symbol_layout_rc_type(symbol, layout);
        }

        variable_rc_types.insert_variables_rc_type_stmt(&proc.body);

        variable_rc_types.variables_rc_type
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

type Variables = MutSet<Symbol>;

type VariablesOwnership = MutMap<Symbol, Ownership>;

// A map keeping track of how many times a variable is used.
type VariableUsage = MutMap<Symbol, u64>;

fn insert_variable_usage(
    variable_rc_types: &VariableRcTypes,
    usage: &mut VariableUsage,
    symbol: Symbol,
) {
    match variable_rc_types
        .get(&symbol)
        .expect("Expected variable to be in the map")
    {
        // If the variable is reference counted, we need to increment the usage count.
        VarRcType::ReferenceCounted => {
            match usage.get(&symbol) {
                Some(count) => usage.insert(symbol, count + 1),
                None => usage.insert(symbol, 1),
            };
        }
        // If the variable is not reference counted, we don't need to do anything.
        VarRcType::NotReferenceCounted => return,
    }
}

fn insert_variable_usages(
    variable_rc_types: &VariableRcTypes,
    usage: &mut VariableUsage,
    symbols: impl Iterator<Item = Symbol>,
) {
    symbols.for_each(|symbol| insert_variable_usage(variable_rc_types, usage, symbol));
}

struct Environment {
    // Keep track which variables are reference counted and which are not.
    variables_rc_type: VariableRcTypes,
    // The Koka implementation assumes everything that is not owned to be borrowed.
    variables_ownership: VariablesOwnership,
}

// TODO take into account what values should and should not be reference counted.
// e.g. stack allocated values should not be reference counted.
fn insert_refcount_operations_stmt<'a>(
    arena: &'a Bump,
    environment: &mut Environment,
    stmt: &Stmt<'a>,
) -> &'a Stmt<'a> {
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
            let new_stmt = insert_refcount_operations_stmt(arena, environment, stmt);

            // Then evaluate the bound expression. where the free variables from the continuation are borrowed.
            let mut usage = VariableUsage::default();
            get_reference_counted_variable_usage_expr(
                arena,
                &environment.variables_rc_type,
                &mut usage,
                expr,
            );

            // Can this clone be avoided?
            let new_let = arena.alloc(Stmt::Let(*binding, expr.clone(), *layout, new_stmt));

            // Insert the reference count operations for the variables used in the expression.
            let new_let_with_refcount = insert_refcount_stmt(arena, environment, usage, new_let);
            new_let_with_refcount
        }
        Stmt::Switch {
            cond_symbol,
            cond_layout,
            branches,
            default_branch,
            ret_layout,
        } => {
            return arena.alloc(Stmt::Switch {
                cond_symbol: *cond_symbol,
                cond_layout: *cond_layout,
                branches: branches.clone(),
                default_branch: default_branch.clone(),
                ret_layout: *ret_layout,
            });
            todo!()
        }
        Stmt::Ret(s) => {
            return arena.alloc(Stmt::Ret(*s));
            todo!()
        }
        Stmt::Refcounting(_, _) => todo!(),
        _ => todo!(),
    }
}

fn insert_refcount_stmt<'a>(
    arena: &'a Bump,
    environment: &mut Environment,
    mut usage: VariableUsage,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    usage
        .iter()
        .fold(continuation, |continuation, (symbol, usage_count)| {
            let new_count = match consume_variable(environment, *symbol) {
                // If the variable is borrowed, we need to increment the reference count for each usage.
                Ownership::Borrowed => *usage_count,
                // If the variable is owned, we need to increment the reference count for each usage except one.
                Ownership::Owned => *usage_count - 1,
            };

            match new_count {
                0 => continuation,
                _ => arena.alloc(Stmt::Refcounting(
                    ModifyRc::Inc(*symbol, *usage_count),
                    continuation,
                )),
            }
        })
}

fn get_reference_counted_variable_usage_expr<'a>(
    arena: &Bump,
    variable_rc_types: &VariableRcTypes,
    usage: &mut VariableUsage,
    expr: &Expr<'a>,
) {
    match expr {
        Expr::Literal(_) => {
            // Literals are not reference counted.
        }
        Expr::Call(call) => {
            insert_variable_usages(variable_rc_types, usage, call.arguments.iter().copied());
        }
        Expr::Tag {
            tag_layout,
            tag_id,
            arguments,
        } => todo!(),
        Expr::Struct(arguments) => {
            insert_variable_usages(variable_rc_types, usage, arguments.iter().copied());
        }
        Expr::StructAtIndex {
            index,
            field_layouts,
            structure,
        } => todo!(),
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
}

fn insert_refcount_operations_expr<'a>(
    arena: &Bump,
    environment: &mut Environment,
    expr: &Expr<'a>,
) -> Expr<'a> {
    match &expr {
        Expr::Literal(lit) => {
            return Expr::Literal(lit.clone());
            todo!()
        }
        Expr::Call(call) => Expr::Call(insert_refcount_operations_expr_call(
            arena,
            environment,
            call,
        )),
        Expr::Tag {
            tag_layout,
            tag_id,
            arguments,
        } => {
            return Expr::Tag {
                tag_layout: *tag_layout,
                tag_id: *tag_id,
                arguments: arguments.clone(),
            };
            todo!()
        }
        Expr::Struct(s) => {
            return Expr::Struct(s);
            todo!()
        }
        Expr::StructAtIndex {
            index,
            field_layouts,
            structure,
        } => {
            return Expr::StructAtIndex {
                index: *index,
                field_layouts,
                structure: *structure,
            };
            todo!()
        }
        Expr::GetTagId {
            structure,
            union_layout,
        } => {
            return Expr::GetTagId {
                structure: *structure,
                union_layout: *union_layout,
            };

            todo!()
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
}

// When performing a function call, we pass the parameters as owned.
// This means that if we have these parameters in the borrowed environment, we need to increment the reference count.
fn insert_refcount_operations_expr_call<'a>(
    arena: &Bump,
    environment: &mut Environment,
    call: &Call<'a>,
) -> Call<'a> {
    // INFO Koka makes a distinction between top level and non top level definitions.
    match &call.call_type {
        CallType::ByName {
            name,
            ret_layout,
            arg_layouts,
            specialization_id,
        } => {
            return call.clone();
            todo!()
        }
        CallType::Foreign {
            foreign_symbol,
            ret_layout,
        } => {
            return call.clone();
            todo!()
        }
        // E.g. lowlevel eq
        CallType::LowLevel { op, update_mode } => {
            return call.clone();
            todo!()
        }
        CallType::HigherOrder(_) => {
            return call.clone();
            todo!()
        }
    }
}

// Retrieve whether the variable is owned or borrowed.
// If it was owned, set it to borrowed.
fn consume_variable<'a>(environment: &mut Environment, variable: Symbol) -> Ownership {
    // Consume the variable by setting it to borrowed (if it was owned before), and return the previous ownership.
    {
        let this = environment
            .variables_ownership
            .insert(variable, Ownership::Borrowed);
        match this {
            Some(val) => val,
            None => panic!("Expected variable to be in environment"),
        }
    }
}

// trait FreeVariables {
//     fn free_variables(&self) -> Variables;
// }

// impl<'a> FreeVariables for Stmt<'a> {
//     // Return the set of variables that are free in the statement.
//     fn free_variables(&self) -> Variables {
//         return MutSet::default();
//         todo!()
//     }
// }

// impl<'a> FreeVariables for Expr<'a> {
//     // Return the set of variables that are free in the expression.
//     fn free_variables(&self) -> Variables {
//         return MutSet::default();
//         todo!()
//     }
// }
