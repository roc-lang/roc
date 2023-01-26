use std::{collections::HashMap, hash::BuildHasherDefault};

use bumpalo::Bump;
use roc_collections::{all::WyHash, MutMap, MutSet};
use roc_module::symbol::{IdentIds, ModuleId, Symbol};

use crate::{
    ir::{Expr, Proc, ProcLayout, Stmt, UpdateModeIds},
    layout::STLayoutInterner,
};

pub fn insert_refcount_operations<'a>(
    arena: &'a Bump,
    layout_interner: &STLayoutInterner,
    module_id: ModuleId,
    ident_ids: &mut IdentIds,
    update_mode_ids: &mut UpdateModeIds,
    procedures: &mut HashMap<(Symbol, ProcLayout), Proc<'a>, BuildHasherDefault<WyHash>>,
) -> () {
    for (_, proc) in procedures.iter_mut() {
        // TODO use params to fill environment.
        let initial_environment = Environment {
            borrowed: MutSet::default(),
            owned: MutSet::default(),
        };
        let new_body = insert_refcount_operations_stmt(arena, initial_environment, &proc.body);
        proc.body = new_body.clone();
    }
}

enum VarType {}

type VariableMap = MutMap<Symbol, VarType>;

type Variables = MutSet<Symbol>;

struct Environment {
    borrowed: Variables,
    owned: Variables,
}

struct Context {
    variables: VariableMap,
    environment: Environment,
}

// TODO take into account what values should and should not be reference counted.
// e.g. stack allocated values should not be reference counted.
fn insert_refcount_operations_stmt<'a>(
    arena: &'a Bump,
    environment: Environment,
    stmt: &Stmt<'a>,
) -> &'a Stmt<'a> {
    // TODO: Deal with potentially stack overflowing let chains with an explicit loop.

    match &stmt {
        Stmt::Let(binding, expr, layout, stmt) => {
            // TODO take into account that the bound variable might not be free in the continuation.
            // And as such, we can drop the value before continuing.

            let mut stmt_owned: Variables = {
                let mut free_variables = Stmt::free_variables(&stmt);

                // remove the bound variable as it should be owned by the expression.
                free_variables.remove(binding);

                // intersect with the given owned variables as some of free variables might be borrowed.
                environment
                    .owned
                    .intersection(&free_variables)
                    .copied()
                    .collect()
            };

            let expr_environment = Environment {
                borrowed: environment.borrowed.union(&stmt_owned).copied().collect(),
                owned: environment.owned.difference(&stmt_owned).copied().collect(),
            };

            let new_expr = insert_refcount_operations_expr(arena, expr_environment, expr);

            let stmt_environment = Environment {
                borrowed: environment.borrowed,
                owned: {
                    stmt_owned.insert(*binding);
                    stmt_owned
                },
            };

            let new_stmt = insert_refcount_operations_stmt(arena, stmt_environment, stmt);

            arena.alloc(Stmt::Let(*binding, new_expr, *layout, new_stmt))
        }
        Stmt::Switch {
            cond_symbol,
            cond_layout,
            branches,
            default_branch,
            ret_layout,
        } => {
            todo!()
        }
        Stmt::Ret(_) => todo!(),
        Stmt::Refcounting(_, _) => todo!(),
        _ => todo!(),
    }
}

fn insert_refcount_operations_expr<'a>(
    arena: &Bump,
    environment: Environment,
    expr: &Expr<'a>,
) -> Expr<'a> {
    match &expr {
        Expr::Literal(_) => todo!(),
        Expr::Call(_) => todo!(),
        Expr::Tag {
            tag_layout,
            tag_id,
            arguments,
        } => todo!(),
        Expr::Struct(_) => todo!(),
        Expr::StructAtIndex {
            index,
            field_layouts,
            structure,
        } => todo!(),
        Expr::GetTagId {
            structure,
            union_layout,
        } => todo!(),
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

trait FreeVariables {
    fn free_variables(&self) -> Variables;
}

impl<'a> FreeVariables for Stmt<'a> {
    // Return the set of variables that are free in the statement.
    fn free_variables(&self) -> Variables {
        todo!()
    }
}

impl<'a> FreeVariables for Expr<'a> {
    // Return the set of variables that are free in the expression.
    fn free_variables(&self) -> Variables {
        todo!()
    }
}
