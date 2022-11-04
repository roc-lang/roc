use roc_types::subs::VarStore;

use crate::{
    lang::core::{def::def::Def, expr::expr2::Expr2},
    mem_pool::{pool::Pool, pool_vec::PoolVec},
};

use super::def::def::Declaration;

pub(crate) fn decl_to_let(
    pool: &mut Pool,
    var_store: &mut VarStore,
    decl: Declaration,
    ret: Expr2,
) -> Expr2 {
    match decl {
        Declaration::Declare(def) => match def {
            Def::AnnotationOnly { .. } => todo!(),
            Def::Value(value_def) => {
                let def_id = pool.add(value_def);

                let body_id = pool.add(ret);

                Expr2::LetValue {
                    def_id,
                    body_id,
                    body_var: var_store.fresh(),
                }
            }
            Def::Function(function_def) => {
                let def_id = pool.add(function_def);
                let body_id = pool.add(ret);

                Expr2::LetFunction {
                    def_id,
                    body_id,
                    body_var: var_store.fresh(),
                }
            }
        },
        Declaration::DeclareRec(defs) => {
            let mut function_defs = vec![];

            for def in defs {
                match def {
                    Def::AnnotationOnly { .. } => todo!(),
                    Def::Function(function_def) => function_defs.push(function_def),
                    Def::Value(_) => unreachable!(),
                }
            }

            let body_id = pool.add(ret);

            Expr2::LetRec {
                defs: PoolVec::new(function_defs.into_iter(), pool),
                body_var: var_store.fresh(),
                body_id,
            }
        }
        Declaration::InvalidCycle(_entries, _) => {
            // TODO: replace with something from Expr2
            // Expr::RuntimeError(RuntimeError::CircularDef(entries))
            todo!()
        }
        Declaration::Builtin(_) => {
            // Builtins should only be added to top-level decls, not to let-exprs!
            unreachable!()
        }
    }
}
