use crate::ir::{Expr, JoinPointId, Param, Proc, Stmt};
use roc_collections::all::{MutMap, MutSet};
use roc_module::symbol::Symbol;

pub fn collect_stmt(
    stmt: &Stmt<'_>,
    jp_live_var_map: MutMap<JoinPointId, MutSet<Symbol>>,
) -> MutSet<Symbol> {
    let mut result = MutSet::default();

    collect_stmt_help(stmt, jp_live_var_map, &mut result);

    result
}

pub fn collect_expr(expr: &Expr<'_>, result: &mut MutSet<Symbol>) {
    use Expr::*;

    match expr {
        FunctionPointer(symbol, _)
        | AccessAtIndex {
            structure: symbol, ..
        } => {
            result.insert(*symbol);
        }

        FunctionCall { args, .. } => {
            // NOTE thouth the function name does occur, it is a static constant in the program
            // for liveness, it should not be included here.
            result.extend(args.iter().copied());
        }

        Tag { arguments, .. }
        | Struct(arguments)
        | Array {
            elems: arguments, ..
        } => {
            result.extend(arguments.iter().copied());
        }
        Reuse {
            symbol, arguments, ..
        } => {
            result.extend(arguments.iter().copied());
            result.insert(*symbol);
        }
        Reset(x) => {
            result.insert(*x);
        }
        RunLowLevel(_, args) => {
            result.extend(args.iter());
        }

        EmptyArray | RuntimeErrorFunction(_) | Literal(_) => {}
    }
}

fn collect_var(symbol: &Symbol, result: &mut MutSet<Symbol>) {
    result.insert(*symbol);
}

fn collect_args(symbols: &[Symbol], result: &mut MutSet<Symbol>) {
    for s in symbols.iter() {
        result.insert(*s);
    }
}

fn collect_jp(
    id: &JoinPointId,
    jp_live_var_map: MutMap<JoinPointId, MutSet<Symbol>>,
    result: &mut MutSet<Symbol>,
) {
    match jp_live_var_map.get(id) {
        None => unreachable!("join point id {:?} is not known", id),
        Some(xs) => result.extend(xs),
    }
}

fn bind_var(symbol: &Symbol, result: &mut MutSet<Symbol>) {
    result.remove(symbol);
}

fn bind_params(params: &[Param], result: &mut MutSet<Symbol>) {
    for param in params.iter() {
        result.remove(&param.symbol);
    }
}

fn collect_stmt_help(
    stmt: &Stmt<'_>,
    jp_live_var_map: MutMap<JoinPointId, MutSet<Symbol>>,
    result: &mut MutSet<Symbol>,
) {
    use crate::ir::Stmt::*;

    match stmt {
        Let(x, v, _layout, b) => {
            collect_stmt_help(b, jp_live_var_map, result);
            bind_var(x, result);
            collect_expr(v, result);
        }

        Join {
            id: j,
            parameters: ys,
            continuation: v,
            remainder: b,
        } => {
            let mut jp_live_vars = MutSet::default();
            collect_stmt_help(v, jp_live_var_map.clone(), &mut jp_live_vars);
            bind_params(ys, &mut jp_live_vars);

            let mut jp_live_var_map = jp_live_var_map;
            jp_live_var_map.insert(*j, jp_live_vars);

            collect_stmt_help(b, jp_live_var_map, result);
        }

        Jump(j, xs) => {
            collect_args(xs, result);
            collect_jp(j, jp_live_var_map, result);
        }

        Inc(x, b) | Dec(x, b) => {
            collect_stmt_help(b, jp_live_var_map, result);
            collect_var(x, result);
        }

        Ret(x) => {
            collect_var(x, result);
        }

        Switch {
            cond_symbol,
            branches,
            default_branch,
            ..
        } => {
            collect_var(cond_symbol, result);

            for (_, branch) in branches.iter() {
                collect_stmt_help(branch, jp_live_var_map.clone(), result);
            }
            collect_stmt_help(default_branch, jp_live_var_map, result);
        }

        Cond {
            cond_symbol,
            branching_symbol,
            pass,
            fail,
            ..
        } => {
            collect_var(cond_symbol, result);
            collect_var(branching_symbol, result);

            collect_stmt_help(pass, jp_live_var_map.clone(), result);
            collect_stmt_help(fail, jp_live_var_map, result);
        }

        RuntimeError(_) => {}
    }
}
