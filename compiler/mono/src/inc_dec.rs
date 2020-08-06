use crate::ir::{Expr, Stmt};
use roc_collections::all::MutSet;
use roc_module::symbol::Symbol;

pub fn free_variables(stmt: &Stmt<'_>) -> MutSet<Symbol> {
    let (mut occuring, bound) = occuring_variables(stmt);

    for ref s in bound {
        occuring.remove(s);
    }

    occuring
}

pub fn occuring_variables(stmt: &Stmt<'_>) -> (MutSet<Symbol>, MutSet<Symbol>) {
    let mut stack = vec![stmt];
    let mut result = MutSet::default();
    let mut bound_variables = MutSet::default();

    while let Some(stmt) = stack.pop() {
        use Stmt::*;

        match stmt {
            Let(symbol, expr, _, cont) => {
                occuring_variables_expr(expr, &mut result);
                result.insert(*symbol);
                bound_variables.insert(*symbol);
                stack.push(cont);
            }
            Ret(symbol) => {
                result.insert(*symbol);
            }

            Inc(symbol, cont) | Dec(symbol, cont) => {
                result.insert(*symbol);
                stack.push(cont);
            }

            Jump(_, arguments) => {
                result.extend(arguments.iter().copied());
            }

            Join {
                arguments,
                continuation,
                remainder,
                ..
            } => {
                result.extend(arguments.iter().map(|(s, _)| s).copied());

                stack.push(continuation);
                stack.push(remainder);
            }

            Switch {
                cond_symbol,
                branches,
                default_branch,
                ..
            } => {
                result.insert(*cond_symbol);

                stack.extend(branches.iter().map(|(_, s)| s));
                stack.push(default_branch);
            }

            Cond {
                cond_symbol,
                branching_symbol,
                pass,
                fail,
                ..
            } => {
                result.insert(*cond_symbol);
                result.insert(*branching_symbol);

                stack.push(pass);
                stack.push(fail);
            }

            RuntimeError(_) => {}
        }
    }

    (result, bound_variables)
}

pub fn occuring_variables_expr(expr: &Expr<'_>, result: &mut MutSet<Symbol>) {
    use crate::ir::CallType;
    use Expr::*;

    match expr {
        FunctionPointer(symbol, _)
        | Alias(symbol)
        | AccessAtIndex {
            structure: symbol, ..
        } => {
            result.insert(*symbol);
        }

        FunctionCall {
            call_type, args, ..
        } => {
            result.extend(args.iter().copied());

            match call_type {
                CallType::ByName(s) => result.insert(*s),
                CallType::ByPointer(s) => result.insert(*s),
            };
        }

        RunLowLevel(_, arguments)
        | Tag { arguments, .. }
        | Struct(arguments)
        | Array {
            elems: arguments, ..
        } => {
            result.extend(arguments.iter().copied());
        }

        EmptyArray | RuntimeErrorFunction(_) | Literal(_) => {}
    }
}
