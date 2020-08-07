use crate::ir::{Expr, Stmt};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::{MutMap, MutSet};
use roc_module::symbol::Symbol;

pub fn free_variables(stmt: &Stmt<'_>) -> MutSet<Symbol> {
    let (mut occuring, bound) = occuring_variables(stmt);

    for ref s in bound {
        occuring.remove(s);
    }

    occuring
}

pub fn occuring_variables(stmt: &Stmt<'_>) -> (MutSet<Symbol>, MutSet<Symbol>) {
    let mut stack = std::vec![stmt];
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

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Ownership {
    Owned,
    Borrowed,
}

struct Env<'a> {
    arena: &'a Bump,
    beta: MutMap<Symbol, &'a [Ownership]>,
    beta_l: MutMap<Symbol, Ownership>,
}

impl<'a> Env<'a> {
    fn ownership(&self, symbol: &Symbol) -> Ownership {
        // default to owned
        match self.beta_l.get(symbol) {
            None => Ownership::Owned,
            Some(o) => *o,
        }
    }

    fn borrow_signature(&self, symbol: &Symbol, arguments: &[Symbol]) -> &'a [(Symbol, Ownership)] {
        use Ownership::*;

        let signature = match self.beta.get(symbol) {
            None => &[] as &[_],
            Some(o) => o,
        };

        let mut result = Vec::with_capacity_in(arguments.len(), self.arena);

        for (i, arg) in arguments.iter().enumerate() {
            let ownership = match signature.get(i) {
                None => Owned,
                Some(o) => *o,
            };
            result.push((*arg, ownership));
        }

        result.into_bump_slice()
    }
}

fn function_o_minus_x<'a>(
    arena: &'a Bump,
    x: Symbol,
    f: &'a Stmt<'a>,
    ownership: Ownership,
) -> &'a Stmt<'a> {
    match ownership {
        Ownership::Owned if !free_variables(&f).contains(&x) => arena.alloc(Stmt::Dec(x, f)),
        _ => f,
    }
}

fn function_o_minus<'a>(arena: &'a Bump, xs: &[Symbol], mut f: &'a Stmt<'a>) -> &'a Stmt<'a> {
    for x in xs.iter() {
        f = function_o_minus_x(arena, *x, f, Ownership::Owned);
    }

    f
}

fn function_o_plus_x<'a>(
    arena: &'a Bump,
    x: Symbol,
    v: &MutSet<Symbol>,
    f: &'a Stmt<'a>,
    ownership: Ownership,
) -> &'a Stmt<'a> {
    match ownership {
        Ownership::Owned if !v.contains(&x) => f,
        _ => arena.alloc(Stmt::Inc(x, f)),
    }
}

fn function_c_app<'a>(
    arena: &'a Bump,
    arguments: &[(Symbol, Ownership)],
    stmt: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    use Ownership::*;
    use Stmt::*;

    match (arguments.get(0), stmt) {
        (Some((y, Owned)), Let(z, _, e, f)) => {
            let ybar = &arguments[1..];

            let mut v = free_variables(f);
            v.extend(ybar.iter().map(|(s, _)| s).copied());

            let rest = function_c_app(arena, ybar, stmt);
            function_o_plus_x(arena, *y, &v, rest, Owned)
        }
        (Some((y, Borrowed)), Let(z, l, e, f)) => {
            let ybar = &arguments[1..];

            let v = ybar.iter().map(|(s, _)| s).copied().collect::<MutSet<_>>();

            let rest = Stmt::Let(
                *z,
                l.clone(),
                e.clone(),
                function_o_minus_x(arena, *y, f, Owned),
            );

            function_c_app(arena, ybar, arena.alloc(rest))
        }
        _ => stmt,
    }
}

fn function_c<'a>(env: &mut Env<'a>, stmt: &'a Stmt<'a>) -> &'a Stmt<'a> {
    use Expr::*;
    use Ownership::*;
    use Stmt::*;

    let arena = env.arena;

    match stmt {
        Ret(x) => function_o_plus_x(arena, *x, &MutSet::default(), stmt, Owned),

        Cond {
            cond_symbol,
            cond_layout,
            branching_symbol,
            branching_layout,
            pass,
            fail,
            ret_layout,
        } => {
            let ybar: Vec<Symbol> = Vec::from_iter_in(
                std::iter::once(free_variables(pass))
                    .chain(std::iter::once(free_variables(fail)))
                    .flatten(),
                arena,
            );

            let new_pass = function_o_minus(arena, &ybar, function_c(env, pass));
            let new_fail = function_o_minus(arena, &ybar, function_c(env, fail));

            let cond = Cond {
                cond_symbol: *cond_symbol,
                cond_layout: cond_layout.clone(),
                branching_symbol: *branching_symbol,
                branching_layout: branching_layout.clone(),
                pass: new_pass,
                fail: new_fail,
                ret_layout: ret_layout.clone(),
            };

            arena.alloc(cond)
        }
        Switch {
            cond_symbol,
            branches,
            default_branch,
            cond_layout,
            ret_layout,
        } => {
            let ybar: Vec<Symbol> = Vec::from_iter_in(
                std::iter::once(free_variables(default_branch))
                    .chain(branches.iter().map(|(_, b)| free_variables(b)))
                    .flatten(),
                arena,
            );

            let new_default_branch =
                function_o_minus(arena, &ybar, function_c(env, default_branch));
            let new_branches: &'a [(u64, Stmt<'a>)] = Vec::from_iter_in(
                branches.iter().map(|(label, branch)| {
                    (
                        *label,
                        function_o_minus(arena, &ybar, function_c(env, branch)).clone(),
                    )
                }),
                arena,
            )
            .into_bump_slice();

            arena.alloc(Switch {
                cond_symbol: *cond_symbol,
                branches: new_branches,
                default_branch: new_default_branch,
                cond_layout: cond_layout.clone(),
                ret_layout: ret_layout.clone(),
            })
        }

        Let(y, e, l, f) => match e {
            AccessAtIndex { structure, .. } => match env.ownership(structure) {
                Owned => {
                    let foo = function_c(env, f);
                    let cont = function_o_minus_x(arena, *structure, arena.alloc(foo), Owned);
                    let rest = arena.alloc(Inc(*y, cont));

                    arena.alloc(Let(*y, e.clone(), l.clone(), rest))
                }
                Borrowed => {
                    let old_y = env.beta_l.insert(*y, Borrowed);

                    let rest = function_c(env, f);

                    match old_y {
                        Some(old) => env.beta_l.insert(*y, old),
                        None => env.beta_l.remove(y),
                    };

                    arena.alloc(Let(*y, e.clone(), l.clone(), rest))
                }
            },
            Tag { arguments, .. }
            | Struct(arguments)
            | Array {
                elems: arguments, ..
            } => {
                let rest = function_c(env, f);
                let let_stmt = arena.alloc(Let(*y, e.clone(), l.clone(), rest));

                let y_owned = Vec::from_iter_in(arguments.iter().map(|s| (*s, Owned)), arena);

                function_c_app(arena, &y_owned, let_stmt)
            }
            FunctionCall {
                call_type, args, ..
            } => {
                use crate::ir::CallType;

                let c = match call_type {
                    CallType::ByName(s) => s,
                    CallType::ByPointer(s) => s,
                };

                let rest = function_c(env, f);
                let let_stmt = arena.alloc(Let(*y, e.clone(), l.clone(), rest));

                let y_owned = env.borrow_signature(&c, args);

                function_c_app(arena, &y_owned, let_stmt)
            }
            _ => todo!(),
        },
        Inc(_, _) | Dec(_, _) => stmt,
        Join { .. } | Jump(_, _) => stmt,
        RuntimeError(_) => stmt,
    }
}
