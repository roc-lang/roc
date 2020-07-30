use crate::expr::Env;
use crate::expr::Expr;

use bumpalo::collections::Vec;
use roc_collections::all::MutSet;
use roc_module::symbol::Symbol;

/*
R : FnBodypure → FnBodyRC
R(let x = e; F ) = let x = e; R(F )
R(ret x) = ret x
R(case x of F ) = case x of D(x,ni
, R(Fi))
where ni = #fields of x in i-th branch
D : Var × N × FnBodyRC → FnBodyRC
D(z,n, case x of F ) = case x of D(z,n, F )
D(z,n, ret x) = ret x
D(z,n, let x = e; F ) = let x = e; D(z,n, F )
if z ∈ e or z ∈ F
D(z,n, F ) = let w = reset z; S(w,n, F )
otherwise, if S(w,n, F ) , F for a fresh w
D(z,n, F ) = F otherwise
S : Var × N × FnBodyRC → FnBodyRC
S(w,n, let x = ctori y; F ) = let x = reuse w in ctori y; F
if | y |= n
S(w,n, let x = e; F ) = let x = e; S(w,n, F ) otherwise
S(w,n, ret x) = ret x
S(w,n, case x of F ) = case x of S(w,n, F )


Maybe a : [ Nothing, Just a ]

map : Maybe a -> (a -> b) -> Maybe b
map = \maybe f ->
    when maybe is
        Nothing -> Nothing
        Just x -> Just (f x)


map : Maybe a -> (a -> b) -> Maybe b
map = \maybe f ->
    when maybe is
        Nothing ->
            let w = reset maybe
            let r = reuse w in AppliedTag("Nothing", vec![])
        Just x ->
            let v = f x
            let w = reset maybe
            let r = reuse w in AppliedTag("Just", vec![v])
*/

pub fn function_r<'a>(env: &mut Env<'a, '_>, body: &'a Expr<'a>) -> Expr<'a> {
    use Expr::*;

    match body {
        Switch {
            cond_symbol,
            branches,
            cond,
            cond_layout,
            default_branch,
            ret_layout,
        } => {
            let stack_size = cond_layout.stack_size(env.pointer_size);
            let mut new_branches = Vec::with_capacity_in(branches.len(), env.arena);

            for (tag, stores, branch) in branches.iter() {
                let new_branch = function_d(env, *cond_symbol, stack_size as _, branch);

                new_branches.push((*tag, *stores, new_branch));
            }

            let new_default_branch = (
                default_branch.0,
                &*env.arena.alloc(function_d(
                    env,
                    *cond_symbol,
                    stack_size as _,
                    default_branch.1,
                )),
            );

            Switch {
                cond_symbol: *cond_symbol,
                branches: new_branches.into_bump_slice(),
                default_branch: new_default_branch,
                ret_layout: ret_layout.clone(),
                cond: *cond,
                cond_layout: cond_layout.clone(),
            }
        }
        Cond {
            cond_symbol,
            cond_layout,
            branching_symbol,
            branching_layout,
            pass,
            fail,
            ret_layout,
        } => {
            let stack_size = cond_layout.stack_size(env.pointer_size);

            let new_pass = (
                pass.0,
                &*env
                    .arena
                    .alloc(function_d(env, *cond_symbol, stack_size as _, pass.1)),
            );

            let new_fail = (
                fail.0,
                &*env
                    .arena
                    .alloc(function_d(env, *cond_symbol, stack_size as _, fail.1)),
            );

            Cond {
                cond_symbol: *cond_symbol,
                cond_layout: cond_layout.clone(),
                branching_symbol: *branching_symbol,
                branching_layout: branching_layout.clone(),
                ret_layout: ret_layout.clone(),
                pass: new_pass,
                fail: new_fail,
            }
        }

        Store(stores, body) => {
            let new_body = function_r(env, body);

            Store(stores, env.arena.alloc(new_body))
        }

        DecAfter(symbol, body) => {
            let new_body = function_r(env, body);

            DecAfter(*symbol, env.arena.alloc(new_body))
        }

        CallByName { .. }
        | CallByPointer(_, _, _)
        | RunLowLevel(_, _)
        | Tag { .. }
        | Struct(_)
        | Array { .. }
        | AccessAtIndex { .. } => {
            // TODO
            // how often are `when` expressions in one of the above?
            body.clone()
        }

        Int(_)
        | Float(_)
        | Str(_)
        | Bool(_)
        | Byte(_)
        | Load(_)
        | EmptyArray
        | LoadWithoutIncrement(_)
        | FunctionPointer(_, _)
        | RuntimeError(_)
        | RuntimeErrorFunction(_) => body.clone(),

        Reset(_, _) | Reuse(_, _) => unreachable!("reset/reuse should not have been inserted yet!"),
    }
}

fn function_d<'a>(
    env: &mut Env<'a, '_>,
    z: Symbol,
    stack_size: usize,
    body: &'a Expr<'a>,
) -> Expr<'a> {
    let symbols = symbols_in_expr(body);
    if symbols.contains(&z) {
        return body.clone();
    }

    if let Ok(reused) = function_s(env, z, stack_size, body) {
        Expr::Reset(z, env.arena.alloc(reused))
    } else {
        body.clone()
    }
    /*
    match body {
        Expr::Tag { .. } => Some(env.arena.alloc(Expr::Reuse(w, body))),
        _ => None,
    }
    */
}

fn function_s<'a>(
    env: &mut Env<'a, '_>,
    w: Symbol,
    stack_size: usize,
    body: &'a Expr<'a>,
) -> Result<&'a Expr<'a>, &'a Expr<'a>> {
    use Expr::*;

    match body {
        Tag { tag_layout, .. } => {
            if tag_layout.stack_size(env.pointer_size) as usize <= stack_size {
                Ok(env.arena.alloc(Expr::Reuse(w, body)))
            } else {
                Err(body)
            }
        }

        Array { .. } | Struct(_) => {
            // TODO
            Err(body)
        }

        Switch {
            cond_symbol,
            branches,
            cond,
            cond_layout,
            default_branch,
            ret_layout,
        } => {
            // we can re-use `w` in each branch
            let mut has_been_reused = false;
            let mut new_branches = Vec::with_capacity_in(branches.len(), env.arena);
            for (tag, stores, branch) in branches.iter() {
                match function_s(env, *cond_symbol, stack_size as _, branch) {
                    Ok(new_branch) => {
                        has_been_reused = true;
                        new_branches.push((*tag, *stores, new_branch.clone()));
                    }
                    Err(new_branch) => {
                        new_branches.push((*tag, *stores, new_branch.clone()));
                    }
                }
            }

            let new_default_branch = (
                default_branch.0,
                match function_s(env, *cond_symbol, stack_size, default_branch.1) {
                    Ok(new) => {
                        has_been_reused = true;
                        new
                    }
                    Err(new) => new,
                },
            );
            let result = env.arena.alloc(Switch {
                cond_symbol: *cond_symbol,
                branches: new_branches.into_bump_slice(),
                default_branch: new_default_branch,
                ret_layout: ret_layout.clone(),
                cond: *cond,
                cond_layout: cond_layout.clone(),
            });

            if has_been_reused {
                Ok(result)
            } else {
                Err(result)
            }
        }

        Cond {
            cond_symbol,
            cond_layout,
            branching_symbol,
            branching_layout,
            pass,
            fail,
            ret_layout,
        } => {
            let mut has_been_reused = false;
            let new_pass = (
                pass.0,
                match function_s(env, *cond_symbol, stack_size, pass.1) {
                    Ok(new) => {
                        has_been_reused = true;
                        new
                    }
                    Err(new) => new,
                },
            );

            let new_fail = (
                fail.0,
                match function_s(env, *cond_symbol, stack_size, fail.1) {
                    Ok(new) => {
                        has_been_reused = true;
                        new
                    }
                    Err(new) => new,
                },
            );

            let result = env.arena.alloc(Cond {
                cond_symbol: *cond_symbol,
                cond_layout: cond_layout.clone(),
                branching_symbol: *branching_symbol,
                branching_layout: branching_layout.clone(),
                ret_layout: ret_layout.clone(),
                pass: new_pass,
                fail: new_fail,
            });

            if has_been_reused {
                Ok(result)
            } else {
                Err(result)
            }
        }

        Store(stores, expr) => {
            let new_expr = function_s(env, w, stack_size, expr)?;

            Ok(env.arena.alloc(Store(*stores, new_expr)))
        }

        DecAfter(symbol, expr) => {
            let new_expr = function_s(env, w, stack_size, expr)?;

            Ok(env.arena.alloc(DecAfter(*symbol, new_expr)))
        }

        CallByName { .. } | CallByPointer(_, _, _) | RunLowLevel(_, _) | AccessAtIndex { .. } => {
            // TODO
            // how often are `Tag` expressions in one of the above?
            Err(body)
        }

        Int(_)
        | Float(_)
        | Str(_)
        | Bool(_)
        | Byte(_)
        | Load(_)
        | EmptyArray
        | LoadWithoutIncrement(_)
        | FunctionPointer(_, _)
        | RuntimeError(_)
        | RuntimeErrorFunction(_) => Err(body),

        Reset(_, _) | Reuse(_, _) => {
            unreachable!("reset/reuse should not have been introduced yet")
        }
    }
}

fn symbols_in_expr<'a>(initial: &Expr<'a>) -> MutSet<Symbol> {
    use Expr::*;
    let mut result = MutSet::default();
    let mut stack = vec![initial];

    while let Some(expr) = stack.pop() {
        match expr {
            FunctionPointer(symbol, _) | LoadWithoutIncrement(symbol) | Load(symbol) => {
                result.insert(*symbol);
            }

            Reset(symbol, expr) | Reuse(symbol, expr) => {
                result.insert(*symbol);
                stack.push(expr)
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

                for (symbol, _, expr) in pass.0.iter() {
                    result.insert(*symbol);
                    stack.push(expr)
                }

                for (symbol, _, expr) in fail.0.iter() {
                    result.insert(*symbol);
                    stack.push(expr)
                }
            }

            Switch {
                cond,
                cond_symbol,
                branches,
                default_branch,
                ..
            } => {
                stack.push(cond);
                result.insert(*cond_symbol);

                for (_, stores, expr) in branches.iter() {
                    stack.push(expr);

                    for (symbol, _, expr) in stores.iter() {
                        result.insert(*symbol);
                        stack.push(expr)
                    }
                }

                stack.push(default_branch.1);
                for (symbol, _, expr) in default_branch.0.iter() {
                    result.insert(*symbol);
                    stack.push(expr)
                }
            }

            Store(stores, body) => {
                for (symbol, _, expr) in stores.iter() {
                    result.insert(*symbol);
                    stack.push(&expr)
                }

                stack.push(body)
            }

            DecAfter(symbol, body) => {
                result.insert(*symbol);
                stack.push(body);
            }

            CallByName { name, args, .. } => {
                result.insert(*name);
                for (expr, _) in args.iter() {
                    stack.push(expr);
                }
            }

            CallByPointer(function, args, _) => {
                stack.push(function);
                stack.extend(args.iter());
            }

            RunLowLevel(_, args) => {
                for (expr, _) in args.iter() {
                    stack.push(expr);
                }
            }

            Tag { arguments, .. } => {
                for (expr, _) in arguments.iter() {
                    stack.push(expr);
                }
            }

            Struct(arguments) => {
                for (expr, _) in arguments.iter() {
                    stack.push(expr);
                }
            }

            Array { elems, .. } => {
                for expr in elems.iter() {
                    stack.push(expr);
                }
            }

            AccessAtIndex { expr, .. } => {
                stack.push(expr);
            }

            Int(_)
            | Float(_)
            | Str(_)
            | Bool(_)
            | Byte(_)
            | EmptyArray
            | RuntimeError(_)
            | RuntimeErrorFunction(_) => {}
        }
    }

    result
}
