use crate::borrow::ParamMap;
use crate::ir::{Expr, JoinPointId, Param, Proc, Stmt};
use crate::layout::Layout;
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
                parameters,
                continuation,
                remainder,
                ..
            } => {
                result.extend(parameters.iter().map(|p| p.symbol));

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

/* Insert explicit RC instructions. So, it assumes the input code does not contain `inc` nor `dec` instructions.
   This transformation is applied before lower level optimizations
   that introduce the instructions `release` and `set`
*/

#[derive(Clone, Debug, Copy)]
struct VarInfo {
    reference: bool,  // true if the variable may be a reference (aka pointer) at runtime
    persistent: bool, // true if the variable is statically known to be marked a Persistent at runtime
    consume: bool,    // true if the variable RC must be "consumed"
}

type VarMap = MutMap<Symbol, VarInfo>;
type LiveVarSet = MutSet<Symbol>;
type JPLiveVarMap = MutMap<JoinPointId, LiveVarSet>;

#[derive(Clone, Debug)]
struct Context<'a> {
    arena: &'a Bump,
    vars: VarMap,
    jp_live_vars: JPLiveVarMap,      // map: join point => live variables
    local_context: LocalContext<'a>, // we use it to store the join point declarations
    param_map: &'a ParamMap<'a>,
}

fn update_live_vars<'a>(expr: &Expr<'a>, v: &LiveVarSet) -> LiveVarSet {
    let mut v = v.clone();

    occuring_variables_expr(expr, &mut v);

    v
}

/// `isFirstOcc xs x i = true` if `xs[i]` is the first occurrence of `xs[i]` in `xs`
fn is_first_occurence(xs: &[Symbol], i: usize) -> bool {
    match xs.get(i) {
        None => unreachable!(),
        Some(s) => i == xs.iter().position(|v| s == v).unwrap(),
    }
}

/// Return `n`, the number of times `x` is consumed.
/// - `ys` is a sequence of instruction parameters where we search for `x`.
/// - `consumeParamPred i = true` if parameter `i` is consumed.
fn get_num_consumptions<F>(x: Symbol, ys: &[Symbol], consume_param_pred: F) -> usize
where
    F: Fn(usize) -> bool,
{
    let mut n = 0;

    for (i, y) in ys.iter().enumerate() {
        if x == *y && consume_param_pred(i) {
            n += 1;
        }
    }
    n
}

/// Return true if `x` also occurs in `ys` in a position that is not consumed.
/// That is, it is also passed as a borrow reference.
fn is_borrow_param_help<F>(x: Symbol, ys: &[Symbol], consume_param_pred: F) -> bool
where
    F: Fn(usize) -> bool,
{
    ys.iter()
        .enumerate()
        .any(|(i, y)| x == *y && !consume_param_pred(i))
}

fn is_borrow_param(x: Symbol, ys: &[Symbol], ps: &[Param]) -> bool {
    // default to owned arguments
    let is_owned = |i: usize| match ps.get(i) {
        Some(param) => !param.borrow,
        None => unreachable!("or?"),
    };
    is_borrow_param_help(x, ys, is_owned)
}

// We do not need to consume the projection of a variable that is not consumed
fn consume_expr(m: &VarMap, e: &Expr<'_>) -> bool {
    match e {
        Expr::AccessAtIndex { structure: x, .. } => match m.get(x) {
            Some(info) => info.consume,
            None => true,
        },
        _ => true,
    }
}

impl<'a> Context<'a> {
    pub fn new(arena: &'a Bump, param_map: &'a ParamMap<'a>) -> Self {
        let mut vars = MutMap::default();

        for (key, _) in param_map.into_iter() {
            if let crate::borrow::Key::Declaration(symbol) = key {
                vars.insert(
                    *symbol,
                    VarInfo {
                        reference: false, // assume function symbols are global constants
                        persistent: true, // assume function symbols are global constants
                        consume: false,   // no need to consume this variable
                    },
                );
            }
        }

        Self {
            arena,
            vars,
            jp_live_vars: MutMap::default(),
            local_context: LocalContext::default(),
            param_map,
        }
    }

    fn get_var_info(&self, symbol: Symbol) -> VarInfo {
        match self.vars.get(&symbol) {
            Some(info) => *info,
            None => panic!(
                "Symbol {:?} {} has no info in {:?}",
                symbol, symbol, self.vars
            ),
        }
    }

    fn add_inc(&self, symbol: Symbol, stmt: &'a Stmt<'a>) -> &'a Stmt<'a> {
        let info = self.get_var_info(symbol);

        if info.persistent {
            // persistent values are never reference counted
            return stmt;
        }

        // if this symbol is never a reference, don't emit
        if !info.reference {
            return stmt;
        }

        self.arena.alloc(Stmt::Inc(symbol, stmt))
    }

    fn add_dec(&self, symbol: Symbol, stmt: &'a Stmt<'a>) -> &'a Stmt<'a> {
        let info = self.get_var_info(symbol);

        if info.persistent {
            // persistent values are never reference counted
            return stmt;
        }

        // if this symbol is never a reference, don't emit
        if !info.reference {
            return stmt;
        }

        self.arena.alloc(Stmt::Dec(symbol, stmt))
    }

    fn add_inc_before_consume_all(
        &self,
        xs: &[Symbol],
        b: &'a Stmt<'a>,
        live_vars_after: &LiveVarSet,
    ) -> &'a Stmt<'a> {
        self.add_inc_before_help(xs, |_: usize| true, b, live_vars_after)
    }

    fn add_inc_before_help<F>(
        &self,
        xs: &[Symbol],
        consume_param_pred: F,
        mut b: &'a Stmt<'a>,
        live_vars_after: &LiveVarSet,
    ) -> &'a Stmt<'a>
    where
        F: Fn(usize) -> bool + Clone,
    {
        for (i, x) in xs.iter().enumerate() {
            let info = self.get_var_info(*x);
            if !info.reference || !is_first_occurence(xs, i) {
                // do nothing
            } else {
                let num_consumptions = get_num_consumptions(*x, xs, consume_param_pred.clone()); // number of times the argument is used

                // `x` is not a variable that must be consumed by the current procedure
                let need_not_consume = !info.consume;

                // `x` is live after executing instruction
                let is_live_after = live_vars_after.contains(x);

                // `x` is used in a position that is passed as a borrow reference
                let is_borrowed = is_borrow_param_help(*x, xs, consume_param_pred.clone());

                let num_incs = if need_not_consume || is_live_after || is_borrowed {
                    num_consumptions
                } else {
                    num_consumptions - 1
                };

                // verify that this is indeed always 1
                debug_assert!(num_incs <= 1);

                if num_incs == 1 {
                    b = self.add_inc(*x, b)
                }
            }
        }
        b
    }

    fn add_inc_before(
        &self,
        xs: &[Symbol],
        ps: &[Param],
        b: &'a Stmt<'a>,
        live_vars_after: &LiveVarSet,
    ) -> &'a Stmt<'a> {
        // default to owned arguments
        let pred = |i: usize| match ps.get(i) {
            Some(param) => !param.borrow,
            None => unreachable!("or?"),
        };
        self.add_inc_before_help(xs, pred, b, live_vars_after)
    }

    fn add_dec_if_needed(
        &self,
        x: Symbol,
        b: &'a Stmt<'a>,
        b_live_vars: &LiveVarSet,
    ) -> &'a Stmt<'a> {
        if self.must_consume(x) && !b_live_vars.contains(&x) {
            self.add_dec(x, b)
        } else {
            b
        }
    }

    fn must_consume(&self, x: Symbol) -> bool {
        let info = self.get_var_info(x);
        info.reference && info.consume
    }

    fn add_dec_after_application(
        &self,
        xs: &[Symbol],
        ps: &[Param],
        mut b: &'a Stmt<'a>,
        b_live_vars: &LiveVarSet,
    ) -> &'a Stmt<'a> {
        for (i, x) in xs.iter().enumerate() {
            // We must add a `dec` if `x` must be consumed, it is alive after the application,
            // and it has been borrowed by the application.
            // Remark: `x` may occur multiple times in the application (e.g., `f x y x`).
            // This is why we check whether it is the first occurrence.
            if self.must_consume(*x)
                && is_first_occurence(xs, i)
                && is_borrow_param(*x, xs, ps)
                && !b_live_vars.contains(x)
            {
                b = self.add_dec(*x, b)
            }
        }

        b
    }

    fn add_dec_after_lowlevel(
        &self,
        xs: &[Symbol],
        ps: &[bool],
        mut b: &'a Stmt<'a>,
        b_live_vars: &LiveVarSet,
    ) -> &'a Stmt<'a> {
        for (i, (x, is_borrow)) in xs.iter().zip(ps.iter()).enumerate() {
            /* We must add a `dec` if `x` must be consumed, it is alive after the application,
            and it has been borrowed by the application.
            Remark: `x` may occur multiple times in the application (e.g., `f x y x`).
            This is why we check whether it is the first occurrence. */

            if self.must_consume(*x)
                && is_first_occurence(xs, i)
                && *is_borrow
                && !b_live_vars.contains(x)
            {
                b = self.add_dec(*x, b);
            }
        }

        b
    }

    #[allow(clippy::many_single_char_names)]
    fn visit_variable_declaration(
        &self,
        z: Symbol,
        v: Expr<'a>,
        l: Layout<'a>,
        b: &'a Stmt<'a>,
        b_live_vars: &LiveVarSet,
    ) -> (&'a Stmt<'a>, LiveVarSet) {
        use Expr::*;

        let mut live_vars = update_live_vars(&v, &b_live_vars);
        live_vars.remove(&z);

        let new_b = match v {
            Reuse { arguments: ys, .. }
            | Tag { arguments: ys, .. }
            | Struct(ys)
            | Array { elems: ys, .. } => self.add_inc_before_consume_all(
                ys,
                self.arena.alloc(Stmt::Let(z, v, l, b)),
                &b_live_vars,
            ),
            AccessAtIndex { structure: x, .. } => {
                let b = self.add_dec_if_needed(x, b, b_live_vars);
                let info_x = self.get_var_info(x);
                let b = if info_x.consume {
                    self.add_inc(z, b)
                } else {
                    b
                };

                self.arena.alloc(Stmt::Let(z, v, l, b))
            }

            RunLowLevel(op, args) => {
                let ps = crate::borrow::lowlevel_borrow_signature(self.arena, op);
                let b = self.add_dec_after_lowlevel(args, ps, b, b_live_vars);

                self.arena.alloc(Stmt::Let(z, v, l, b))
            }

            FunctionCall {
                args: ys,
                arg_layouts,
                call_type,
                ..
            } => {
                // get the borrow signature
                let ps = match self.param_map.get_symbol(call_type.get_inner()) {
                    Some(slice) => slice,
                    None => Vec::from_iter_in(
                        arg_layouts.iter().cloned().map(|layout| Param {
                            symbol: Symbol::UNDERSCORE,
                            borrow: false,
                            layout,
                        }),
                        self.arena,
                    )
                    .into_bump_slice(),
                };

                let b = self.add_dec_after_application(ys, ps, b, b_live_vars);
                let b = self.arena.alloc(Stmt::Let(z, v, l, b));

                self.add_inc_before(ys, ps, b, b_live_vars)
            }

            EmptyArray
            | FunctionPointer(_, _)
            | Literal(_)
            | Reset(_)
            | RuntimeErrorFunction(_) => {
                // EmptyArray is always stack-allocated
                // function pointers are persistent
                self.arena.alloc(Stmt::Let(z, v, l, b))
            }
        };

        (new_b, live_vars)
    }

    fn update_var_info(&self, symbol: Symbol, layout: &Layout<'a>, expr: &Expr<'a>) -> Self {
        let mut ctx = self.clone();

        // can this type be reference-counted at runtime?
        let reference = layout.contains_refcounted();

        // is this value a constant?
        // TODO do function pointers also fall into this category?
        let persistent = match expr {
            Expr::FunctionCall { args, .. } => args.is_empty(),
            _ => false,
        };

        // must this value be consumed?
        let consume = consume_expr(&ctx.vars, expr);

        let info = VarInfo {
            reference,
            persistent,
            consume,
        };

        ctx.vars.insert(symbol, info);

        ctx
    }

    fn update_var_info_with_params(&self, ps: &[Param]) -> Self {
        let mut ctx = self.clone();

        for p in ps.iter() {
            let info = VarInfo {
                reference: p.layout.contains_refcounted(),
                consume: !p.borrow,
                persistent: false,
            };
            ctx.vars.insert(p.symbol, info);
        }

        ctx
    }

    // Add `dec` instructions for parameters that are
    //
    //  - references
    //  - not alive in `b`
    //  - not borrow.
    //
    // That is, we must make sure these parameters are consumed.
    fn add_dec_for_dead_params(
        &self,
        ps: &[Param<'a>],
        mut b: &'a Stmt<'a>,
        b_live_vars: &LiveVarSet,
    ) -> &'a Stmt<'a> {
        for p in ps.iter() {
            if !p.borrow && p.layout.contains_refcounted() && !b_live_vars.contains(&p.symbol) {
                b = self.add_dec(p.symbol, b)
            }
        }

        b
    }

    fn add_dec_for_alt(
        &self,
        case_live_vars: &LiveVarSet,
        alt_live_vars: &LiveVarSet,
        mut b: &'a Stmt<'a>,
    ) -> &'a Stmt<'a> {
        for x in case_live_vars.iter() {
            if !alt_live_vars.contains(x) && self.must_consume(*x) {
                b = self.add_dec(*x, b);
            }
        }

        b
    }

    fn visit_stmt(&self, stmt: &'a Stmt<'a>) -> (&'a Stmt<'a>, LiveVarSet) {
        use Stmt::*;

        // let-chains can be very long, especially for large (list) literals
        // in (rust) debug mode, this function can overflow the stack for such values
        // so we have to write an explicit loop.
        {
            let mut cont = stmt;
            let mut triples = Vec::new_in(self.arena);
            while let Stmt::Let(symbol, expr, layout, new_cont) = cont {
                triples.push((symbol, expr, layout));
                cont = new_cont;
            }

            if !triples.is_empty() {
                let mut ctx = self.clone();
                for (symbol, expr, layout) in triples.iter() {
                    ctx = ctx.update_var_info(**symbol, layout, expr);
                }
                let (mut b, mut b_live_vars) = ctx.visit_stmt(cont);
                for (symbol, expr, layout) in triples.into_iter().rev() {
                    let pair = ctx.visit_variable_declaration(
                        *symbol,
                        (*expr).clone(),
                        (*layout).clone(),
                        b,
                        &b_live_vars,
                    );

                    b = pair.0;
                    b_live_vars = pair.1;
                }

                return (b, b_live_vars);
            }
        }

        match stmt {
            Let(symbol, expr, layout, cont) => {
                let ctx = self.update_var_info(*symbol, layout, expr);
                let (b, b_live_vars) = ctx.visit_stmt(cont);
                ctx.visit_variable_declaration(
                    *symbol,
                    expr.clone(),
                    layout.clone(),
                    b,
                    &b_live_vars,
                )
            }

            Join {
                id: j,
                parameters: _,
                remainder: b,
                continuation: v,
            } => {
                // get the parameters with borrow signature
                let xs = self.param_map.get_join_point(*j);

                let (v, v_live_vars) = {
                    let ctx = self.update_var_info_with_params(xs);
                    ctx.visit_stmt(v)
                };

                let mut ctx = self.clone();
                let v = ctx.add_dec_for_dead_params(xs, v, &v_live_vars);

                update_jp_live_vars(*j, xs, v, &mut ctx.jp_live_vars);

                let (b, b_live_vars) = ctx.visit_stmt(b);

                (
                    ctx.arena.alloc(Join {
                        id: *j,
                        parameters: xs,
                        remainder: b,
                        continuation: v,
                    }),
                    b_live_vars,
                )
            }

            Ret(x) => {
                let info = self.get_var_info(*x);

                let mut live_vars = MutSet::default();
                live_vars.insert(*x);

                if info.reference && !info.consume {
                    (self.add_inc(*x, stmt), live_vars)
                } else {
                    (stmt, live_vars)
                }
            }

            Jump(j, xs) => {
                let empty = MutSet::default();
                let j_live_vars = match self.jp_live_vars.get(j) {
                    Some(vars) => vars,
                    None => &empty,
                };
                // TODO use borrow signature here?
                let ps = self.param_map.get_join_point(*j);
                // let ps = self.local_context.join_points.get(j).unwrap().0;

                let b = self.add_inc_before(xs, ps, stmt, j_live_vars);

                let b_live_vars = collect_stmt(b, &self.jp_live_vars, MutSet::default());

                (b, b_live_vars)
            }

            Cond {
                pass,
                fail,
                cond_symbol,
                cond_layout,
                branching_symbol,
                branching_layout,
                ret_layout,
            } => {
                let case_live_vars = collect_stmt(stmt, &self.jp_live_vars, MutSet::default());

                let pass = {
                    // TODO should we use ctor info like Lean?
                    let ctx = self.clone();
                    let (b, alt_live_vars) = ctx.visit_stmt(pass);
                    ctx.add_dec_for_alt(&case_live_vars, &alt_live_vars, b)
                };

                let fail = {
                    // TODO should we use ctor info like Lean?
                    let ctx = self.clone();
                    let (b, alt_live_vars) = ctx.visit_stmt(fail);
                    ctx.add_dec_for_alt(&case_live_vars, &alt_live_vars, b)
                };

                let cond = self.arena.alloc(Cond {
                    cond_symbol: *cond_symbol,
                    cond_layout: cond_layout.clone(),
                    branching_symbol: *branching_symbol,
                    branching_layout: branching_layout.clone(),
                    pass,
                    fail,
                    ret_layout: ret_layout.clone(),
                });

                (cond, case_live_vars)
            }

            Switch {
                cond_symbol,
                cond_layout,
                branches,
                default_branch,
                ret_layout,
            } => {
                let case_live_vars = collect_stmt(stmt, &self.jp_live_vars, MutSet::default());

                let branches = Vec::from_iter_in(
                    branches.iter().map(|(label, branch)| {
                        // TODO should we use ctor info like Lean?
                        let ctx = self.clone();
                        let (b, alt_live_vars) = ctx.visit_stmt(branch);
                        let b = ctx.add_dec_for_alt(&case_live_vars, &alt_live_vars, b);

                        (*label, b.clone())
                    }),
                    self.arena,
                )
                .into_bump_slice();

                let default_branch = {
                    // TODO should we use ctor info like Lean?
                    let ctx = self.clone();
                    let (b, alt_live_vars) = ctx.visit_stmt(default_branch);
                    ctx.add_dec_for_alt(&case_live_vars, &alt_live_vars, b)
                };

                let switch = self.arena.alloc(Switch {
                    cond_symbol: *cond_symbol,
                    branches,
                    default_branch,
                    cond_layout: cond_layout.clone(),
                    ret_layout: ret_layout.clone(),
                });

                (switch, case_live_vars)
            }

            RuntimeError(_) | Inc(_, _) | Dec(_, _) => (stmt, MutSet::default()),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct LocalContext<'a> {
    join_points: MutMap<JoinPointId, (&'a [Param<'a>], &'a Stmt<'a>)>,
}

pub fn collect_stmt(
    stmt: &Stmt<'_>,
    jp_live_vars: &JPLiveVarMap,
    mut vars: LiveVarSet,
) -> LiveVarSet {
    use Stmt::*;

    match stmt {
        Let(symbol, expr, _, cont) => {
            vars = collect_stmt(cont, jp_live_vars, vars);
            vars.remove(symbol);
            let mut result = MutSet::default();
            occuring_variables_expr(expr, &mut result);
            vars.extend(result);

            vars
        }
        Ret(symbol) => {
            vars.insert(*symbol);
            vars
        }

        Inc(symbol, cont) | Dec(symbol, cont) => {
            vars.insert(*symbol);
            collect_stmt(cont, jp_live_vars, vars)
        }

        Jump(id, arguments) => {
            vars.extend(arguments.iter().copied());

            // NOTE deviation from Lean
            // we fall through when no join point is available
            if let Some(jvars) = jp_live_vars.get(id) {
                vars.extend(jvars);
            }

            vars
        }

        Join {
            id: j,
            parameters,
            remainder: b,
            continuation: v,
        } => {
            let mut j_live_vars = collect_stmt(v, jp_live_vars, MutSet::default());
            for param in parameters.iter() {
                j_live_vars.remove(&param.symbol);
            }

            let mut jp_live_vars = jp_live_vars.clone();
            jp_live_vars.insert(*j, j_live_vars);

            collect_stmt(b, &jp_live_vars, vars)
        }

        Switch {
            cond_symbol,
            branches,
            default_branch,
            ..
        } => {
            vars.insert(*cond_symbol);

            for (_, branch) in branches.iter() {
                vars.extend(collect_stmt(branch, jp_live_vars, vars.clone()));
            }

            vars.extend(collect_stmt(default_branch, jp_live_vars, vars.clone()));

            vars
        }

        Cond {
            cond_symbol,
            branching_symbol,
            pass,
            fail,
            ..
        } => {
            vars.insert(*cond_symbol);
            vars.insert(*branching_symbol);

            vars.extend(collect_stmt(pass, jp_live_vars, vars.clone()));
            vars.extend(collect_stmt(fail, jp_live_vars, vars.clone()));

            vars
        }

        RuntimeError(_) => vars,
    }
}

fn update_jp_live_vars(j: JoinPointId, ys: &[Param], v: &Stmt<'_>, m: &mut JPLiveVarMap) {
    let j_live_vars = MutSet::default();
    let mut j_live_vars = collect_stmt(v, m, j_live_vars);

    for param in ys {
        j_live_vars.remove(&param.symbol);
    }

    m.insert(j, j_live_vars);
}

/// used to process the main function in the repl
pub fn visit_declaration<'a>(
    arena: &'a Bump,
    param_map: &'a ParamMap<'a>,
    stmt: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    let ctx = Context::new(arena, param_map);

    let params = &[] as &[_];
    let ctx = ctx.update_var_info_with_params(params);
    let (b, b_live_vars) = ctx.visit_stmt(stmt);
    ctx.add_dec_for_dead_params(params, b, &b_live_vars)
}

pub fn visit_proc<'a>(arena: &'a Bump, param_map: &'a ParamMap<'a>, proc: &mut Proc<'a>) {
    let ctx = Context::new(arena, param_map);

    let params = match param_map.get_symbol(proc.name) {
        Some(slice) => slice,
        None => Vec::from_iter_in(
            proc.args.iter().cloned().map(|(layout, symbol)| Param {
                symbol,
                borrow: false,
                layout,
            }),
            arena,
        )
        .into_bump_slice(),
    };

    let stmt = arena.alloc(proc.body.clone());
    let ctx = ctx.update_var_info_with_params(params);
    let (b, b_live_vars) = ctx.visit_stmt(stmt);
    let b = ctx.add_dec_for_dead_params(params, b, &b_live_vars);

    proc.body = b.clone();
}
