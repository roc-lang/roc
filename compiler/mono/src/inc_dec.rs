use crate::borrow::{ParamMap, BORROWED, OWNED};
use crate::ir::{
    CallType, Expr, HigherOrderLowLevel, JoinPointId, ModifyRc, Param, Proc, ProcLayout, Stmt,
};
use crate::layout::Layout;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::{MutMap, MutSet};
use roc_module::symbol::Symbol;

pub fn free_variables(stmt: &Stmt<'_>) -> MutSet<Symbol> {
    let (mut occurring, bound) = occurring_variables(stmt);

    for ref s in bound {
        occurring.remove(s);
    }

    occurring
}

pub fn occurring_variables(stmt: &Stmt<'_>) -> (MutSet<Symbol>, MutSet<Symbol>) {
    let mut stack = std::vec![stmt];
    let mut result = MutSet::default();
    let mut bound_variables = MutSet::default();

    while let Some(stmt) = stack.pop() {
        use Stmt::*;

        match stmt {
            Let(symbol, expr, _, cont) => {
                occurring_variables_expr(expr, &mut result);
                result.insert(*symbol);
                bound_variables.insert(*symbol);
                stack.push(cont);
            }

            Ret(symbol) => {
                result.insert(*symbol);
            }

            Refcounting(modify, cont) => {
                let symbol = modify.get_symbol();
                result.insert(symbol);
                stack.push(cont);
            }

            Jump(_, arguments) => {
                result.extend(arguments.iter().copied());
            }

            Join {
                parameters,
                body: continuation,
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

                stack.extend(branches.iter().map(|(_, _, s)| s));
                stack.push(default_branch.1);
            }

            RuntimeError(_) => {}
        }
    }

    (result, bound_variables)
}

fn occurring_variables_call(call: &crate::ir::Call<'_>, result: &mut MutSet<Symbol>) {
    // NOTE though the function name does occur, it is a static constant in the program
    // for liveness, it should not be included here.
    result.extend(call.arguments.iter().copied());
}

pub fn occurring_variables_expr(expr: &Expr<'_>, result: &mut MutSet<Symbol>) {
    use Expr::*;

    match expr {
        StructAtIndex {
            structure: symbol, ..
        } => {
            result.insert(*symbol);
        }

        Call(call) => occurring_variables_call(call, result),

        Array {
            elems: arguments, ..
        } => result.extend(arguments.iter().filter_map(|e| e.to_symbol())),

        Tag { arguments, .. } | Struct(arguments) => {
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

        EmptyArray | RuntimeErrorFunction(_) | Literal(_) => {}

        GetTagId {
            structure: symbol, ..
        } => {
            result.insert(*symbol);
        }

        UnionAtIndex {
            structure: symbol, ..
        } => {
            result.insert(*symbol);
        }
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
    reset: bool,      // true if the variable is the result of a Reset operation
}

type VarMap = MutMap<Symbol, VarInfo>;
pub type LiveVarSet = MutSet<Symbol>;
pub type JPLiveVarMap = MutMap<JoinPointId, LiveVarSet>;

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

    occurring_variables_expr(expr, &mut v);

    v
}

/// `isFirstOcc xs x i = true` if `xs[i]` is the first occurrence of `xs[i]` in `xs`
fn is_first_occurrence(xs: &[Symbol], i: usize) -> bool {
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
        Expr::StructAtIndex { structure: x, .. } => match m.get(x) {
            Some(info) => info.consume,
            None => true,
        },
        Expr::UnionAtIndex { structure: x, .. } => match m.get(x) {
            Some(info) => info.consume,
            None => true,
        },
        _ => true,
    }
}

impl<'a> Context<'a> {
    pub fn new(arena: &'a Bump, param_map: &'a ParamMap<'a>) -> Self {
        let mut vars = MutMap::default();

        for symbol in param_map.iter_symbols() {
            vars.insert(
                *symbol,
                VarInfo {
                    reference: false, // assume function symbols are global constants
                    persistent: true, // assume function symbols are global constants
                    consume: false,   // no need to consume this variable
                    reset: false,     // reset symbols cannot be passed as function arguments
                },
            );
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
            None => {
                eprintln!(
                    "Symbol {:?} {} has no info in self.vars",
                    symbol,
                    symbol, // self.vars
                );

                VarInfo {
                    persistent: true,
                    reference: false,
                    consume: false,
                    reset: false,
                }
            }
        }
    }

    fn add_inc(&self, symbol: Symbol, inc_amount: u64, stmt: &'a Stmt<'a>) -> &'a Stmt<'a> {
        debug_assert!(inc_amount > 0);

        let info = self.get_var_info(symbol);

        if info.persistent {
            // persistent values are never reference counted
            return stmt;
        }

        // if this symbol is never a reference, don't emit
        if !info.reference {
            return stmt;
        }

        let modify = ModifyRc::Inc(symbol, inc_amount);
        self.arena.alloc(Stmt::Refcounting(modify, stmt))
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

        let modify = if info.reset {
            ModifyRc::DecRef(symbol)
        } else {
            ModifyRc::Dec(symbol)
        };

        self.arena.alloc(Stmt::Refcounting(modify, stmt))
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
            if !info.reference || !is_first_occurrence(xs, i) {
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

                if num_incs >= 1 {
                    b = self.add_inc(*x, num_incs as u64, b)
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
                && is_first_occurrence(xs, i)
                && is_borrow_param(*x, xs, ps)
                && !b_live_vars.contains(x)
            {
                b = self.add_dec(*x, b);
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
                && is_first_occurrence(xs, i)
                && *is_borrow
                && !b_live_vars.contains(x)
            {
                b = self.add_dec(*x, b);
            }
        }

        b
    }

    fn visit_call(
        &self,
        z: Symbol,
        call_type: crate::ir::CallType<'a>,
        arguments: &'a [Symbol],
        l: Layout<'a>,
        b: &'a Stmt<'a>,
        b_live_vars: &LiveVarSet,
    ) -> &'a Stmt<'a> {
        use crate::ir::CallType::*;

        match &call_type {
            LowLevel { op, .. } => {
                let ps = crate::borrow::lowlevel_borrow_signature(self.arena, *op);
                let b = self.add_dec_after_lowlevel(arguments, ps, b, b_live_vars);

                let v = Expr::Call(crate::ir::Call {
                    call_type,
                    arguments,
                });

                &*self.arena.alloc(Stmt::Let(z, v, l, b))
            }

            HigherOrder(HigherOrderLowLevel {
                op,
                closure_env_layout,
                specialization_id,
                update_mode,
                arg_layouts,
                ret_layout,
                function_name,
                function_env,
                ..
            }) => {
                // setup
                use crate::low_level::HigherOrder::*;

                macro_rules! create_call {
                    ($borrows:expr) => {
                        Expr::Call(crate::ir::Call {
                            call_type: if let Some(OWNED) = $borrows.map(|p| p.borrow) {
                                let higher_order = HigherOrderLowLevel {
                                    op: *op,
                                    closure_env_layout: *closure_env_layout,
                                    function_owns_closure_data: true,
                                    specialization_id: *specialization_id,
                                    update_mode: *update_mode,
                                    function_name: *function_name,
                                    function_env: *function_env,
                                    arg_layouts,
                                    ret_layout: *ret_layout,
                                };

                                CallType::HigherOrder(self.arena.alloc(higher_order))
                            } else {
                                call_type
                            },
                            arguments,
                        })
                    };
                }

                macro_rules! decref_if_owned {
                    ($borrows:expr, $argument:expr, $stmt:expr) => {
                        if !$borrows {
                            self.arena.alloc(Stmt::Refcounting(
                                ModifyRc::DecRef($argument),
                                self.arena.alloc($stmt),
                            ))
                        } else {
                            $stmt
                        }
                    };
                }

                const FUNCTION: bool = BORROWED;
                const CLOSURE_DATA: bool = BORROWED;

                let function_layout = ProcLayout {
                    arguments: arg_layouts,
                    result: *ret_layout,
                };

                let function_ps = match self.param_map.get_symbol(*function_name, function_layout) {
                    Some(function_ps) => function_ps,
                    None => unreachable!(),
                };

                match op {
                    ListMap { xs }
                    | ListKeepIf { xs }
                    | ListKeepOks { xs }
                    | ListKeepErrs { xs }
                    | ListAny { xs }
                    | ListFindUnsafe { xs } => {
                        let borrows = [function_ps[0].borrow, FUNCTION, CLOSURE_DATA];

                        let b = self.add_dec_after_lowlevel(arguments, &borrows, b, b_live_vars);

                        // if the list is owned, then all elements have been consumed, but not the list itself
                        let b = decref_if_owned!(function_ps[0].borrow, *xs, b);

                        let v = create_call!(function_ps.get(1));

                        &*self.arena.alloc(Stmt::Let(z, v, l, b))
                    }
                    ListMap2 { xs, ys } => {
                        let borrows = [
                            function_ps[0].borrow,
                            function_ps[1].borrow,
                            FUNCTION,
                            CLOSURE_DATA,
                        ];

                        let b = self.add_dec_after_lowlevel(arguments, &borrows, b, b_live_vars);

                        let b = decref_if_owned!(function_ps[0].borrow, *xs, b);
                        let b = decref_if_owned!(function_ps[1].borrow, *ys, b);

                        let v = create_call!(function_ps.get(2));

                        &*self.arena.alloc(Stmt::Let(z, v, l, b))
                    }
                    ListMap3 { xs, ys, zs } => {
                        let borrows = [
                            function_ps[0].borrow,
                            function_ps[1].borrow,
                            function_ps[2].borrow,
                            FUNCTION,
                            CLOSURE_DATA,
                        ];

                        let b = self.add_dec_after_lowlevel(arguments, &borrows, b, b_live_vars);

                        let b = decref_if_owned!(function_ps[0].borrow, *xs, b);
                        let b = decref_if_owned!(function_ps[1].borrow, *ys, b);
                        let b = decref_if_owned!(function_ps[2].borrow, *zs, b);

                        let v = create_call!(function_ps.get(3));

                        &*self.arena.alloc(Stmt::Let(z, v, l, b))
                    }
                    ListMap4 { xs, ys, zs, ws } => {
                        let borrows = [
                            function_ps[0].borrow,
                            function_ps[1].borrow,
                            function_ps[2].borrow,
                            function_ps[3].borrow,
                            FUNCTION,
                            CLOSURE_DATA,
                        ];

                        let b = self.add_dec_after_lowlevel(arguments, &borrows, b, b_live_vars);

                        let b = decref_if_owned!(function_ps[0].borrow, *xs, b);
                        let b = decref_if_owned!(function_ps[1].borrow, *ys, b);
                        let b = decref_if_owned!(function_ps[2].borrow, *zs, b);
                        let b = decref_if_owned!(function_ps[3].borrow, *ws, b);

                        let v = create_call!(function_ps.get(3));

                        &*self.arena.alloc(Stmt::Let(z, v, l, b))
                    }
                    ListMapWithIndex { xs } => {
                        let borrows = [function_ps[1].borrow, FUNCTION, CLOSURE_DATA];

                        let b = self.add_dec_after_lowlevel(arguments, &borrows, b, b_live_vars);

                        let b = decref_if_owned!(function_ps[1].borrow, *xs, b);

                        let v = create_call!(function_ps.get(2));

                        &*self.arena.alloc(Stmt::Let(z, v, l, b))
                    }
                    ListSortWith { xs: _ } => {
                        let borrows = [OWNED, FUNCTION, CLOSURE_DATA];

                        let b = self.add_dec_after_lowlevel(arguments, &borrows, b, b_live_vars);

                        let v = create_call!(function_ps.get(2));

                        &*self.arena.alloc(Stmt::Let(z, v, l, b))
                    }
                    ListWalk { xs, state: _ }
                    | ListWalkUntil { xs, state: _ }
                    | ListWalkBackwards { xs, state: _ }
                    | DictWalk { xs, state: _ } => {
                        // borrow data structure based on first argument of the folded function
                        // borrow the default based on second argument of the folded function
                        let borrows = [
                            function_ps[1].borrow,
                            function_ps[0].borrow,
                            FUNCTION,
                            CLOSURE_DATA,
                        ];

                        let b = self.add_dec_after_lowlevel(arguments, &borrows, b, b_live_vars);

                        let b = decref_if_owned!(function_ps[1].borrow, *xs, b);

                        let v = create_call!(function_ps.get(2));

                        &*self.arena.alloc(Stmt::Let(z, v, l, b))
                    }
                }
            }

            Foreign { .. } => {
                let ps = crate::borrow::foreign_borrow_signature(self.arena, arguments.len());
                let b = self.add_dec_after_lowlevel(arguments, ps, b, b_live_vars);

                let v = Expr::Call(crate::ir::Call {
                    call_type,
                    arguments,
                });

                &*self.arena.alloc(Stmt::Let(z, v, l, b))
            }

            ByName {
                name,
                ret_layout,
                arg_layouts,
                ..
            } => {
                let top_level = ProcLayout::new(self.arena, arg_layouts, **ret_layout);

                // get the borrow signature
                let ps = self
                    .param_map
                    .get_symbol(*name, top_level)
                    .expect("function is defined");

                let v = Expr::Call(crate::ir::Call {
                    call_type,
                    arguments,
                });

                let b = self.add_dec_after_application(arguments, ps, b, b_live_vars);
                let b = self.arena.alloc(Stmt::Let(z, v, l, b));

                self.add_inc_before(arguments, ps, b, b_live_vars)
            }
        }
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

        let mut live_vars = update_live_vars(&v, b_live_vars);
        live_vars.remove(&z);

        let new_b = match v {
            Reuse { arguments: ys, .. } | Tag { arguments: ys, .. } | Struct(ys) => self
                .add_inc_before_consume_all(
                    ys,
                    self.arena.alloc(Stmt::Let(z, v, l, b)),
                    b_live_vars,
                ),

            Array { elems, .. } => {
                let ys = Vec::from_iter_in(elems.iter().filter_map(|e| e.to_symbol()), self.arena);
                self.add_inc_before_consume_all(
                    &ys,
                    self.arena.alloc(Stmt::Let(z, v, l, b)),
                    b_live_vars,
                )
            }

            Call(crate::ir::Call {
                call_type,
                arguments,
            }) => self.visit_call(z, call_type, arguments, l, b, b_live_vars),

            StructAtIndex { structure: x, .. } => {
                let b = self.add_dec_if_needed(x, b, b_live_vars);
                let info_x = self.get_var_info(x);
                let b = if info_x.consume {
                    self.add_inc(z, 1, b)
                } else {
                    b
                };

                self.arena.alloc(Stmt::Let(z, v, l, b))
            }

            GetTagId { structure: x, .. } => {
                let b = self.add_dec_if_needed(x, b, b_live_vars);
                let info_x = self.get_var_info(x);
                let b = if info_x.consume {
                    self.add_inc(z, 1, b)
                } else {
                    b
                };

                self.arena.alloc(Stmt::Let(z, v, l, b))
            }

            UnionAtIndex { structure: x, .. } => {
                let b = self.add_dec_if_needed(x, b, b_live_vars);
                let info_x = self.get_var_info(x);
                let b = if info_x.consume {
                    self.add_inc(z, 1, b)
                } else {
                    b
                };

                self.arena.alloc(Stmt::Let(z, v, l, b))
            }

            EmptyArray | Literal(_) | Reset(_) | RuntimeErrorFunction(_) => {
                // EmptyArray is always stack-allocated
                // function pointers are persistent
                self.arena.alloc(Stmt::Let(z, v, l, b))
            }
        };

        (new_b, live_vars)
    }

    fn update_var_info(&self, symbol: Symbol, layout: &Layout<'a>, expr: &Expr<'a>) -> Self {
        // is this value a constant?
        // TODO do function pointers also fall into this category?
        let persistent = false;

        // must this value be consumed?
        let consume = consume_expr(&self.vars, expr);

        let reset = matches!(expr, Expr::Reset(_));

        self.update_var_info_help(symbol, layout, persistent, consume, reset)
    }

    fn update_var_info_help(
        &self,
        symbol: Symbol,
        layout: &Layout<'a>,
        persistent: bool,
        consume: bool,
        reset: bool,
    ) -> Self {
        // should we perform incs and decs on this value?
        let reference = layout.contains_refcounted();

        let info = VarInfo {
            reference,
            persistent,
            consume,
            reset,
        };

        let mut ctx = self.clone();

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
                reset: false,
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
                        *layout,
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
                ctx.visit_variable_declaration(*symbol, expr.clone(), *layout, b, &b_live_vars)
            }

            Join {
                id: j,
                parameters: _,
                remainder: b,
                body: v,
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
                        body: v,
                    }),
                    b_live_vars,
                )
            }

            Ret(x) => {
                let info = self.get_var_info(*x);

                let mut live_vars = MutSet::default();
                live_vars.insert(*x);

                if info.reference && !info.consume {
                    (self.add_inc(*x, 1, stmt), live_vars)
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

            Switch {
                cond_symbol,
                cond_layout,
                branches,
                default_branch,
                ret_layout,
            } => {
                let case_live_vars = collect_stmt(stmt, &self.jp_live_vars, MutSet::default());

                let branches = Vec::from_iter_in(
                    branches.iter().map(|(label, info, branch)| {
                        // TODO should we use ctor info like Lean?
                        let ctx = self.clone();
                        let (b, alt_live_vars) = ctx.visit_stmt(branch);
                        let b = ctx.add_dec_for_alt(&case_live_vars, &alt_live_vars, b);

                        (*label, info.clone(), b.clone())
                    }),
                    self.arena,
                )
                .into_bump_slice();

                let default_branch = {
                    // TODO should we use ctor info like Lean?
                    let ctx = self.clone();
                    let (b, alt_live_vars) = ctx.visit_stmt(default_branch.1);

                    (
                        default_branch.0.clone(),
                        ctx.add_dec_for_alt(&case_live_vars, &alt_live_vars, b),
                    )
                };

                let switch = self.arena.alloc(Switch {
                    cond_symbol: *cond_symbol,
                    branches,
                    default_branch,
                    cond_layout: *cond_layout,
                    ret_layout: *ret_layout,
                });

                (switch, case_live_vars)
            }

            RuntimeError(_) | Refcounting(_, _) => (stmt, MutSet::default()),
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
            occurring_variables_expr(expr, &mut result);
            vars.extend(result);

            vars
        }

        Ret(symbol) => {
            vars.insert(*symbol);
            vars
        }

        Refcounting(modify, cont) => {
            let symbol = modify.get_symbol();
            vars.insert(symbol);
            collect_stmt(cont, jp_live_vars, vars)
        }

        Join {
            id: j,
            parameters,
            remainder: b,
            body: v,
        } => {
            let mut j_live_vars = collect_stmt(v, jp_live_vars, MutSet::default());
            for param in parameters.iter() {
                j_live_vars.remove(&param.symbol);
            }

            let mut jp_live_vars = jp_live_vars.clone();
            jp_live_vars.insert(*j, j_live_vars);

            collect_stmt(b, &jp_live_vars, vars)
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

        Switch {
            cond_symbol,
            branches,
            default_branch,
            ..
        } => {
            vars.insert(*cond_symbol);

            for (_, _info, branch) in branches.iter() {
                vars.extend(collect_stmt(branch, jp_live_vars, vars.clone()));
            }

            vars.extend(collect_stmt(default_branch.1, jp_live_vars, vars.clone()));

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

pub fn visit_procs<'a>(
    arena: &'a Bump,
    param_map: &'a ParamMap<'a>,
    procs: &mut MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) {
    let ctx = Context::new(arena, param_map);

    for (key, proc) in procs.iter_mut() {
        visit_proc(arena, param_map, &ctx, proc, key.1);
    }
}

fn visit_proc<'a>(
    arena: &'a Bump,
    param_map: &'a ParamMap<'a>,
    ctx: &Context<'a>,
    proc: &mut Proc<'a>,
    layout: ProcLayout<'a>,
) {
    let params = match param_map.get_symbol(proc.name, layout) {
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
