use crate::borrow::{ParamMap, BORROWED, OWNED};
use crate::ir::{
    BranchInfo, CallType, Expr, HigherOrderLowLevel, JoinPointId, ModifyRc, Param, Proc,
    ProcLayout, Stmt,
};
use crate::layout::Layout;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::{MutMap, MutSet};
use roc_module::symbol::{IdentIds, ModuleId, Symbol};

/// Data and Function ownership relation
///
/// Normally, the borrowing algorithm figures out how to own/borrow data so that
/// the borrows match up. But that fails for our higher-order lowlevels, because
/// they are rigid (cannot add extra inc/dec in there dynamically) and opaque to
/// the borrow inference.
///
/// So, we must fix this up ourselves. This code is deliberately verbose to make
/// it easier to understand without full context.
#[allow(clippy::enum_variant_names)]
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
enum DataFunction {
    DataOwnedFunctionOwns,
    DataOwnedFunctionBorrows,
    DataBorrowedFunctionBorrows,
    DataBorrowedFunctionOwns,
}

impl DataFunction {
    fn new(vars: &VarMap, lowlevel_argument: Symbol, passed_function_argument: Param) -> Self {
        use DataFunction::*;

        let data_borrowed = !vars[&lowlevel_argument].consume;
        let function_borrows = passed_function_argument.borrow;

        match (data_borrowed, function_borrows) {
            (BORROWED, BORROWED) => DataBorrowedFunctionBorrows,
            (BORROWED, OWNED) => DataBorrowedFunctionOwns,
            (OWNED, BORROWED) => DataOwnedFunctionBorrows,
            (OWNED, OWNED) => DataOwnedFunctionOwns,
        }
    }
}

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
        Reset { symbol: x, .. } => {
            result.insert(*x);
        }

        ExprBox { symbol } | ExprUnbox { symbol } => {
            result.insert(*symbol);
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
    jp_live_vars: JPLiveVarMap, // map: join point => live variables
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

    fn visit_call<'i>(
        &self,
        home: ModuleId,
        ident_ids: &'i mut IdentIds,
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

            HigherOrder(lowlevel) => self.visit_higher_order_lowlevel(
                home,
                ident_ids,
                z,
                lowlevel,
                arguments,
                l,
                b,
                b_live_vars,
            ),

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

    fn visit_higher_order_lowlevel<'i>(
        &self,
        home: ModuleId,
        ident_ids: &'i mut IdentIds,
        z: Symbol,
        lowlevel: &'a crate::ir::HigherOrderLowLevel,
        arguments: &'a [Symbol],
        l: Layout<'a>,
        b: &'a Stmt<'a>,
        b_live_vars: &LiveVarSet,
    ) -> &'a Stmt<'a> {
        use crate::low_level::HigherOrder::*;
        use DataFunction::*;

        let HigherOrderLowLevel {
            op,
            passed_function,
            ..
        } = lowlevel;

        macro_rules! create_call {
            ($borrows:expr) => {
                create_holl_call(self.arena, lowlevel, $borrows, arguments)
            };
        }

        const FUNCTION: bool = BORROWED;
        const CLOSURE_DATA: bool = BORROWED;

        let function_layout = ProcLayout {
            arguments: passed_function.argument_layouts,
            result: passed_function.return_layout,
        };

        let function_ps = match self
            .param_map
            .get_symbol(passed_function.name, function_layout)
        {
            Some(function_ps) => function_ps,
            None => unreachable!(),
        };

        macro_rules! handle_ownerships_post {
            ($stmt:expr, $args:expr) => {{
                let mut stmt = $stmt;

                for (argument, function_ps) in $args.iter().copied() {
                    let ownership = DataFunction::new(&self.vars, argument, function_ps);

                    match ownership {
                        DataOwnedFunctionOwns | DataBorrowedFunctionOwns => {
                            // elements have been consumed, must still consume the list itself
                            let rest = self.arena.alloc($stmt);
                            let rc = Stmt::Refcounting(ModifyRc::DecRef(argument), rest);

                            stmt = self.arena.alloc(rc);
                        }
                        DataOwnedFunctionBorrows => {
                            // must consume list and elements
                            let rest = self.arena.alloc($stmt);
                            let rc = Stmt::Refcounting(ModifyRc::Dec(argument), rest);

                            stmt = self.arena.alloc(rc);
                        }
                        DataBorrowedFunctionBorrows => {
                            // list borrows, function borrows, so there is nothing to do
                        }
                    }
                }

                stmt
            }};
        }

        macro_rules! handle_ownerships_pre {
            ($stmt:expr, $args:expr) => {{
                let mut stmt = self.arena.alloc($stmt);

                for (argument, function_ps) in $args.iter().copied() {
                    let ownership = DataFunction::new(&self.vars, argument, function_ps);

                    match ownership {
                        DataBorrowedFunctionOwns => {
                            // the data is borrowed;
                            // increment it to own the values so the function can use them
                            let rc = Stmt::Refcounting(ModifyRc::Inc(argument, 1), stmt);

                            stmt = self.arena.alloc(rc);
                        }
                        DataOwnedFunctionOwns | DataOwnedFunctionBorrows => {
                            // we actually own the data; nothing to do
                        }
                        DataBorrowedFunctionBorrows => {
                            // list borrows, function borrows, so there is nothing to do
                        }
                    }
                }

                stmt
            }};
        }

        let borrows = [FUNCTION, CLOSURE_DATA];
        let after_arguments = &arguments[op.function_index()..];

        match *op {
            ListMap { xs }
            | ListKeepIf { xs }
            | ListKeepOks { xs }
            | ListKeepErrs { xs }
            | ListAny { xs }
            | ListAll { xs }
            | ListFindUnsafe { xs } => {
                let ownerships = [(xs, function_ps[0])];

                let b = self.add_dec_after_lowlevel(after_arguments, &borrows, b, b_live_vars);

                let b = handle_ownerships_post!(b, ownerships);

                let v = create_call!(function_ps.get(1));

                handle_ownerships_pre!(Stmt::Let(z, v, l, b), ownerships)
            }
            ListMap2 { xs, ys } => {
                let ownerships = [(xs, function_ps[0]), (ys, function_ps[1])];

                let b = self.add_dec_after_lowlevel(after_arguments, &borrows, b, b_live_vars);

                let b = handle_ownerships_post!(b, ownerships);

                let v = create_call!(function_ps.get(2));

                handle_ownerships_pre!(Stmt::Let(z, v, l, b), ownerships)
            }
            ListMap3 { xs, ys, zs } => {
                let ownerships = [
                    (xs, function_ps[0]),
                    (ys, function_ps[1]),
                    (zs, function_ps[2]),
                ];

                let b = self.add_dec_after_lowlevel(after_arguments, &borrows, b, b_live_vars);

                let b = handle_ownerships_post!(b, ownerships);

                let v = create_call!(function_ps.get(3));

                handle_ownerships_pre!(Stmt::Let(z, v, l, b), ownerships)
            }
            ListMap4 { xs, ys, zs, ws } => {
                let ownerships = [
                    (xs, function_ps[0]),
                    (ys, function_ps[1]),
                    (zs, function_ps[2]),
                    (ws, function_ps[3]),
                ];

                let b = self.add_dec_after_lowlevel(after_arguments, &borrows, b, b_live_vars);

                let b = handle_ownerships_post!(b, ownerships);

                let v = create_call!(function_ps.get(3));

                handle_ownerships_pre!(Stmt::Let(z, v, l, b), ownerships)
            }
            ListMapWithIndex { xs } => {
                let ownerships = [(xs, function_ps[0])];

                let b = self.add_dec_after_lowlevel(after_arguments, &borrows, b, b_live_vars);

                let b = handle_ownerships_post!(b, ownerships);

                let v = create_call!(function_ps.get(2));

                handle_ownerships_pre!(Stmt::Let(z, v, l, b), ownerships)
            }
            ListSortWith { xs } => {
                // NOTE: we may apply the function to the same argument multiple times.
                // for that to be valid, the function must borrow its argument. This is not
                // enforced at the moment
                //
                // we also don't check that both arguments have the same ownership characteristics
                let ownerships = [(xs, function_ps[0])];

                let b = self.add_dec_after_lowlevel(after_arguments, &borrows, b, b_live_vars);

                // list-sort will sort in-place; that really changes how RC should work
                let b = {
                    let ownership = DataFunction::new(&self.vars, xs, function_ps[0]);

                    match ownership {
                        DataOwnedFunctionOwns => {
                            // elements have been consumed, must still consume the list itself
                            let mut stmt = b;

                            let condition_symbol = Symbol::new(home, ident_ids.gen_unique());

                            // unique branch
                            // (u64, BranchInfo<'a>, Stmt<'a>)
                            let unique_branch = (1u64, BranchInfo::None, stmt.clone());

                            // non-unique branch
                            let rc = Stmt::Refcounting(ModifyRc::DecRef(xs), stmt);
                            let non_unique_branch = (BranchInfo::None, &*self.arena.alloc(rc));

                            // branch on the condition

                            let when_stmt = Stmt::Switch {
                                cond_symbol: condition_symbol,
                                cond_layout: Layout::bool(),
                                branches: &*self.arena.alloc([unique_branch]),
                                default_branch: non_unique_branch,
                                ret_layout: l,
                            };

                            stmt = self.arena.alloc(when_stmt);

                            // define the condition

                            let condition_call_type = CallType::LowLevel {
                                op: roc_module::low_level::LowLevel::ListIsUnique,
                                update_mode: crate::ir::UpdateModeId::BACKEND_DUMMY,
                            };

                            let condition_call = crate::ir::Call {
                                call_type: condition_call_type,
                                arguments: self.arena.alloc([xs]),
                            };

                            let condition_stmt = Stmt::Let(
                                condition_symbol,
                                Expr::Call(condition_call),
                                Layout::bool(),
                                stmt,
                            );

                            stmt = self.arena.alloc(condition_stmt);

                            stmt
                        }
                        DataOwnedFunctionBorrows => {
                            // must consume list and elements
                            let rest = self.arena.alloc(b);
                            let rc = Stmt::Refcounting(ModifyRc::Dec(xs), rest);

                            &*self.arena.alloc(rc)
                        }
                        DataBorrowedFunctionOwns => {
                            // elements have been consumed, must still consume the list itself
                            let rest = self.arena.alloc(b);
                            let rc = Stmt::Refcounting(ModifyRc::DecRef(xs), rest);

                            &*self.arena.alloc(rc)
                        }
                        DataBorrowedFunctionBorrows => {
                            // list borrows, function borrows, so there is nothing to do
                            b
                        }
                    }
                };

                let v = create_call!(function_ps.get(2));

                handle_ownerships_pre!(Stmt::Let(z, v, l, b), ownerships)
            }
            ListWalk { xs, state }
            | ListWalkUntil { xs, state }
            | ListWalkBackwards { xs, state }
            | DictWalk { xs, state } => {
                let ownerships = [
                    // borrow data structure based on second argument of the folded function
                    (xs, function_ps[1]),
                ];
                // borrow the default based on first argument of the folded function
                // (state, function_ps[0])

                let b = self.add_dec_after_lowlevel(after_arguments, &borrows, b, b_live_vars);

                let b = handle_ownerships_post!(b, ownerships);

                let v = create_call!(function_ps.get(2));

                handle_ownerships_pre!(Stmt::Let(z, v, l, b), ownerships)
            }
        }
    }

    #[allow(clippy::many_single_char_names)]
    fn visit_variable_declaration<'i>(
        &self,
        home: ModuleId,
        ident_ids: &'i mut IdentIds,
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
            }) => self.visit_call(home, ident_ids, z, call_type, arguments, l, b, b_live_vars),

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

            ExprBox { symbol: x } => {
                // mimics Tag
                self.add_inc_before_consume_all(
                    &[x],
                    self.arena.alloc(Stmt::Let(z, v, l, b)),
                    b_live_vars,
                )
            }

            ExprUnbox { symbol: x } => {
                // mimics UnionAtIndex
                let b = self.add_dec_if_needed(x, b, b_live_vars);
                let info_x = self.get_var_info(x);
                let b = if info_x.consume {
                    self.add_inc(z, 1, b)
                } else {
                    b
                };

                self.arena.alloc(Stmt::Let(z, v, l, b))
            }

            EmptyArray | Literal(_) | Reset { .. } | RuntimeErrorFunction(_) => {
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

        let reset = matches!(expr, Expr::Reset { .. });

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

    fn visit_stmt<'i>(
        &self,
        home: ModuleId,
        ident_ids: &'i mut IdentIds,
        stmt: &'a Stmt<'a>,
    ) -> (&'a Stmt<'a>, LiveVarSet) {
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
                let (mut b, mut b_live_vars) = ctx.visit_stmt(home, ident_ids, cont);
                for (symbol, expr, layout) in triples.into_iter().rev() {
                    let pair = ctx.visit_variable_declaration(
                        home,
                        ident_ids,
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
                let (b, b_live_vars) = ctx.visit_stmt(home, ident_ids, cont);
                ctx.visit_variable_declaration(
                    home,
                    ident_ids,
                    *symbol,
                    expr.clone(),
                    *layout,
                    b,
                    &b_live_vars,
                )
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
                    ctx.visit_stmt(home, ident_ids, v)
                };

                let mut ctx = self.clone();
                let v = ctx.add_dec_for_dead_params(xs, v, &v_live_vars);

                update_jp_live_vars(*j, xs, v, &mut ctx.jp_live_vars);

                let (b, b_live_vars) = ctx.visit_stmt(home, ident_ids, b);

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
                        let (b, alt_live_vars) = ctx.visit_stmt(home, ident_ids, branch);
                        let b = ctx.add_dec_for_alt(&case_live_vars, &alt_live_vars, b);

                        (*label, info.clone(), b.clone())
                    }),
                    self.arena,
                )
                .into_bump_slice();

                let default_branch = {
                    // TODO should we use ctor info like Lean?
                    let ctx = self.clone();
                    let (b, alt_live_vars) = ctx.visit_stmt(home, ident_ids, default_branch.1);

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

fn create_holl_call<'a>(
    arena: &'a Bump,
    holl: &'a crate::ir::HigherOrderLowLevel,
    param: Option<&Param>,
    arguments: &'a [Symbol],
) -> Expr<'a> {
    let call = crate::ir::Call {
        call_type: if let Some(OWNED) = param.map(|p| p.borrow) {
            let mut passed_function = holl.passed_function;
            passed_function.owns_captured_environment = true;

            let higher_order = HigherOrderLowLevel {
                op: holl.op,
                closure_env_layout: holl.closure_env_layout,
                update_mode: holl.update_mode,
                passed_function,
            };

            CallType::HigherOrder(arena.alloc(higher_order))
        } else {
            CallType::HigherOrder(holl)
        },
        arguments,
    };

    Expr::Call(call)
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

pub fn visit_procs<'a, 'i>(
    arena: &'a Bump,
    home: ModuleId,
    ident_ids: &'i mut IdentIds,
    param_map: &'a ParamMap<'a>,
    procs: &mut MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) {
    let ctx = Context::new(arena, param_map);

    for (key, proc) in procs.iter_mut() {
        visit_proc(arena, home, ident_ids, param_map, &ctx, proc, key.1);
    }
}

fn visit_proc<'a, 'i>(
    arena: &'a Bump,
    home: ModuleId,
    ident_ids: &'i mut IdentIds,
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
    let (b, b_live_vars) = ctx.visit_stmt(home, ident_ids, stmt);
    let b = ctx.add_dec_for_dead_params(params, b, &b_live_vars);

    proc.body = b.clone();
}
