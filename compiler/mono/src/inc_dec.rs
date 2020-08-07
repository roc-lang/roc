use crate::ir::{Expr, JoinPointId, Stmt};
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

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub beta: MutMap<Symbol, &'a [Ownership]>,
    pub beta_l: MutMap<Symbol, Ownership>,
    pub join_points: MutMap<JoinPointId, MutSet<Symbol>>,
}

impl<'a> Env<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        Self {
            arena,
            beta: MutMap::default(),
            beta_l: MutMap::default(),
            join_points: MutMap::default(),
        }
    }

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

pub fn function_c<'a>(env: &mut Env<'a>, stmt: &'a Stmt<'a>) -> &'a Stmt<'a> {
    use Expr::*;
    use Ownership::*;
    use Stmt::*;

    let arena = env.arena;

    dbg!(stmt);

    /*
        | b@(FnBody.jmp j xs), ctx =>
          let jLiveVars := getJPLiveVars ctx j;
          let ps        := getJPParams ctx j;
          let b         := addIncBefore ctx xs ps b jLiveVars;
          let bLiveVars := collectLiveVars b ctx.jpLiveVarMap;
          (b, bLiveVars)

    | FnBody.jdecl j xs v b,     ctx =>
      let (v, vLiveVars) := visitFnBody v (updateVarInfoWithParams ctx xs);
      let v   := addDecForDeadParams ctx xs v vLiveVars;
      let ctx := { ctx with jpLiveVarMap := updateJPLiveVarMap j xs v ctx.jpLiveVarMap };
      let (b, bLiveVars) := visitFnBody b ctx;
      (FnBody.jdecl j xs v b, bLiveVars)
            */

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
            EmptyArray => {
                let rest = function_c(env, f);
                let let_stmt = arena.alloc(Let(*y, e.clone(), l.clone(), rest));

                let y_owned = &[] as &[_];

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
            RunLowLevel(_, args) => {
                use crate::ir::CallType;

                let rest = function_c(env, f);
                let let_stmt = arena.alloc(Let(*y, e.clone(), l.clone(), rest));

                let y_owned = {
                    let signature = &[] as &[_];

                    let mut result = Vec::with_capacity_in(args.len(), env.arena);

                    for (i, arg) in args.iter().enumerate() {
                        let ownership = match signature.get(i) {
                            None => Owned,
                            Some(o) => *o,
                        };
                        result.push((*arg, ownership));
                    }

                    result.into_bump_slice()
                };

                function_c_app(arena, &y_owned, let_stmt)
            }
            Literal(_) | Alias(_) | FunctionPointer(_, _) | RuntimeErrorFunction(_) => {
                // leaves in terms of RC
                let rest = function_c(env, f);
                arena.alloc(Let(*y, e.clone(), l.clone(), rest))
            }
        },
        Inc(_, _) | Dec(_, _) => stmt,
        Join { .. } | Jump(_, _) => stmt,
        RuntimeError(_) => stmt,
    }
}

/*
fn visit_fn_body<'a>(env: &mut Env<'a>, stmt: &'a Stmt<'a>) -> (&'a Stmt<'a>, LiveVarSet) {
    use Stmt::*;

    todo!()
}
*/

mod live_vars {
    use crate::ir::{Expr, JoinPointId, Stmt};
    use bumpalo::collections::Vec;
    use bumpalo::Bump;
    use roc_collections::all::{MutMap, MutSet};
    use roc_module::symbol::Symbol;

    type LiveVarSet = MutSet<Symbol>;
    type JPLiveVarMap = MutMap<JoinPointId, LiveVarSet>;

    pub fn collect_live_vars(b: &Stmt<'_>, _m: &JPLiveVarMap, v: &mut LiveVarSet) {
        // inefficient, but it works
        let (mut occuring, _) = crate::inc_dec::occuring_variables(b);

        v.extend(occuring);

        v.retain(|s| is_live(&MutMap::default(), *s, b));
    }

    fn is_live<'a>(
        join_points: &MutMap<JoinPointId, (&'a [Symbol], Stmt<'a>)>,
        needle: Symbol,
        stmt: &Stmt<'a>,
    ) -> bool {
        use Stmt::*;

        let needle = &needle;

        let mut stack = std::vec![stmt];
        let mut result = MutSet::default();

        while let Some(stmt) = stack.pop() {
            use Stmt::*;

            match stmt {
                Let(symbol, expr, _, cont) => {
                    result.clear();
                    crate::inc_dec::occuring_variables_expr(expr, &mut result);
                    if result.contains(needle) {
                        return true;
                    }
                    stack.push(cont);
                }
                Ret(symbol) => {
                    if symbol == needle {
                        return true;
                    }
                }

                Inc(symbol, cont) | Dec(symbol, cont) => {
                    if symbol == needle {
                        return true;
                    }
                    stack.push(cont);
                }

                Jump(j, _arguments) => {
                    match join_points.get(j) {
                        Some((_, b)) => {
                            // `j` is not a local join point since we assume we cannot shadow join point declarations.
                            // Instead of marking the join points that we have already been visited, we permanently remove `j` from the context.
                            let mut join_points = join_points.clone();
                            join_points.remove(j);

                            if is_live(&join_points, *needle, b) {
                                return true;
                            }
                        }
                        None => {
                            // `j` must be a local join point. So do nothing since we have already visite its body.
                        }
                    }
                }

                Join {
                    parameters,
                    continuation,
                    remainder,
                    ..
                } => {
                    if parameters
                        .iter()
                        .map(|p| p.symbol)
                        .find(|s| s == needle)
                        .is_some()
                    {
                        return true;
                    }

                    stack.push(continuation);
                    stack.push(remainder);
                }

                Switch {
                    cond_symbol,
                    branches,
                    default_branch,
                    ..
                } => {
                    if cond_symbol == needle {
                        return true;
                    }

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
                    if cond_symbol == needle || branching_symbol == needle {
                        return true;
                    }
                    stack.push(pass);
                    stack.push(fail);
                }

                RuntimeError(_) => {}
            }
        }

        return false;
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

// TODO this should contain more information ( e.g. make it contain layout, and whether it is
// borrowed)
type Param = Symbol;

#[derive(Clone, Debug)]
struct Context<'a> {
    arena: &'a Bump,
    vars: VarMap,
    jp_live_vars: JPLiveVarMap,      // map: join point => live variables
    local_context: LocalContext<'a>, // we use it to store the join point declarations
}

fn update_live_vars<'a>(e: &Expr<'a>, v: &LiveVarSet) -> LiveVarSet {
    todo!()
}

fn is_first_occurence(xs: &[Symbol], i: usize) -> bool {
    match xs.get(i) {
        None => unreachable!(),
        Some(s) => i == xs.iter().position(|v| s == v).unwrap(),
    }
}

fn get_num_consumptions<F>(x: Symbol, ys: &[Symbol], mut consume_param_pred: F) -> usize
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

fn is_borrow_param_help<F>(x: Symbol, ys: &[Symbol], mut consume_param_pred: F) -> bool
where
    F: Fn(usize) -> bool,
{
    ys.iter()
        .enumerate()
        .any(|(i, y)| x == *y && !consume_param_pred(i))
}

fn is_borrow_param(x: Symbol, ys: &[Symbol], ps: &[Symbol]) -> bool {
    // Lean: isBorrowParamAux x ys (fun i => not (ps.get! i).borrow)
    is_borrow_param_help(x, ys, |_| true)
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
    pub fn new(arena: &'a Bump) -> Self {
        Self {
            arena,
            vars: MutMap::default(),
            jp_live_vars: MutMap::default(),
            local_context: LocalContext::default(),
        }
    }

    fn get_var_info(&self, symbol: Symbol) -> VarInfo {
        *self.vars.get(&symbol).unwrap()
    }

    fn add_inc(&self, symbol: Symbol, stmt: &'a Stmt<'a>) -> &'a Stmt<'a> {
        let info = self.get_var_info(symbol);

        if info.persistent {
            // persistent values are never reference counted
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

        self.arena.alloc(Stmt::Dec(symbol, stmt))
    }

    fn add_inc_before_consume_all_help<F>(
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
                // number of times the argument is used (in the body?)
                let num_consumptions = get_num_consumptions(*x, xs, consume_param_pred.clone());

                let num_incs = if !info.consume ||                     // `x` is not a variable that must be consumed by the current procedure
                         live_vars_after.contains(x) ||          // `x` is live after executing instruction
                         is_borrow_param_help(*x, xs, consume_param_pred.clone())
                // `x` is used in a position that is passed as a borrow reference
                {
                    num_consumptions
                } else {
                    num_consumptions - 1
                };

                // Lean can increment by more than 1 at once. Is that needed?
                debug_assert_eq!(num_incs, 1);

                b = self.add_inc(*x, b);
            }
        }

        b
    }

    fn add_inc_before_consume_all(
        &self,
        xs: &[Symbol],
        b: &'a Stmt<'a>,
        live_vars_after: &LiveVarSet,
    ) -> &'a Stmt<'a> {
        self.add_inc_before_consume_all_help(xs, |_: usize| true, b, live_vars_after)
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
                let numConsuptions = get_num_consumptions(*x, xs, consume_param_pred.clone()); // number of times the argument is used
                let num_incs = if !info.consume ||                     // `x` is not a variable that must be consumed by the current procedure
             live_vars_after.contains(x) ||          // `x` is live after executing instruction
             is_borrow_param_help( *x ,xs, consume_param_pred.clone())
                // `x` is used in a position that is passed as a borrow reference
                {
                    numConsuptions
                } else {
                    numConsuptions - 1
                };

                // verify that this is indeed always 1
                debug_assert_eq!(num_incs, 1);

                b = self.add_inc(*x, b)
            }
        }
        b
    }

    fn add_inc_before(
        &self,
        xs: &[Symbol],
        ps: &[Param],
        mut b: &'a Stmt<'a>,
        live_vars_after: &LiveVarSet,
    ) -> &'a Stmt<'a> {
        // TODO closure is actuall (fun i => not (ps.get! i).borrow)
        self.add_inc_before_help(xs, |x| true, b, live_vars_after)
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
        ps: &[Symbol],
        mut b: &'a Stmt<'a>,
        b_live_vars: &LiveVarSet,
    ) -> &'a Stmt<'a> {
        for (i, x) in xs.iter().enumerate() {
            /* We must add a `dec` if `x` must be consumed, it is alive after the application,
            and it has been borrowed by the application.
            Remark: `x` may occur multiple times in the application (e.g., `f x y x`).
            This is why we check whether it is the first occurrence. */
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
            Tag { arguments: ys, .. } | Struct(ys) | Array { elems: ys, .. } => self
                .add_inc_before_consume_all(
                    ys,
                    self.arena.alloc(Stmt::Let(z, v, l, b)),
                    &b_live_vars,
                ),
            AccessAtIndex { structure: x, .. } => {
                let b = self.add_dec_if_needed(x, b, b_live_vars);
                let b = if self.get_var_info(x).consume {
                    self.add_inc(z, b)
                } else {
                    b
                };

                self.arena.alloc(Stmt::Let(z, v, l, b))
            }

            RunLowLevel(_, ys) => {
                // this is where the borrow signature would come in
                //let ps := (getDecl ctx f).params;
                let ps = &[] as &[_];
                let b = self.add_dec_after_application(ys, ps, b, b_live_vars);
                self.arena.alloc(Stmt::Let(z, v, l, b))
            }

            FunctionCall { args: ys, .. } => {
                // this is where the borrow signature would come in
                //let ps := (getDecl ctx f).params;
                let ps = &[] as &[_];
                let b = self.add_dec_after_application(ys, ps, b, b_live_vars);
                self.arena.alloc(Stmt::Let(z, v, l, b))
            }

            Alias(_) => unreachable!("well, it should be unreachable!"),

            EmptyArray | FunctionPointer(_, _) => todo!("unsure about this one"),

            Literal(_) | RuntimeErrorFunction(_) => self.arena.alloc(Stmt::Let(z, v, l, b)),
        };

        (new_b, live_vars)
    }

    fn update_var_info(&self, symbol: Symbol, layout: &Layout<'a>, expr: &Expr<'a>) -> Self {
        let mut ctx = self.clone();

        // TODO actually make these non-constant

        // can this type be reference-counted at runtime?
        use crate::layout::Builtin;
        let reference = match layout {
            Layout::Builtin(Builtin::List(_, _)) => true,
            _ => false,
        };

        // is this value a constant?
        let persistent = false;

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
        //def updateVarInfoWithParams (ctx : Context) (ps : Array Param) : Context :=
        //let m := ps.foldl (fun (m : VarMap) p => m.insert p.x { ref := p.ty.isObj, consume := !p.borrow }) ctx.varMap;
        //{ ctx with varMap := m }
        todo!()
    }

    /* Add `dec` instructions for parameters that are references, are not alive in `b`, and are not borrow.
    That is, we must make sure these parameters are consumed. */
    fn add_dec_for_dead_params(
        &self,
        ps: &[Symbol],
        b: &'a Stmt<'a>,
        b_live_vars: &LiveVarSet,
    ) -> &'a Stmt<'a> {
        /*

        ps.foldl
          (fun b p => if !p.borrow && p.ty.isObj && !bLiveVars.contains p.x then addDec ctx p.x b else b)
          b
                */
        todo!()
    }

    fn visit_stmt(&self, stmt: &'a Stmt<'a>) -> (&'a Stmt<'a>, LiveVarSet) {
        use Stmt::*;

        match stmt {
            Let(symbol, expr, layout, cont) => {
                let ctx = self.update_var_info(*symbol, layout, expr);
                let (b, b_live_vars) = self.visit_stmt(cont);
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
                parameters,
                remainder: v,
                continuation: b,
            } => {
                let xs = Vec::from_iter_in(parameters.iter().map(|p| p.symbol), self.arena)
                    .into_bump_slice();
                let (v, v_live_vars) = {
                    let ctx = self.update_var_info_with_params(xs);
                    ctx.visit_stmt(v)
                };

                let v = self.add_dec_for_dead_params(xs, v, &v_live_vars);
                let mut ctx = self.clone();
                ctx.local_context.join_points.insert(*j, (xs, v));

                let (b, b_live_vars) = ctx.visit_stmt(b);

                (
                    ctx.arena.alloc(Join {
                        id: *j,
                        parameters,
                        remainder: v,
                        continuation: b,
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
                let ps = self.local_context.join_points.get(j).unwrap().0;
                let b = self.add_inc_before(xs, ps, stmt, j_live_vars);

                let mut b_live_vars = MutSet::default();
                crate::inc_dec::live_vars::collect_live_vars(
                    b,
                    &self.jp_live_vars,
                    &mut b_live_vars,
                );

                (b, b_live_vars)
            }

            _ => todo!(),
        }
    }
}

#[derive(Clone, Debug, Default)]
struct LocalContext<'a> {
    join_points: MutMap<JoinPointId, (&'a [Symbol], &'a Stmt<'a>)>,
}
