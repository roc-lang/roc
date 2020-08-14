use crate::ir::{Expr, JoinPointId, Param, Proc, Stmt};
use crate::layout::Layout;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::{MutMap, MutSet};
use roc_module::symbol::Symbol;

pub fn infer_borrow<'a>(
    arena: &'a Bump,
    procs: &MutMap<(Symbol, Layout<'a>), Proc<'a>>,
) -> ParamMap<'a> {
    let mut param_map = ParamMap {
        items: MutMap::default(),
    };

    for proc in procs.values() {
        param_map.visit_proc(arena, proc);
    }

    let mut env = BorrowInfState {
        current_proc: Symbol::ATTR_ATTR,
        param_set: MutSet::default(),
        owned: MutSet::default(),
        modified: false,
        param_map,
        arena,
    };

    // sort the symbols (roughly) in definition order.
    // TODO in the future I think we need to do this properly, and group
    // mutually recursive functions (or just make all their arguments owned)
    let mut values = procs.values().collect::<std::vec::Vec<_>>();
    values.sort_by(|a, b| b.name.cmp(&a.name));

    for proc in values.iter() {
        env.collect_proc(proc);
    }

    env.param_map
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum Key {
    Declaration(Symbol),
    JoinPoint(JoinPointId),
}

#[derive(Debug, Clone, Default)]
pub struct ParamMap<'a> {
    items: MutMap<Key, &'a [Param<'a>]>,
}

impl<'a> ParamMap<'a> {
    pub fn get_symbol(&self, symbol: Symbol) -> &'a [Param<'a>] {
        let key = Key::Declaration(symbol);

        self.items.get(&key).unwrap()
    }
    pub fn get_join_point(&self, id: JoinPointId) -> &'a [Param<'a>] {
        let key = Key::JoinPoint(id);

        self.items.get(&key).unwrap()
    }
}

impl<'a> ParamMap<'a> {
    fn init_borrow_params(arena: &'a Bump, ps: &'a [Param<'a>]) -> &'a [Param<'a>] {
        Vec::from_iter_in(
            ps.iter().map(|p| Param {
                borrow: p.layout.is_refcounted(),
                layout: p.layout.clone(),
                symbol: p.symbol,
            }),
            arena,
        )
        .into_bump_slice()
    }

    fn init_borrow_args(arena: &'a Bump, ps: &'a [(Layout<'a>, Symbol)]) -> &'a [Param<'a>] {
        Vec::from_iter_in(
            ps.iter().map(|(layout, symbol)| Param {
                borrow: layout.is_refcounted(),
                layout: layout.clone(),
                symbol: *symbol,
            }),
            arena,
        )
        .into_bump_slice()
    }

    fn visit_proc(&mut self, arena: &'a Bump, proc: &Proc<'a>) {
        self.items.insert(
            Key::Declaration(proc.name),
            Self::init_borrow_args(arena, proc.args),
        );

        self.visit_stmt(arena, proc.name, &proc.body);
    }

    fn visit_stmt(&mut self, arena: &'a Bump, fnid: Symbol, stmt: &Stmt<'a>) {
        use Stmt::*;

        let mut stack = bumpalo::vec![ in arena; stmt ];

        while let Some(stmt) = stack.pop() {
            match stmt {
                Join {
                    id: j,
                    parameters: xs,
                    remainder: v,
                    continuation: b,
                } => {
                    self.items
                        .insert(Key::JoinPoint(*j), Self::init_borrow_params(arena, xs));

                    stack.push(v);
                    stack.push(b);
                }
                Let(_, _, _, cont) => {
                    stack.push(cont);
                }
                Cond { pass, fail, .. } => {
                    stack.push(pass);
                    stack.push(fail);
                }
                Switch {
                    branches,
                    default_branch,
                    ..
                } => {
                    stack.extend(branches.iter().map(|b| &b.1));
                    stack.push(default_branch);
                }
                Inc(_, _) | Dec(_, _) => unreachable!("these have not been introduced yet"),

                Ret(_) | Jump(_, _) | RuntimeError(_) => {
                    // these are terminal, do nothing
                }
            }
        }
    }
}

// Apply the inferred borrow annotations stored in ParamMap to a block of mutually recursive procs

struct BorrowInfState<'a> {
    current_proc: Symbol,
    param_set: MutSet<Symbol>,
    owned: MutSet<Symbol>,
    modified: bool,
    param_map: ParamMap<'a>,
    arena: &'a Bump,
}

impl<'a> BorrowInfState<'a> {
    pub fn own_var(&mut self, x: Symbol) {
        if self.owned.contains(&x) {
            // do nothing
        } else {
            self.owned.insert(x);
            self.modified = true;
        }
    }

    fn is_owned(&self, x: Symbol) -> bool {
        self.owned.contains(&x)
    }

    fn update_param_map(&mut self, k: Key) {
        let arena = self.arena;
        if let Some(ps) = self.param_map.items.get(&k) {
            let ps = Vec::from_iter_in(
                ps.iter().map(|p| {
                    if !p.borrow {
                        p.clone()
                    } else if self.is_owned(p.symbol) {
                        self.modified = true;
                        let mut p = p.clone();
                        p.borrow = false;

                        p
                    } else {
                        p.clone()
                    }
                }),
                arena,
            );

            self.param_map.items.insert(k, ps.into_bump_slice());
        }
    }

    /// This looks at an application `f x1 x2 x3`
    /// If the parameter (based on the definition of `f`) is owned,
    /// then the argument must also be owned
    fn own_args_using_params(&mut self, xs: &[Symbol], ps: &[Param<'a>]) {
        debug_assert_eq!(xs.len(), ps.len());

        for (x, p) in xs.iter().zip(ps.iter()) {
            if !p.borrow {
                self.own_var(*x);
            }
        }
    }

    /// This looks at an application `f x1 x2 x3`
    /// If the parameter (based on the definition of `f`) is owned,
    /// then the argument must also be owned
    fn own_args_using_bools(&mut self, xs: &[Symbol], ps: &[bool]) {
        debug_assert_eq!(xs.len(), ps.len());

        for (x, borrow) in xs.iter().zip(ps.iter()) {
            if !borrow {
                self.own_var(*x);
            }
        }
    }

    /// For each xs[i], if xs[i] is owned, then mark ps[i] as owned.
    /// We use this action to preserve tail calls. That is, if we have
    /// a tail call `f xs`, if the i-th parameter is borrowed, but `xs[i]` is owned
    /// we would have to insert a `dec xs[i]` after `f xs` and consequently
    /// "break" the tail call.
    fn own_params_using_args(&mut self, xs: &[Symbol], ps: &[Param<'a>]) {
        debug_assert_eq!(xs.len(), ps.len());

        for (x, p) in xs.iter().zip(ps.iter()) {
            if self.is_owned(*x) {
                self.own_var(p.symbol);
            }
        }
    }

    /// Mark `xs[i]` as owned if it is one of the parameters `ps`.
    /// We use this action to mark function parameters that are being "packed" inside constructors.
    /// This is a heuristic, and is not related with the effectiveness of the reset/reuse optimization.
    /// It is useful for code such as
    /// ```
    /// def f (x y : obj) :=
    /// let z := ctor_1 x y;
    /// ret z
    /// ```
    fn own_args_if_param(&mut self, xs: &[Symbol]) {
        for x in xs.iter() {
            // TODO may also be asking for the index here? see Lean
            if self.param_set.contains(x) {
                self.own_var(*x);
            }
        }
    }

    fn collect_expr(&mut self, z: Symbol, e: &Expr<'a>) {
        use Expr::*;

        match e {
            Tag { arguments: xs, .. } | Struct(xs) | Array { elems: xs, .. } => {
                self.own_var(z);
                self.own_args_if_param(xs);
            }
            EmptyArray => {
                self.own_var(z);
            }
            AccessAtIndex { structure: x, .. } => {
                if self.is_owned(*x) {
                    self.own_var(*x);
                }
                if self.is_owned(z) {
                    self.own_var(z);
                }
            }
            FunctionCall {
                call_type, args, ..
            } => {
                let g = Key::Declaration(call_type.into_inner());

                // DEVIATION: the two lines below are swapped for borrow checker reasons
                // I think it should be fine.
                self.own_var(z);
                let ps = self.param_map.items.get(&g).unwrap().clone();
                self.own_args_using_params(args, ps);
            }

            RunLowLevel(op, args) => {
                use roc_module::low_level::LowLevel::*;

                self.own_var(z);

                // TODO is true or false more efficient for non-refcounted layouts?
                let irrelevant = true;
                let owned = false;
                let borrowed = true;

                let arena = self.arena;

                // Here we define the borrow signature of low-level operations
                //
                // - arguments with non-refcounted layouts (ints, floats) are `irrelevant`
                // - arguments that we may want to update destructively must be Owned
                // - other refcounted arguments are Borrowed
                let ps = match op {
                    ListLen => arena.alloc_slice_copy(&[borrowed]),
                    ListSet => arena.alloc_slice_copy(&[owned, irrelevant, irrelevant]),
                    ListSetInPlace => arena.alloc_slice_copy(&[owned, irrelevant, irrelevant]),

                    ListGetUnsafe => arena.alloc_slice_copy(&[irrelevant, irrelevant]),
                    ListSingle => arena.alloc_slice_copy(&[irrelevant]),
                    ListRepeat => arena.alloc_slice_copy(&[irrelevant, irrelevant]),
                    ListReverse => arena.alloc_slice_copy(&[owned]),
                    ListConcat => arena.alloc_slice_copy(&[irrelevant, irrelevant]),
                    ListAppend => arena.alloc_slice_copy(&[owned, owned]),
                    ListPrepend => arena.alloc_slice_copy(&[owned, owned]),
                    ListJoin => arena.alloc_slice_copy(&[irrelevant]),

                    Eq | NotEq | And | Or | NumAdd | NumSub | NumMul | NumGt | NumGte | NumLt
                    | NumLte | NumDivUnchecked | NumRemUnchecked => {
                        arena.alloc_slice_copy(&[irrelevant, irrelevant])
                    }

                    NumAbs | NumNeg | NumSin | NumCos | NumSqrtUnchecked | NumRound
                    | NumToFloat | Not => arena.alloc_slice_copy(&[irrelevant]),
                };

                self.own_args_using_bools(args, ps);
            }
            Literal(_) | FunctionPointer(_, _) | RuntimeErrorFunction(_) => {}
        }
    }

    fn preserve_tail_call(&mut self, x: Symbol, v: &Expr<'a>, b: &Stmt<'a>) {
        if let (
            Expr::FunctionCall {
                call_type,
                args: ys,
                ..
            },
            Stmt::Ret(z),
        ) = (v, b)
        {
            let g = call_type.into_inner();
            if self.current_proc == g && x == *z {
                let ps = self
                    .param_map
                    .items
                    .get(&Key::Declaration(g))
                    .unwrap()
                    .clone();
                self.own_params_using_args(ys, ps)
            }
        }
    }

    fn update_param_set(&mut self, ps: &[Param<'a>]) {
        for p in ps.iter() {
            self.param_set.insert(p.symbol);
        }
    }

    fn update_param_set_symbols(&mut self, ps: &[Symbol]) {
        for p in ps.iter() {
            self.param_set.insert(*p);
        }
    }

    fn collect_stmt(&mut self, stmt: &Stmt<'a>) {
        use Stmt::*;

        match stmt {
            Join {
                id: j,
                parameters: ys,
                remainder: v,
                continuation: b,
            } => {
                let old = self.param_set.clone();
                self.update_param_set(ys);
                self.collect_stmt(v);
                self.param_set = old;

                self.update_param_map(Key::JoinPoint(*j));

                self.collect_stmt(b);
            }

            Let(x, v, _, b) => {
                self.collect_stmt(b);
                self.collect_expr(*x, v);
                self.preserve_tail_call(*x, v, b);
            }
            Jump(j, ys) => {
                let ps = self.param_map.get_join_point(*j).clone();

                // for making sure the join point can reuse
                self.own_args_using_params(ys, ps);

                // for making sure the tail call is preserved
                self.own_params_using_args(ys, ps);
            }
            Cond { pass, fail, .. } => {
                self.collect_stmt(pass);
                self.collect_stmt(fail);
            }
            Switch {
                branches,
                default_branch,
                ..
            } => {
                for (_, b) in branches.iter() {
                    self.collect_stmt(b);
                }
                self.collect_stmt(default_branch);
            }
            Inc(_, _) | Dec(_, _) => unreachable!("these have not been introduced yet"),

            Ret(_) | RuntimeError(_) => {
                // these are terminal, do nothing
            }
        }
    }

    fn collect_proc(&mut self, proc: &Proc<'a>) {
        let old = self.param_set.clone();

        let ys = Vec::from_iter_in(proc.args.iter().map(|t| t.1), self.arena).into_bump_slice();
        self.update_param_set_symbols(ys);
        self.current_proc = proc.name;

        self.collect_stmt(&proc.body);
        self.update_param_map(Key::Declaration(proc.name));

        self.param_set = old;
    }
}
