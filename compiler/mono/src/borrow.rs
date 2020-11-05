use crate::ir::{Expr, JoinPointId, Param, Proc, Stmt};
use crate::layout::Layout;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::{MutMap, MutSet};
use roc_module::low_level::LowLevel;
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
        owned: MutMap::default(),
        modified: false,
        param_map,
        arena,
    };

    // This is a fixed-point analysis
    //
    // all functions initiall own all their paramters
    // through a series of checks and heuristics, some arguments are set to borrowed
    // when that doesn't lead to conflicts the change is kept, otherwise it may be reverted
    //
    // when the signatures no longer change, the analysis stops and returns the signatures
    loop {
        // sort the symbols (roughly) in definition order.
        // TODO in the future I think we need to do this properly, and group
        // mutually recursive functions (or just make all their arguments owned)

        for proc in procs.values() {
            env.collect_proc(proc);
        }

        if !env.modified {
            // if there were no modifications, we're done
            break;
        } else {
            // otherwise see if there are changes after another iteration
            env.modified = false;
        }
    }

    env.param_map
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Key {
    Declaration(Symbol),
    JoinPoint(JoinPointId),
}

#[derive(Debug, Clone, Default)]
pub struct ParamMap<'a> {
    items: MutMap<Key, &'a [Param<'a>]>,
}

impl<'a> IntoIterator for ParamMap<'a> {
    type Item = (Key, &'a [Param<'a>]);
    type IntoIter = <std::collections::HashMap<Key, &'a [Param<'a>]> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<'a> IntoIterator for &'a ParamMap<'a> {
    type Item = (&'a Key, &'a &'a [Param<'a>]);
    type IntoIter = <&'a std::collections::HashMap<Key, &'a [Param<'a>]> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}

impl<'a> ParamMap<'a> {
    pub fn get_symbol(&self, symbol: Symbol) -> Option<&'a [Param<'a>]> {
        let key = Key::Declaration(symbol);

        self.items.get(&key).copied()
    }
    pub fn get_join_point(&self, id: JoinPointId) -> &'a [Param<'a>] {
        let key = Key::JoinPoint(id);

        match self.items.get(&key) {
            Some(slice) => slice,
            None => unreachable!("join point not in param map: {:?}", id),
        }
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

    fn visit_stmt(&mut self, arena: &'a Bump, _fnid: Symbol, stmt: &Stmt<'a>) {
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
    owned: MutMap<Symbol, MutSet<Symbol>>,
    modified: bool,
    param_map: ParamMap<'a>,
    arena: &'a Bump,
}

impl<'a> BorrowInfState<'a> {
    pub fn own_var(&mut self, x: Symbol) {
        let current = self.owned.get_mut(&self.current_proc).unwrap();

        if current.contains(&x) {
            // do nothing
        } else {
            current.insert(x);
            self.modified = true;
        }
    }

    fn is_owned(&self, x: Symbol) -> bool {
        match self.owned.get(&self.current_proc) {
            None => unreachable!(
                "the current procedure symbol {:?} is not in the owned map",
                self.current_proc
            ),
            Some(set) => set.contains(&x),
        }
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
    ///
    /// > def f (x y : obj) :=
    /// > let z := ctor_1 x y;
    /// > ret z
    fn own_args_if_param(&mut self, xs: &[Symbol]) {
        for x in xs.iter() {
            // TODO may also be asking for the index here? see Lean
            if self.param_set.contains(x) {
                self.own_var(*x);
            }
        }
    }

    /// This looks at the assignement
    ///
    /// let z = e in ...
    ///
    /// and determines whether z and which of the symbols used in e
    /// must be taken as owned paramters
    fn collect_expr(&mut self, z: Symbol, e: &Expr<'a>) {
        use Expr::*;

        match e {
            Tag { arguments: xs, .. } | Struct(xs) | Array { elems: xs, .. } => {
                self.own_var(z);

                // if the used symbol is an argument to the current function,
                // the function must take it as an owned parameter
                self.own_args_if_param(xs);
            }
            Reset(x) => {
                self.own_var(z);
                self.own_var(*x);
            }
            Reuse {
                symbol: x,
                arguments: ys,
                ..
            } => {
                self.own_var(z);
                self.own_var(*x);
                self.own_args_if_param(ys);
            }
            EmptyArray => {
                self.own_var(z);
            }
            AccessAtIndex { structure: x, .. } => {
                // if the structure (record/tag/array) is owned, the extracted value is
                if self.is_owned(*x) {
                    self.own_var(z);
                }

                // if the extracted value is owned, the structure must be too
                if self.is_owned(z) {
                    self.own_var(*x);
                }
            }

            FunctionCall {
                call_type,
                args,
                arg_layouts,
                ..
            } => {
                // get the borrow signature of the applied function
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

                // the return value will be owned
                self.own_var(z);

                // if the function exects an owned argument (ps), the argument must be owned (args)
                self.own_args_using_params(args, ps);
            }

            RunLowLevel(op, args) => {
                // very unsure what demand RunLowLevel should place upon its arguments
                self.own_var(z);

                let ps = lowlevel_borrow_signature(self.arena, *op);

                self.own_args_using_bools(args, ps);
            }

            ForeignCall(_, args) => {
                // very unsure what demand ForeignCall should place upon its arguments
                self.own_var(z);

                let ps = foreign_borrow_signature(self.arena, args.len());

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
            let g = call_type.get_inner();
            if self.current_proc == g && x == *z {
                // anonymous functions (for which the ps may not be known)
                // can never be tail-recursive, so this is fine
                if let Some(ps) = self.param_map.get_symbol(g) {
                    self.own_params_using_args(ys, ps)
                }
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

            Let(x, Expr::FunctionPointer(fsymbol, layout), _, b) => {
                // ensure that the function pointed to is in the param map
                if let Some(params) = self.param_map.get_symbol(*fsymbol) {
                    self.param_map.items.insert(Key::Declaration(*x), params);
                }

                self.collect_stmt(b);
                self.preserve_tail_call(*x, &Expr::FunctionPointer(*fsymbol, layout.clone()), b);
            }
            Let(x, v, _, b) => {
                self.collect_stmt(b);
                self.collect_expr(*x, v);
                self.preserve_tail_call(*x, v, b);
            }
            Jump(j, ys) => {
                let ps = self.param_map.get_join_point(*j);

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

        // ensure that current_proc is in the owned map
        self.owned.entry(proc.name).or_default();

        self.collect_stmt(&proc.body);
        self.update_param_map(Key::Declaration(proc.name));

        self.param_set = old;
    }
}

pub fn foreign_borrow_signature(arena: &Bump, arity: usize) -> &[bool] {
    let all = bumpalo::vec![in arena; false; arity];
    all.into_bump_slice()
}

pub fn lowlevel_borrow_signature(arena: &Bump, op: LowLevel) -> &[bool] {
    use LowLevel::*;

    // TODO is true or false more efficient for non-refcounted layouts?
    let irrelevant = false;
    let owned = false;
    let borrowed = true;

    // Here we define the borrow signature of low-level operations
    //
    // - arguments with non-refcounted layouts (ints, floats) are `irrelevant`
    // - arguments that we may want to update destructively must be Owned
    // - other refcounted arguments are Borrowed
    match op {
        ListLen | StrIsEmpty => arena.alloc_slice_copy(&[borrowed]),
        ListSet => arena.alloc_slice_copy(&[owned, irrelevant, irrelevant]),
        ListSetInPlace => arena.alloc_slice_copy(&[owned, irrelevant, irrelevant]),
        ListGetUnsafe => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        ListConcat | StrConcat => arena.alloc_slice_copy(&[owned, borrowed]),

        ListSingle => arena.alloc_slice_copy(&[irrelevant]),
        ListRepeat => arena.alloc_slice_copy(&[irrelevant, irrelevant]),
        ListReverse => arena.alloc_slice_copy(&[owned]),
        ListAppend => arena.alloc_slice_copy(&[owned, owned]),
        ListPrepend => arena.alloc_slice_copy(&[owned, owned]),
        ListJoin => arena.alloc_slice_copy(&[irrelevant]),
        ListMap => arena.alloc_slice_copy(&[owned, irrelevant]),
        ListKeepIf => arena.alloc_slice_copy(&[owned, irrelevant]),
        ListWalkRight => arena.alloc_slice_copy(&[borrowed, irrelevant, owned]),

        Eq | NotEq | And | Or | NumAdd | NumAddWrap | NumAddChecked | NumSub | NumMul | NumGt
        | NumGte | NumLt | NumLte | NumCompare | NumDivUnchecked | NumRemUnchecked | NumPow
        | NumPowInt => arena.alloc_slice_copy(&[irrelevant, irrelevant]),

        NumAbs | NumNeg | NumSin | NumCos | NumSqrtUnchecked | NumRound | NumCeiling | NumFloor
        | NumToFloat | Not | NumIsFinite | NumAtan | NumAcos | NumAsin => {
            arena.alloc_slice_copy(&[irrelevant])
        }
    }
}
