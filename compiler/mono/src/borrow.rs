use crate::ir::{Expr, JoinPointId, Param, Proc, ProcLayout, Stmt};
use crate::layout::Layout;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::{MutMap, MutSet};
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;

pub const OWNED: bool = false;
pub const BORROWED: bool = true;

fn should_borrow_layout(layout: &Layout) -> bool {
    match layout {
        Layout::Closure(_, _, _) => false,
        _ => layout.is_refcounted(),
    }
}

pub fn infer_borrow<'a>(
    arena: &'a Bump,
    procs: &MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> ParamMap<'a> {
    let mut param_map = ParamMap {
        items: MutMap::default(),
    };

    for (key, proc) in procs {
        param_map.visit_proc(arena, proc, *key);
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
    // all functions initiall own all their parameters
    // through a series of checks and heuristics, some arguments are set to borrowed
    // when that doesn't lead to conflicts the change is kept, otherwise it may be reverted
    //
    // when the signatures no longer change, the analysis stops and returns the signatures
    loop {
        // sort the symbols (roughly) in definition order.
        // TODO in the future I think we need to do this properly, and group
        // mutually recursive functions (or just make all their arguments owned)

        for (key, proc) in procs {
            env.collect_proc(proc, key.1);
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
pub enum Key<'a> {
    Declaration(Symbol, ProcLayout<'a>),
    JoinPoint(JoinPointId),
}

#[derive(Debug, Clone, Default)]
pub struct ParamMap<'a> {
    items: MutMap<Key<'a>, &'a [Param<'a>]>,
}

impl<'a> IntoIterator for ParamMap<'a> {
    type Item = (Key<'a>, &'a [Param<'a>]);
    type IntoIter = <std::collections::HashMap<Key<'a>, &'a [Param<'a>]> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<'a> IntoIterator for &'a ParamMap<'a> {
    type Item = (&'a Key<'a>, &'a &'a [Param<'a>]);
    type IntoIter =
        <&'a std::collections::HashMap<Key<'a>, &'a [Param<'a>]> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}

impl<'a> ParamMap<'a> {
    pub fn get_symbol(&self, symbol: Symbol, layout: ProcLayout<'a>) -> Option<&'a [Param<'a>]> {
        let key = Key::Declaration(symbol, layout);

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
                layout: p.layout,
                symbol: p.symbol,
            }),
            arena,
        )
        .into_bump_slice()
    }

    fn init_borrow_args(arena: &'a Bump, ps: &'a [(Layout<'a>, Symbol)]) -> &'a [Param<'a>] {
        Vec::from_iter_in(
            ps.iter().map(|(layout, symbol)| Param {
                borrow: should_borrow_layout(layout),
                layout: *layout,
                symbol: *symbol,
            }),
            arena,
        )
        .into_bump_slice()
    }

    fn init_borrow_args_always_owned(
        arena: &'a Bump,
        ps: &'a [(Layout<'a>, Symbol)],
    ) -> &'a [Param<'a>] {
        Vec::from_iter_in(
            ps.iter().map(|(layout, symbol)| Param {
                borrow: false,
                layout: *layout,
                symbol: *symbol,
            }),
            arena,
        )
        .into_bump_slice()
    }

    fn visit_proc(&mut self, arena: &'a Bump, proc: &Proc<'a>, key: (Symbol, ProcLayout<'a>)) {
        if proc.must_own_arguments {
            self.visit_proc_always_owned(arena, proc, key);
            return;
        }
        let already_in_there = self.items.insert(
            Key::Declaration(proc.name, key.1),
            Self::init_borrow_args(arena, proc.args),
        );
        debug_assert!(already_in_there.is_none());

        self.visit_stmt(arena, proc.name, &proc.body);
    }

    fn visit_proc_always_owned(
        &mut self,
        arena: &'a Bump,
        proc: &Proc<'a>,
        key: (Symbol, ProcLayout<'a>),
    ) {
        let already_in_there = self.items.insert(
            Key::Declaration(proc.name, key.1),
            Self::init_borrow_args_always_owned(arena, proc.args),
        );
        debug_assert!(already_in_there.is_none());

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
                    body: b,
                } => {
                    let already_in_there = self
                        .items
                        .insert(Key::JoinPoint(*j), Self::init_borrow_params(arena, xs));
                    debug_assert!(
                        already_in_there.is_none(),
                        "join point {:?} is already defined!",
                        j
                    );

                    stack.push(v);
                    stack.push(b);
                }
                Let(_, _, _, cont) => {
                    stack.push(cont);
                }
                Invoke { pass, fail, .. } => {
                    stack.push(pass);
                    stack.push(fail);
                }
                Switch {
                    branches,
                    default_branch,
                    ..
                } => {
                    stack.extend(branches.iter().map(|b| &b.2));
                    stack.push(default_branch.1);
                }
                Refcounting(_, _) => unreachable!("these have not been introduced yet"),

                Ret(_) | Resume(_) | Jump(_, _) | RuntimeError(_) => {
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

    fn update_param_map(&mut self, k: Key<'a>) {
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
    /// must be taken as owned parameters
    fn collect_call(&mut self, z: Symbol, e: &crate::ir::Call<'a>) {
        use crate::ir::CallType::*;

        let crate::ir::Call {
            call_type,
            arguments,
        } = e;

        match call_type {
            ByName {
                name,
                ret_layout,
                arg_layouts,
                ..
            } => {
                let top_level = ProcLayout::new(self.arena, arg_layouts, *ret_layout);

                // get the borrow signature of the applied function
                let ps = self
                    .param_map
                    .get_symbol(*name, top_level)
                    .expect("function is defined");

                // the return value will be owned
                self.own_var(z);

                // if the function exects an owned argument (ps), the argument must be owned (args)
                debug_assert_eq!(
                    arguments.len(),
                    ps.len(),
                    "{:?} has {} parameters, but was applied to {} arguments",
                    name,
                    ps.len(),
                    arguments.len()
                );
                self.own_args_using_params(arguments, ps);
            }

            LowLevel { op, .. } => {
                debug_assert!(!op.is_higher_order());

                self.own_var(z);

                let ps = lowlevel_borrow_signature(self.arena, *op);

                self.own_args_using_bools(arguments, ps);
            }

            HigherOrderLowLevel {
                op,
                arg_layouts,
                ret_layout,
                ..
            } => {
                use roc_module::low_level::LowLevel::*;

                debug_assert!(op.is_higher_order());

                let closure_layout = ProcLayout {
                    arguments: arg_layouts,
                    result: *ret_layout,
                };

                match op {
                    ListMap | ListKeepIf | ListKeepOks | ListKeepErrs => {
                        match self.param_map.get_symbol(arguments[1], closure_layout) {
                            Some(function_ps) => {
                                // own the list if the function wants to own the element
                                if !function_ps[0].borrow {
                                    self.own_var(arguments[0]);
                                }

                                // own the closure environment if the function needs to own it
                                if let Some(false) = function_ps.get(1).map(|p| p.borrow) {
                                    self.own_var(arguments[2]);
                                }
                            }
                            None => unreachable!(),
                        }
                    }
                    ListMapWithIndex => {
                        match self.param_map.get_symbol(arguments[1], closure_layout) {
                            Some(function_ps) => {
                                // own the list if the function wants to own the element
                                if !function_ps[1].borrow {
                                    self.own_var(arguments[0]);
                                }

                                // own the closure environment if the function needs to own it
                                if let Some(false) = function_ps.get(2).map(|p| p.borrow) {
                                    self.own_var(arguments[2]);
                                }
                            }
                            None => unreachable!(),
                        }
                    }
                    ListMap2 => match self.param_map.get_symbol(arguments[2], closure_layout) {
                        Some(function_ps) => {
                            // own the lists if the function wants to own the element
                            if !function_ps[0].borrow {
                                self.own_var(arguments[0]);
                            }

                            if !function_ps[1].borrow {
                                self.own_var(arguments[1]);
                            }

                            // own the closure environment if the function needs to own it
                            if let Some(false) = function_ps.get(2).map(|p| p.borrow) {
                                self.own_var(arguments[3]);
                            }
                        }
                        None => unreachable!(),
                    },
                    ListMap3 => match self.param_map.get_symbol(arguments[3], closure_layout) {
                        Some(function_ps) => {
                            // own the lists if the function wants to own the element
                            if !function_ps[0].borrow {
                                self.own_var(arguments[0]);
                            }
                            if !function_ps[1].borrow {
                                self.own_var(arguments[1]);
                            }
                            if !function_ps[2].borrow {
                                self.own_var(arguments[2]);
                            }

                            // own the closure environment if the function needs to own it
                            if let Some(false) = function_ps.get(3).map(|p| p.borrow) {
                                self.own_var(arguments[4]);
                            }
                        }
                        None => unreachable!(),
                    },
                    ListSortWith => {
                        match self.param_map.get_symbol(arguments[1], closure_layout) {
                            Some(function_ps) => {
                                // always own the input list
                                self.own_var(arguments[0]);

                                // own the closure environment if the function needs to own it
                                if let Some(false) = function_ps.get(2).map(|p| p.borrow) {
                                    self.own_var(arguments[2]);
                                }
                            }
                            None => unreachable!(),
                        }
                    }
                    ListWalk | ListWalkUntil | ListWalkBackwards | DictWalk => {
                        match self.param_map.get_symbol(arguments[2], closure_layout) {
                            Some(function_ps) => {
                                // own the data structure if the function wants to own the element
                                if !function_ps[0].borrow {
                                    self.own_var(arguments[0]);
                                }

                                // own the default value if the function wants to own it
                                if !function_ps[1].borrow {
                                    self.own_var(arguments[1]);
                                }

                                // own the closure environment if the function needs to own it
                                if let Some(false) = function_ps.get(2).map(|p| p.borrow) {
                                    self.own_var(arguments[3]);
                                }
                            }
                            None => unreachable!(),
                        }
                    }
                    _ => {
                        // very unsure what demand RunLowLevel should place upon its arguments
                        self.own_var(z);

                        let ps = lowlevel_borrow_signature(self.arena, *op);

                        self.own_args_using_bools(arguments, ps);
                    }
                }
            }

            Foreign { .. } => {
                // very unsure what demand ForeignCall should place upon its arguments
                self.own_var(z);

                let ps = foreign_borrow_signature(self.arena, arguments.len());

                self.own_args_using_bools(arguments, ps);
            }
        }
    }

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

            Call(call) => self.collect_call(z, call),

            Literal(_) | RuntimeErrorFunction(_) => {}

            StructAtIndex { structure: x, .. } => {
                // if the structure (record/tag/array) is owned, the extracted value is
                if self.is_owned(*x) {
                    self.own_var(z);
                }

                // if the extracted value is owned, the structure must be too
                if self.is_owned(z) {
                    self.own_var(*x);
                }
            }

            UnionAtIndex { structure: x, .. } => {
                // if the structure (record/tag/array) is owned, the extracted value is
                if self.is_owned(*x) {
                    self.own_var(z);
                }

                // if the extracted value is owned, the structure must be too
                if self.is_owned(z) {
                    self.own_var(*x);
                }
            }

            GetTagId { structure: x, .. } => {
                // if the structure (record/tag/array) is owned, the extracted value is
                if self.is_owned(*x) {
                    self.own_var(z);
                }

                // if the extracted value is owned, the structure must be too
                if self.is_owned(z) {
                    self.own_var(*x);
                }
            }
        }
    }

    #[allow(clippy::many_single_char_names)]
    fn preserve_tail_call(&mut self, x: Symbol, v: &Expr<'a>, b: &Stmt<'a>) {
        if let (
            Expr::Call(crate::ir::Call {
                call_type:
                    crate::ir::CallType::ByName {
                        name: g,
                        arg_layouts,
                        ret_layout,
                        ..
                    },
                arguments: ys,
                ..
            }),
            Stmt::Ret(z),
        ) = (v, b)
        {
            let top_level = ProcLayout::new(self.arena, arg_layouts, *ret_layout);

            if self.current_proc == *g && x == *z {
                // anonymous functions (for which the ps may not be known)
                // can never be tail-recursive, so this is fine
                if let Some(ps) = self.param_map.get_symbol(*g, top_level) {
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
                body: b,
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

            Invoke {
                symbol,
                call,
                layout: _,
                pass,
                fail,
                exception_id: _,
            } => {
                self.collect_stmt(pass);
                self.collect_stmt(fail);

                self.collect_call(*symbol, call);

                // TODO how to preserve the tail call of an invoke?
                // self.preserve_tail_call(*x, v, b);
            }

            Jump(j, ys) => {
                let ps = self.param_map.get_join_point(*j);

                // for making sure the join point can reuse
                self.own_args_using_params(ys, ps);

                // for making sure the tail call is preserved
                self.own_params_using_args(ys, ps);
            }
            Switch {
                branches,
                default_branch,
                ..
            } => {
                for (_, _, b) in branches.iter() {
                    self.collect_stmt(b);
                }
                self.collect_stmt(default_branch.1);
            }
            Refcounting(_, _) => unreachable!("these have not been introduced yet"),

            Ret(_) | RuntimeError(_) | Resume(_) => {
                // these are terminal, do nothing
            }
        }
    }

    fn collect_proc(&mut self, proc: &Proc<'a>, layout: ProcLayout<'a>) {
        let old = self.param_set.clone();

        let ys = Vec::from_iter_in(proc.args.iter().map(|t| t.1), self.arena).into_bump_slice();
        self.update_param_set_symbols(ys);
        self.current_proc = proc.name;

        // ensure that current_proc is in the owned map
        self.owned.entry(proc.name).or_default();

        self.collect_stmt(&proc.body);
        self.update_param_map(Key::Declaration(proc.name, layout));

        self.param_set = old;
    }
}

pub fn foreign_borrow_signature(arena: &Bump, arity: usize) -> &[bool] {
    // NOTE this means that Roc is responsible for cleaning up resources;
    // the host cannot (currently) take ownership
    let all = bumpalo::vec![in arena; BORROWED; arity];
    all.into_bump_slice()
}

pub fn lowlevel_borrow_signature(arena: &Bump, op: LowLevel) -> &[bool] {
    use LowLevel::*;

    // TODO is true or false more efficient for non-refcounted layouts?
    let irrelevant = OWNED;
    let function = irrelevant;
    let closure_data = irrelevant;
    let owned = OWNED;
    let borrowed = BORROWED;

    // Here we define the borrow signature of low-level operations
    //
    // - arguments with non-refcounted layouts (ints, floats) are `irrelevant`
    // - arguments that we may want to update destructively must be Owned
    // - other refcounted arguments are Borrowed
    match op {
        ListLen | StrIsEmpty | StrCountGraphemes => arena.alloc_slice_copy(&[borrowed]),
        ListSet => arena.alloc_slice_copy(&[owned, irrelevant, irrelevant]),
        ListGetUnsafe => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        ListConcat => arena.alloc_slice_copy(&[owned, owned]),
        StrConcat => arena.alloc_slice_copy(&[owned, borrowed]),
        StrSplit => arena.alloc_slice_copy(&[borrowed, borrowed]),
        ListSingle => arena.alloc_slice_copy(&[irrelevant]),
        ListRepeat => arena.alloc_slice_copy(&[irrelevant, borrowed]),
        ListReverse => arena.alloc_slice_copy(&[owned]),
        ListPrepend => arena.alloc_slice_copy(&[owned, owned]),
        StrJoinWith => arena.alloc_slice_copy(&[borrowed, borrowed]),
        ListJoin => arena.alloc_slice_copy(&[irrelevant]),
        ListMap | ListMapWithIndex => arena.alloc_slice_copy(&[owned, function, closure_data]),
        ListMap2 => arena.alloc_slice_copy(&[owned, owned, function, closure_data]),
        ListMap3 => arena.alloc_slice_copy(&[owned, owned, owned, function, closure_data]),
        ListKeepIf | ListKeepOks | ListKeepErrs => {
            arena.alloc_slice_copy(&[owned, function, closure_data])
        }
        ListContains => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        ListRange => arena.alloc_slice_copy(&[irrelevant, irrelevant]),
        ListWalk | ListWalkUntil | ListWalkBackwards => {
            arena.alloc_slice_copy(&[owned, owned, function, closure_data])
        }
        ListSortWith => arena.alloc_slice_copy(&[owned, function, closure_data]),

        // TODO when we have lists with capacity (if ever)
        // List.append should own its first argument
        ListAppend => arena.alloc_slice_copy(&[owned, owned]),
        ListDrop => arena.alloc_slice_copy(&[owned, irrelevant]),
        ListSwap => arena.alloc_slice_copy(&[owned, irrelevant, irrelevant]),

        Eq | NotEq => arena.alloc_slice_copy(&[borrowed, borrowed]),

        And | Or | NumAdd | NumAddWrap | NumAddChecked | NumSub | NumSubWrap | NumSubChecked
        | NumMul | NumMulWrap | NumMulChecked | NumGt | NumGte | NumLt | NumLte | NumCompare
        | NumDivUnchecked | NumRemUnchecked | NumIsMultipleOf | NumPow | NumPowInt
        | NumBitwiseAnd | NumBitwiseXor | NumBitwiseOr | NumShiftLeftBy | NumShiftRightBy
        | NumShiftRightZfBy => arena.alloc_slice_copy(&[irrelevant, irrelevant]),

        NumAbs | NumNeg | NumSin | NumCos | NumSqrtUnchecked | NumLogUnchecked | NumRound
        | NumCeiling | NumFloor | NumToFloat | Not | NumIsFinite | NumAtan | NumAcos | NumAsin
        | NumIntCast => arena.alloc_slice_copy(&[irrelevant]),
        StrStartsWith | StrEndsWith => arena.alloc_slice_copy(&[owned, borrowed]),
        StrStartsWithCodePoint => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        StrFromUtf8 => arena.alloc_slice_copy(&[owned]),
        StrToBytes => arena.alloc_slice_copy(&[owned]),
        StrFromInt | StrFromFloat => arena.alloc_slice_copy(&[irrelevant]),
        Hash => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        DictSize => arena.alloc_slice_copy(&[borrowed]),
        DictEmpty => &[],
        DictInsert => arena.alloc_slice_copy(&[owned, owned, owned]),
        DictRemove => arena.alloc_slice_copy(&[owned, borrowed]),
        DictContains => arena.alloc_slice_copy(&[borrowed, borrowed]),
        DictGetUnsafe => arena.alloc_slice_copy(&[borrowed, borrowed]),
        DictKeys | DictValues => arena.alloc_slice_copy(&[borrowed]),
        DictUnion | DictDifference | DictIntersection => arena.alloc_slice_copy(&[owned, borrowed]),

        // borrow function argument so we don't have to worry about RC of the closure
        DictWalk => arena.alloc_slice_copy(&[owned, owned, function, closure_data]),

        SetFromList => arena.alloc_slice_copy(&[owned]),

        ExpectTrue => arena.alloc_slice_copy(&[irrelevant]),
    }
}
