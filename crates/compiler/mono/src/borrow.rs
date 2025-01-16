use bumpalo::{
    collections::{CollectIn, Vec},
    Bump,
};
use roc_collections::{MutMap, ReferenceMatrix};
use roc_module::symbol::Symbol;

use crate::{
    inc_dec::Ownership,
    ir::{Call, CallType, Expr, JoinPointId, Param, Proc, ProcLayout, Stmt},
    layout::{Builtin, InLayout, LayoutInterner, LayoutRepr, Niche},
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) struct BorrowSignature(u64);

impl std::fmt::Debug for BorrowSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = &mut f.debug_struct("BorrowSignature");

        for (i, ownership) in self.iter().enumerate() {
            f = f.field(&format!("_{i}"), &ownership);
        }

        f.finish()
    }
}

impl BorrowSignature {
    fn new(len: usize) -> Self {
        assert!(len < 64 - 8);

        Self(len as _)
    }

    fn from_layouts<'a>(
        interner: &impl LayoutInterner<'a>,
        layouts: impl ExactSizeIterator<Item = &'a InLayout<'a>>,
    ) -> Self {
        let mut signature = BorrowSignature::new(layouts.len());

        for (i, layout) in layouts.enumerate() {
            signature.set(i, layout_to_ownership(*layout, interner));
        }

        signature
    }

    fn len(&self) -> usize {
        (self.0 & 0xFF) as usize
    }

    fn get(&self, index: usize) -> Option<&Ownership> {
        if index >= self.len() {
            return None;
        }

        match self.0 & (1 << (index + 8)) {
            0 => Some(&Ownership::Borrowed),
            _ => Some(&Ownership::Owned),
        }
    }

    fn set(&mut self, index: usize, ownership: Ownership) -> bool {
        assert!(index < self.len());

        let modified = self.get(index) != Some(&ownership);

        let mask = 1 << (index + 8);

        match ownership {
            Ownership::Owned => self.0 |= mask,
            Ownership::Borrowed => self.0 &= !mask,
        }

        modified
    }

    pub fn iter(&self) -> impl Iterator<Item = Ownership> + '_ {
        let mut i = 0;

        std::iter::from_fn(move || {
            let value = self.get(i)?;
            i += 1;
            Some(*value)
        })
    }
}

impl std::ops::Index<usize> for BorrowSignature {
    type Output = Ownership;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).unwrap()
    }
}

pub(crate) struct BorrowSignatures<'a> {
    pub(crate) procs: MutMap<(Symbol, ProcLayout<'a>), BorrowSignature>,
}

pub(crate) fn infer_borrow_signatures<'a>(
    arena: &'a Bump,
    interner: &impl LayoutInterner<'a>,
    procs: &MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> BorrowSignatures<'a> {
    let mut borrow_signatures: BorrowSignatures = BorrowSignatures {
        procs: procs
            .iter()
            .map(|(_key, proc)| {
                let key = (proc.name.name(), proc.proc_layout(arena));
                let signature = BorrowSignature::from_layouts(interner, key.1.arguments.iter());
                (key, signature)
            })
            .collect(),
    };

    // for every proc (by index) a collection of its join points
    let mut join_points: Vec<_> = std::iter::repeat_with(MutMap::default)
        .take(procs.len())
        .collect_in(arena);

    // next we first partition the functions into strongly connected components, then do a
    // topological sort on these components, finally run the fix-point borrow analysis on each
    // component (in top-sorted order, from primitives (std-lib) to main)

    let matrix = construct_reference_matrix(arena, procs);
    let sccs = matrix.strongly_connected_components_all();

    let mut join_point_stack = Vec::new_in(arena);
    let mut proc_join_points = MutMap::default();

    for (group, _) in sccs.groups() {
        // This is a fixed-point analysis
        //
        // all functions initially own all their parameters
        // through a series of checks and heuristics, some arguments are set to borrowed
        // when that doesn't lead to conflicts the change is kept, otherwise it may be reverted
        //
        // when the signatures no longer change, the analysis stops and returns the signatures

        loop {
            let mut modified = false;

            for index in group.iter_ones() {
                let (_, proc) = procs.iter().nth(index).unwrap();
                let key = (proc.name.name(), proc.proc_layout(arena));

                if proc.args.is_empty() {
                    continue;
                }

                std::mem::swap(&mut proc_join_points, &mut join_points[index]);

                let mut state = State {
                    args: proc.args,
                    borrow_signature: *borrow_signatures.procs.get(&key).unwrap(),
                    join_point_stack,
                    join_points: proc_join_points,
                    modified: false,
                };

                state.inspect_stmt(interner, &mut borrow_signatures, &proc.body);

                // did any proc signature get modified?
                //
                // NOTE: this does not directly include updates to join point signatures. The
                // assumption is that a relevant change in join point signature is immediately
                // (i.e. no fixpoint is required) reflected in the proc signature.
                //
                // TODO: this is a load-bearing assert! There must be UB somewhere, removing this
                // assert causes the code to run into an infinite loop that terminates when the
                // memory on the system is exhausted.
                assert_eq!(
                    state.modified,
                    borrow_signatures
                        .procs
                        .insert(key, state.borrow_signature)
                        .unwrap()
                        != state.borrow_signature
                );
                modified |= state.modified;

                proc_join_points = state.join_points;

                std::mem::swap(&mut proc_join_points, &mut join_points[index]);

                join_point_stack = state.join_point_stack;
                join_point_stack.clear();
            }

            if !modified {
                break;
            }
        }
    }

    borrow_signatures
}

struct State<'state, 'arena> {
    /// Argument symbols with a layout of `List *` or `Str`, i.e. the layouts
    /// for which borrow inference might decide to pass as borrowed
    args: &'state [(InLayout<'arena>, Symbol)],
    borrow_signature: BorrowSignature,
    join_point_stack: Vec<'arena, (JoinPointId, &'state [Param<'arena>])>,
    join_points: MutMap<JoinPointId, BorrowSignature>,
    modified: bool,
}

fn layout_to_ownership<'a>(
    in_layout: InLayout<'a>,
    interner: &impl LayoutInterner<'a>,
) -> Ownership {
    match interner.get_repr(in_layout) {
        LayoutRepr::Builtin(Builtin::Str) => Ownership::Borrowed,
        LayoutRepr::Builtin(Builtin::List(_)) => Ownership::Borrowed,
        LayoutRepr::LambdaSet(inner) => {
            layout_to_ownership(inner.runtime_representation(), interner)
        }
        _ => Ownership::Owned,
    }
}

impl<'state, 'a> State<'state, 'a> {
    /// Mark the given argument symbol as Owned if the symbol participates in borrow inference
    ///
    /// Currently argument symbols participate if `layout_to_ownership` returns `Borrowed` for their layout.
    fn mark_owned(&mut self, symbol: Symbol) {
        if let Some(index) = self.args.iter().position(|(_, s)| *s == symbol) {
            self.modified |= self.borrow_signature.set(index, Ownership::Owned);
        }

        // theory: relevant modification to a join point borrow signature is always immediately
        // reflected in the borrow signature of its surrounding function. Therefore we don't need
        // to include changes to join point signatures in the `modified` flag.
        for (id, params) in &self.join_point_stack {
            if let Some(index) = params.iter().position(|p| p.symbol == symbol) {
                self.join_points
                    .get_mut(id)
                    .unwrap()
                    .set(index, Ownership::Owned);
            }
        }
    }

    fn inspect_stmt(
        &mut self,
        interner: &impl LayoutInterner<'a>,
        borrow_signatures: &mut BorrowSignatures<'a>,
        stmt: &Stmt<'a>,
    ) {
        match stmt {
            Stmt::Let(_, expr, _, stmt) => {
                self.inspect_expr(borrow_signatures, expr);
                self.inspect_stmt(interner, borrow_signatures, stmt);
            }
            Stmt::Switch {
                branches,
                default_branch,
                ..
            } => {
                for (_, _, stmt) in branches.iter() {
                    self.inspect_stmt(interner, borrow_signatures, stmt);
                }
                self.inspect_stmt(interner, borrow_signatures, default_branch.1);
            }
            Stmt::Ret(s) => {
                // to return a value we must own it
                // (with the current implementation anyway)
                self.mark_owned(*s);
            }
            Stmt::Refcounting(_, _) => unreachable!("not inserted yet"),
            Stmt::Expect { remainder, .. } => {
                // based on my reading of inc_dec.rs, expect borrows the symbols
                self.inspect_stmt(interner, borrow_signatures, remainder);
            }
            Stmt::Dbg { remainder, .. } => {
                // based on my reading of inc_dec.rs, expect borrows the symbol
                self.inspect_stmt(interner, borrow_signatures, remainder);
            }
            Stmt::Join {
                id,
                parameters,
                body,
                remainder,
            } => {
                // insert the default borrow signature if we're seeing this JP for the first time
                self.join_points.entry(*id).or_insert_with(|| {
                    BorrowSignature::from_layouts(interner, parameters.iter().map(|p| &p.layout))
                });

                // within the body, changes to ownership for symbols introduced by this join point
                // must be propagated. An example is
                //
                // ```roc
                // writeIndents = \buf, indents ->
                //     if indents <= 0 then
                //         buf
                //     else
                //         buf
                //         |> Str.concat "    "
                //         |> writeIndents (indents - 1)
                // ```
                //
                // where the mono will jump immediately to a (recursive) join point. The fact that
                // the `buf` value is owned within the join point for efficient concatentation must
                // be propagated to `writeIndents`' function argument
                self.join_point_stack.push((*id, parameters));
                self.inspect_stmt(interner, borrow_signatures, body);
                self.join_point_stack.pop().unwrap();

                self.inspect_stmt(interner, borrow_signatures, remainder);
            }

            Stmt::Jump(id, arguments) => {
                let borrow_signature = match self.join_points.get(id) {
                    Some(s) => *s,
                    None => unreachable!("no borrow signature for join point {id:?} layout"),
                };

                for (argument, ownership) in arguments.iter().zip(borrow_signature.iter()) {
                    if let Ownership::Owned = ownership {
                        self.mark_owned(*argument);
                    }
                }
            }

            Stmt::Crash(_, _) => { /* not relevant for ownership */ }
        }
    }

    fn inspect_expr(&mut self, borrow_signatures: &mut BorrowSignatures<'a>, expr: &Expr<'a>) {
        if let Expr::Call(call) = expr {
            self.inspect_call(borrow_signatures, call)
        }
    }

    fn inspect_call(&mut self, borrow_signatures: &mut BorrowSignatures<'a>, call: &Call<'a>) {
        let Call {
            call_type,
            arguments,
        } = call;

        match call_type {
            CallType::ByName {
                name,
                arg_layouts,
                ret_layout,
                ..
            } => {
                let proc_layout = ProcLayout {
                    arguments: arg_layouts,
                    result: *ret_layout,
                    niche: Niche::NONE,
                };

                let borrow_signature =
                    match borrow_signatures.procs.get(&(name.name(), proc_layout)) {
                        Some(s) => s,
                        None =>  unreachable!(
                            "\n\tNo borrow signature for {name:?} layout.\n\n\t\
                            Tip 1: This can happen when you call a function with fewer arguments than it expects.\n\t\
                            Like `Arg.list!` instead of `Arg.list! {{}}`.\n\t\
                            Tip 2: `roc check yourfile.roc` can sometimes give you a helpful error.
                            "
                        )
                    };

                for (argument, ownership) in arguments.iter().zip(borrow_signature.iter()) {
                    if let Ownership::Owned = ownership {
                        self.mark_owned(*argument);
                    }
                }
            }
            CallType::LowLevel { op, .. } => {
                // if the lowlevel must own the argument, mark it as owned
                let borrow_signature = crate::inc_dec::lowlevel_borrow_signature(*op);

                for (argument, ownership) in arguments.iter().zip(borrow_signature) {
                    if ownership.is_owned() {
                        self.mark_owned(*argument);
                    }
                }
            }
            CallType::ByPointer { .. } | CallType::Foreign { .. } | CallType::HigherOrder(_) => {
                for argument in arguments.iter() {
                    self.mark_owned(*argument)
                }
            }
        }
    }
}

fn construct_reference_matrix<'a>(
    arena: &'a Bump,
    procs: &MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> ReferenceMatrix {
    let mut matrix = ReferenceMatrix::new(procs.len());

    let mut call_info = CallInfo::new(arena);

    for (row, proc) in procs.values().enumerate() {
        call_info.clear();
        call_info.stmt(arena, &proc.body);

        for key in call_info.keys.iter() {
            // the same symbol can be in `keys` multiple times (with different layouts)
            for (col, (k, _)) in procs.keys().enumerate() {
                if k == key {
                    matrix.set_row_col(row, col, true);
                }
            }
        }
    }

    matrix
}

struct CallInfo<'a> {
    keys: Vec<'a, Symbol>,
}

impl<'a> CallInfo<'a> {
    fn new(arena: &'a Bump) -> Self {
        CallInfo {
            keys: Vec::new_in(arena),
        }
    }

    fn clear(&mut self) {
        self.keys.clear()
    }

    fn call(&mut self, call: &crate::ir::Call<'a>) {
        use crate::ir::CallType::*;
        use crate::ir::HigherOrderLowLevel;
        use crate::ir::PassedFunction;

        match call.call_type {
            ByName { name, .. } => {
                self.keys.push(name.name());
            }
            ByPointer { .. } => {
                // nothing to be done
            }
            Foreign { .. } => {}
            LowLevel { .. } => {}
            HigherOrder(HigherOrderLowLevel {
                passed_function: PassedFunction { name, .. },
                ..
            }) => {
                self.keys.push(name.name());
            }
        }
    }

    fn stmt(&mut self, arena: &'a Bump, stmt: &Stmt<'a>) {
        use Stmt::*;

        let mut stack = bumpalo::vec![in arena; stmt];

        while let Some(stmt) = stack.pop() {
            match stmt {
                Join {
                    remainder: v,
                    body: b,
                    ..
                } => {
                    stack.push(v);
                    stack.push(b);
                }
                Let(_, expr, _, cont) => {
                    if let Expr::Call(call) = expr {
                        self.call(call);
                    }
                    stack.push(cont);
                }
                Switch {
                    branches,
                    default_branch,
                    ..
                } => {
                    stack.extend(branches.iter().map(|b| &b.2));
                    stack.push(default_branch.1);
                }

                Dbg { remainder, .. } => stack.push(remainder),
                Expect { remainder, .. } => stack.push(remainder),

                Refcounting(_, _) => unreachable!("these have not been introduced yet"),

                Ret(_) | Jump(_, _) | Crash(..) => {
                    // these are terminal, do nothing
                }
            }
        }
    }
}
