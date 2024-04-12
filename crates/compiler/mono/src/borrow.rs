use bumpalo::{collections::Vec, Bump};
use roc_module::symbol::Symbol;

use crate::{
    inc_dec::Ownership,
    ir::{Call, CallType, Expr, Proc, Stmt},
    layout::{Builtin, InLayout, LayoutInterner, LayoutRepr},
};

#[allow(unused)]
pub(crate) fn infer_borrow_signature<'a>(
    arena: &'a Bump,
    interner: &impl LayoutInterner<'a>,
    proc: &'a Proc<'a>,
) -> &'a [Ownership] {
    let mut state = State::new(arena, interner, proc);
    state.inspect_stmt(&proc.body);
    state.borrow_signature
}

struct State<'a> {
    /// Argument symbols with a layout of `List *` or `Str`, i.e. the layouts
    /// for which borrow inference might decide to pass as borrowed
    args: &'a [(InLayout<'a>, Symbol)],
    borrow_signature: &'a mut [Ownership],
}

fn layout_to_ownership<'a>(
    in_layout: InLayout<'a>,
    interner: &impl LayoutInterner<'a>,
) -> Ownership {
    match interner.get_repr(in_layout) {
        LayoutRepr::Builtin(Builtin::Str | Builtin::List(_)) => Ownership::Borrowed,
        LayoutRepr::LambdaSet(inner) => {
            layout_to_ownership(inner.runtime_representation(), interner)
        }
        _ => Ownership::Owned,
    }
}

impl<'a> State<'a> {
    fn new(arena: &'a Bump, interner: &impl LayoutInterner<'a>, proc: &'a Proc<'a>) -> Self {
        let borrow_signature = Vec::from_iter_in(
            proc.args
                .iter()
                .map(|(in_layout, _)| layout_to_ownership(*in_layout, interner)),
            arena,
        )
        .into_bump_slice_mut();

        Self {
            args: proc.args,
            borrow_signature,
        }
    }

    fn mark_owned(&mut self, symbol: Symbol) {
        if let Some(index) = self.args.iter().position(|(_, s)| *s == symbol) {
            self.borrow_signature[index] = Ownership::Owned;
        }
    }

    fn inspect_stmt(&mut self, stmt: &'a Stmt<'a>) {
        match stmt {
            Stmt::Let(_, expr, _, stmt) => {
                self.inspect_expr(expr);
                self.inspect_stmt(stmt);
            }
            Stmt::Switch {
                branches,
                default_branch,
                ..
            } => {
                for (_, _, stmt) in branches.iter() {
                    self.inspect_stmt(stmt);
                }
                self.inspect_stmt(default_branch.1);
            }
            Stmt::Ret(_) => todo!(),
            Stmt::Refcounting(_, _) => todo!(),
            Stmt::Expect { .. } | Stmt::ExpectFx { .. } => {
                // TODO do we rely on values being passed by-value here?
                // it would be better to pass by-reference in general
            }
            Stmt::Dbg { .. } => {
                // TODO do we rely on values being passed by-value here?
                // it would be better to pass by-reference in general
            }
            Stmt::Join {
                body, remainder, ..
            } => {
                self.inspect_stmt(body);
                self.inspect_stmt(remainder);
            }

            Stmt::Jump(_, _) | Stmt::Crash(_, _) => { /* not relevant for ownership */ }
        }
    }

    fn inspect_expr(&mut self, expr: &'a Expr<'a>) {
        if let Expr::Call(call) = expr {
            self.inspect_call(call)
        }
    }

    fn inspect_call(&mut self, call: &'a Call<'a>) {
        let Call {
            call_type,
            arguments,
        } = call;

        match call_type {
            CallType::ByName { name: _, .. } => {
                // TODO ownership should depend on the borrow signature of the called function
                for argument in arguments.iter() {
                    self.mark_owned(*argument)
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
