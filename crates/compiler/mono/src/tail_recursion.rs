#![allow(clippy::manual_map)]

use crate::borrow::Ownership;
use crate::ir::{Call, CallType, Env, Expr, JoinPointId, Param, Proc, SelfRecursive, Stmt};
use crate::layout::{InLayout, LambdaName, LayoutInterner, LayoutRepr, TagIdIntType, UnionLayout};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_module::symbol::Symbol;

/// Make tail calls into loops (using join points)
///
/// e.g.
///
/// > factorial n accum = if n == 1 then accum else factorial (n - 1) (n * accum)
///
/// becomes
///
/// ```elm
/// factorial n1 accum1 =
///     let joinpoint j n accum =
///             if n == 1 then
///                 accum
///             else
///                 jump j (n - 1) (n * accum)
///
///     in
///         jump j n1 accum1
/// ```
///
/// This will effectively compile into a loop in llvm, and
/// won't grow the call stack for each iteration

pub fn make_tail_recursive<'a>(
    arena: &'a Bump,
    id: JoinPointId,
    needle: LambdaName,
    stmt: Stmt<'a>,
    args: &'a [(InLayout<'a>, Symbol, Symbol)],
    ret_layout: InLayout<'a>,
) -> Option<Stmt<'a>> {
    let allocated = arena.alloc(stmt);

    let new_stmt = insert_jumps(arena, allocated, id, needle, args, ret_layout)?;

    // if we did not early-return, jumps were inserted, we must now add a join point

    let params = Vec::from_iter_in(
        args.iter().map(|(layout, symbol, _)| Param {
            symbol: *symbol,
            layout: *layout,
            ownership: Ownership::Borrowed,
        }),
        arena,
    )
    .into_bump_slice();

    // TODO could this be &[]?
    let args = Vec::from_iter_in(args.iter().map(|t| t.2), arena).into_bump_slice();

    let jump = arena.alloc(Stmt::Jump(id, args));

    let join = Stmt::Join {
        id,
        remainder: jump,
        parameters: params,
        body: new_stmt,
    };

    Some(join)
}

fn insert_jumps<'a>(
    arena: &'a Bump,
    stmt: &'a Stmt<'a>,
    goal_id: JoinPointId,
    needle: LambdaName,
    needle_arguments: &'a [(InLayout<'a>, Symbol, Symbol)],
    needle_result: InLayout<'a>,
) -> Option<&'a Stmt<'a>> {
    use Stmt::*;

    // to insert a tail-call, it must not just be a call to the function itself, but it must also
    // have the same layout. In particular when lambda sets get involved, a self-recursive call may
    // have a different type and should not be converted to a jump!
    let is_equal_function = |function_name: LambdaName, arguments: &[_], result| {
        let it = needle_arguments.iter().map(|t| &t.0);
        needle == function_name && it.eq(arguments.iter()) && needle_result == result
    };

    match stmt {
        Let(
            symbol,
            Expr::Call(crate::ir::Call {
                call_type:
                    CallType::ByName {
                        name: fsym,
                        ret_layout,
                        arg_layouts,
                        ..
                    },
                arguments,
            }),
            _,
            Stmt::Ret(rsym),
        ) if symbol == rsym && is_equal_function(*fsym, arg_layouts, *ret_layout) => {
            // replace the call and return with a jump

            let jump = Stmt::Jump(goal_id, arguments);

            Some(arena.alloc(jump))
        }

        Let(symbol, expr, layout, cont) => {
            let opt_cont = insert_jumps(
                arena,
                cont,
                goal_id,
                needle,
                needle_arguments,
                needle_result,
            );

            if opt_cont.is_some() {
                let cont = opt_cont.unwrap_or(cont);

                Some(arena.alloc(Let(*symbol, expr.clone(), *layout, cont)))
            } else {
                None
            }
        }

        Join {
            id,
            parameters,
            remainder,
            body: continuation,
        } => {
            let opt_remainder = insert_jumps(
                arena,
                remainder,
                goal_id,
                needle,
                needle_arguments,
                needle_result,
            );
            let opt_continuation = insert_jumps(
                arena,
                continuation,
                goal_id,
                needle,
                needle_arguments,
                needle_result,
            );

            if opt_remainder.is_some() || opt_continuation.is_some() {
                let remainder = opt_remainder.unwrap_or(remainder);
                let continuation = opt_continuation.unwrap_or(*continuation);

                Some(arena.alloc(Join {
                    id: *id,
                    parameters,
                    remainder,
                    body: continuation,
                }))
            } else {
                None
            }
        }
        Switch {
            cond_symbol,
            cond_layout,
            branches,
            default_branch,
            ret_layout,
        } => {
            let opt_default = insert_jumps(
                arena,
                default_branch.1,
                goal_id,
                needle,
                needle_arguments,
                needle_result,
            );

            let mut did_change = false;

            let opt_branches = Vec::from_iter_in(
                branches.iter().map(|(label, info, branch)| {
                    match insert_jumps(
                        arena,
                        branch,
                        goal_id,
                        needle,
                        needle_arguments,
                        needle_result,
                    ) {
                        None => None,
                        Some(branch) => {
                            did_change = true;
                            Some((*label, info.clone(), branch.clone()))
                        }
                    }
                }),
                arena,
            );

            if opt_default.is_some() || did_change {
                let default_branch = (
                    default_branch.0.clone(),
                    opt_default.unwrap_or(default_branch.1),
                );

                let branches = if did_change {
                    let new = Vec::from_iter_in(
                        opt_branches.into_iter().zip(branches.iter()).map(
                            |(opt_branch, branch)| match opt_branch {
                                None => branch.clone(),
                                Some(new_branch) => new_branch,
                            },
                        ),
                        arena,
                    );

                    new.into_bump_slice()
                } else {
                    branches
                };

                Some(arena.alloc(Switch {
                    cond_symbol: *cond_symbol,
                    cond_layout: *cond_layout,
                    default_branch,
                    branches,
                    ret_layout: *ret_layout,
                }))
            } else {
                None
            }
        }
        Refcounting(modify, cont) => {
            match insert_jumps(
                arena,
                cont,
                goal_id,
                needle,
                needle_arguments,
                needle_result,
            ) {
                Some(cont) => Some(arena.alloc(Refcounting(*modify, cont))),
                None => None,
            }
        }

        Dbg {
            symbol,
            variable,
            remainder,
        } => match insert_jumps(
            arena,
            remainder,
            goal_id,
            needle,
            needle_arguments,
            needle_result,
        ) {
            Some(cont) => Some(arena.alloc(Dbg {
                symbol: *symbol,
                variable: *variable,
                remainder: cont,
            })),
            None => None,
        },

        Expect {
            condition,
            region,
            lookups,
            variables,
            remainder,
        } => match insert_jumps(
            arena,
            remainder,
            goal_id,
            needle,
            needle_arguments,
            needle_result,
        ) {
            Some(cont) => Some(arena.alloc(Expect {
                condition: *condition,
                region: *region,
                lookups,
                variables,
                remainder: cont,
            })),
            None => None,
        },

        ExpectFx {
            condition,
            region,
            lookups,
            variables,
            remainder,
        } => match insert_jumps(
            arena,
            remainder,
            goal_id,
            needle,
            needle_arguments,
            needle_result,
        ) {
            Some(cont) => Some(arena.alloc(ExpectFx {
                condition: *condition,
                region: *region,
                lookups,
                variables,
                remainder: cont,
            })),
            None => None,
        },

        Ret(_) => None,
        Jump(_, _) => None,
        Crash(..) => None,
    }
}

pub(crate) fn is_trmc_candidate<'a, I>(interner: &I, proc: &Proc<'a>) -> bool
where
    I: LayoutInterner<'a>,
{
    // it must be a self-recursive function
    if !matches!(
        proc.is_self_recursive,
        crate::ir::SelfRecursive::SelfRecursive(_)
    ) {
        return false;
    }

    // and return a recursive tag union
    match interner.get_repr(proc.ret_layout) {
        LayoutRepr::Union(union_layout) => union_layout.is_recursive(),
        _ => false,
    }
}

#[derive(Clone)]
pub(crate) struct TrmcEnv<'a> {
    function_name: LambdaName<'a>,
    hole_symbol: Symbol,
    null_symbol: Symbol,
    initial_box_symbol: Symbol,
    joinpoint_id: JoinPointId,
    return_layout: InLayout<'a>,
    box_return_layout: InLayout<'a>,

    // the call we are performing TRMC on
    recursive_call: Option<(Symbol, Call<'a>)>,
}

struct ConstructorInfo<'a> {
    tag_layout: UnionLayout<'a>,
    tag_id: TagIdIntType,
    arguments: &'a [Symbol],
}

impl<'a> TrmcEnv<'a> {
    fn is_recursive_expr(&mut self, expr: &Expr<'a>) -> Option<Call<'a>> {
        if let Expr::Call(call) = expr {
            self.is_recursive_call(call).then_some(call.clone())
        } else {
            None
        }
    }

    fn is_terminal_constructor(&mut self, stmt: &Stmt<'a>) -> Option<ConstructorInfo<'a>> {
        match stmt {
            Stmt::Let(s1, expr, _layout, Stmt::Ret(s2)) if s1 == s2 => {
                self.get_contructor_info(expr)
            }

            _ => None,
        }
    }

    fn get_contructor_info(&mut self, expr: &Expr<'a>) -> Option<ConstructorInfo<'a>> {
        if let Expr::Tag {
            tag_layout,
            tag_id,
            arguments,
        } = expr
        {
            let info = ConstructorInfo {
                tag_layout: *tag_layout,
                tag_id: *tag_id,
                arguments,
            };

            Some(info)
        } else {
            None
        }
    }

    fn is_recursive_call(&mut self, call: &Call<'a>) -> bool {
        match call.call_type {
            CallType::ByName {
                name,
                ret_layout,
                arg_layouts,
                specialization_id,
            } => {
                // TODO are there other restrictions?
                name == self.function_name
            }
            CallType::Foreign { .. } | CallType::LowLevel { .. } | CallType::HigherOrder(_) => {
                false
            }
        }
    }

    fn ptr_write(
        env: &mut Env<'a, '_>,
        interner: &mut impl LayoutInterner<'a>,
        return_layout: InLayout<'a>,
        ptr: Symbol,
        value: Symbol,
        next: &'a Stmt<'a>,
    ) -> Stmt<'a> {
        let box_write = Call {
            call_type: crate::ir::CallType::LowLevel {
                op: roc_module::low_level::LowLevel::PtrWrite,
                update_mode: env.next_update_mode_id(),
            },
            arguments: env.arena.alloc([ptr, value]),
        };

        Stmt::Let(
            env.named_unique_symbol("_ptr_write_unit"),
            Expr::Call(box_write),
            interner.insert_direct_no_semantic(LayoutRepr::Boxed(return_layout)),
            next,
        )
    }

    pub fn init(
        env: &mut Env<'a, '_>,
        interner: &mut impl LayoutInterner<'a>,
        proc: &Proc<'a>,
    ) -> Proc<'a> {
        let arena = env.arena;
        let return_layout = proc.ret_layout;

        let mut joinpoint_parameters = Vec::with_capacity_in(proc.args.len() + 1, env.arena);
        let mut new_proc_arguments = Vec::with_capacity_in(proc.args.len(), env.arena);
        let mut jump_arguments = Vec::with_capacity_in(proc.args.len() + 1, env.arena);

        for (i, (layout, old_symbol)) in proc.args.iter().enumerate() {
            let symbol = env.named_unique_symbol(&format!("arg_{i}"));
            new_proc_arguments.push((*layout, symbol));
            jump_arguments.push(symbol);

            let param = Param {
                symbol: *old_symbol,
                ownership: Ownership::Owned,
                layout: *layout,
            };
            joinpoint_parameters.push(param);
        }

        // the root of the recursive structure that we'll be building
        let initial_box_symbol = env.named_unique_symbol("initial");
        jump_arguments.push(initial_box_symbol);

        let null_symbol = env.named_unique_symbol("null");
        let let_null = |next| Stmt::Let(null_symbol, Expr::NullPointer, return_layout, next);

        let box_return_layout =
            interner.insert_direct_no_semantic(LayoutRepr::Boxed(return_layout));
        let box_null = Expr::ExprBox {
            symbol: null_symbol,
        };
        let let_box = |next| Stmt::Let(initial_box_symbol, box_null, box_return_layout, next);

        let joinpoint_id = JoinPointId(env.named_unique_symbol("trmc"));
        let hole_symbol = env.named_unique_symbol("hole");

        let jump_stmt = Stmt::Jump(joinpoint_id, jump_arguments.into_bump_slice());

        let mut this = Self {
            function_name: proc.name,
            hole_symbol,
            null_symbol,
            initial_box_symbol,
            joinpoint_id,
            return_layout,
            box_return_layout,
            recursive_call: None,
        };

        let param = Param {
            symbol: hole_symbol,
            ownership: Ownership::Owned,
            layout: box_return_layout,
        };
        joinpoint_parameters.push(param);

        let joinpoint = Stmt::Join {
            id: joinpoint_id,
            parameters: joinpoint_parameters.into_bump_slice(),
            body: arena.alloc(this.walk_stmt(env, interner, &proc.body)),
            remainder: arena.alloc(jump_stmt),
        };

        let body = let_null(arena.alloc(
            //
            let_box(arena.alloc(
                //
                joinpoint,
            )),
        ));

        #[cfg(debug_assertions)]
        env.home.register_debug_idents(env.ident_ids);

        Proc {
            name: proc.name,
            args: new_proc_arguments.into_bump_slice(),
            body,
            closure_data_layout: proc.closure_data_layout,
            ret_layout: proc.ret_layout,
            is_self_recursive: SelfRecursive::NotSelfRecursive,
            host_exposed_layouts: proc.host_exposed_layouts.clone(),
        }
    }

    fn walk_stmt(
        &mut self,
        env: &mut Env<'a, '_>,
        interner: &mut impl LayoutInterner<'a>,
        stmt: &Stmt<'a>,
    ) -> Stmt<'a> {
        let arena = env.arena;

        match stmt {
            Stmt::Let(symbol, expr, layout, next) => {
                if self.recursive_call.is_none() {
                    if let Some(call) = self.is_recursive_expr(expr) {
                        self.recursive_call = Some((*symbol, call));
                        return self.walk_stmt(env, interner, next);
                    }
                }

                if let Some(cons_info) = self.is_terminal_constructor(stmt) {
                    match &self.recursive_call {
                        None => {
                            // this control flow path did not encounter a recursive call. Just
                            // write the end result into the hole and we're done.

                            let define_tag = |next| Stmt::Let(*symbol, expr.clone(), *layout, next);

                            let output = define_tag(arena.alloc(
                                //
                                self.non_trmc_return(env, interner, *symbol),
                            ));

                            return output;
                        }
                        Some((call_symbol, call)) => {
                            // we did encounter a recursive call, and can perform TRMC in this
                            // branch.

                            // TODO remove unwrap. also what if the symbol occurs more than once?
                            let recursive_field_index = cons_info
                                .arguments
                                .iter()
                                .position(|s| *s == *call_symbol)
                                .unwrap();

                            let mut arguments =
                                Vec::from_iter_in(cons_info.arguments.iter().copied(), env.arena);
                            arguments[recursive_field_index] = self.null_symbol;

                            let tag_expr = Expr::Tag {
                                tag_layout: cons_info.tag_layout,
                                tag_id: cons_info.tag_id,
                                arguments: arguments.into_bump_slice(),
                            };

                            let let_tag = |next| Stmt::Let(*symbol, tag_expr, *layout, next);

                            let get_reference_expr = Expr::ExprBox {
                                symbol: self.null_symbol,
                            };

                            let new_hole_symbol = env.named_unique_symbol("newHole");
                            let let_new_hole = |next| {
                                Stmt::Let(
                                    new_hole_symbol,
                                    get_reference_expr,
                                    self.box_return_layout,
                                    next,
                                )
                            };

                            let mut jump_arguments =
                                Vec::from_iter_in(call.arguments.iter().copied(), env.arena);
                            jump_arguments.push(new_hole_symbol);

                            let jump =
                                Stmt::Jump(self.joinpoint_id, jump_arguments.into_bump_slice());

                            let output = let_tag(arena.alloc(
                                //
                                let_new_hole(arena.alloc(
                                    //
                                    Self::ptr_write(
                                        env,
                                        interner,
                                        *layout,
                                        self.hole_symbol,
                                        *symbol,
                                        arena.alloc(jump),
                                    ),
                                )),
                            ));

                            return output;
                        }
                    }
                }

                let next = self.walk_stmt(env, interner, next);
                Stmt::Let(*symbol, expr.clone(), *layout, arena.alloc(next))
            }
            Stmt::Switch {
                cond_symbol,
                cond_layout,
                branches,
                default_branch,
                ret_layout,
            } => {
                let mut new_branches = Vec::with_capacity_in(branches.len(), arena);

                let opt_recursive_call = self.recursive_call.clone();

                for (id, info, stmt) in branches.iter() {
                    self.recursive_call = opt_recursive_call.clone();
                    let new_stmt = self.walk_stmt(env, interner, stmt);

                    new_branches.push((*id, info.clone(), new_stmt));
                }

                self.recursive_call = opt_recursive_call;
                let new_default_branch =
                    &*arena.alloc(self.walk_stmt(env, interner, default_branch.1));

                Stmt::Switch {
                    cond_symbol: *cond_symbol,
                    cond_layout: *cond_layout,
                    branches: &*arena.alloc(new_branches.into_bump_slice()),
                    default_branch: (default_branch.0.clone(), new_default_branch),
                    ret_layout: *ret_layout,
                }
            }
            Stmt::Ret(symbol) => {
                // write the symbol we're supposed to return into the hole
                // then read initial_symbol and return its contents
                self.non_trmc_return(env, interner, *symbol)
            }
            Stmt::Refcounting(_, _) => todo!(),
            Stmt::Expect { .. } => todo!(),
            Stmt::ExpectFx { .. } => todo!(),
            Stmt::Dbg { .. } => todo!(),
            Stmt::Join { .. } => todo!(),
            Stmt::Jump(_, _) => todo!(),
            Stmt::Crash(_, _) => todo!(),
        }
    }

    fn non_trmc_return(
        &mut self,
        env: &mut Env<'a, '_>,
        interner: &mut impl LayoutInterner<'a>,
        value_symbol: Symbol,
    ) -> Stmt<'a> {
        let arena = env.arena;
        let layout = self.return_layout;

        let unbox_expr = Expr::ExprUnbox {
            symbol: self.initial_box_symbol,
        };
        let final_symbol = env.named_unique_symbol("final");
        let unbox = |next| Stmt::Let(final_symbol, unbox_expr, layout, next);

        Self::ptr_write(
            env,
            interner,
            layout,
            self.hole_symbol,
            value_symbol,
            arena.alloc(
                //
                unbox(arena.alloc(Stmt::Ret(final_symbol))),
            ),
        )
    }
}
