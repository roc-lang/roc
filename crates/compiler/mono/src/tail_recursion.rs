#![allow(clippy::manual_map)]

use crate::borrow::Ownership;
use crate::ir::{
    Call, CallType, Expr, JoinPointId, Param, Proc, ProcLayout, SelfRecursive, Stmt, UpdateModeId,
};
use crate::layout::{
    InLayout, LambdaName, Layout, LayoutInterner, LayoutRepr, STLayoutInterner, TagIdIntType,
    UnionLayout,
};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::{MutMap, VecSet};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};

pub struct Env<'a, 'i> {
    arena: &'a Bump,
    home: ModuleId,
    interner: &'i mut STLayoutInterner<'a>,
    ident_ids: &'i mut IdentIds,
}

impl<'a, 'i> Env<'a, 'i> {
    fn unique_symbol(&mut self) -> Symbol {
        let ident_id = self.ident_ids.gen_unique();

        Symbol::new(self.home, ident_id)
    }

    fn named_unique_symbol(&mut self, name: &str) -> Symbol {
        let ident_id = self.ident_ids.add_str(name);
        Symbol::new(self.home, ident_id)
    }
}

pub fn apply_trmc<'a, 'i>(
    arena: &'a Bump,
    interner: &'i mut STLayoutInterner<'a>,
    home: ModuleId,
    ident_ids: &'i mut IdentIds,
    procs: &mut MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) {
    let mut env = Env {
        arena,
        interner,
        home,
        ident_ids,
    };

    let env = &mut env;

    for proc in procs.values_mut() {
        use self::SelfRecursive::*;
        if let SelfRecursive(id) = proc.is_self_recursive {
            let trmc_candidate_symbols = trmc_candidates(env.interner, proc);

            if !trmc_candidate_symbols.is_empty() {
                let new_proc =
                    crate::tail_recursion::TrmcEnv::init(env, proc, trmc_candidate_symbols);
                *proc = new_proc;
            } else {
                let mut args = Vec::with_capacity_in(proc.args.len(), arena);
                let mut proc_args = Vec::with_capacity_in(proc.args.len(), arena);

                for (layout, symbol) in proc.args {
                    let new = env.unique_symbol();
                    args.push((*layout, *symbol, new));
                    proc_args.push((*layout, new));
                }

                let transformed = crate::tail_recursion::make_tail_recursive(
                    arena,
                    id,
                    proc.name,
                    proc.body.clone(),
                    args.into_bump_slice(),
                    proc.ret_layout,
                );

                if let Some(with_tco) = transformed {
                    proc.body = with_tco;
                    proc.args = proc_args.into_bump_slice();
                }
            }
        }
    }
}

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

fn make_tail_recursive<'a>(
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

#[derive(Debug, Clone, Default)]
struct TrmcCandidateSet {
    /// Recursive calls for which we have found a TRMC opportunity
    confirmed: VecSet<Symbol>,
    /// Recursive calls that are (still) considered for TRMC
    active: VecSet<Symbol>,
    /// Recursive calls that are used in such a way that makes TRMC impossible
    invalid: VecSet<Symbol>,
}

impl TrmcCandidateSet {
    fn insert(&mut self, call: Symbol) {
        // there really is no way it could have been inserted already
        debug_assert!(!self.invalid.contains(&call));

        self.active.insert(call);
    }

    fn extend(&mut self, other: Self) {
        self.confirmed.keep_if_in_either(other.confirmed);
        self.invalid.keep_if_in_either(other.invalid);
        self.active.keep_if_in_either(other.active);

        self.active.retain(|k| !self.invalid.contains(k));
        self.confirmed.retain(|k| !self.invalid.contains(k));
    }

    fn retain<F>(&mut self, keep: F)
    where
        F: Fn(&Symbol) -> bool,
    {
        for c in self.active.iter() {
            if !keep(c) {
                self.invalid.insert(*c);
            }
        }

        self.active.retain(|k| !self.invalid.contains(k));
        self.confirmed.retain(|k| !self.invalid.contains(k));
    }
}

fn trmc_candidates<'a, I>(interner: &'_ I, proc: &'_ Proc<'a>) -> VecSet<Symbol>
where
    I: LayoutInterner<'a>,
{
    // it must be a self-recursive function
    if !matches!(
        proc.is_self_recursive,
        crate::ir::SelfRecursive::SelfRecursive(_)
    ) {
        return VecSet::default();
    }

    // and return a recursive tag union
    if !matches!(interner.get_repr(proc.ret_layout), LayoutRepr::Union(union_layout) if union_layout.is_recursive())
    {
        return VecSet::default();
    }

    trmc_candidates_help(proc.name, &proc.body, TrmcCandidateSet::default()).confirmed
}

fn trmc_candidates_help<'a>(
    function_name: LambdaName,
    stmt: &'_ Stmt<'a>,
    mut candidates: TrmcCandidateSet,
) -> TrmcCandidateSet {
    // if this stmt is the literal tail tag application and return, then this is a TRMC opportunity
    if let Some(cons_info) = TrmcEnv::is_terminal_constructor(stmt) {
        // must use the result of a recursive call directly as an argument
        // we pick the (syntactically) first one
        for recursive_call in candidates.active.iter() {
            if cons_info.arguments.contains(recursive_call) {
                return TrmcCandidateSet {
                    confirmed: VecSet::singleton(*recursive_call),
                    active: VecSet::default(),
                    invalid: candidates.invalid,
                };
            }
        }
    }

    // if the stmt uses the active recursive call, that invalidates the recursive call for this branch
    candidates.retain(|recursive_call| !stmt_contains_symbol_nonrec(stmt, *recursive_call));

    match stmt {
        Stmt::Let(symbol, expr, _, next) => {
            // find a new recursive call if we currently have none
            // that means we generally pick the first recursive call we find
            if TrmcEnv::is_recursive_expr(expr, function_name).is_some() {
                candidates.insert(*symbol);
            }

            trmc_candidates_help(function_name, next, candidates)
        }
        Stmt::Switch {
            branches,
            default_branch,
            ..
        } => {
            let it = branches
                .iter()
                .map(|(_, _, stmt)| stmt)
                .chain([default_branch.1]);

            let mut accum = candidates.clone();

            for next in it {
                let x = trmc_candidates_help(function_name, next, candidates.clone());

                accum.extend(x);
            }

            accum
        }
        Stmt::Refcounting(_, next) => trmc_candidates_help(function_name, next, candidates),
        Stmt::Expect { remainder, .. }
        | Stmt::ExpectFx { remainder, .. }
        | Stmt::Dbg { remainder, .. } => trmc_candidates_help(function_name, remainder, candidates),
        Stmt::Join {
            body, remainder, ..
        } => {
            let mut x = trmc_candidates_help(function_name, body, candidates.clone());
            let y = trmc_candidates_help(function_name, remainder, candidates.clone());

            x.extend(y);

            x
        }
        Stmt::Ret(_) | Stmt::Jump(_, _) | Stmt::Crash(_, _) => candidates,
    }
}

#[derive(Clone)]
pub(crate) struct TrmcEnv<'a> {
    hole_symbol: Symbol,
    head_symbol: Symbol,
    joinpoint_id: JoinPointId,
    return_layout: InLayout<'a>,
    ptr_return_layout: InLayout<'a>,

    trmc_calls: MutMap<Symbol, Option<Call<'a>>>,
}

#[derive(Debug)]
struct ConstructorInfo<'a> {
    tag_layout: UnionLayout<'a>,
    tag_id: TagIdIntType,
    arguments: &'a [Symbol],
}

impl<'a> TrmcEnv<'a> {
    #[inline(always)]
    fn is_terminal_constructor(stmt: &Stmt<'a>) -> Option<ConstructorInfo<'a>> {
        match stmt {
            Stmt::Let(s1, expr, _layout, Stmt::Ret(s2)) if s1 == s2 => {
                Self::get_contructor_info(expr)
            }

            _ => None,
        }
    }

    fn get_contructor_info(expr: &Expr<'a>) -> Option<ConstructorInfo<'a>> {
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

    fn is_recursive_expr(expr: &Expr<'a>, lambda_name: LambdaName<'_>) -> Option<Call<'a>> {
        if let Expr::Call(call) = expr {
            Self::is_recursive_call(call, lambda_name).then_some(call.clone())
        } else {
            None
        }
    }

    fn is_recursive_call(call: &Call<'a>, lambda_name: LambdaName<'_>) -> bool {
        match call.call_type {
            CallType::ByName { name, .. } => {
                // TODO are there other restrictions?
                name == lambda_name
            }
            CallType::Foreign { .. } | CallType::LowLevel { .. } | CallType::HigherOrder(_) => {
                false
            }
        }
    }

    fn ptr_write(
        env: &mut Env<'a, '_>,
        ptr: Symbol,
        value: Symbol,
        next: &'a Stmt<'a>,
    ) -> Stmt<'a> {
        let ptr_write = Call {
            call_type: crate::ir::CallType::LowLevel {
                op: LowLevel::PtrStore,
                // update_mode: env.next_update_mode_id(),
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: env.arena.alloc([ptr, value]),
        };

        Stmt::Let(
            env.named_unique_symbol("_ptr_write_unit"),
            Expr::Call(ptr_write),
            Layout::UNIT,
            next,
        )
    }

    pub fn init<'i>(
        env: &mut Env<'a, 'i>,
        proc: &Proc<'a>,
        trmc_calls: VecSet<Symbol>,
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
        let initial_ptr_symbol = env.named_unique_symbol("initial");
        jump_arguments.push(initial_ptr_symbol);
        jump_arguments.push(initial_ptr_symbol);

        let null_symbol = env.named_unique_symbol("null");
        let let_null = |next| Stmt::Let(null_symbol, Expr::NullPointer, return_layout, next);

        let ptr_return_layout = env
            .interner
            .insert_direct_no_semantic(LayoutRepr::Ptr(return_layout));

        let call = Call {
            call_type: CallType::LowLevel {
                op: LowLevel::PtrToStackValue,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: arena.alloc([null_symbol]),
        };

        let ptr_null = Expr::Call(call);
        let let_ptr = |next| Stmt::Let(initial_ptr_symbol, ptr_null, ptr_return_layout, next);

        let joinpoint_id = JoinPointId(env.named_unique_symbol("trmc"));
        let hole_symbol = env.named_unique_symbol("hole");
        let head_symbol = env.named_unique_symbol("head");

        let jump_stmt = Stmt::Jump(joinpoint_id, jump_arguments.into_bump_slice());

        let trmc_calls = trmc_calls.iter().map(|s| (*s, None)).collect();

        let mut this = Self {
            hole_symbol,
            head_symbol,
            joinpoint_id,
            return_layout,
            ptr_return_layout,
            trmc_calls,
        };

        let param = Param {
            symbol: hole_symbol,
            ownership: Ownership::Owned,
            layout: ptr_return_layout,
        };
        joinpoint_parameters.push(param);

        let param = Param {
            symbol: head_symbol,
            ownership: Ownership::Owned,
            layout: ptr_return_layout,
        };
        joinpoint_parameters.push(param);

        let joinpoint = Stmt::Join {
            id: joinpoint_id,
            parameters: joinpoint_parameters.into_bump_slice(),
            body: arena.alloc(this.walk_stmt(env, &proc.body)),
            remainder: arena.alloc(jump_stmt),
        };

        let body = let_null(arena.alloc(
            //
            let_ptr(arena.alloc(
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

    fn walk_stmt(&mut self, env: &mut Env<'a, '_>, stmt: &Stmt<'a>) -> Stmt<'a> {
        let arena = env.arena;

        match stmt {
            Stmt::Let(symbol, expr, layout, next) => {
                // if this is a TRMC call,
                if let Some(opt_call) = self.trmc_calls.get_mut(symbol) {
                    debug_assert!(opt_call.is_none());

                    let call = match expr {
                        Expr::Call(call) => call,
                        _ => unreachable!(),
                    };

                    *opt_call = Some(call.clone());

                    return self.walk_stmt(env, next);
                }

                if let Some(cons_info) = Self::is_terminal_constructor(stmt) {
                    // figure out which TRMC call to use here. We pick the first one that works
                    let opt_recursive_call = cons_info.arguments.iter().find_map(|arg| {
                        self.trmc_calls
                            .get(arg)
                            .and_then(|x| x.as_ref())
                            .map(|x| (arg, x))
                    });

                    match opt_recursive_call {
                        None => {
                            // this control flow path did not encounter a recursive call. Just
                            // write the end result into the hole and we're done.

                            let define_tag = |next| Stmt::Let(*symbol, expr.clone(), *layout, next);

                            let output = define_tag(arena.alloc(
                                //
                                self.non_trmc_return(env, *symbol),
                            ));

                            return output;
                        }
                        Some((call_symbol, call)) => {
                            // we did encounter a recursive call, and can perform TRMC in this
                            // branch.

                            let opt_recursive_field_index =
                                cons_info.arguments.iter().position(|s| *s == *call_symbol);

                            let recursive_field_index = match opt_recursive_field_index {
                                None => {
                                    let next = self.walk_stmt(env, next);
                                    return Stmt::Let(
                                        *symbol,
                                        expr.clone(),
                                        *layout,
                                        arena.alloc(next),
                                    );
                                }
                                Some(v) => v,
                            };

                            let tag_arg_null_symbol = env.named_unique_symbol("tag_arg_null");
                            let let_tag_arg_null = |next| {
                                Stmt::Let(
                                    tag_arg_null_symbol,
                                    Expr::NullPointer,
                                    self.return_layout,
                                    next,
                                )
                            };

                            let mut arguments =
                                Vec::from_iter_in(cons_info.arguments.iter().copied(), env.arena);
                            arguments[recursive_field_index] = tag_arg_null_symbol;

                            let tag_expr = Expr::Tag {
                                tag_layout: cons_info.tag_layout,
                                tag_id: cons_info.tag_id,
                                arguments: arguments.into_bump_slice(),
                            };

                            let let_tag = |next| Stmt::Let(*symbol, tag_expr, *layout, next);

                            let get_reference_expr = Expr::UnionFieldPtrAtIndex {
                                structure: *symbol,
                                tag_id: cons_info.tag_id,
                                union_layout: cons_info.tag_layout,
                                index: recursive_field_index as _,
                            };

                            let new_hole_symbol = env.named_unique_symbol("newHole");
                            let let_new_hole = |next| {
                                Stmt::Let(
                                    new_hole_symbol,
                                    get_reference_expr,
                                    self.ptr_return_layout,
                                    next,
                                )
                            };

                            let mut jump_arguments =
                                Vec::from_iter_in(call.arguments.iter().copied(), env.arena);
                            jump_arguments.push(new_hole_symbol);
                            jump_arguments.push(self.head_symbol);

                            let jump =
                                Stmt::Jump(self.joinpoint_id, jump_arguments.into_bump_slice());

                            let output = let_tag_arg_null(arena.alloc(
                                //
                                let_tag(arena.alloc(
                                    //
                                    let_new_hole(arena.alloc(
                                        //
                                        Self::ptr_write(
                                            env,
                                            self.hole_symbol,
                                            *symbol,
                                            arena.alloc(jump),
                                        ),
                                    )),
                                )),
                            ));

                            return output;
                        }
                    }
                }

                let next = self.walk_stmt(env, next);
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

                for (id, info, stmt) in branches.iter() {
                    let new_stmt = self.walk_stmt(env, stmt);

                    new_branches.push((*id, info.clone(), new_stmt));
                }

                let new_default_branch = &*arena.alloc(self.walk_stmt(env, default_branch.1));

                Stmt::Switch {
                    cond_symbol: *cond_symbol,
                    cond_layout: *cond_layout,
                    branches: arena.alloc(new_branches.into_bump_slice()),
                    default_branch: (default_branch.0.clone(), new_default_branch),
                    ret_layout: *ret_layout,
                }
            }
            Stmt::Ret(symbol) => {
                // write the symbol we're supposed to return into the hole
                // then read initial_symbol and return its contents
                self.non_trmc_return(env, *symbol)
            }
            Stmt::Refcounting(op, next) => {
                let new_next = self.walk_stmt(env, next);
                Stmt::Refcounting(*op, arena.alloc(new_next))
            }
            Stmt::Expect {
                condition,
                region,
                lookups,
                variables,
                remainder,
            } => Stmt::Expect {
                condition: *condition,
                region: *region,
                lookups,
                variables,
                remainder: arena.alloc(self.walk_stmt(env, remainder)),
            },
            Stmt::ExpectFx {
                condition,
                region,
                lookups,
                variables,
                remainder,
            } => Stmt::Expect {
                condition: *condition,
                region: *region,
                lookups,
                variables,
                remainder: arena.alloc(self.walk_stmt(env, remainder)),
            },
            Stmt::Dbg {
                symbol,
                variable,
                remainder,
            } => Stmt::Dbg {
                symbol: *symbol,
                variable: *variable,
                remainder: arena.alloc(self.walk_stmt(env, remainder)),
            },
            Stmt::Join {
                id,
                parameters,
                body,
                remainder,
            } => {
                let new_body = self.walk_stmt(env, body);
                let new_remainder = self.walk_stmt(env, remainder);

                Stmt::Join {
                    id: *id,
                    parameters,
                    body: arena.alloc(new_body),
                    remainder: arena.alloc(new_remainder),
                }
            }
            Stmt::Jump(id, arguments) => Stmt::Jump(*id, arguments),
            Stmt::Crash(symbol, crash_tag) => Stmt::Crash(*symbol, *crash_tag),
        }
    }

    fn non_trmc_return(&mut self, env: &mut Env<'a, '_>, value_symbol: Symbol) -> Stmt<'a> {
        let arena = env.arena;
        let layout = self.return_layout;

        let final_symbol = env.named_unique_symbol("final");

        let call = Call {
            call_type: CallType::LowLevel {
                op: LowLevel::PtrLoad,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: &*arena.alloc([self.head_symbol]),
        };

        let ptr_load = |next| Stmt::Let(final_symbol, Expr::Call(call), layout, next);

        Self::ptr_write(
            env,
            self.hole_symbol,
            value_symbol,
            arena.alloc(
                //
                ptr_load(arena.alloc(Stmt::Ret(final_symbol))),
            ),
        )
    }
}

fn expr_contains_symbol(expr: &Expr, needle: Symbol) -> bool {
    match expr {
        Expr::Literal(_) => false,
        Expr::Call(call) => call.arguments.contains(&needle),
        Expr::Tag { arguments, .. } => arguments.contains(&needle),
        Expr::Struct(fields) => fields.contains(&needle),
        Expr::NullPointer => false,
        Expr::StructAtIndex { structure, .. }
        | Expr::GetTagId { structure, .. }
        | Expr::UnionAtIndex { structure, .. }
        | Expr::UnionFieldPtrAtIndex { structure, .. } => needle == *structure,
        Expr::Array { elems, .. } => elems.iter().any(|element| match element {
            crate::ir::ListLiteralElement::Literal(_) => false,
            crate::ir::ListLiteralElement::Symbol(symbol) => needle == *symbol,
        }),
        Expr::EmptyArray => false,
        Expr::ExprBox { symbol } | Expr::ExprUnbox { symbol } => needle == *symbol,
        Expr::Reuse {
            symbol, arguments, ..
        } => needle == *symbol || arguments.contains(&needle),
        Expr::Reset { symbol, .. } | Expr::ResetRef { symbol, .. } => needle == *symbol,
        Expr::RuntimeErrorFunction(_) => false,
    }
}

fn stmt_contains_symbol_nonrec(stmt: &Stmt, needle: Symbol) -> bool {
    use crate::ir::ModifyRc::*;

    match stmt {
        Stmt::Let(_, expr, _, _) => expr_contains_symbol(expr, needle),
        Stmt::Switch { cond_symbol, .. } => needle == *cond_symbol,
        Stmt::Ret(symbol) => needle == *symbol,
        Stmt::Refcounting(modify, _) => {
            matches!( modify, Inc(symbol, _) | Dec(symbol) | DecRef(symbol)  if needle == *symbol  )
        }
        Stmt::Expect {
            condition, lookups, ..
        }
        | Stmt::ExpectFx {
            condition, lookups, ..
        } => needle == *condition || lookups.contains(&needle),
        Stmt::Dbg { symbol, .. } => needle == *symbol,
        Stmt::Join { .. } => false,
        Stmt::Jump(_, arguments) => arguments.contains(&needle),
        Stmt::Crash(symbol, _) => needle == *symbol,
    }
}
