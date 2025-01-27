// This program was written by Jelle Teeuwissen within a final
// thesis project of the Computing Science master program at Utrecht
// University under supervision of Wouter Swierstra (w.s.swierstra@uu.nl).

// Implementation based of Reference Counting with Frame Limited Reuse
// https://www.microsoft.com/en-us/research/uploads/prod/2021/11/flreuse-tr.pdf

use std::hash::Hash;

use crate::ir::{
    BranchInfo, Expr, JoinPointId, ModifyRc, Param, Proc, ProcLayout, ReuseToken, Stmt,
    UpdateModeId, UpdateModeIds,
};
use crate::layout::{InLayout, LayoutInterner, LayoutRepr, STLayoutInterner, UnionLayout};

use bumpalo::Bump;

use bumpalo::collections::vec::Vec;
use bumpalo::collections::CollectIn;
use roc_collections::{MutMap, MutSet};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_target::Target;

/**
 Insert reset and reuse operations into the IR.
To allow for the reuse of memory allocation when said memory is no longer used.
 */
pub fn insert_reset_reuse_operations<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i STLayoutInterner<'a>,
    home: ModuleId,
    target: Target,
    ident_ids: &'i mut IdentIds,
    update_mode_ids: &'i mut UpdateModeIds,
    procs: &mut MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) {
    let mut global_layouts = SymbolLayout::default();
    for (symbol, _layout) in procs.keys() {
        global_layouts.insert(*symbol, LayoutOption::GloballyDefined);
    }

    for proc in procs.values_mut() {
        let new_proc = insert_reset_reuse_operations_proc(
            arena,
            layout_interner,
            target,
            home,
            ident_ids,
            update_mode_ids,
            global_layouts.clone(),
            proc.clone(),
        );
        *proc = new_proc;
    }
}

fn insert_reset_reuse_operations_proc<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i STLayoutInterner<'a>,
    target: Target,
    home: ModuleId,
    ident_ids: &'i mut IdentIds,
    update_mode_ids: &'i mut UpdateModeIds,
    mut symbol_layout: SymbolLayout<'a>,
    mut proc: Proc<'a>,
) -> Proc<'a> {
    for (layout, symbol) in proc.args {
        symbol_layout.insert(*symbol, LayoutOption::Layout(layout));
    }

    let mut env = ReuseEnvironment {
        target,
        symbol_tags: MutMap::default(),
        non_unique_symbols: MutSet::default(),
        reuse_tokens: MutMap::default(),
        symbol_layouts: symbol_layout,
        joinpoint_reuse_tokens: MutMap::default(),
        jump_reuse_tokens: MutMap::default(),
    };

    let new_body = insert_reset_reuse_operations_stmt(
        arena,
        layout_interner,
        home,
        ident_ids,
        update_mode_ids,
        &mut env,
        arena.alloc(proc.body),
    );

    // All reuse tokens either have to be used by reuse or not be inserted at all at the reset (and removed from the environment).
    debug_assert!(env.reuse_tokens.is_empty());

    proc.body = new_body.clone();
    proc
}

fn insert_reset_reuse_operations_stmt<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i STLayoutInterner<'a>,
    home: ModuleId,
    ident_ids: &'i mut IdentIds,
    update_mode_ids: &'i mut UpdateModeIds,
    environment: &mut ReuseEnvironment<'a>,
    stmt: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    match stmt {
        Stmt::Let(_, _, _, _) => {
            // Collect all the subsequent let bindings (including the current one).
            // To prevent the stack from overflowing when there are many let bindings.
            let mut triples = vec![];
            let mut current_stmt = stmt;
            while let Stmt::Let(binding, expr, layout, next_stmt) = current_stmt {
                triples.push((binding, expr, layout));
                current_stmt = next_stmt
            }

            debug_assert!(
                !triples.is_empty(),
                "Expected at least one let binding in the vector"
            );
            debug_assert!(
                !matches!(current_stmt, Stmt::Let(_, _, _, _)),
                "All let bindings should be in the vector"
            );

            // Update the triplets with reuse operations. Making sure to update the environment before the next let binding.
            let mut new_triplets = vec![];
            for (binding, expr, layout) in triples {
                let new_expr = match expr {
                    Expr::Tag {
                        tag_layout,
                        tag_id,
                        arguments,
                        reuse,
                    } => {
                        debug_assert!(reuse.is_none());

                        // The value of the tag is currently only used in the case of nullable recursive unions.
                        // But for completeness we add every kind of union to the layout_tags.
                        environment.add_symbol_tag(*binding, *tag_id);

                        // Check if the tag id for this layout can be reused at all.
                        match can_reuse_union_layout_tag(*tag_layout, Option::Some(*tag_id)) {
                            // The tag is reusable.
                            Reuse::Reusable(union_layout) => {
                                // See if we have a token.
                                match environment.pop_reuse_token(&get_reuse_layout_info(
                                    layout_interner,
                                    union_layout,
                                )) {
                                    // We have a reuse token for this layout, use it.
                                    Some(TokenWithInLayout {
                                        token: mut reuse_token,
                                        inlayout: layout_info,
                                    }) => {
                                        if layout_info == layout {
                                            // The reuse token layout is the same, we can use it without casting.
                                            (
                                                None,
                                                Expr::Tag {
                                                    tag_layout: *tag_layout,
                                                    tag_id: *tag_id,
                                                    arguments,
                                                    reuse: Some(reuse_token),
                                                },
                                            )
                                        } else {
                                            // The reuse token has a different layout from the tag, we need to pointercast it before.
                                            let new_symbol =
                                                Symbol::new(home, ident_ids.gen_unique());

                                            let ptr_cast = move |new_let| {
                                                arena.alloc(Stmt::Let(
                                                    new_symbol,
                                                    create_ptr_cast(arena, reuse_token.symbol),
                                                    *layout,
                                                    new_let,
                                                ))
                                            };

                                            // we now want to reuse the cast pointer
                                            reuse_token.symbol = new_symbol;

                                            (
                                                Some(ptr_cast),
                                                Expr::Tag {
                                                    tag_layout: *tag_layout,
                                                    tag_id: *tag_id,
                                                    arguments,
                                                    reuse: Some(reuse_token),
                                                },
                                            )
                                        }
                                    }

                                    // We have no reuse token available, keep the old expression with a fresh allocation.
                                    None => (None, expr.clone()),
                                }
                            }
                            // We cannot reuse this tag id because it's a null pointer.
                            Reuse::Nonreusable => (None, expr.clone()),
                        }
                    }
                    _ => (None, expr.clone()),
                };

                environment.add_symbol_layout(*binding, layout);
                new_triplets.push((binding, new_expr, layout))
            }

            let new_continuation = insert_reset_reuse_operations_stmt(
                arena,
                layout_interner,
                home,
                ident_ids,
                update_mode_ids,
                environment,
                current_stmt,
            );

            new_triplets.into_iter().rev().fold(
                new_continuation,
                |new_continuation, (binding, (opt_ptr_cast, new_expr), layout)| {
                    let new_let =
                        arena.alloc(Stmt::Let(*binding, new_expr, *layout, new_continuation));

                    // if the layout for the reuse does not match that of the reset, use PtrCast to convert the layout.
                    match opt_ptr_cast {
                        Some(ptr_cast) => ptr_cast(new_let),
                        None => new_let,
                    }
                },
            )
        }
        Stmt::Switch {
            cond_symbol,
            cond_layout,
            branches,
            default_branch,
            ret_layout,
        } => {
            macro_rules! update_env_with_constructor {
                ($branch_env:expr, $info:expr) => {{
                    match $info {
                        BranchInfo::Constructor {
                            scrutinee,
                            tag_id: tag,
                            ..
                        } => {
                            $branch_env.add_symbol_tag(*scrutinee, *tag);
                        }
                        BranchInfo::Unique {
                            scrutinee,
                            unique: false,
                        } => {
                            $branch_env.non_unique_symbols.insert(*scrutinee);
                        }
                        BranchInfo::None
                        | BranchInfo::List { .. }
                        | BranchInfo::Unique { unique: true, .. } => {}
                    }
                }};
            }

            let new_branches = branches
                .iter()
                .map(|(tag_id, info, branch)| {
                    let mut branch_env = environment.clone();

                    update_env_with_constructor!(branch_env, info);

                    let new_branch = insert_reset_reuse_operations_stmt(
                        arena,
                        layout_interner,
                        home,
                        ident_ids,
                        update_mode_ids,
                        &mut branch_env,
                        branch,
                    );

                    (*tag_id, info.clone(), new_branch, branch_env)
                })
                .collect_in::<Vec<_>>(arena);

            let new_default_branch = {
                let (info, branch) = default_branch;

                let mut branch_env = environment.clone();

                update_env_with_constructor!(branch_env, info);

                let new_branch = insert_reset_reuse_operations_stmt(
                    arena,
                    layout_interner,
                    home,
                    ident_ids,
                    update_mode_ids,
                    &mut branch_env,
                    branch,
                );

                (info.clone(), new_branch, branch_env)
            };

            // First we determine the minimum of reuse tokens available for each layout.
            let layout_min_reuse_tokens = {
                let branch_envs = {
                    let mut branch_environments =
                        Vec::with_capacity_in(new_branches.len() + 1, arena);

                    for (_, _, _, branch_env) in new_branches.iter() {
                        branch_environments.push(branch_env);
                    }

                    branch_environments.push(&new_default_branch.2);

                    branch_environments
                };

                let layout_min_reuse_tokens =
                    environment.reuse_tokens.keys().copied().map(|layout| {
                        let min_reuse_tokens = branch_envs
                            .iter()
                            .map(|branch_environment| {
                                branch_environment
                                    .reuse_tokens
                                    .get(&layout)
                                    .map_or(0, |tokens| tokens.len())
                            })
                            .min()
                            .expect("There should be at least one branch");
                        (layout, min_reuse_tokens)
                    });

                layout_min_reuse_tokens.collect::<MutMap<_, _>>()
            };

            // Then we drop any unused reuse tokens in branches where the minimum is not reached.
            let msg =
                "All layouts in the environment should be in the layout_min_reuse_tokens map.";
            let newer_branches = Vec::from_iter_in(
                new_branches
                    .iter()
                    .map(|(label, info, branch, branch_env)| {
                        let unused_tokens = branch_env
                            .reuse_tokens
                            .iter()
                            .flat_map(|(layout, reuse_tokens)| {
                                let min_reuse_tokens =
                                    layout_min_reuse_tokens.get(layout).expect(msg);
                                &reuse_tokens[*min_reuse_tokens..]
                            })
                            .map(|token_with_layout| token_with_layout.token);

                        let newer_branch = drop_unused_reuse_tokens(arena, unused_tokens, branch);

                        (*label, info.clone(), newer_branch.clone())
                    }),
                arena,
            )
            .into_bump_slice();

            let newer_default_branch = {
                // let (info, branch, branch_env) = new_default_branch;
                let unused_tokens=  new_default_branch.2.reuse_tokens.iter().flat_map(|(layout, reuse_tokens) |{
                    let min_reuse_tokens = layout_min_reuse_tokens.get(layout).expect("All layouts in the environment should be in the layout_min_reuse_tokens map.");
                    &reuse_tokens[*min_reuse_tokens..]
                }).map(|token_with_layout|{token_with_layout.token});

                let newer_branch =
                    drop_unused_reuse_tokens(arena, unused_tokens, new_default_branch.1);

                (new_default_branch.0, newer_branch)
            };

            // And finally we update the current environment to reflect the correct number of reuse tokens.
            for (layout, reuse_tokens) in environment.reuse_tokens.iter_mut() {
                let min_reuse_tokens = layout_min_reuse_tokens.get(layout).expect(
                    "All layouts in the environment should be in the layout_min_reuse_tokens map.",
                );
                reuse_tokens.truncate(*min_reuse_tokens)
            }

            // And remove any layouts that are no longer used.
            environment
                .reuse_tokens
                .retain(|_, reuse_tokens| !reuse_tokens.is_empty());

            // Propagate jump reuse tokens upwards.
            environment.propagate_jump_reuse_tokens(
                new_branches
                    .into_iter()
                    .map(|(_, _, _, branch_env)| branch_env)
                    .chain([new_default_branch.2]),
            );

            arena.alloc(Stmt::Switch {
                cond_symbol: *cond_symbol,
                cond_layout: *cond_layout,
                branches: newer_branches,
                default_branch: newer_default_branch,
                ret_layout: *ret_layout,
            })
        }
        Stmt::Refcounting(rc, continuation) => {
            enum SymbolIsUnique {
                Never,
                Always(Symbol),
                MustCheck(Symbol),
            }

            let can_reuse = match rc {
                ModifyRc::Dec(symbol) | ModifyRc::DecRef(symbol) => {
                    // can only reuse if the symbol is (potentially) unique
                    if environment.non_unique_symbols.contains(symbol) {
                        SymbolIsUnique::Never
                    } else {
                        SymbolIsUnique::MustCheck(*symbol)
                    }
                }
                ModifyRc::Free(symbol) => {
                    // a free'd symbol is guaranteed to be unique
                    SymbolIsUnique::Always(*symbol)
                }
                ModifyRc::Inc(_, _) => {
                    // an incremented symbol is never unique
                    SymbolIsUnique::Never
                }
            };

            enum ResetOperation {
                Reset,
                ResetRef,
                ClearTagId,
                Nothing,
            }

            let reuse_pair = match can_reuse {
                SymbolIsUnique::MustCheck(symbol) | SymbolIsUnique::Always(symbol) => {
                    // Get the layout of the symbol from where it is defined.
                    let layout_option = environment.get_symbol_layout(symbol);

                    // If the symbol is defined in the current proc, we can use the layout from the environment.
                    match layout_option {
                        LayoutOption::Layout(layout) => {
                            match symbol_layout_reusability(
                                layout_interner,
                                environment,
                                &symbol,
                                layout,
                            ) {
                                Reuse::Reusable(union_layout) => {
                                    let (reuse_symbol, reset_op) = match rc {
                                        ModifyRc::Dec(_) => (
                                            Symbol::new(home, ident_ids.gen_unique()),
                                            ResetOperation::Reset,
                                        ),
                                        ModifyRc::DecRef(_) => (
                                            Symbol::new(home, ident_ids.gen_unique()),
                                            ResetOperation::ResetRef,
                                        ),
                                        ModifyRc::Free(_) => {
                                            if union_layout
                                                .stores_tag_id_in_pointer(environment.target)
                                            {
                                                (
                                                    Symbol::new(home, ident_ids.gen_unique()),
                                                    ResetOperation::ClearTagId,
                                                )
                                            } else {
                                                (symbol, ResetOperation::Nothing)
                                            }
                                        }
                                        _ => unreachable!(),
                                    };

                                    let reuse_token = ReuseToken {
                                        symbol: reuse_symbol,
                                        update_mode: update_mode_ids.next_id(),
                                        // for now, always overwrite the tag ID just to be sure
                                        update_tag_id: true,
                                    };

                                    let owned_layout = **layout;

                                    environment.push_reuse_token(
                                        arena,
                                        get_reuse_layout_info(layout_interner, union_layout),
                                        reuse_token,
                                        layout,
                                    );

                                    Some((
                                        owned_layout,
                                        union_layout,
                                        symbol,
                                        reuse_token,
                                        reset_op,
                                    ))
                                }
                                Reuse::Nonreusable => None,
                            }
                        }
                        _ => None,
                    }
                }
                SymbolIsUnique::Never => {
                    // We don't need to do anything for an inc or symbols known to be non-unique.
                    None
                }
            };

            let new_continuation = insert_reset_reuse_operations_stmt(
                arena,
                layout_interner,
                home,
                ident_ids,
                update_mode_ids,
                environment,
                continuation,
            );

            // If we inserted a reuse token, we need to insert a reset reuse operation if the reuse token is consumed.
            if let Some((layout, union_layout, symbol, reuse_token, reset_op)) = reuse_pair {
                let stack_reuse_token = environment
                    .peek_reuse_token(&get_reuse_layout_info(layout_interner, union_layout));

                match stack_reuse_token {
                    Some(token_with_layout) if token_with_layout.token == reuse_token => {
                        // The token we inserted is still on the stack, so we don't need to insert a reset operation.
                        // We do need to remove the token from the environment. To prevent errors higher in the tree.
                        let _ = environment
                            .pop_reuse_token(&get_reuse_layout_info(layout_interner, union_layout));
                    }
                    _ => {
                        // The token we inserted is no longer on the stack, it must have been consumed.
                        // So we need to insert a reset operation.
                        match reset_op {
                            ResetOperation::Reset => {
                                // a dec will be replaced by a reset.
                                let reset_expr = Expr::Reset {
                                    symbol,
                                    update_mode: reuse_token.update_mode,
                                };

                                return arena.alloc(Stmt::Let(
                                    reuse_token.symbol,
                                    reset_expr,
                                    layout,
                                    new_continuation,
                                ));
                            }
                            ResetOperation::ResetRef => {
                                // a decref will be replaced by a resetref.
                                let reset_expr = Expr::ResetRef {
                                    symbol,
                                    update_mode: reuse_token.update_mode,
                                };

                                return arena.alloc(Stmt::Let(
                                    reuse_token.symbol,
                                    reset_expr,
                                    layout,
                                    new_continuation,
                                ));
                            }
                            ResetOperation::ClearTagId => {
                                let reset_expr = Expr::Call(crate::ir::Call {
                                    call_type: crate::ir::CallType::LowLevel {
                                        op: LowLevel::PtrClearTagId,
                                        update_mode: update_mode_ids.next_id(),
                                    },
                                    arguments: arena.alloc([symbol]),
                                });

                                return arena.alloc(Stmt::Let(
                                    reuse_token.symbol,
                                    reset_expr,
                                    layout,
                                    new_continuation,
                                ));
                            }
                            ResetOperation::Nothing => {
                                // the reuse token is already in a valid state
                                return new_continuation;
                            }
                        }
                    }
                }
            }

            // TODO update jump join points for the returned environment.

            arena.alloc(Stmt::Refcounting(*rc, new_continuation))
        }
        Stmt::Ret(_) => {
            // The return statement just doesn't consume any tokens. Dropping these tokens will be handled before.
            stmt
        }
        Stmt::Expect {
            condition,
            region,
            lookups,
            variables,
            remainder,
        } => {
            let new_remainder = insert_reset_reuse_operations_stmt(
                arena,
                layout_interner,
                home,
                ident_ids,
                update_mode_ids,
                environment,
                remainder,
            );

            arena.alloc(Stmt::Expect {
                condition: *condition,
                region: *region,
                lookups,
                variables,
                remainder: new_remainder,
            })
        }
        Stmt::Dbg {
            source_location,
            source,
            symbol,
            variable,
            remainder,
        } => {
            let new_remainder = insert_reset_reuse_operations_stmt(
                arena,
                layout_interner,
                home,
                ident_ids,
                update_mode_ids,
                environment,
                remainder,
            );

            arena.alloc(Stmt::Dbg {
                source_location,
                source,
                symbol: *symbol,
                variable: *variable,
                remainder: new_remainder,
            })
        }
        Stmt::Join {
            id: joinpoint_id,
            parameters,
            body,
            remainder,
        } => {
            // First we evaluate the remainder, to see what reuse tokens are available at each jump. We generate code as if no reuse tokens are used.
            // Then we evaluate the body, to see what reuse tokens are consumed by the body.
            // - If no reuse tokens are consumed (or when there were no available in the previous step), we stop here and return the first pass symbols.
            // Then we evaluate the body and remainder again, given the consumed reuse tokens. And we update the joinpoint parameters.

            let (first_pass_remainder_environment, first_pass_remainder) = {
                let mut first_pass_environment = environment.clone();

                first_pass_environment.add_joinpoint_reuse_tokens(
                    *joinpoint_id,
                    JoinPointReuseTokens::RemainderFirst,
                );

                let first_pass_remainder = insert_reset_reuse_operations_stmt(
                    arena,
                    layout_interner,
                    home,
                    ident_ids,
                    update_mode_ids,
                    &mut first_pass_environment,
                    remainder,
                );

                first_pass_environment.remove_joinpoint_reuse_tokens(*joinpoint_id);

                (first_pass_environment, first_pass_remainder)
            };

            let max_reuse_tokens =
                match first_pass_remainder_environment.get_jump_reuse_tokens(*joinpoint_id) {
                    Some(all_reuse_maps) => {
                        let all_token_layouts = all_reuse_maps
                            .iter()
                            .flat_map(|reuse_map| reuse_map.keys())
                            // PERF: replace this collect with an unique iterator. To make sure every layout is only used once.
                            .collect::<MutSet<_>>()
                            .into_iter();
                        let reuse_layouts_max_tokens = all_token_layouts.map(|token_layout| {
                            // We get the tokens from the jump with the most tokens for this token layout.
                            // So we have an inlayout for each token. And can cast when needed.
                            let max_token_inlayouts = all_reuse_maps
                                .iter()
                                .filter_map(|reuse_map| reuse_map.get(token_layout))
                                .max_by_key(|tokens| tokens.len())
                                .expect("all layouts should be in at least one of the reuse maps");
                            (token_layout, max_token_inlayouts)
                        });
                        Vec::from_iter_in(reuse_layouts_max_tokens, arena)
                    }
                    // Normally the remainder should always have jumps and this would not be None,
                    // But for testing this might not be the case, so default to no available reuse tokens.
                    None => Vec::new_in(arena),
                };

            let (first_pass_body_environment, first_pass_body, used_reuse_tokens) = {
                // For each possibly available reuse token, create a reuse token to add to the join point environment.
                let max_reuse_token_symbols = max_reuse_tokens
                    .iter()
                    .copied()
                    .map(|(token_layout, tokens)| {
                        (
                            *token_layout,
                            Vec::from_iter_in(
                                tokens.iter().map(|token| TokenWithInLayout {
                                    token: ReuseToken {
                                        symbol: Symbol::new(home, ident_ids.gen_unique()),
                                        update_mode: update_mode_ids.next_id(),
                                        // for now, always overwrite the tag ID just to be sure
                                        update_tag_id: true,
                                    },
                                    inlayout: token.inlayout,
                                }),
                                arena,
                            ),
                        )
                    })
                    .collect::<ReuseTokens>();

                // Create a new environment for the body. With everything but the jump reuse tokens. As those should be given by the jump.
                let mut first_pass_body_environment = ReuseEnvironment {
                    target: environment.target,
                    symbol_tags: environment.symbol_tags.clone(),
                    non_unique_symbols: environment.non_unique_symbols.clone(),
                    reuse_tokens: max_reuse_token_symbols.clone(),
                    symbol_layouts: environment.symbol_layouts.clone(),
                    joinpoint_reuse_tokens: environment.joinpoint_reuse_tokens.clone(),
                    jump_reuse_tokens: environment.jump_reuse_tokens.clone(),
                };

                // Add the parameters to the body environment as well.
                for param in parameters.iter() {
                    first_pass_body_environment.add_symbol_layout(param.symbol, &param.layout);
                }

                // Add a entry so that the body knows any jumps to this join point is recursive.
                first_pass_body_environment
                    .add_joinpoint_reuse_tokens(*joinpoint_id, JoinPointReuseTokens::BodyFirst);

                let first_pass_body = insert_reset_reuse_operations_stmt(
                    arena,
                    layout_interner,
                    home,
                    ident_ids,
                    update_mode_ids,
                    &mut first_pass_body_environment,
                    body,
                );

                first_pass_body_environment.remove_joinpoint_reuse_tokens(*joinpoint_id);

                let used_reuse_tokens = {
                    max_reuse_token_symbols
                        .iter()
                        .filter_map(|(layout, reuse_tokens)| {
                            match first_pass_body_environment.reuse_tokens.get(layout) {
                                Some(remaining_tokens) => {
                                    // There are tokens left, remove those from the bottom of the stack and return the consumed ones.
                                    let mut consumed_reuse_tokens = reuse_tokens
                                        .iter()
                                        .skip(remaining_tokens.len())
                                        .copied()
                                        .peekable();

                                    #[allow(clippy::manual_map)]
                                    match consumed_reuse_tokens.peek() {
                                        // If there are no consumed tokens, remove the layout from the map.
                                        None => None,
                                        // Otherwise return the layout and the consumed tokens.
                                        Some(_) => Some((
                                            *layout,
                                            Vec::from_iter_in(consumed_reuse_tokens, arena),
                                        )),
                                    }
                                }
                                None => {
                                    // All tokens were consumed. Meaning all of them should be passed from the jump. Keep tokens as is.
                                    Some((*layout, reuse_tokens.clone()))
                                }
                            }
                        })
                        .collect::<ReuseTokens>()
                };

                (
                    first_pass_body_environment,
                    first_pass_body,
                    used_reuse_tokens,
                )
            };

            // In the evaluation of the body and remainder we assumed no reuse tokens to be used.
            // So if there indeed are no reuse tokens used, we can just return the body and remainder as is.
            if used_reuse_tokens.is_empty() {
                // We evaluated the first pass using a cloned environment to be able to do a second pass with the same environment.
                // But if we don't need a second environment, we override the passed env with the first pass env.
                *environment = first_pass_remainder_environment.clone();

                // Propagate jump reuse tokens upwards.
                environment
                    .propagate_jump_reuse_tokens(std::iter::once(first_pass_body_environment));

                return arena.alloc(Stmt::Join {
                    id: *joinpoint_id,
                    parameters,
                    body: first_pass_body,
                    remainder: first_pass_remainder,
                });
            }

            let layouts_for_reuse = Vec::from_iter_in(
                used_reuse_tokens.iter().flat_map(|(layout, tokens)| {
                    tokens.iter().map(|token| (token.inlayout, *layout))
                }),
                arena,
            );

            let second_pass_remainder = {
                environment.add_joinpoint_reuse_tokens(
                    *joinpoint_id,
                    JoinPointReuseTokens::RemainderSecond(layouts_for_reuse.clone()),
                );

                let second_pass_remainder = insert_reset_reuse_operations_stmt(
                    arena,
                    layout_interner,
                    home,
                    ident_ids,
                    update_mode_ids,
                    environment,
                    remainder,
                );

                environment.remove_joinpoint_reuse_tokens(*joinpoint_id);

                second_pass_remainder
            };

            let extended_parameters = {
                let layouts_for_reuse_with_token = Vec::from_iter_in(
                    used_reuse_tokens
                        .iter()
                        .flat_map(|(layout, tokens)| tokens.iter().map(|token| (*layout, *token))),
                    arena,
                );

                let token_params =
                    layouts_for_reuse_with_token
                        .into_iter()
                        .map(|(_reuse_layout, token)| Param {
                            symbol: token.token.symbol,
                            layout: *token.inlayout,
                        });

                // Add the reuse tokens to the join arguments to match the expected arguments of the jump.
                let extended_parameters =
                    Vec::from_iter_in(parameters.iter().copied().chain(token_params), arena)
                        .into_bump_slice();

                extended_parameters
            };

            if first_pass_body_environment
                .get_jump_reuse_tokens(*joinpoint_id)
                .is_none()
            {
                // The body has no jumps to this join point. So we can just return the body and remainder as is.
                // As there are no jumps to update.

                // Propagate jump reuse tokens upwards.
                environment
                    .propagate_jump_reuse_tokens(std::iter::once(first_pass_body_environment));

                return arena.alloc(Stmt::Join {
                    id: *joinpoint_id,
                    parameters: extended_parameters,
                    body: first_pass_body,
                    remainder: second_pass_remainder,
                });
            }

            let (second_pass_body_environment, second_pass_body) = {
                // Create a new environment for the body. With everything but the jump reuse tokens. As those should be given by the jump.
                let mut body_environment = ReuseEnvironment {
                    target: environment.target,
                    symbol_tags: environment.symbol_tags.clone(),
                    non_unique_symbols: environment.non_unique_symbols.clone(),
                    reuse_tokens: used_reuse_tokens.clone(),
                    symbol_layouts: environment.symbol_layouts.clone(),
                    joinpoint_reuse_tokens: environment.joinpoint_reuse_tokens.clone(),
                    jump_reuse_tokens: environment.jump_reuse_tokens.clone(),
                };

                // Add the parameters to the body environment as well.
                for param in parameters.iter() {
                    body_environment.add_symbol_layout(param.symbol, &param.layout);
                }

                // Add a entry so that the body knows any jumps to this join point is recursive.
                body_environment.add_joinpoint_reuse_tokens(
                    *joinpoint_id,
                    JoinPointReuseTokens::BodySecond(layouts_for_reuse),
                );

                let second_pass_body = insert_reset_reuse_operations_stmt(
                    arena,
                    layout_interner,
                    home,
                    ident_ids,
                    update_mode_ids,
                    &mut body_environment,
                    body,
                );

                body_environment.remove_joinpoint_reuse_tokens(*joinpoint_id);

                (body_environment, second_pass_body)
            };

            environment.propagate_jump_reuse_tokens(std::iter::once(second_pass_body_environment));

            arena.alloc(Stmt::Join {
                id: *joinpoint_id,
                parameters: extended_parameters,
                body: second_pass_body,
                remainder: second_pass_remainder,
            })
        }
        Stmt::Jump(id, arguments) => {
            // TODO make sure that the reuse tokens that are provided by most jumps are the tokens that are used in most paths.
            let joinpoint_tokens = environment.get_joinpoint_reuse_tokens(*id);

            match joinpoint_tokens {
                JoinPointReuseTokens::RemainderFirst | JoinPointReuseTokens::BodyFirst => {
                    // For both the first pass of the continuation and the body, act as if there are no tokens to reuse.
                    environment.add_jump_reuse_tokens(*id, environment.reuse_tokens.clone());
                    arena.alloc(Stmt::Jump(*id, arguments))
                }
                JoinPointReuseTokens::RemainderSecond(token_layouts) => {
                    // If there are no tokens to reuse, we can just jump.
                    if token_layouts.is_empty() {
                        return arena.alloc(Stmt::Jump(*id, arguments));
                    }

                    let token_layouts_clone = token_layouts.clone();

                    let mut reuse_tokens_to_cast = Vec::new_in(arena);
                    let mut void_pointer_layout_symbols = Vec::new_in(arena);

                    // See what tokens we can get from the env, if none are available, use a void pointer.
                    // We process the tokens in reverse order, so that when we consume the tokens we last added,
                    // we consume the tokens that are most likely not to be null.
                    let tokens = token_layouts_clone
                        .iter()
                        .rev()
                        .map(|(param_layout, token_layout)| {
                            match environment.pop_reuse_token(token_layout) {
                                Some(reuse_token) => {
                                    if reuse_token.inlayout != *param_layout {
                                        let new_symbol = Symbol::new(home, ident_ids.gen_unique());
                                        reuse_tokens_to_cast.push((
                                            *param_layout,
                                            reuse_token.token.symbol,
                                            new_symbol,
                                        ));
                                        new_symbol
                                    } else {
                                        reuse_token.token.symbol
                                    }
                                }
                                None => match void_pointer_layout_symbols
                                    .iter()
                                    .find(|(layout, _)| layout == param_layout)
                                {
                                    Some(existing_symbol) => existing_symbol.1,
                                    None => {
                                        let new_symbol = Symbol::new(home, ident_ids.gen_unique());
                                        void_pointer_layout_symbols
                                            .push((*param_layout, new_symbol));
                                        new_symbol
                                    }
                                },
                            }
                        })
                        // Collect to prevent revs from cancelling out.
                        .collect_in::<Vec<_>>(arena);

                    // Add the void tokens to the jump arguments to match the expected arguments of the join point.
                    let extended_arguments = Vec::from_iter_in(
                        arguments
                            .iter()
                            .copied()
                            .chain(tokens.iter().copied().rev()),
                        arena,
                    )
                    .into_bump_slice();

                    let casted_tokens = reuse_tokens_to_cast.into_iter().fold(
                        arena.alloc(Stmt::Jump(*id, extended_arguments)),
                        |child, (layout, old_symbol, new_symbol)| {
                            arena.alloc(Stmt::Let(
                                new_symbol,
                                create_ptr_cast(arena, old_symbol),
                                *layout,
                                child,
                            ))
                        },
                    );

                    // Wrap the jump in a let statement for each void pointer token layout.
                    void_pointer_layout_symbols.into_iter().fold(
                        casted_tokens,
                        |child, (param_layout, symbol)| {
                            arena.alloc(Stmt::Let(symbol, Expr::NullPointer, *param_layout, child))
                        },
                    )
                }
                JoinPointReuseTokens::BodySecond(token_layouts) => {
                    // If there are no tokens to reuse, we can just jump.
                    if token_layouts.is_empty() {
                        return arena.alloc(Stmt::Jump(*id, arguments));
                    }

                    // We currently don't pass any reuse tokens to recursive jumps.
                    // This is to avoid keeping reuse tokens alive for too long. But it could be changed.
                    let mut void_pointer_layout_symbols: std::vec::Vec<(&'a InLayout<'a>, Symbol)> =
                        vec![];

                    let void_tokens = token_layouts.iter().map(|(param_layout, _token_layout)| {
                        match void_pointer_layout_symbols
                            .iter()
                            .find(|(void_layout, _)| void_layout == param_layout)
                        {
                            Some(existing_symbol) => existing_symbol.1,
                            None => {
                                let new_symbol = Symbol::new(home, ident_ids.gen_unique());
                                void_pointer_layout_symbols.push((*param_layout, new_symbol));
                                new_symbol
                            }
                        }
                    });

                    // Add the void tokens to the jump arguments to match the expected arguments of the join point.
                    let extended_arguments =
                        Vec::from_iter_in(arguments.iter().copied().chain(void_tokens), arena)
                            .into_bump_slice();

                    // Wrap the jump in a let statement for each void pointer token layout.
                    void_pointer_layout_symbols.into_iter().fold(
                        arena.alloc(Stmt::Jump(*id, extended_arguments)),
                        |child, (layout, symbol)| {
                            arena.alloc(Stmt::Let(symbol, Expr::NullPointer, *layout, child))
                        },
                    )
                }
            }
        }
        Stmt::Crash(_, _) => stmt,
    }
}

fn create_ptr_cast(arena: &Bump, symbol: Symbol) -> Expr {
    Expr::Call(crate::ir::Call {
        call_type: crate::ir::CallType::LowLevel {
            op: LowLevel::PtrCast,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: Vec::from_iter_in([symbol], arena).into_bump_slice(),
    })
}

// TODO make sure all dup/drop operations are already inserted statically.
// (e.g. not as a side effect of another operation) To make sure we can actually reuse.

enum Reuse<'a> {
    // Reuseable but the pointer *might* be null, which will cause a fresh allocation.
    Reusable(UnionLayout<'a>),
    Nonreusable,
}

/**
Map containing the reuse tokens of a layout.
A vec is used as a stack as we want to use the latest reuse token available.
*/
type ReuseTokens<'a> = MutMap<TokenLayout, Vec<'a, TokenWithInLayout<'a>>>;

/**
Struct to to check whether two reuse layouts are interchangeable.
*/
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct TokenLayout {
    size: u32,
    alignment: u32,
}

/**
Combines a reuse token with it's possible layout info.
*/
#[derive(Clone, Copy, PartialEq, Eq)]
struct TokenWithInLayout<'a> {
    token: ReuseToken,
    inlayout: &'a InLayout<'a>,
}

type Tag = u16;

/**
Map containing the layout of a symbol.
Used to determine whether the pointer of a symbol can be reused, if it is reference counted (heap allocated).
*/
type SymbolLayout<'a> = MutMap<Symbol, LayoutOption<'a>>;

#[derive(Clone)]
enum LayoutOption<'a> {
    // A normal layout defined in the current function.
    Layout(&'a InLayout<'a>),

    // No layout as this symbol is defined in a global scope and should not be reused.
    GloballyDefined,
}

#[derive(Clone)]
enum JoinPointReuseTokens<'a> {
    // The body indicates that we currently in the body of the join point.
    // This means that (for now) don't pass any reuse tokens from the jump.
    // As we only use reuse tokens from jumps outside the join point.
    BodyFirst,

    // Second body pass, to update any jump calls to pass void pointer parameters instead of no parameters.
    BodySecond(Vec<'a, (&'a InLayout<'a>, TokenLayout)>),

    // The first pass is used to determine the amount of reuse tokens a join point can expect.
    // Therefore, we don't know the amount of reuse tokens yet.
    RemainderFirst,

    // In the second pass, we determined the amount of reuse tokens a join point can expect.
    // Therefore, we know the amount of reuse tokens and can use.
    RemainderSecond(Vec<'a, (&'a InLayout<'a>, TokenLayout)>),
}

#[derive(Clone)]
struct ReuseEnvironment<'a> {
    target: Target,
    symbol_tags: MutMap<Symbol, Tag>,
    non_unique_symbols: MutSet<Symbol>,
    reuse_tokens: ReuseTokens<'a>,
    symbol_layouts: SymbolLayout<'a>,
    // A map containing the amount of reuse tokens a join point expects for each layout.
    joinpoint_reuse_tokens: MutMap<JoinPointId, JoinPointReuseTokens<'a>>,
    // A map containing the reuse tokens for each jump.
    jump_reuse_tokens: MutMap<JoinPointId, std::vec::Vec<ReuseTokens<'a>>>,
}

impl<'a> ReuseEnvironment<'a> {
    /**
     Add the known tag for a layout.
     Used to optimize reuse of unions that are know to have a null pointer.
    */
    fn add_symbol_tag(&mut self, symbol: Symbol, tag: Tag) {
        self.symbol_tags.insert(symbol, tag);
    }

    /**
    Retrieve the known tag for a layout.
     */
    fn get_symbol_tag(&self, symbol: &Symbol) -> Option<Tag> {
        self.symbol_tags.get(symbol).copied()
    }

    /**
    Retrieve a reuse token for a layout from the stack for said layout.
    */
    fn pop_reuse_token(&mut self, token_layout: &TokenLayout) -> Option<TokenWithInLayout> {
        let reuse_tokens = self.reuse_tokens.get_mut(token_layout)?;
        // If the layout is in the map, pop the token from the stack.
        let reuse_token = reuse_tokens.pop();
        // If the stack is empty, remove the layout from the map.
        if reuse_tokens.is_empty() {
            self.reuse_tokens.remove(token_layout);
        }
        reuse_token
    }

    /**
    Retrieve a reuse token for a layout from the stack for said layout.
    Without consuming the token.
    */
    fn peek_reuse_token(&mut self, token_layout: &TokenLayout) -> Option<TokenWithInLayout> {
        let reuse_tokens = self.reuse_tokens.get(token_layout)?;
        // If the layout is in the map, peek at the last element.
        let reuse_token = reuse_tokens.last();
        reuse_token.copied()
    }

    /**
    Push a reuse token for a layout on the stack for said layout.
    */
    fn push_reuse_token(
        &mut self,
        arena: &'a Bump,
        token_layout: TokenLayout,
        reuse_token: ReuseToken,
        layout: &'a InLayout<'a>,
    ) {
        let with_info = TokenWithInLayout {
            token: reuse_token,
            inlayout: layout,
        };
        self.reuse_tokens
            .entry(token_layout)
            .and_modify(|reuse_tokens| reuse_tokens.push(with_info))
            .or_insert_with(|| Vec::from_iter_in([with_info], arena));
    }

    /**
     Add the layout of a symbol.
    */
    fn add_symbol_layout(&mut self, symbol: Symbol, layout: &'a InLayout<'a>) {
        self.symbol_layouts
            .insert(symbol, LayoutOption::Layout(layout));
    }

    /**
    Retrieve the layout of a symbol.
     */
    fn get_symbol_layout(&self, symbol: Symbol) -> &LayoutOption<'a> {
        self.symbol_layouts
            .get(&symbol)
            .expect(
            "Expected symbol to have a layout. \
            It should have been inserted in the environment already. \
            We are investigating this issue, follow github.com/roc-lang/roc/issues/7461 for updates.",
        )
    }

    /**
     Add the reuse tokens of a jump to be used by a join point.
    */
    fn add_jump_reuse_tokens(&mut self, joinpoint_id: JoinPointId, reuse_tokens: ReuseTokens<'a>) {
        match self.jump_reuse_tokens.get_mut(&joinpoint_id) {
            Some(jump_reuse_tokens) => {
                jump_reuse_tokens.push(reuse_tokens);
            }
            None => {
                self.jump_reuse_tokens
                    .insert(joinpoint_id, vec![reuse_tokens]);
            }
        };
    }

    /**
    Propagate the reuse tokens of jumps from multiple environments to the current environment.
    */
    fn propagate_jump_reuse_tokens(&mut self, envs: impl Iterator<Item = ReuseEnvironment<'a>>) {
        for (joinpoint_id, layout_reuse_tokens) in
            envs.flat_map(|env| env.jump_reuse_tokens.into_iter())
        {
            for layout_reuse_token in layout_reuse_tokens.iter() {
                self.add_jump_reuse_tokens(joinpoint_id, layout_reuse_token.clone());
            }
        }
    }

    /**
    Get the all available reuse tokens from all jumps to a join point.
    */
    fn get_jump_reuse_tokens(
        &self,
        joinpoint_id: JoinPointId,
    ) -> Option<&std::vec::Vec<ReuseTokens<'a>>> {
        self.jump_reuse_tokens.get(&joinpoint_id)
    }

    /**
    Insert join_point_reuse_tokens for a join point.
    */
    fn add_joinpoint_reuse_tokens(
        &mut self,
        joinpoint_id: JoinPointId,
        join_point_reuse_tokens: JoinPointReuseTokens<'a>,
    ) {
        self.joinpoint_reuse_tokens
            .insert(joinpoint_id, join_point_reuse_tokens);
    }

    /**
    Retrieve the reuse tokens amount of a join point.
    */
    fn get_joinpoint_reuse_tokens(&self, joinpoint_id: JoinPointId) -> &JoinPointReuseTokens<'a> {
        self.joinpoint_reuse_tokens
            .get(&joinpoint_id)
            .expect("Expected join point to have reuse tokens.")
    }

    /**
     Remove the reuse tokens of a joinpoint for cleanup
    */
    fn remove_joinpoint_reuse_tokens(&mut self, joinpoint_id: JoinPointId) {
        self.joinpoint_reuse_tokens.remove(&joinpoint_id);
    }
}

/**
Check if a layout can be reused. by verifying if the layout is a union and if the tag is not nullable.
*/
fn symbol_layout_reusability<'a>(
    layout_interner: &STLayoutInterner<'a>,
    environment: &ReuseEnvironment<'a>,
    symbol: &Symbol,
    layout: &InLayout<'a>,
) -> Reuse<'a> {
    match layout_interner.get_repr(*layout) {
        LayoutRepr::Union(union_layout) => {
            can_reuse_union_layout_tag(union_layout, environment.get_symbol_tag(symbol))
        }
        // Strings literals are constants.
        // Arrays are probably given to functions and reused there. Little use to reuse them here.
        _ => Reuse::Nonreusable,
    }
}

/**
   Check if a union layout can be reused. by verifying if the tag is not nullable.
*/
fn can_reuse_union_layout_tag(union_layout: UnionLayout, tag_id_option: Option<Tag>) -> Reuse {
    match union_layout {
        UnionLayout::NonRecursive(_) => Reuse::Nonreusable,
        // Non nullable union layouts
        UnionLayout::Recursive(_) | UnionLayout::NonNullableUnwrapped(_) => {
            // Non nullable union layouts can always be reused.
            Reuse::Reusable(union_layout)
        }
        // Nullable union layouts
        UnionLayout::NullableWrapped { .. } | UnionLayout::NullableUnwrapped { .. } => {
            match tag_id_option {
                Some(tag_id) => {
                    if union_layout.tag_is_null(tag_id) {
                        // Symbol of layout is always null, so it can't ever be reused.
                        Reuse::Nonreusable
                    } else {
                        // Symbol of layout is not null, so it can be reused.
                        Reuse::Reusable(union_layout)
                    }
                }
                None => {
                    // Symbol of layout might be null, so it might be reused.
                    // If null will cause null pointer and fresh allocation.
                    Reuse::Reusable(union_layout)
                }
            }
        }
    }
}

/**
Drop the reuse tokens that are not used anymore.
Useful when reuse tokens are used in a branch, and thus should be created.
But not in all branches, and thus should be dropped in those branches.
*/
fn drop_unused_reuse_tokens<'a>(
    arena: &'a Bump,
    unused_tokens: impl Iterator<Item = ReuseToken>,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    unused_tokens.fold(continuation, |continuation, reuse_token| {
        arena.alloc(Stmt::Refcounting(
            ModifyRc::DecRef(reuse_token.symbol),
            continuation,
        ))
    })
}

fn get_reuse_layout_info<'a>(
    layout_interner: &STLayoutInterner<'a>,
    union_layout: UnionLayout<'a>,
) -> TokenLayout {
    let (size, alignment) = union_layout.data_size_and_alignment(layout_interner);

    TokenLayout { size, alignment }
}
