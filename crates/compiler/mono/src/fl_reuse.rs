// Frame limited reuse
// Based on Reference Counting with Frame Limited Reuse

use crate::borrow::Ownership;
use crate::ir::{
    BranchInfo, Expr, JoinPointId, ModifyRc, Param, Proc, ProcLayout, Stmt, UpdateModeId,
    UpdateModeIds,
};
use crate::layout::{InLayout, Layout, LayoutInterner, STLayoutInterner, UnionLayout};

use bumpalo::Bump;

use bumpalo::collections::vec::Vec;
use roc_collections::{MutMap, MutSet};
use roc_module::symbol::{IdentIds, ModuleId, Symbol};

/**
 Insert reset and reuse operations into the IR.
To allow for the reuse of memory allocation when said memory is no longer used.
 */
pub fn insert_reset_reuse_operations<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i mut STLayoutInterner<'a>,
    home: ModuleId,
    ident_ids: &'i mut IdentIds,
    update_mode_ids: &'i mut UpdateModeIds,
    procs: &mut MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) {
    let mut global_layouts = SymbolLayout::default();
    for (symbol, layout) in procs.keys() {
        global_layouts.insert(*symbol, LayoutOption::GloballyDefined);
    }

    for proc in procs.values_mut() {
        let new_proc = insert_reset_reuse_operations_proc(
            arena,
            layout_interner,
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
    layout_interner: &'i mut STLayoutInterner<'a>,
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
        layout_tags: MutMap::default(),
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
    layout_interner: &'i mut STLayoutInterner<'a>,
    home: ModuleId,
    ident_ids: &'i mut IdentIds,
    update_mode_ids: &'i mut UpdateModeIds,
    environment: &mut ReuseEnvironment<'a>,
    stmt: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    match stmt {
        Stmt::Let(binding, expr, layout, continuation) => {
            let new_expr = match expr {
                Expr::Tag {
                    tag_layout,
                    tag_id,
                    arguments,
                } => {
                    // The value of the tag is currently only used in the case of nullable recursive unions.
                    // But for completeness we add every kind of union to the layout_tags.
                    environment.add_layout_tag(layout, *tag_id);

                    // See if we have a reuse token
                    match environment.pop_reuse_token(layout) {
                        // We have a reuse token for this layout, use it.
                        Some(reuse_token) => {
                            Expr::Reuse {
                                symbol: reuse_token.symbol,
                                update_mode: reuse_token.update_mode_id,
                                // for now, always overwrite the tag ID just to be sure
                                update_tag_id: true,
                                tag_layout: *tag_layout,
                                tag_id: *tag_id,
                                arguments,
                            }
                        }

                        // We have no reuse token available, keep the old expression with a fresh allocation.
                        None => expr.clone(),
                    }
                }
                _ => expr.clone(),
            };

            environment.add_symbol_layout(*binding, layout);

            let new_continuation = insert_reset_reuse_operations_stmt(
                arena,
                layout_interner,
                home,
                ident_ids,
                update_mode_ids,
                environment,
                continuation,
            );

            //  // for now, always overwrite the tag ID just to be sure
            //  let update_tag_id = true;

            arena.alloc(Stmt::Let(*binding, new_expr, *layout, new_continuation))
        }
        Stmt::Switch {
            cond_symbol,
            cond_layout,
            branches,
            default_branch,
            ret_layout,
        } => {
            let new_branches = branches
                .iter()
                .map(|(tag_id, info, branch)| {
                    let mut branch_env = environment.clone();
                    if let BranchInfo::Constructor {
                        tag_id: tag,
                        layout: branch_layout,
                        ..
                    } = info
                    {
                        branch_env.add_layout_tag(branch_layout, *tag);
                    }

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
                .collect::<std::vec::Vec<_>>();

            let new_default_branch = {
                let (info, branch) = default_branch;

                let mut branch_env = environment.clone();
                if let BranchInfo::Constructor {
                    tag_id: tag,
                    layout: branch_layout,
                    ..
                } = info
                {
                    branch_env.add_layout_tag(branch_layout, *tag);
                }

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
            let newer_branches = Vec::from_iter_in(
                new_branches
                    .iter()
                    .map(|(label, info, branch, branch_env)| {
                        let unused_tokens=  branch_env.reuse_tokens.iter().flat_map(|(layout, reuse_tokens) |{
                            let min_reuse_tokens = layout_min_reuse_tokens.get(&layout).expect("All layouts in the environment should be in the layout_min_reuse_tokens map.");
                            let unused_tokens = &reuse_tokens[*min_reuse_tokens..];
                            unused_tokens
                        });

                        let newer_branch = drop_unused_reuse_tokens(arena, unused_tokens.copied(), branch);

                        (*label, info.clone(), newer_branch.clone())
                    }),
                arena,
            )
            .into_bump_slice();

            let newer_default_branch = {
                // let (info, branch, branch_env) = new_default_branch;
                let unused_tokens=  new_default_branch.2.reuse_tokens.iter().flat_map(|(layout, reuse_tokens) |{
                    let min_reuse_tokens = layout_min_reuse_tokens.get(&layout).expect("All layouts in the environment should be in the layout_min_reuse_tokens map.");
                    let unused_tokens = &reuse_tokens[*min_reuse_tokens..];
                    unused_tokens
                });

                let newer_branch =
                    drop_unused_reuse_tokens(arena, unused_tokens.copied(), new_default_branch.1);

                (new_default_branch.0, newer_branch)
            };

            // And finally we update the current environment to reflect the correct number of reuse tokens.
            for (layout, reuse_tokens) in environment.reuse_tokens.iter_mut() {
                let min_reuse_tokens = layout_min_reuse_tokens.get(&layout).expect(
                    "All layouts in the environment should be in the layout_min_reuse_tokens map.",
                );
                reuse_tokens.truncate(*min_reuse_tokens)
            }

            // And remove any layouts that are no longer used.
            environment
                .reuse_tokens
                .retain(|_, reuse_tokens| !reuse_tokens.is_empty());

            // Propagate jump reuse tokens upwards.
            for (joinpoint_id, layout_reuse_tokens) in new_branches
                .iter()
                .map(|(_, _, _, branch_env)| branch_env)
                .chain(std::iter::once(&new_default_branch.2))
                .flat_map(|branch_env| branch_env.jump_reuse_tokens.iter())
            {
                for layout_reuse_token in layout_reuse_tokens.iter() {
                    environment.add_jump_reuse_tokens(*joinpoint_id, layout_reuse_token.clone());
                }
            }

            arena.alloc(Stmt::Switch {
                cond_symbol: *cond_symbol,
                cond_layout: *cond_layout,
                branches: newer_branches,
                default_branch: newer_default_branch,
                ret_layout: *ret_layout,
            })
        }
        Stmt::Refcounting(rc, continuation) => {
            let reuse_pair = match rc {
                ModifyRc::Inc(_, _) => {
                    // We don't need to do anything for an inc.
                    None
                }
                ModifyRc::Dec(symbol) | ModifyRc::DecRef(symbol) => {
                    // Get the layout of the symbol from where it is defined.
                    let layout_option = environment.get_symbol_layout(*symbol);

                    // If the symbol is defined in the current proc, we can use the layout from the environment.
                    match layout_option {
                        LayoutOption::Layout(layout)
                            if matches!(
                                can_reuse_layout_tag(layout_interner, &environment, &layout),
                                Reuse::Reusable
                            ) =>
                        {
                            let layout_clone = layout.clone();
                            let reuse_token = ReuseToken {
                                symbol: Symbol::new(home, ident_ids.gen_unique()),
                                update_mode_id: update_mode_ids.next_id(),
                            };
                            environment.push_reuse_token(arena, &layout_clone, reuse_token);
                            Some((layout_clone, *symbol, reuse_token))
                        }
                        _ => None,
                    }
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
            if let Some((layout, symbol, reuse_token)) = reuse_pair {
                let stack_reuse_token = environment.peek_reuse_token(&layout);

                match stack_reuse_token {
                    Some(reuse_symbol) if reuse_symbol == reuse_token => {
                        // The token we inserted is still on the stack, so we don't need to insert a reset operation.
                        // We do need to remove the token from the environment. To prevent errors higher in the tree.
                        let _ = environment.pop_reuse_token(&layout);
                    }
                    _ => {
                        // The token we inserted is no longer on the stack, it must have been consumed.
                        // So we need to insert a reset operation.
                        let reset_expr = Expr::Reset {
                            symbol,
                            update_mode: reuse_token.update_mode_id,
                        };

                        // If we generate a reuse token, we no longer want to use the drop statement anymore. So we just return the reset expression.
                        // TODO verify if this works for both dec and decref.
                        // TODO reset probably decrements it's children. So we probably need to create a resetref that only does the token.
                        return arena.alloc(Stmt::Let(
                            reuse_token.symbol,
                            reset_expr,
                            // TODO not sure what the layout should be for a reset token. Currently it is the layout of the symbol.
                            *layout,
                            new_continuation,
                        ));
                    }
                }
            }

            // TODO update jump join points for the returned environment.

            arena.alloc(Stmt::Refcounting(rc.clone(), new_continuation))
        }
        Stmt::Ret(symbol) => {
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
                lookups: lookups.clone(),
                variables: variables.clone(),
                remainder: new_remainder,
            })
        }
        Stmt::ExpectFx {
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

            arena.alloc(Stmt::ExpectFx {
                condition: *condition,
                region: *region,
                lookups: lookups.clone(),
                variables: variables.clone(),
                remainder: new_remainder,
            })
        }
        Stmt::Dbg {
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
            // - If no reuse tokens are consumed (or when there were no available in the previous step), we stop here and return the first pass variables.
            // Then we evaluate the body and remainder again, given the consumed reuse tokens. And we update the joinpoint parameters.

            let (first_pass_remainder_environment, first_pass_remainder) = {
                let mut first_pass_environment = environment.clone();

                first_pass_environment.add_joinpoint_reuse_tokens(
                    *joinpoint_id,
                    JoinPointReuseTokens::RemainderFirstPass,
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

            let max_reuse_tokens = {
                let all_reuse_maps = first_pass_remainder_environment
                    .get_jump_reuse_tokens(*joinpoint_id)
                    .expect(
                        "Expected join point to be jumped to at least once from the remainder.",
                    );
                let all_reuse_layouts = all_reuse_maps
                    .iter()
                    .flat_map(|reuse_map| reuse_map.keys())
                    // PERF: replace this collect with an unique iterator. To make sure every layout is only used once.
                    .collect::<MutSet<_>>()
                    .into_iter();
                let reuse_layouts_max_tokens = all_reuse_layouts.map(|reuse_layout| {
                    let max_token = all_reuse_maps
                        .iter()
                        .map(|reuse_map| {
                            reuse_map
                                .get(reuse_layout)
                                .map(|tokens| tokens.len())
                                .unwrap_or(0)
                        })
                        .max()
                        .expect("all layouts should be in at least one of the reuse maps");
                    (reuse_layout, max_token)
                });
                Vec::from_iter_in(reuse_layouts_max_tokens, arena)
            };

            let (first_pass_body_environment, first_pass_body, used_reuse_tokens) = {
                // For each possibly available reuse token, create a reuse token to add to the join point environment.
                let max_reuse_token_symbols = max_reuse_tokens
                    .iter()
                    .map(|(layout, size)| {
                        (
                            **layout,
                            Vec::from_iter_in(
                                (0..*size).map(|_| ReuseToken {
                                    symbol: Symbol::new(home, ident_ids.gen_unique()),
                                    update_mode_id: update_mode_ids.next_id(),
                                }),
                                arena,
                            ),
                        )
                    })
                    .collect::<ReuseTokens>();

                // Create a new environment for the body. With everything but the jump reuse tokens. As those should be given by the jump.
                let mut first_pass_body_environment = ReuseEnvironment {
                    layout_tags: environment.layout_tags.clone(),
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
                    .add_joinpoint_reuse_tokens(*joinpoint_id, JoinPointReuseTokens::BodyFirstPass);

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
                                    // There are tokens left, remove those from the bottom of the stack and retun the consumed ones.
                                    let mut consumed_reuse_tokens = reuse_tokens
                                        .iter()
                                        .skip(remaining_tokens.len())
                                        .copied()
                                        .peekable();
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
                // TODO verify if this works as intended.
                *environment = first_pass_remainder_environment.clone();

                return arena.alloc(Stmt::Join {
                    id: *joinpoint_id,
                    parameters: parameters.clone(),
                    body: first_pass_body,
                    remainder: first_pass_remainder,
                });
            }

            let layouts_for_reuse = {
                let mut layouts = Vec::from_iter_in(
                    used_reuse_tokens
                        .iter()
                        .flat_map(|(layout, tokens)| tokens.iter().map(|_| *layout)),
                    arena,
                );
                // Make sure the layouts are sorted, so that we can provide them from the jump.
                // In the same order as we consume them from the join point.
                layouts.sort();
                layouts
            };

            let second_pass_remainder = {
                environment.add_joinpoint_reuse_tokens(
                    *joinpoint_id,
                    JoinPointReuseTokens::RemainderSecondPass(layouts_for_reuse.clone()),
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
                let layouts_for_reuse_with_token = {
                    let mut layout_with_tokens = Vec::from_iter_in(
                        used_reuse_tokens.iter().flat_map(|(layout, tokens)| {
                            tokens.iter().map(|token| (*layout, *token))
                        }),
                        arena,
                    );
                    // Make sure the layouts are sorted, so that we can provide them from the jump.
                    // In the same order as we consume them from the join point.
                    layout_with_tokens.sort_by_key(|(layout, _)| *layout);
                    layout_with_tokens
                };

                let token_params =
                    layouts_for_reuse_with_token
                        .into_iter()
                        .map(|(layout, token)| Param {
                            symbol: token.symbol,
                            ownership: Ownership::Owned,
                            layout,
                        });

                // Add the void tokens to the jump arguments to match the expected arguments of the join point.
                let extended_parameters =
                    Vec::from_iter_in(parameters.iter().copied().chain(token_params), arena)
                        .into_bump_slice();

                extended_parameters
            };

            if let None = first_pass_body_environment.get_jump_reuse_tokens(*joinpoint_id) {
                // The body has no jumps to this join point. So we can just return the body and remainder as is.
                // As there are no jumps to update.
                return arena.alloc(Stmt::Join {
                    id: *joinpoint_id,
                    parameters: extended_parameters,
                    body: first_pass_body,
                    remainder: second_pass_remainder,
                });
            }

            let second_pass_body = {
                // Create a new environment for the body. With everything but the jump reuse tokens. As those should be given by the jump.
                let mut body_environment = ReuseEnvironment {
                    layout_tags: environment.layout_tags.clone(),
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
                    JoinPointReuseTokens::BodySecondPass(layouts_for_reuse),
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

                second_pass_body
            };

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
                JoinPointReuseTokens::RemainderFirstPass | JoinPointReuseTokens::BodyFirstPass => {
                    // For both the first pass of the continuation and the body, act as if there are no tokens to reuse.
                    environment.add_jump_reuse_tokens(*id, environment.reuse_tokens.clone());
                    arena.alloc(Stmt::Jump(*id, arguments.clone()))
                }
                JoinPointReuseTokens::RemainderSecondPass(token_layouts) => {
                    // If there are no tokens to reuse, we can just jump.
                    if token_layouts.is_empty() {
                        return arena.alloc(Stmt::Jump(*id, arguments.clone()));
                    }

                    let token_layouts_clone = token_layouts.clone();

                    // See what tokens we can get from the env, if none are available, use a void pointer.
                    let tokens = token_layouts_clone.iter().map(|token_layout| {
                        environment
                            .pop_reuse_token(&token_layout)
                            .map_or_else(|| void_pointer_symbol(), |reuse_token| reuse_token.symbol)
                    });

                    // Add the void tokens to the jump arguments to match the expected arguments of the join point.
                    let extended_arguments =
                        Vec::from_iter_in(arguments.iter().copied().chain(tokens), arena)
                            .into_bump_slice();

                    arena.alloc(Stmt::Jump(*id, extended_arguments))
                }
                JoinPointReuseTokens::BodySecondPass(token_layouts) => {
                    // If there are no tokens to reuse, we can just jump.
                    if token_layouts.is_empty() {
                        return arena.alloc(Stmt::Jump(*id, arguments.clone()));
                    }

                    // We currently don't pass any reuse tokens to recursive jumps.
                    // This is to avoid keeping reuse tokens alive for too long. But it could be changed.
                    let void_tokens = token_layouts.iter().map(|_| void_pointer_symbol());

                    // Add the void tokens to the jump arguments to match the expected arguments of the join point.
                    let extended_arguments =
                        Vec::from_iter_in(arguments.iter().copied().chain(void_tokens), arena)
                            .into_bump_slice();

                    arena.alloc(Stmt::Jump(*id, extended_arguments))
                }
            }
        }
        Stmt::Crash(symbol, tag) => stmt,
    }
}

// TODO make sure all dup/drop operations are already inserted statically.
// (e.g. not as a side effect of another operation) To make sure we can actually reuse.

enum Reuse {
    // Reuseable but the pointer *might* be null, which will cause a fresh allocation.
    Reusable,
    Nonreusable,
}

/**
Map containing the curren't known tag of a layout.
A layout with a tag will be inserted e.g. after pattern matching. where the tag is known.
*/
type LayoutTags<'a> = MutMap<&'a InLayout<'a>, Tag>;

/**
Map containing the reuse tokens of a layout.
A vec is used as a stack as we want to use the latest reuse token available.
*/
type ReuseTokens<'a> = MutMap<InLayout<'a>, Vec<'a, ReuseToken>>;

/**
A reuse token is a symbol that is used to reset a layout.
Matches variables that are pointers.
*/
#[derive(Clone, Copy, PartialEq, Eq)]
struct ReuseToken {
    // The symbol of the reuse token.
    symbol: Symbol,

    // Index that can be used later to determine if in place mutation is possible.
    update_mode_id: UpdateModeId,
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
    BodyFirstPass,

    // Second body pass, to update any jump calls to pass void pointer parameters instead of no parameters.
    BodySecondPass(Vec<'a, InLayout<'a>>),

    // The first pass is used to determine the amount of reuse tokens a join point can expect.
    // Therefore, we don't know the amount of reuse tokens yet.
    RemainderFirstPass,

    // In the second pass, we determined the amount of reuse tokens a join point can expect.
    // Therefore, we know the amount of reuse tokens and can use.
    RemainderSecondPass(Vec<'a, InLayout<'a>>),
}

#[derive(Default, Clone)]
struct ReuseEnvironment<'a> {
    layout_tags: LayoutTags<'a>,
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
    fn add_layout_tag(&mut self, layout: &'a InLayout<'a>, tag: Tag) {
        self.layout_tags.insert(layout, tag);
    }

    /**
    Retrieve the known tag for a layout.
     */
    fn get_layout_tag(&self, layout: &InLayout<'a>) -> Option<Tag> {
        self.layout_tags.get(layout).copied()
    }

    /**
    Retrieve a reuse token for a layout from the stack for said layout.
    */
    fn pop_reuse_token(&mut self, layout: &InLayout<'a>) -> Option<ReuseToken> {
        let reuse_tokens = self.reuse_tokens.get_mut(layout)?;
        // If the layout is in the map, pop the token from the stack.
        let reuse_token = reuse_tokens.pop();
        // If the stack is empty, remove the layout from the map.
        if reuse_tokens.is_empty() {
            self.reuse_tokens.remove(layout);
        }
        reuse_token
    }

    /**
    Retrieve a reuse token for a layout from the stack for said layout.
    Without consuming the token.
    */
    fn peek_reuse_token(&mut self, layout: &InLayout<'a>) -> Option<ReuseToken> {
        let reuse_tokens = self.reuse_tokens.get(layout)?;
        // If the layout is in the map, pop the token from the stack.
        let reuse_token = reuse_tokens.last();
        reuse_token.copied()
    }

    /**
    Push a reuse token for a layout on the stack for said layout.
    */
    fn push_reuse_token(&mut self, arena: &'a Bump, layout: &InLayout<'a>, token: ReuseToken) {
        match self.reuse_tokens.get_mut(layout) {
            Some(reuse_tokens) => {
                // If the layout is already in the map, push the token on the stack.
                reuse_tokens.push(token);
            }
            None => {
                // If the layout is not in the map, create a new stack with the token.
                self.reuse_tokens
                    .insert(*layout, Vec::from_iter_in(std::iter::once(token), arena));
            }
        };
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
        self.symbol_layouts.get(&symbol).expect("Expected symbol to have a layout. It should have been inserted in the environment already.")
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

fn can_reuse_layout_tag<'a, 'i>(
    layout_interner: &'i mut STLayoutInterner<'a>,
    environment: &ReuseEnvironment<'a>,
    layout: &InLayout<'a>,
) -> Reuse {
    match layout_interner.get(*layout) {
        Layout::Union(union_layout) => match union_layout {
            UnionLayout::NonRecursive(_) => Reuse::Nonreusable,
            // Non nullable union layouts
            UnionLayout::Recursive(_) | UnionLayout::NonNullableUnwrapped(_) => {
                // Non nullable union layouts can always be reused.
                Reuse::Reusable
            }
            // Nullable union layouts
            UnionLayout::NullableWrapped { .. } | UnionLayout::NullableUnwrapped { .. } => {
                // Nullable union layouts can only be reused if the tag is not null.
                match environment.get_layout_tag(layout) {
                    Some(tag_id) => {
                        if union_layout.tag_is_null(tag_id) {
                            // Variable of layout is always null, so it can't ever be reused.
                            Reuse::Nonreusable
                        } else {
                            // Variable of layout is not null, so it can be reused.
                            Reuse::Reusable
                        }
                    }
                    None => {
                        // Variable of layout might be null, so it might be reused.
                        // If null will cause null pointer and fresh allocation.
                        Reuse::Reusable
                    }
                }
            }
        },
        // Strings literals are constants.
        // Arrays are probably given to functions and reused there. Little use to reuse them here.
        _ => Reuse::Nonreusable,
    }
}

/**
Drop the reuse tokens that are not used anymore.
Usefull when reuse tokens are used in a branch, and thus should be created.
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

fn void_pointer_symbol() -> Symbol {
    todo!()
}
