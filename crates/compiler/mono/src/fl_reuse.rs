// Frame limited reuse
// Based on Reference Counting with Frame Limited Reuse

use crate::ir::{BranchInfo, Expr, ModifyRc, Proc, ProcLayout, Stmt, UpdateModeId, UpdateModeIds};
use crate::layout::{InLayout, Layout, LayoutInterner, STLayoutInterner, UnionLayout};

use bumpalo::Bump;

use bumpalo::collections::vec::Vec;
use roc_collections::MutMap;
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
                    .into_iter()
                    .map(|(label, info, branch, branch_env)| {
                        let unused_tokens=  branch_env.reuse_tokens.iter().flat_map(|(layout, reuse_tokens) |{
                            let min_reuse_tokens = layout_min_reuse_tokens.get(&layout).expect("All layouts in the environment should be in the layout_min_reuse_tokens map.");
                            let unused_tokens = &reuse_tokens[*min_reuse_tokens..];
                            unused_tokens
                        });

                        let newer_branch = drop_unused_reuse_tokens(arena, unused_tokens.copied(), branch);

                        (label, info, newer_branch.clone())
                    }),
                arena,
            )
            .into_bump_slice();

            let newer_default_branch = {
                let (info, branch, branch_env) = new_default_branch;
                let unused_tokens=  branch_env.reuse_tokens.iter().flat_map(|(layout, reuse_tokens) |{
                    let min_reuse_tokens = layout_min_reuse_tokens.get(&layout).expect("All layouts in the environment should be in the layout_min_reuse_tokens map.");
                    let unused_tokens = &reuse_tokens[*min_reuse_tokens..];
                    unused_tokens
                });

                let newer_branch = drop_unused_reuse_tokens(arena, unused_tokens.copied(), branch);

                (info, newer_branch)
            };

            // And finally we update the current environment to reflect the correct number of reuse tokens.
            for (layout, reuse_tokens) in environment.reuse_tokens.iter_mut() {
                let min_reuse_tokens = layout_min_reuse_tokens.get(&layout).expect(
                    "All layouts in the environment should be in the layout_min_reuse_tokens map.",
                );
                reuse_tokens.truncate(*min_reuse_tokens)
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
                            environment.push_reuse_token(&layout_clone, reuse_token);
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
            id,
            parameters,
            body,
            remainder,
        } => {
            // TODO how to deal with joinpoints?
            // We could like to reuse any memory before the jump, but we don't know wherefrom we will jump yet.
            // Perhaps evaluate the body first, see what layouts could be reused, and then at the jump actually use some of them. While giving that back to here so we can only insert the used ones.
            // Or evaluate the remainder first, see what is available at each jump to this join point. And then reuse those available from all jumps. (would require fixed point over the whole remainder...).
            // And we need to pass the parameter to the join point as well, so we can reuse it.
            todo!()
        }
        Stmt::Jump(_, _) => todo!(),
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
type ReuseTokens<'a> = MutMap<InLayout<'a>, std::vec::Vec<ReuseToken>>;

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

#[derive(Default, Clone)]
struct ReuseEnvironment<'a> {
    layout_tags: LayoutTags<'a>,
    reuse_tokens: ReuseTokens<'a>,
    symbol_layouts: SymbolLayout<'a>,
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
    fn push_reuse_token(&mut self, layout: &InLayout<'a>, token: ReuseToken) {
        match self.reuse_tokens.get_mut(layout) {
            Some(reuse_tokens) => {
                // If the layout is already in the map, push the token on the stack.
                reuse_tokens.push(token);
            }
            None => {
                // If the layout is not in the map, create a new stack with the token.
                self.reuse_tokens.insert(*layout, vec![token]);
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
            ModifyRc::Dec(reuse_token.symbol),
            continuation,
        ))
    })
}
