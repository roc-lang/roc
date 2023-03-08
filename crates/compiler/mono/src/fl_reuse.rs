// Frame limited reuse
// Based on Reference Counting with Frame Limited Reuse

use crate::ir::{BranchInfo, Expr, ModifyRc, Proc, ProcLayout, Stmt, UpdateModeIds};
use crate::layout::{InLayout, Layout, LayoutInterner, STLayoutInterner, UnionLayout};

use bumpalo::Bump;

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
    for proc in procs.values_mut() {
        let new_proc = insert_reset_reuse_operations_proc(
            arena,
            layout_interner,
            home,
            ident_ids,
            update_mode_ids,
            proc.clone(),
        );
        *proc = new_proc;
    }
}

pub fn insert_reset_reuse_operations_proc<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i mut STLayoutInterner<'a>,
    home: ModuleId,
    ident_ids: &'i mut IdentIds,
    update_mode_ids: &'i mut UpdateModeIds,
    mut proc: Proc<'a>,
) -> Proc<'a> {
    let env = ReuseEnvironment::default();

    let new_body = insert_reset_reuse_operations_stmt(
        arena,
        layout_interner,
        home,
        ident_ids,
        update_mode_ids,
        env,
        arena.alloc(proc.body),
    );

    proc.body = new_body.clone();
    proc
}

fn insert_reset_reuse_operations_stmt<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i mut STLayoutInterner<'a>,
    home: ModuleId,
    ident_ids: &'i mut IdentIds,
    update_mode_ids: &'i mut UpdateModeIds,
    mut environment: ReuseEnvironment<'a>,
    stmt: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    match stmt {
        Stmt::Let(binding, expr, layout, continuation) => {
            // TODO store the layout for the binding in the environment.
            // TODO deal with fresh allocations w/ reuse.
            if let Expr::Tag {
                tag_layout,
                tag_id,
                arguments,
            } = expr
            {
                // The value of the tag is currently only used in the case of nullable recursive unions.
                // But for completeness we add every kind of union to the layout_tags.
                environment.add_layout_tag(layout, *tag_id);
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

            arena.alloc(Stmt::Let(*binding, expr.clone(), *layout, new_continuation))
        }
        Stmt::Switch {
            cond_symbol,
            cond_layout: layout,
            branches,
            default_branch,
            ret_layout,
        } => {
            let new_branches = branches
                .iter()
                .map(|(tag_id, info, branch)| {
                    let mut branch_env = environment.clone();
                    if let BranchInfo::Constructor { tag_id: tag, .. } = info {
                        branch_env.add_layout_tag(layout, *tag);
                    }

                    let new_branch = insert_reset_reuse_operations_stmt(
                        arena,
                        layout_interner,
                        home,
                        ident_ids,
                        update_mode_ids,
                        environment.clone(),
                        branch,
                    );

                    (*tag_id, info, new_branch, branch_env)
                })
                .collect::<std::vec::Vec<_>>();

            let new_default_branch = {
                let (info, branch) = default_branch;

                let mut branch_env = environment.clone();
                if let BranchInfo::Constructor { tag_id: tag, .. } = info {
                    branch_env.add_layout_tag(layout, *tag);
                }

                let new_branch = insert_reset_reuse_operations_stmt(
                    arena,
                    layout_interner,
                    home,
                    ident_ids,
                    update_mode_ids,
                    environment.clone(),
                    branch,
                );

                (info.clone(), new_branch, branch_env)
            };

            // TODO collect the used reuse token from each branch and drop the unused ones.
            // Make sure the current env is updated to have the correct reuse tokens consumed.

            // TODO add value to layout_tags
            todo!()
        }
        Stmt::Refcounting(rc, continuation) => {
            match rc {
                ModifyRc::Inc(_, _) => todo!(),
                // TODO verify whether reuse works for a decref as well. It should be.
                ModifyRc::Dec(symbol) | ModifyRc::DecRef(symbol) => {
                    // TODO insert actual reuse statements. if the reuse token is consumed (check by checking the top of the stack)

                    // Get the layout of the symbol from where it is defined.
                    let layout = environment.get_symbol_layout(*symbol);

                    // If the layout is reusable, add a reuse token for the layout to the environment.
                    if matches!(
                        can_reuse_layout_tag(layout_interner, &environment, &layout),
                        Reuse::Reusable
                    ) {
                        environment.push_reuse_token(&layout, *symbol);
                    };
                }
            }

            insert_reset_reuse_operations_stmt(
                arena,
                layout_interner,
                home,
                ident_ids,
                update_mode_ids,
                environment,
                continuation,
            )
        }
        Stmt::Ret(symbol) =>
        // The return statement just doesn't consume any tokens. Dropping these tokens will be handled before.
        {
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
type LayoutTags<'a> = MutMap<&'a InLayout<'a>, FooBar>;

/**
Map containing the reuse tokens of a layout.
A vec is used as a stack as we want to use the latest reuse token available.
*/
type ReuseTokens<'a> = MutMap<InLayout<'a>, Vec<ResetToken>>;

/**
A reuse token is a symbol that is used to reset a layout.
Matches variables that are pointers.
*/
type ResetToken = Symbol;

type FooBar = u16;

#[derive(Default, Clone)]
struct ReuseEnvironment<'a> {
    layout_tags: LayoutTags<'a>,
    reuse_tokens: ReuseTokens<'a>,
    symbol_layouts: MutMap<Symbol, &'a InLayout<'a>>,
}

impl<'a> ReuseEnvironment<'a> {
    /**
     Add the known tag for a layout.
     Used to optimize reuse of unions that are know to have a null pointer.
    */
    fn add_layout_tag(&mut self, layout: &'a InLayout<'a>, tag: FooBar) {
        self.layout_tags.insert(layout, tag);
    }

    /**
    Retrieve the known tag for a layout.
     */
    fn get_layout_tag(&self, layout: &InLayout<'a>) -> Option<FooBar> {
        self.layout_tags.get(layout).copied()
    }

    /**
    Retrieve a reuse token for a layout from the stack for said layout.
    */
    fn get_reuse_token(&mut self, layout: &InLayout<'a>) -> Option<ResetToken> {
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
    Push a reuse token for a layout on the stack for said layout.
    */
    fn push_reuse_token(&mut self, layout: &InLayout<'a>, token: ResetToken) {
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
        self.symbol_layouts.insert(symbol, layout);
    }

    /**
    Retrieve the layout of a symbol.
     */
    fn get_symbol_layout(&self, symbol: Symbol) -> &'a InLayout<'a> {
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
        // TODO why are strings/lists not reusable?
        _ => Reuse::Nonreusable,
    }
}
