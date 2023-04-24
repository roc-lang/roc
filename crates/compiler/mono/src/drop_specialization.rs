// This program was written by Jelle Teeuwissen within a final
// thesis project of the Computing Science master program at Utrecht
// University under supervision of Wouter Swierstra (w.s.swierstra@uu.nl).

// Implementation based of Drop Specialization from Perceus: Garbage Free Reference Counting with Reuse
// https://www.microsoft.com/en-us/research/uploads/prod/2021/06/perceus-pldi21.pdf

#![allow(clippy::too_many_arguments)]

use std::cmp::{self, Ord};
use std::iter::Iterator;
use std::ops::Neg;

use bumpalo::collections::vec::Vec;
use bumpalo::collections::CollectIn;

use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_target::{PtrWidth, TargetInfo};

use crate::ir::{
    BranchInfo, Call, CallType, Expr, JoinPointId, Literal, ModifyRc, Proc, ProcLayout, Stmt,
    UpdateModeId,
};
use crate::layout::{InLayout, Layout, LayoutInterner, STLayoutInterner, UnionLayout};

use bumpalo::Bump;

use roc_collections::{MutMap, MutSet};

/**
Try to find increments of symbols followed by decrements of the symbol they were indexed out of (their parent).
Then inline the decrement operation of the parent and removing matching pairs of increments and decrements.
*/
pub fn specialize_drops<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i mut STLayoutInterner<'a>,
    home: ModuleId,
    ident_ids: &'i mut IdentIds,
    target_info: TargetInfo,
    procs: &mut MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) {
    for ((_symbol, proc_layout), proc) in procs.iter_mut() {
        let mut environment =
            DropSpecializationEnvironment::new(arena, home, proc_layout.result, target_info);
        specialize_drops_proc(arena, layout_interner, ident_ids, &mut environment, proc);
    }
}

fn specialize_drops_proc<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i mut STLayoutInterner<'a>,
    ident_ids: &'i mut IdentIds,
    environment: &mut DropSpecializationEnvironment<'a>,
    proc: &mut Proc<'a>,
) {
    for (layout, symbol) in proc.args.iter().copied() {
        environment.add_symbol_layout(symbol, layout);
    }

    let new_body =
        specialize_drops_stmt(arena, layout_interner, ident_ids, environment, &proc.body);

    proc.body = new_body.clone();
}

fn specialize_drops_stmt<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i mut STLayoutInterner<'a>,
    ident_ids: &'i mut IdentIds,
    environment: &mut DropSpecializationEnvironment<'a>,
    stmt: &Stmt<'a>,
) -> &'a Stmt<'a> {
    match stmt {
        Stmt::Let(binding, expr, layout, continuation) => {
            environment.add_symbol_layout(*binding, *layout);

            macro_rules! alloc_let_with_continuation {
                ($environment:expr) => {{
                    let new_continuation = specialize_drops_stmt(
                        arena,
                        layout_interner,
                        ident_ids,
                        $environment,
                        continuation,
                    );
                    arena.alloc(Stmt::Let(*binding, expr.clone(), *layout, new_continuation))
                }};
            }

            match expr {
                Expr::Call(_) | Expr::Struct(_) => {
                    // TODO perhaps allow for some e.g. lowlevel functions to be called if they cannot modify the RC of the symbol.

                    // Calls can modify the RC of the symbol.
                    // If we move a increment of children after the function,
                    // the function might deallocate the child before we can use it after the function.
                    // If we move the decrement of the parent to before the function,
                    // the parent might be deallocated before the function can use it.
                    // Thus forget everything about any increments.

                    let mut new_environment = environment.clone_without_incremented();

                    alloc_let_with_continuation!(&mut new_environment)
                }
                Expr::Tag { tag_id, .. } => {
                    let mut new_environment = environment.clone_without_incremented();

                    new_environment.symbol_tag.insert(*binding, *tag_id);

                    alloc_let_with_continuation!(&mut new_environment)
                }
                Expr::StructAtIndex {
                    index, structure, ..
                } => {
                    environment.add_struct_child(*structure, *binding, *index);
                    // alloc_let_with_continuation!(environment)

                    // TODO do we need to remove the indexed value to prevent it from being dropped sooner?
                    // It will only be dropped sooner if the reference count is 1. Which can only happen if there is no increment before.
                    // So we should be fine.
                    alloc_let_with_continuation!(environment)
                }
                Expr::UnionAtIndex {
                    structure,
                    tag_id,
                    union_layout: _,
                    index,
                } => {
                    // TODO perhaps we need the union_layout later as well? if so, create a new function/map to store it.
                    environment.add_union_child(*structure, *binding, *tag_id, *index);
                    alloc_let_with_continuation!(environment)
                }
                Expr::ExprUnbox { symbol } => {
                    environment.add_box_child(*symbol, *binding);
                    alloc_let_with_continuation!(environment)
                }

                Expr::Reuse { .. } => {
                    alloc_let_with_continuation!(environment)
                }
                Expr::Reset { .. } => {
                    // TODO allow to inline this to replace it with resetref
                    alloc_let_with_continuation!(environment)
                }
                Expr::ResetRef { .. } => {
                    alloc_let_with_continuation!(environment)
                }
                Expr::RuntimeErrorFunction(_)
                | Expr::ExprBox { .. }
                | Expr::NullPointer
                | Expr::Literal(_)
                | Expr::GetTagId { .. }
                | Expr::EmptyArray
                | Expr::Array { .. } => {
                    // Does nothing relevant to drop specialization. So we can just continue.
                    alloc_let_with_continuation!(environment)
                }
            }
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
                .map(|(label, info, branch)| {
                    let mut branch_env = environment.clone_without_incremented();

                    if let BranchInfo::Constructor {
                        scrutinee: symbol,
                        tag_id: tag,
                        ..
                    } = info
                    {
                        branch_env.symbol_tag.insert(*symbol, *tag);
                    }

                    let new_branch = specialize_drops_stmt(
                        arena,
                        layout_interner,
                        ident_ids,
                        &mut branch_env,
                        branch,
                    );

                    (*label, info.clone(), new_branch.clone())
                })
                .collect_in::<Vec<_>>(arena)
                .into_bump_slice();

            let new_default_branch = {
                let (info, branch) = default_branch;

                let mut branch_env = environment.clone_without_incremented();

                if let BranchInfo::Constructor {
                    scrutinee: symbol,
                    tag_id: tag,
                    ..
                } = info
                {
                    branch_env.symbol_tag.insert(*symbol, *tag);
                }

                let new_branch = specialize_drops_stmt(
                    arena,
                    layout_interner,
                    ident_ids,
                    &mut branch_env,
                    branch,
                );

                (info.clone(), new_branch)
            };

            arena.alloc(Stmt::Switch {
                cond_symbol: *cond_symbol,
                cond_layout: *cond_layout,
                branches: new_branches,
                default_branch: new_default_branch,
                ret_layout: *ret_layout,
            })
        }
        Stmt::Ret(symbol) => arena.alloc(Stmt::Ret(*symbol)),
        Stmt::Refcounting(rc, continuation) => match rc {
            ModifyRc::Inc(symbol, count) => {
                let any = environment.any_incremented(symbol);

                // Add a symbol for every increment performed.
                environment.add_incremented(*symbol, *count);

                let new_continuation = specialize_drops_stmt(
                    arena,
                    layout_interner,
                    ident_ids,
                    environment,
                    continuation,
                );

                if any {
                    // There were increments before this one, best to let the first one do the increments.
                    // Or there are no increments left, so we can just continue.
                    new_continuation
                } else {
                    match environment.get_incremented(symbol) {
                        // This is the first increment, but all increments are consumed. So don't insert any.
                        0 => new_continuation,
                        // We still need to do some increments.
                        new_count => arena.alloc(Stmt::Refcounting(
                            ModifyRc::Inc(*symbol, new_count),
                            new_continuation,
                        )),
                    }
                }
            }
            ModifyRc::Dec(symbol) => {
                // We first check if there are any outstanding increments we can cross of with this decrement.
                // Then we check the continuation, since it might have a decrement of a symbol that's a child of this one.
                // Afterwards we perform drop specialization.
                // In the following example, we don't want to inline `dec b`, we want to remove the `inc a` and `dec a` instead.
                // let a = index b
                // inc a
                // dec a
                // dec b

                if environment.pop_incremented(symbol) {
                    // This decremented symbol was incremented before, so we can remove it.
                    specialize_drops_stmt(
                        arena,
                        layout_interner,
                        ident_ids,
                        environment,
                        continuation,
                    )
                } else {
                    // Collect all children that were incremented and make sure that one increment remains in the environment afterwards.
                    // To prevent
                    // let a = index b; inc a; dec b; ...; dec a
                    // from being translated to
                    // let a = index b; dec b
                    // As a might get dropped as a result of the decrement of b.
                    let mut incremented_children = environment
                        .get_children(symbol)
                        .iter()
                        .copied()
                        .filter_map(|child| environment.pop_incremented(&child).then_some(child))
                        .collect::<MutSet<_>>();

                    // This decremented symbol was not incremented before, perhaps the children were.
                    let in_layout = environment.get_symbol_layout(symbol);
                    let layout = layout_interner.get(*in_layout);

                    let new_dec = match layout {
                        // Layout has children, try to inline them.
                        Layout::Struct { field_layouts, .. } => specialize_struct(
                            arena,
                            layout_interner,
                            ident_ids,
                            environment,
                            symbol,
                            field_layouts,
                            &mut incremented_children,
                            continuation,
                        ),
                        Layout::Union(union_layout) => specialize_union(
                            arena,
                            layout_interner,
                            ident_ids,
                            environment,
                            symbol,
                            *in_layout,
                            union_layout,
                            &mut incremented_children,
                            continuation,
                        ),
                        Layout::Boxed(_layout) => specialize_boxed(
                            arena,
                            layout_interner,
                            ident_ids,
                            environment,
                            &mut incremented_children,
                            symbol,
                            continuation,
                        ),
                        // TODO: Implement this with uniqueness checks.
                        _ => {
                            let new_continuation = specialize_drops_stmt(
                                arena,
                                layout_interner,
                                ident_ids,
                                environment,
                                continuation,
                            );

                            // No children, keep decrementing the symbol.
                            arena.alloc(Stmt::Refcounting(ModifyRc::Dec(*symbol), new_continuation))
                        }
                    };

                    // Add back the increments for the children to the environment.
                    for child_symbol in incremented_children.iter() {
                        environment.add_incremented(*child_symbol, 1)
                    }

                    new_dec
                }
            }
            ModifyRc::DecRef(_) => {
                // Inlining has no point, since it doesn't decrement it's children
                arena.alloc(Stmt::Refcounting(
                    *rc,
                    specialize_drops_stmt(
                        arena,
                        layout_interner,
                        ident_ids,
                        environment,
                        continuation,
                    ),
                ))
            }
        },
        Stmt::Expect {
            condition,
            region,
            lookups,
            variables,
            remainder,
        } => arena.alloc(Stmt::Expect {
            condition: *condition,
            region: *region,
            lookups: *lookups,
            variables: *variables,
            remainder: specialize_drops_stmt(
                arena,
                layout_interner,
                ident_ids,
                environment,
                remainder,
            ),
        }),
        Stmt::ExpectFx {
            condition,
            region,
            lookups,
            variables,
            remainder,
        } => arena.alloc(Stmt::ExpectFx {
            condition: *condition,
            region: *region,
            lookups: *lookups,
            variables: *variables,
            remainder: specialize_drops_stmt(
                arena,
                layout_interner,
                ident_ids,
                environment,
                remainder,
            ),
        }),
        Stmt::Dbg {
            symbol,
            variable,
            remainder,
        } => arena.alloc(Stmt::Dbg {
            symbol: *symbol,
            variable: *variable,
            remainder: specialize_drops_stmt(
                arena,
                layout_interner,
                ident_ids,
                environment,
                remainder,
            ),
        }),
        Stmt::Join {
            id,
            parameters,
            body,
            remainder,
        } => {
            let mut new_environment = environment.clone_without_incremented();

            for param in parameters.iter() {
                new_environment.add_symbol_layout(param.symbol, param.layout);
            }

            let new_body = specialize_drops_stmt(
                arena,
                layout_interner,
                ident_ids,
                &mut new_environment,
                body,
            );

            arena.alloc(Stmt::Join {
                id: *id,
                parameters: *parameters,
                body: new_body,
                remainder: specialize_drops_stmt(
                    arena,
                    layout_interner,
                    ident_ids,
                    environment,
                    remainder,
                ),
            })
        }
        Stmt::Jump(joinpoint_id, arguments) => arena.alloc(Stmt::Jump(*joinpoint_id, *arguments)),
        Stmt::Crash(symbol, crash_tag) => arena.alloc(Stmt::Crash(*symbol, *crash_tag)),
    }
}

fn specialize_struct<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i mut STLayoutInterner<'a>,
    ident_ids: &'i mut IdentIds,
    environment: &mut DropSpecializationEnvironment<'a>,
    symbol: &Symbol,
    struct_layout: &'a [InLayout],
    incremented_children: &mut MutSet<Child>,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    match environment.struct_children.get(symbol) {
        // TODO all these children might be non reference counting, inlining the dec without any benefit.
        // Perhaps only insert children that are reference counted.
        Some(children) => {
            // TODO perhaps this allocation can be avoided.
            let children_clone = children.clone();

            // Map tracking which index of the struct is contained in which symbol.
            // And whether the child no longer has to be decremented.
            let mut index_symbols = MutMap::default();

            for (index, _layout) in struct_layout.iter().enumerate() {
                for (child, _i) in children_clone.iter().filter(|(_, i)| *i == index as u64) {
                    let removed = incremented_children.remove(&child);
                    index_symbols.insert(index, (*child, removed));

                    if removed {
                        break;
                    }
                }
            }

            let mut new_continuation =
                specialize_drops_stmt(arena, layout_interner, ident_ids, environment, continuation);

            // Make sure every field is decremented.
            // Reversed to ensure that the generated code decrements the fields in the correct order.
            for (i, field_layout) in struct_layout.iter().enumerate().rev() {
                // Only insert decrements for fields that are/contain refcounted values.
                if layout_interner.contains_refcounted(*field_layout) {
                    new_continuation = match index_symbols.get(&i) {
                        // This value has been indexed before, use that symbol.
                        Some((s, popped)) => {
                            if *popped {
                                // This symbol was popped, so we can skip the decrement.
                                new_continuation
                            } else {
                                // This symbol was indexed but not decremented, so we will decrement it.
                                arena.alloc(Stmt::Refcounting(ModifyRc::Dec(*s), new_continuation))
                            }
                        }

                        // This value has not been index before, create a new symbol.
                        None => {
                            let field_symbol =
                                environment.create_symbol(ident_ids, &format!("field_val_{}", i));

                            let field_val_expr = Expr::StructAtIndex {
                                index: i as u64,
                                field_layouts: struct_layout,
                                structure: *symbol,
                            };

                            arena.alloc(Stmt::Let(
                                field_symbol,
                                field_val_expr,
                                *field_layout,
                                arena.alloc(Stmt::Refcounting(
                                    ModifyRc::Dec(field_symbol),
                                    new_continuation,
                                )),
                            ))
                        }
                    };
                }
            }

            new_continuation
        }
        None => {
            // No known children, keep decrementing the symbol.
            let new_continuation =
                specialize_drops_stmt(arena, layout_interner, ident_ids, environment, continuation);

            arena.alloc(Stmt::Refcounting(ModifyRc::Dec(*symbol), new_continuation))
        }
    }
}

fn specialize_union<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i mut STLayoutInterner<'a>,
    ident_ids: &'i mut IdentIds,
    environment: &mut DropSpecializationEnvironment<'a>,
    symbol: &Symbol,
    layout: InLayout<'a>,
    union_layout: UnionLayout<'a>,
    incremented_children: &mut MutSet<Child>,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    let current_tag = environment.symbol_tag.get(symbol).copied();

    macro_rules! keep_original_decrement {
        () => {{
            let new_continuation =
                specialize_drops_stmt(arena, layout_interner, ident_ids, environment, continuation);
            arena.alloc(Stmt::Refcounting(ModifyRc::Dec(*symbol), new_continuation))
        }};
    }

    match get_union_tag_layout(union_layout, current_tag) {
        // No known tag, decrement the symbol as usual.
        UnionFieldLayouts::Unknown => {
            keep_original_decrement!()
        }

        // The union is null, so we can skip the decrement.
        UnionFieldLayouts::Null => {
            specialize_drops_stmt(arena, layout_interner, ident_ids, environment, continuation)
        }

        // We know the tag, we can specialize the decrement for the tag.
        UnionFieldLayouts::Found { field_layouts, tag } => {
            match environment.union_children.get(symbol) {
                None => keep_original_decrement!(),
                Some(children) => {
                    // TODO perhaps this allocation can be avoided.
                    let children_clone = children.clone();

                    // Map tracking which index of the struct is contained in which symbol.
                    // And whether the child no longer has to be decremented.
                    let mut index_symbols = MutMap::default();

                    for (index, _layout) in field_layouts.iter().enumerate() {
                        for (child, t, _i) in children_clone
                            .iter()
                            .filter(|(_child, _t, i)| *i == index as u64)
                        {
                            debug_assert_eq!(tag, *t);

                            let removed = incremented_children.remove(&child);
                            index_symbols.insert(index, (*child, removed));

                            if removed {
                                break;
                            }
                        }
                    }

                    let new_continuation = specialize_drops_stmt(
                        arena,
                        layout_interner,
                        ident_ids,
                        environment,
                        continuation,
                    );

                    let refcount_fields = |layout_interner: &mut STLayoutInterner<'a>,
                                           ident_ids: &mut IdentIds,
                                           rc_popped: Option<
                        fn(arena: &'a Bump, Symbol, &'a Stmt<'a>) -> &'a Stmt<'a>,
                    >,
                                           rc_unpopped: Option<
                        fn(arena: &'a Bump, Symbol, &'a Stmt<'a>) -> &'a Stmt<'a>,
                    >,
                                           continuation: &'a Stmt<'a>|
                     -> &'a Stmt<'a> {
                        let mut new_continuation = continuation;

                        // Reversed to ensure that the generated code decrements the fields in the correct order.
                        for (i, field_layout) in field_layouts.iter().enumerate().rev() {
                            // Only insert decrements for fields that are/contain refcounted values.
                            if layout_interner.contains_refcounted(*field_layout) {
                                new_continuation = match index_symbols.get(&i) {
                                    // This value has been indexed before, use that symbol.
                                    Some((s, popped)) => {
                                        if *popped {
                                            // This symbol was popped, so we can skip the decrement.
                                            match rc_popped {
                                                Some(rc) => rc(arena, *s, new_continuation),
                                                None => new_continuation,
                                            }
                                        } else {
                                            // This symbol was indexed but not decremented, so we will decrement it.
                                            match rc_unpopped {
                                                Some(rc) => rc(arena, *s, new_continuation),
                                                None => new_continuation,
                                            }
                                        }
                                    }

                                    // This value has not been index before, create a new symbol.
                                    None => match rc_unpopped {
                                        Some(rc) => {
                                            let field_symbol = environment.create_symbol(
                                                ident_ids,
                                                &format!("field_val_{}", i),
                                            );

                                            let field_val_expr = Expr::UnionAtIndex {
                                                structure: *symbol,
                                                tag_id: tag,
                                                union_layout,
                                                index: i as u64,
                                            };

                                            arena.alloc(Stmt::Let(
                                                field_symbol,
                                                field_val_expr,
                                                *field_layout,
                                                rc(arena, field_symbol, new_continuation),
                                            ))
                                        }
                                        None => new_continuation,
                                    },
                                };
                            }
                        }

                        new_continuation
                    };

                    match union_layout {
                        UnionLayout::NonRecursive(_) => refcount_fields(
                            layout_interner,
                            ident_ids,
                            // Do nothing for the children that were incremented before, as the decrement will cancel out.
                            None,
                            // Decrement the children that were not incremented before. And thus don't cancel out.
                            Some(|arena, symbol, continuation| {
                                arena.alloc(Stmt::Refcounting(ModifyRc::Dec(symbol), continuation))
                            }),
                            new_continuation,
                        ),
                        UnionLayout::Recursive(_)
                        | UnionLayout::NonNullableUnwrapped(_)
                        | UnionLayout::NullableWrapped { .. }
                        | UnionLayout::NullableUnwrapped { .. } => {
                            branch_uniqueness(
                                arena,
                                ident_ids,
                                layout_interner,
                                environment,
                                *symbol,
                                layout,
                                // If the symbol is unique:
                                // - drop the children that were not incremented before
                                // - don't do anything for the children that were incremented before
                                // - free the parent
                                |layout_interner, ident_ids, continuation| {
                                    refcount_fields(
                                        layout_interner,
                                        ident_ids,
                                        // Do nothing for the children that were incremented before, as the decrement will cancel out.
                                        None,
                                        // Decrement the children that were not incremented before. And thus don't cancel out.
                                        Some(|arena, symbol, continuation| {
                                            arena.alloc(Stmt::Refcounting(
                                                ModifyRc::Dec(symbol),
                                                continuation,
                                            ))
                                        }),
                                        arena.alloc(Stmt::Refcounting(
                                            // TODO this could be replaced by a free if ever added to the IR.
                                            ModifyRc::DecRef(*symbol),
                                            continuation,
                                        )),
                                    )
                                },
                                // If the symbol is not unique:
                                // - increment the children that were incremented before
                                // - don't do anything for the children that were not incremented before
                                // - decref the parent
                                |layout_interner, ident_ids, continuation| {
                                    refcount_fields(
                                        layout_interner,
                                        ident_ids,
                                        Some(|arena, symbol, continuation| {
                                            arena.alloc(Stmt::Refcounting(
                                                ModifyRc::Inc(symbol, 1),
                                                continuation,
                                            ))
                                        }),
                                        None,
                                        arena.alloc(Stmt::Refcounting(
                                            ModifyRc::DecRef(*symbol),
                                            continuation,
                                        )),
                                    )
                                },
                                new_continuation,
                            )
                        }
                    }
                }
            }
        }
    }
}

fn specialize_boxed<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i mut STLayoutInterner<'a>,
    ident_ids: &'i mut IdentIds,
    environment: &mut DropSpecializationEnvironment<'a>,
    incremented_children: &mut MutSet<Child>,
    symbol: &Symbol,
    continuation: &'a Stmt<'a>,
) -> &'a mut Stmt<'a> {
    let removed = match incremented_children.iter().next() {
        Some(s) => incremented_children.remove(&s.clone()),
        None => false,
    };

    let new_continuation =
        specialize_drops_stmt(arena, layout_interner, ident_ids, environment, continuation);

    if removed {
        // No need to decrement the containing value since we already decremented the child.
        arena.alloc(Stmt::Refcounting(
            ModifyRc::DecRef(*symbol),
            new_continuation,
        ))
    } else {
        // No known children, keep decrementing the symbol.
        arena.alloc(Stmt::Refcounting(ModifyRc::Dec(*symbol), new_continuation))
    }
}

/**
Get the field layouts of a union given a tag.
*/
fn get_union_tag_layout(union_layout: UnionLayout<'_>, tag: Option<Tag>) -> UnionFieldLayouts {
    match (union_layout, tag) {
        (UnionLayout::NonRecursive(union_layouts), Some(tag)) => UnionFieldLayouts::Found {
            field_layouts: union_layouts[tag as usize],
            tag,
        },
        (UnionLayout::Recursive(union_layouts), Some(tag)) => UnionFieldLayouts::Found {
            field_layouts: union_layouts[tag as usize],
            tag,
        },
        (UnionLayout::NonNullableUnwrapped(union_layouts), None) => {
            // This union has just a single tag. So the tag is 0.
            UnionFieldLayouts::Found {
                field_layouts: union_layouts,
                tag: 0,
            }
        }
        (
            UnionLayout::NullableWrapped {
                nullable_id,
                other_tags,
            },
            Some(tag),
        ) => {
            match Ord::cmp(&tag, &nullable_id) {
                // tag is less than nullable_id, so the index is the same as the tag.
                cmp::Ordering::Less => UnionFieldLayouts::Found {
                    field_layouts: other_tags[tag as usize],
                    tag,
                },
                // tag and nullable_id are equal, so the union is null.
                cmp::Ordering::Equal => UnionFieldLayouts::Null,
                // tag is greater than nullable_id, so the index is the tag - 1 (as the nullable tag is in between).
                cmp::Ordering::Greater => UnionFieldLayouts::Found {
                    field_layouts: other_tags[(tag as usize) - 1],
                    tag,
                },
            }
        }
        (
            UnionLayout::NullableUnwrapped {
                nullable_id,
                other_fields,
            },
            Some(tag),
        ) => {
            if tag == (nullable_id as u16) {
                UnionFieldLayouts::Null
            } else {
                UnionFieldLayouts::Found {
                    field_layouts: other_fields,
                    tag,
                }
            }
        }
        (_, _) => UnionFieldLayouts::Unknown,
    }
}

/**
Branch on the uniqueness of a symbol.
Using a joinpoint with the continuation as the body.
*/
fn branch_uniqueness<'a, 'i, F1, F2>(
    arena: &'a Bump,
    ident_ids: &'i mut IdentIds,
    layout_interner: &'i mut STLayoutInterner<'a>,
    environment: &DropSpecializationEnvironment<'a>,
    symbol: Symbol,
    layout: InLayout<'a>,
    unique: F1,
    not_unique: F2,
    continutation: &'a Stmt<'a>,
) -> &'a Stmt<'a>
where
    F1: FnOnce(&mut STLayoutInterner<'a>, &mut IdentIds, &'a Stmt<'a>) -> &'a Stmt<'a>,
    F2: FnOnce(&mut STLayoutInterner<'a>, &mut IdentIds, &'a Stmt<'a>) -> &'a Stmt<'a>,
{
    let join_id = JoinPointId(environment.create_symbol(ident_ids, &format!("uniqueness_join")));

    let jump = arena.alloc(Stmt::Jump(join_id, arena.alloc([])));

    let u = unique(layout_interner, ident_ids, jump);
    let n = not_unique(layout_interner, ident_ids, jump);

    let join = |unique_symbol| {
        let switch = arena.alloc(Stmt::Switch {
            cond_symbol: unique_symbol,
            cond_layout: Layout::BOOL,
            branches: &*arena.alloc([(1, BranchInfo::None, u.clone())]),
            default_branch: (BranchInfo::None, n),
            ret_layout: environment.layout,
        });

        let join = arena.alloc(Stmt::Join {
            id: join_id,
            parameters: arena.alloc([]),
            body: switch,
            remainder: continutation,
        });

        join
    };

    unique_symbol(
        arena,
        ident_ids,
        layout_interner,
        environment,
        symbol,
        layout,
        join,
    )
}

fn unique_symbol<'a, 'i, F>(
    arena: &'a Bump,
    ident_ids: &'i mut IdentIds,
    layout_interner: &'i mut STLayoutInterner<'a>,
    environment: &DropSpecializationEnvironment<'a>,
    symbol: Symbol,
    layout: InLayout<'a>,
    continuation: F,
) -> &'a Stmt<'a>
where
    F: FnOnce(Symbol) -> &'a mut Stmt<'a>,
{
    let rc_ptr = environment.create_symbol(ident_ids, "rc_ptr");
    let rc = environment.create_symbol(ident_ids, "rc");
    let refcount_1 = environment.create_symbol(ident_ids, "refcount_1");
    let is_unique = environment.create_symbol(ident_ids, "is_unique");
    let addr = environment.create_symbol(ident_ids, "addr");

    let refcount_layout = Layout::isize(environment.target_info);
    let union_layout = UnionLayout::NonNullableUnwrapped(arena.alloc([refcount_layout]));

    // Uniqueness test
    let is_unique_stmt = arena.alloc(Stmt::Let(
        is_unique,
        Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::Eq,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: arena.alloc_slice_copy(&[rc, refcount_1]),
        }),
        Layout::BOOL,
        continuation(is_unique),
    ));

    // Constant for unique refcount
    let refcount_1_encoded = match environment.target_info.ptr_width() {
        PtrWidth::Bytes4 => i32::MIN as i128,
        PtrWidth::Bytes8 => i64::MIN as i128,
    }
    .to_ne_bytes();
    let refcount_1_expr = Expr::Literal(Literal::Int(refcount_1_encoded));
    let refcount_1_stmt = Stmt::Let(
        refcount_1,
        refcount_1_expr,
        refcount_layout,
        arena.alloc(is_unique_stmt),
    );

    // Refcount value
    let rc_expr = Expr::UnionAtIndex {
        structure: rc_ptr,
        tag_id: 0,
        union_layout,
        index: 0,
    };
    let rc_stmt = Stmt::Let(rc, rc_expr, refcount_layout, arena.alloc(refcount_1_stmt));

    // Refcount pointer
    let rc_ptr_stmt = {
        rc_ptr_from_data_ptr_help(
            arena,
            ident_ids,
            layout_interner,
            environment,
            symbol,
            layout,
            rc_ptr,
            union_layout.stores_tag_id_in_pointer(environment.target_info),
            arena.alloc(rc_stmt),
            addr,
        )
    };

    rc_ptr_stmt
}

fn rc_ptr_from_data_ptr_help<'a, 'i>(
    arena: &'a Bump,
    ident_ids: &mut IdentIds,
    layout_interner: &'i mut STLayoutInterner<'a>,
    environment: &DropSpecializationEnvironment<'a>,
    symbol: Symbol,
    layout: InLayout<'a>,
    rc_ptr_sym: Symbol,
    mask_lower_bits: bool,
    following: &'a Stmt<'a>,
    addr_sym: Symbol,
) -> &'a Stmt<'a> {
    let refcount_layout = Layout::isize(environment.target_info);

    // Typecast the structure pointer to an integer
    // Backends expect a number Layout to choose the right "subtract" instruction
    let as_int_sym = if mask_lower_bits {
        environment.create_symbol(ident_ids, "as_int")
    } else {
        addr_sym
    };
    let as_int_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::PtrCast,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: arena.alloc([symbol]),
    });
    let as_int_stmt = |next| arena.alloc(Stmt::Let(as_int_sym, as_int_expr, refcount_layout, next));

    // Pointer size constant
    let ptr_size_sym = environment.create_symbol(ident_ids, "ptr_size");
    let ptr_size_expr = Expr::Literal(Literal::Int(
        (environment.target_info.ptr_width() as i128).to_ne_bytes(),
    ));
    let ptr_size_stmt = |next| {
        arena.alloc(Stmt::Let(
            ptr_size_sym,
            ptr_size_expr,
            refcount_layout,
            next,
        ))
    };

    // Refcount address
    let rc_addr_sym = environment.create_symbol(ident_ids, "rc_addr");
    let sub_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::NumSubSaturated,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: arena.alloc([addr_sym, ptr_size_sym]),
    });
    let sub_stmt = |next| {
        arena.alloc(Stmt::Let(
            rc_addr_sym,
            sub_expr,
            Layout::usize(environment.target_info),
            next,
        ))
    };

    // Typecast the refcount address from integer to pointer
    let cast_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::PtrCast,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: arena.alloc([rc_addr_sym]),
    });
    let recursion_ptr = layout_interner.insert(Layout::RecursivePointer(layout));
    let cast_stmt = |next| arena.alloc(Stmt::Let(rc_ptr_sym, cast_expr, recursion_ptr, next));

    if mask_lower_bits {
        // Mask for lower bits (for tag union id)
        let mask_sym = environment.create_symbol(ident_ids, "mask");
        let mask_expr = Expr::Literal(Literal::Int(
            (environment.target_info.ptr_width() as i128)
                .neg()
                .to_ne_bytes(),
        ));
        let mask_stmt = |next| arena.alloc(Stmt::Let(mask_sym, mask_expr, refcount_layout, next));

        let and_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::And,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: arena.alloc([as_int_sym, mask_sym]),
        });
        let and_stmt = |next| arena.alloc(Stmt::Let(addr_sym, and_expr, refcount_layout, next));

        as_int_stmt(mask_stmt(and_stmt(ptr_size_stmt(sub_stmt(cast_stmt(
            following,
        ))))))
    } else {
        as_int_stmt(ptr_size_stmt(sub_stmt(cast_stmt(following))))
    }
}

enum UnionFieldLayouts<'a> {
    Found {
        field_layouts: &'a [InLayout<'a>],
        tag: Tag,
    },
    Unknown,
    Null,
}

type Index = u64;

type Parent = Symbol;

type Child = Symbol;

type Tag = u16;

#[derive(Clone)]
struct DropSpecializationEnvironment<'a> {
    arena: &'a Bump,
    home: ModuleId,
    layout: InLayout<'a>,
    target_info: TargetInfo,

    symbol_layouts: MutMap<Symbol, InLayout<'a>>,

    // Keeps track of which parent symbol is indexed by which child symbol for structs
    struct_children: MutMap<Parent, Vec<'a, (Child, Index)>>,

    // Keeps track of which parent symbol is indexed by which child symbol for unions
    union_children: MutMap<Parent, Vec<'a, (Child, Tag, Index)>>,

    // Keeps track of which parent symbol is indexed by which child symbol for boxes
    box_children: MutMap<Parent, Vec<'a, Child>>,

    // Keeps track of all incremented symbols.
    incremented_symbols: MutMap<Symbol, u64>,

    // Map containing the curren't known tag of a layout.
    symbol_tag: MutMap<Symbol, Tag>,
}

impl<'a> DropSpecializationEnvironment<'a> {
    fn new(arena: &'a Bump, home: ModuleId, layout: InLayout<'a>, target_info: TargetInfo) -> Self {
        Self {
            arena,
            home,
            layout,
            target_info,
            symbol_layouts: MutMap::default(),
            struct_children: MutMap::default(),
            union_children: MutMap::default(),
            box_children: MutMap::default(),
            incremented_symbols: MutMap::default(),
            symbol_tag: MutMap::default(),
        }
    }

    fn clone_without_incremented(&self) -> Self {
        Self {
            arena: self.arena,
            home: self.home,
            layout: self.layout,
            target_info: self.target_info,
            symbol_layouts: self.symbol_layouts.clone(),
            struct_children: self.struct_children.clone(),
            union_children: self.union_children.clone(),
            box_children: self.box_children.clone(),
            incremented_symbols: MutMap::default(),
            symbol_tag: self.symbol_tag.clone(),
        }
    }

    fn create_symbol<'i>(&self, ident_ids: &'i mut IdentIds, debug_name: &str) -> Symbol {
        let ident_id = ident_ids.add_str(debug_name);
        Symbol::new(self.home, ident_id)
    }

    fn add_symbol_layout(&mut self, symbol: Symbol, layout: InLayout<'a>) {
        self.symbol_layouts.insert(symbol, layout);
    }

    fn get_symbol_layout(&self, symbol: &Symbol) -> &InLayout<'a> {
        self.symbol_layouts
            .get(symbol)
            .expect("All symbol layouts should be known.")
    }

    fn add_struct_child(&mut self, parent: Parent, child: Child, index: Index) {
        self.struct_children
            .entry(parent)
            .or_insert(Vec::new_in(self.arena))
            .push((child, index));
    }
    fn add_union_child(&mut self, parent: Parent, child: Child, tag: u16, index: Index) {
        self.union_children
            .entry(parent)
            .or_insert(Vec::new_in(self.arena))
            .push((child, tag, index));
    }
    fn add_box_child(&mut self, parent: Parent, child: Child) {
        self.box_children
            .entry(parent)
            .or_insert(Vec::new_in(self.arena))
            .push(child);
    }

    fn get_children(&self, parent: &Parent) -> Vec<'a, Symbol> {
        let mut res = Vec::new_in(self.arena);

        if let Some(children) = self.struct_children.get(parent) {
            children.iter().for_each(|(child, _)| res.push(*child));
        }

        if let Some(children) = self.union_children.get(parent) {
            children.iter().for_each(|(child, _, _)| res.push(*child));
        }

        if let Some(children) = self.box_children.get(parent) {
            children.iter().for_each(|child| res.push(*child));
        }

        res
    }

    /**
    Add a symbol for every increment performed.
     */
    fn add_incremented(&mut self, symbol: Symbol, count: u64) {
        self.incremented_symbols
            .entry(symbol)
            .and_modify(|c| *c += count)
            .or_insert(count);
    }

    fn any_incremented(&self, symbol: &Symbol) -> bool {
        self.incremented_symbols.contains_key(symbol)
    }

    /**
    Return the amount of times a symbol still has to be incremented.
    Accounting for later consumtion and removal of the increment.
    */
    fn get_incremented(&mut self, symbol: &Symbol) -> u64 {
        self.incremented_symbols.remove(symbol).unwrap_or(0)
    }

    fn pop_incremented(&mut self, symbol: &Symbol) -> bool {
        match self.incremented_symbols.get_mut(symbol) {
            Some(1) => {
                self.incremented_symbols.remove(symbol);
                true
            }
            Some(c) => {
                *c -= 1;
                true
            }
            None => false,
        }
    }

    // TODO assert that a parent is only inlined once / assert max single dec per parent.
}

// TODO Lowlevel is unqiue check
