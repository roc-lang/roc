// This program was written by Jelle Teeuwissen within a final
// thesis project of the Computing Science master program at Utrecht
// University under supervision of Wouter Swierstra (w.s.swierstra@uu.nl).

// Implementation based of Drop Specialization from Perceus: Garbage Free Reference Counting with Reuse
// https://www.microsoft.com/en-us/research/uploads/prod/2021/06/perceus-pldi21.pdf

#![allow(clippy::too_many_arguments)]

use std::cmp::{self, Ord};
use std::iter::Iterator;

use bumpalo::collections::vec::Vec;
use bumpalo::collections::CollectIn;

use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};

use crate::ir::{
    BranchInfo, Call, CallType, ErasedField, Expr, JoinPointId, ListLiteralElement, ModifyRc, Proc,
    ProcLayout, Stmt, UpdateModeId,
};
use crate::layout::{
    Builtin, InLayout, Layout, LayoutInterner, LayoutRepr, STLayoutInterner, UnionLayout,
};

use bumpalo::Bump;

use roc_collections::MutMap;

/**
Try to find increments of symbols followed by decrements of the symbol they were indexed out of (their parent).
Then inline the decrement operation of the parent and removing matching pairs of increments and decrements.
*/
pub fn specialize_drops<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i mut STLayoutInterner<'a>,
    home: ModuleId,
    ident_ids: &'i mut IdentIds,
    procs: &mut MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) {
    for ((_symbol, proc_layout), proc) in procs.iter_mut() {
        let mut environment = DropSpecializationEnvironment::new(arena, home, proc_layout.result);
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
        Stmt::Let(binding, expr @ Expr::Call(call), layout, continuation) => {
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

            match call.call_type.clone().replace_lowlevel_wrapper() {
                CallType::LowLevel {
                    op: LowLevel::ListGetUnsafe,
                    ..
                } => {
                    let [structure, index] = match call.arguments {
                        [structure, index] => [structure, index],
                        _ => unreachable!("List get should have two arguments"),
                    };

                    environment.add_list_child_symbol(*structure, *binding, index);

                    alloc_let_with_continuation!(environment)
                }
                // Check whether the increments can be passed to the continuation.
                CallType::LowLevel { op, .. } => match low_level_no_rc(&op) {
                    // It should be safe to pass the increments to the continuation.
                    RC::NoRc => alloc_let_with_continuation!(environment),
                    // We probably should not pass the increments to the continuation.
                    RC::Rc | RC::Uknown => {
                        let incremented_symbols = environment.incremented_symbols.drain();

                        let new_stmt = alloc_let_with_continuation!(environment);

                        // The new_environment might have inserted increments that were set to 0 before. We need to add th
                        for (symbol, increment) in incremented_symbols.map.into_iter() {
                            environment
                                .incremented_symbols
                                .insert_count(symbol, increment);
                        }

                        new_stmt
                    }
                },
                _ => {
                    // Calls can modify the RC of the symbol.
                    // If we move a increment of children after the function,
                    // the function might deallocate the child before we can use it after the function.
                    // If we move the decrement of the parent to before the function,
                    // the parent might be deallocated before the function can use it.
                    // Thus forget everything about any increments.

                    let incremented_symbols = environment.incremented_symbols.drain();

                    let new_stmt = alloc_let_with_continuation!(environment);

                    // The new_environment might have inserted increments that were set to 0 before. We need to add th
                    for (symbol, increment) in incremented_symbols.map.into_iter() {
                        environment
                            .incremented_symbols
                            .insert_count(symbol, increment);
                    }

                    new_stmt
                }
            }
        }
        Stmt::Let(_, _, _, _) => {
            use Expr::*;

            // to prevent stack overflows, try to use an explicit stack to accumulate a bunch of
            // Let statements. Call expressions require more logic and are never put on this stack
            let mut stack = vec![];

            let mut stmt = stmt;

            while let Stmt::Let(binding, expr, layout, continuation) = stmt {
                environment.add_symbol_layout(*binding, *layout);

                // update the environment based on the expr
                match expr {
                    Call(_) => {
                        // Expr::Call is tricky and we are lazy and handle it elsewhere. it
                        // ends a chain of eligible Let statements.
                        break;
                    }
                    Literal(crate::ir::Literal::Int(i)) => {
                        environment
                            .symbol_index
                            .insert(*binding, i128::from_ne_bytes(*i) as u64);
                    }
                    Literal(_) => { /* do nothing */ }
                    Tag {
                        tag_id,
                        arguments: children,
                        ..
                    } => {
                        environment.symbol_tag.insert(*binding, *tag_id);

                        for (index, child) in children.iter().enumerate() {
                            environment.add_union_child(*binding, *child, *tag_id, index as u64);
                        }
                    }
                    Struct(children) => {
                        for (index, child) in children.iter().enumerate() {
                            environment.add_struct_child(*binding, *child, index as u64);
                        }
                    }
                    StructAtIndex {
                        index, structure, ..
                    } => {
                        environment.add_struct_child(*structure, *binding, *index);

                        // TODO do we need to remove the indexed value to prevent it from being dropped sooner?
                        // It will only be dropped sooner if the reference count is 1. Which can only happen if there is no increment before.
                        // So we should be fine.
                    }
                    UnionAtIndex {
                        structure,
                        tag_id,
                        index,
                        ..
                    } => {
                        // TODO perhaps we need the union_layout later as well? if so, create a new function/map to store it.
                        environment.add_union_child(*structure, *binding, *tag_id, *index);
                        // Generated code might know the tag of the union without switching on it.
                        // So if we UnionAtIndex, we must know the tag and we can use it to specialize the drop.
                        environment.symbol_tag.insert(*structure, *tag_id);
                    }
                    GetElementPointer {
                        structure, indices, ..
                    } => {
                        // Generated code might know the tag of the union without switching on it.
                        // So if we GetElementPointer, we must know the tag and we can use it to specialize the drop.
                        environment.symbol_tag.insert(*structure, indices[0] as u16);
                    }
                    Array {
                        elems: children, ..
                    } => {
                        let it =
                            children
                                .iter()
                                .enumerate()
                                .filter_map(|(index, child)| match child {
                                    ListLiteralElement::Literal(_) => None,
                                    ListLiteralElement::Symbol(s) => Some((index, s)),
                                });

                        for (index, child) in it {
                            environment.add_list_child(*binding, *child, index as u64);
                        }
                    }
                    ErasedMake { value, callee: _ } => {
                        if let Some(value) = value {
                            environment.add_struct_child(*binding, *value, 0);
                        }
                    }
                    ErasedLoad { symbol, field } => {
                        match field {
                            ErasedField::Value => {
                                environment.add_struct_child(*symbol, *binding, 0);
                            }
                            ErasedField::Callee | ErasedField::ValuePtr => {
                                // nothing to own
                            }
                        }
                    }
                    Reset { .. } | Expr::ResetRef { .. } => { /* do nothing */ }
                    FunctionPointer { .. }
                    | GetTagId { .. }
                    | Alloca { .. }
                    | EmptyArray
                    | NullPointer => { /* do nothing */ }
                }

                // now store the let binding for later
                stack.push((*binding, expr.clone(), *layout));

                // and "recurse" down the statement chain
                stmt = continuation;
            }

            stack.into_iter().rev().fold(
                specialize_drops_stmt(arena, layout_interner, ident_ids, environment, stmt),
                |acc, (binding, expr, layout)| arena.alloc(Stmt::Let(binding, expr, layout, acc)),
            )
        }
        Stmt::Switch {
            cond_symbol,
            cond_layout,
            branches,
            default_branch,
            ret_layout,
        } => {
            macro_rules! insert_branch_info {
                ($branch_env:expr,$info:expr ) => {
                    match $info {
                        BranchInfo::Constructor {
                            scrutinee: symbol,
                            tag_id: tag,
                            ..
                        } => {
                            $branch_env.symbol_tag.insert(*symbol, *tag);
                        }
                        BranchInfo::List {
                            scrutinee: symbol,
                            len,
                        } => {
                            $branch_env.list_length.insert(*symbol, *len);
                        }
                        _ => (),
                    }
                };
            }

            let new_branches = branches
                .iter()
                .map(|(label, info, branch)| {
                    let mut branch_env = environment.clone();

                    insert_branch_info!(branch_env, info);

                    let new_branch = specialize_drops_stmt(
                        arena,
                        layout_interner,
                        ident_ids,
                        &mut branch_env,
                        branch,
                    );

                    (*label, info.clone(), new_branch.clone(), branch_env)
                })
                .collect_in::<Vec<_>>(arena)
                .into_bump_slice();

            let new_default_branch = {
                let (info, branch) = default_branch;

                let mut branch_env = environment.clone();

                insert_branch_info!(branch_env, info);

                let new_branch = specialize_drops_stmt(
                    arena,
                    layout_interner,
                    ident_ids,
                    &mut branch_env,
                    branch,
                );

                (info.clone(), new_branch, branch_env)
            };

            // Find consumed increments in each branch and make sure they are consumed in all branches.
            // By incrementing them in each branch where they were not consumed.
            {
                let branch_envs = {
                    let mut branch_environments =
                        Vec::with_capacity_in(new_branches.len() + 1, arena);

                    for (_, _, _, branch_env) in new_branches.iter() {
                        branch_environments.push(branch_env);
                    }

                    branch_environments.push(&new_default_branch.2);

                    branch_environments
                };

                // Find the lowest symbol count for each symbol in each branch, and update the environment to match.
                for (symbol, count) in environment.incremented_symbols.map.iter_mut() {
                    let consumed = branch_envs
                        .iter()
                        .map(|branch_env| {
                            branch_env.incremented_symbols.map.get(symbol).unwrap_or(&0)
                        })
                        .min()
                        .unwrap();

                    // Update the existing env to match the lowest count.
                    *count = *consumed;
                }
            }

            macro_rules! insert_incs {
                ($branch_env:expr, $branch:expr ) => {{
                    let symbol_differences =
                        environment
                            .incremented_symbols
                            .map
                            .iter()
                            .filter_map(|(symbol, count)| {
                                let branch_count = $branch_env
                                    .incremented_symbols
                                    .map
                                    .get(symbol)
                                    .unwrap_or(&0);

                                match branch_count - count {
                                    0 => None,
                                    difference => Some((symbol, difference)),
                                }
                            });

                    symbol_differences.fold($branch, |new_branch, (symbol, difference)| {
                        arena.alloc(Stmt::Refcounting(
                            ModifyRc::Inc(*symbol, difference),
                            new_branch,
                        ))
                    })
                }};
            }

            environment.jump_incremented_symbols =
                new_default_branch.2.jump_incremented_symbols.clone();

            let newer_branches = new_branches
                .iter()
                .map(|(label, info, branch, branch_env)| {
                    for (joinpoint, current_incremented_symbols) in
                        environment.jump_incremented_symbols.iter_mut()
                    {
                        let opt_symbols = branch_env.jump_incremented_symbols.get(joinpoint);
                        if let Some(branch_incremented_symbols) = opt_symbols {
                            current_incremented_symbols.map.retain(|key, join_count| {
                                let opt_count = branch_incremented_symbols.map.get(key);
                                if let Some(count) = opt_count {
                                    *join_count = std::cmp::min(*join_count, *count);
                                }

                                // retain only the Some cases
                                opt_count.is_some()
                            });
                        }
                    }

                    let new_branch = insert_incs!(branch_env, branch);

                    (*label, info.clone(), new_branch.clone())
                })
                .collect_in::<Vec<_>>(arena)
                .into_bump_slice();

            let newer_default_branch = {
                let (info, branch, branch_env) = new_default_branch;

                let new_branch = insert_incs!(branch_env, branch);

                (info.clone(), new_branch)
            };

            arena.alloc(Stmt::Switch {
                cond_symbol: *cond_symbol,
                cond_layout: *cond_layout,
                branches: newer_branches,
                default_branch: newer_default_branch,
                ret_layout: *ret_layout,
            })
        }
        Stmt::Ret(symbol) => arena.alloc(Stmt::Ret(*symbol)),
        Stmt::Refcounting(rc, continuation) => match rc {
            ModifyRc::Inc(symbol, count) => {
                let inc_before = environment.incremented_symbols.contains(symbol);

                // Add a symbol for every increment performed.
                environment
                    .incremented_symbols
                    .insert_count(*symbol, *count);

                let new_continuation = specialize_drops_stmt(
                    arena,
                    layout_interner,
                    ident_ids,
                    environment,
                    continuation,
                );

                if inc_before {
                    // There were increments before this one, best to let the first one do the increments.
                    // Or there are no increments left, so we can just continue.
                    new_continuation
                } else {
                    match environment
                        .incremented_symbols
                        .map
                        .remove(symbol)
                        .unwrap_or(0)
                    {
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

                if environment.incremented_symbols.pop(symbol) {
                    // This decremented symbol was incremented before, so we can remove it.
                    specialize_drops_stmt(
                        arena,
                        layout_interner,
                        ident_ids,
                        environment,
                        continuation,
                    )
                } else {
                    // Collect all children (recursively) that were incremented and make sure that one increment remains in the environment afterwards.
                    // To prevent
                    // let a = index b; inc a; dec b; ...; dec a
                    // from being translated to
                    // let a = index b; dec b
                    // As a might get dropped as a result of the decrement of b.
                    let mut incremented_children = {
                        let mut todo_children = bumpalo::vec![in arena; *symbol];
                        let mut incremented_children = CountingMap::new();

                        while let Some(child) = todo_children.pop() {
                            if environment.incremented_symbols.pop(&child) {
                                incremented_children.insert(child);
                            } else {
                                todo_children.extend(environment.get_children(&child));
                            }
                        }

                        incremented_children
                    };

                    // This decremented symbol was not incremented before, perhaps the children were.
                    let in_layout = environment.get_symbol_layout(symbol);
                    let runtime_repr = layout_interner.runtime_representation(*in_layout);

                    let updated_stmt = match runtime_repr {
                        // Layout has children, try to inline them.
                        LayoutRepr::Struct(field_layouts) => specialize_struct(
                            arena,
                            layout_interner,
                            ident_ids,
                            environment,
                            symbol,
                            field_layouts,
                            &mut incremented_children,
                            continuation,
                        ),
                        LayoutRepr::Union(union_layout) => specialize_union(
                            arena,
                            layout_interner,
                            ident_ids,
                            environment,
                            symbol,
                            union_layout,
                            &mut incremented_children,
                            continuation,
                        ),
                        LayoutRepr::Builtin(Builtin::List(layout)) => specialize_list(
                            arena,
                            layout_interner,
                            ident_ids,
                            environment,
                            &mut incremented_children,
                            symbol,
                            layout,
                            continuation,
                        ),
                        // TODO: lambda sets should not be reachable, yet they are.
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
                    for (child_symbol, symbol_count) in incremented_children.map.into_iter() {
                        environment
                            .incremented_symbols
                            .insert_count(child_symbol, symbol_count)
                    }

                    updated_stmt
                }
            }
            ModifyRc::DecRef(_) | ModifyRc::Free(_) => {
                // These operations are not recursive (the children are not touched)
                // so inlining is not useful
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
            lookups,
            variables,
            remainder: specialize_drops_stmt(
                arena,
                layout_interner,
                ident_ids,
                environment,
                remainder,
            ),
        }),
        Stmt::Dbg {
            source_location,
            source,
            symbol,
            variable,
            remainder,
        } => arena.alloc(Stmt::Dbg {
            source_location,
            source,
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
            // We cannot perform this optimization if the joinpoint is recursive.
            // E.g. if the body of a recursive joinpoint contains an increment, we do not want to move that increment up to the remainder.

            let mut remainder_environment = environment.clone();

            let new_remainder = specialize_drops_stmt(
                arena,
                layout_interner,
                ident_ids,
                &mut remainder_environment,
                remainder,
            );

            let mut body_environment = environment.clone();
            for param in parameters.iter() {
                body_environment.add_symbol_layout(param.symbol, param.layout);
            }
            body_environment.incremented_symbols.clear();

            let new_body = specialize_drops_stmt(
                arena,
                layout_interner,
                ident_ids,
                &mut body_environment,
                body,
            );

            let remainder_jump_info = remainder_environment.jump_incremented_symbols.get(id);

            let body_jump_info = body_environment.jump_incremented_symbols.get(id);

            let (newer_body, newer_remainder) = match (remainder_jump_info, body_jump_info) {
                // We have info from the remainder, and the body is not recursive.
                // Meaning we can pass the incremented_symbols from the remainder to the body.
                (Some(jump_info), None) if !jump_info.is_empty() => {
                    // Update body with incremented symbols from remainder
                    body_environment.incremented_symbols = jump_info.clone();

                    let newer_body = specialize_drops_stmt(
                        arena,
                        layout_interner,
                        ident_ids,
                        &mut body_environment,
                        body,
                    );

                    // Update remainder
                    environment.join_incremented_symbols.insert(
                        *id,
                        JoinUsage {
                            join_consumes: jump_info.clone(),
                            join_returns: body_environment.incremented_symbols,
                        },
                    );
                    let newer_remainder = specialize_drops_stmt(
                        arena,
                        layout_interner,
                        ident_ids,
                        environment,
                        remainder,
                    );

                    (newer_body, newer_remainder)
                }
                _ => {
                    // Keep the body and remainder as is.
                    // Update the environment with remainder environment.

                    *environment = remainder_environment;

                    (new_body, new_remainder)
                }
            };

            arena.alloc(Stmt::Join {
                id: *id,
                parameters,
                body: newer_body,
                remainder: newer_remainder,
            })
        }
        Stmt::Jump(joinpoint_id, arguments) => {
            match environment.join_incremented_symbols.get(joinpoint_id) {
                Some(JoinUsage {
                    join_consumes,
                    join_returns,
                }) => {
                    // Consume all symbols that were consumed in the join.
                    for (symbol, count) in join_consumes.map.iter() {
                        for _ in 0..*count {
                            let popped = environment.incremented_symbols.pop(symbol);
                            debug_assert!(
                                popped,
                                "Every incremented symbol should be available from jumps"
                            );
                        }
                    }
                    for (symbol, count) in join_returns.map.iter() {
                        environment
                            .incremented_symbols
                            .insert_count(*symbol, *count);
                    }
                }
                None => {
                    // No join usage, let the join know the minimum amount of symbols that were incremented from each jump.
                    environment
                        .jump_incremented_symbols
                        .insert(*joinpoint_id, environment.incremented_symbols.clone());
                }
            }
            arena.alloc(Stmt::Jump(*joinpoint_id, arguments))
        }
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
    incremented_children: &mut CountingMap<Child>,
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
                    let removed = incremented_children.pop(child);
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
                                environment.create_symbol(ident_ids, &format!("field_val_{i}"));

                            let field_val_expr = Expr::StructAtIndex {
                                index: i as u64,
                                field_layouts: struct_layout,
                                structure: *symbol,
                            };

                            arena.alloc(Stmt::Let(
                                field_symbol,
                                field_val_expr,
                                layout_interner.chase_recursive_in(*field_layout),
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
    union_layout: UnionLayout<'a>,
    incremented_children: &mut CountingMap<Child>,
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
            match environment.union_children.get(&(*symbol, tag)) {
                None => keep_original_decrement!(),
                Some(children) => {
                    // TODO perhaps this allocation can be avoided.
                    let children_clone = children.clone();

                    // Map tracking which index of the struct is contained in which symbol.
                    // And whether the child no longer has to be decremented.
                    let mut index_symbols = MutMap::default();

                    for (index, _layout) in field_layouts.iter().enumerate() {
                        for (child, _i) in children_clone
                            .iter()
                            .rev()
                            .filter(|(_child, i)| *i == index as u64)
                        {
                            let removed = incremented_children.pop(child);
                            index_symbols.entry(index).or_insert((*child, removed));

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

                    type RCFun<'a> =
                        Option<fn(arena: &'a Bump, Symbol, &'a Stmt<'a>) -> &'a Stmt<'a>>;
                    let refcount_fields = |layout_interner: &mut STLayoutInterner<'a>,
                                           ident_ids: &mut IdentIds,
                                           rc_popped: RCFun<'a>,
                                           rc_unpopped: RCFun<'a>,
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
                                                &format!("field_val_{i}"),
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
                                                layout_interner.chase_recursive_in(*field_layout),
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
                                            // we know for sure that the allocation is unique at
                                            // this point. Therefore we can free (or maybe reuse)
                                            // without checking the refcount again.
                                            ModifyRc::Free(*symbol),
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

fn specialize_list<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i mut STLayoutInterner<'a>,
    ident_ids: &'i mut IdentIds,
    environment: &mut DropSpecializationEnvironment<'a>,
    _incremented_children: &mut CountingMap<Child>,
    symbol: &Symbol,
    _item_layout: InLayout<'a>,
    continuation: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    macro_rules! keep_original_decrement {
        () => {{
            let new_continuation =
                specialize_drops_stmt(arena, layout_interner, ident_ids, environment, continuation);
            arena.alloc(Stmt::Refcounting(ModifyRc::Dec(*symbol), new_continuation))
        }};
    }

    // TODO: Maybe re-enable drop specialization for lists.
    // It won't be as useful now, but it can still apply to lists that we know aren't seamless slices.
    // It also could technically apply to seamless slices if we know everything about their underlying allocation.
    // To fix this would require adding the restrictions on application above and properly implementing DecRef for the new list.
    // DecRef would be easy to implement, but currently the new list has Dec/DecRef both as the same.
    // Extra context starts here: https://roc.zulipchat.com/#narrow/stream/316715-contributing/topic/Implement.20RocRefcounted.20for.20RocResult/near/451651633
    // With most important message here: https://roc.zulipchat.com/#narrow/stream/316715-contributing/topic/Implement.20RocRefcounted.20for.20RocResult/near/451692266
    //
    // If we don't re-enable this, we should fully remove the list wiring and trackind at some point.
    //
    // Fundamentally, an allocation for a list holds onto exactly one reference to all elements in the allocation.
    // So even if there are 100 references to the list, the list will hold only 1 reference to each element.
    // On top of that, the list now holds onto a reference to dead elements.
    // A single slice to 10 elements of a 100 element list will hold 1 reference to each to the 100 elements.
    // The whole allocation will get freed in a unit.
    // This makes it much closer to the lifetime of the underlying vector for a reference slice in rust.
    // If this specialization does not make sense anymore delete it as a whole.
    keep_original_decrement!()

    // let current_length = environment.list_length.get(symbol).copied();
    // match (
    //     layout_interner.contains_refcounted(item_layout),
    //     current_length,
    // ) {
    //     // Only specialize lists if the amount of children is known.
    //     // Otherwise we might have to insert an unbouned number of decrements.
    //     (true, Some(length)) => {
    //         match environment.list_children.get(symbol) {
    //             Some(children) => {
    //                 // TODO perhaps this allocation can be avoided.
    //                 let children_clone = children.clone();

    //                 // Map tracking which index of the struct is contained in which symbol.
    //                 // And whether the child no longer has to be decremented.
    //                 let mut index_symbols = MutMap::default();

    //                 for index in 0..length {
    //                     for (child, i) in children_clone
    //                         .iter()
    //                         .rev()
    //                         .filter(|(_child, i)| *i == index)
    //                     {
    //                         debug_assert!(length > *i);

    //                         let removed = incremented_children.pop(child);
    //                         index_symbols.insert(index, (*child, removed));

    //                         if removed {
    //                             break;
    //                         }
    //                     }
    //                 }

    //                 let new_continuation = specialize_drops_stmt(
    //                     arena,
    //                     layout_interner,
    //                     ident_ids,
    //                     environment,
    //                     continuation,
    //                 );

    //                 let mut newer_continuation = arena.alloc(Stmt::Refcounting(
    //                     ModifyRc::DecRef(*symbol),
    //                     new_continuation,
    //                 ));

    //                 // Reversed to ensure that the generated code decrements the items in the correct order.
    //                 for i in (0..length).rev() {
    //                     match index_symbols.get(&i) {
    //                         // If the symbol is known, we can decrement it (if incremented before).
    //                         Some((s, popped)) => {
    //                             if !*popped {
    //                                 // Decrement the children that were not incremented before. And thus don't cancel out.
    //                                 newer_continuation = arena.alloc(Stmt::Refcounting(
    //                                     ModifyRc::Dec(*s),
    //                                     newer_continuation,
    //                                 ));
    //                             }

    //                             // Do nothing for the children that were incremented before, as the decrement will cancel out.
    //                         }
    //                         // If the symbol is unknown, we have to get the value from the list.
    //                         // Should only happen when list elements are discarded.
    //                         None => {
    //                             let field_symbol =
    //                                 environment.create_symbol(ident_ids, &format!("field_val_{i}"));

    //                             let index_symbol =
    //                                 environment.create_symbol(ident_ids, &format!("index_val_{i}"));

    //                             let dec = arena.alloc(Stmt::Refcounting(
    //                                 ModifyRc::Dec(field_symbol),
    //                                 newer_continuation,
    //                             ));

    //                             let index = arena.alloc(Stmt::Let(
    //                                 field_symbol,
    //                                 Expr::Call(Call {
    //                                     call_type: CallType::LowLevel {
    //                                         op: LowLevel::ListGetUnsafe,
    //                                         update_mode: UpdateModeId::BACKEND_DUMMY,
    //                                     },
    //                                     arguments: arena.alloc([*symbol, index_symbol]),
    //                                 }),
    //                                 item_layout,
    //                                 dec,
    //                             ));

    //                             newer_continuation = arena.alloc(Stmt::Let(
    //                                 index_symbol,
    //                                 Expr::Literal(Literal::Int(i128::to_ne_bytes(i as i128))),
    //                                 Layout::isize(layout_interner.target()),
    //                                 index,
    //                             ));
    //                         }
    //                     };
    //                 }

    //                 newer_continuation
    //             }
    //             _ => keep_original_decrement!(),
    //         }
    //     }
    //     _ => {
    //         // List length is unknown or the children are not reference counted, so we can't specialize.
    //         keep_original_decrement!()
    //     }
    // }
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
    unique: F1,
    not_unique: F2,
    continutation: &'a Stmt<'a>,
) -> &'a Stmt<'a>
where
    F1: FnOnce(&mut STLayoutInterner<'a>, &mut IdentIds, &'a Stmt<'a>) -> &'a Stmt<'a>,
    F2: FnOnce(&mut STLayoutInterner<'a>, &mut IdentIds, &'a Stmt<'a>) -> &'a Stmt<'a>,
{
    match continutation {
        // The continuation is a single stmt. So we can insert it inline and skip creating a joinpoint.
        Stmt::Ret(_) | Stmt::Jump(_, _) => {
            let u = unique(layout_interner, ident_ids, continutation);
            let n = not_unique(layout_interner, ident_ids, continutation);

            let switch = |unique_symbol| {
                arena.alloc(Stmt::Switch {
                    cond_symbol: unique_symbol,
                    cond_layout: Layout::BOOL,
                    branches: &*arena.alloc([(1, BranchInfo::None, u.clone())]),
                    default_branch: (BranchInfo::None, n),
                    ret_layout: environment.layout,
                })
            };

            unique_symbol(arena, ident_ids, environment, symbol, switch)
        }
        // We put the continuation in a joinpoint. To prevent duplicating the content.
        _ => {
            let join_id = JoinPointId(environment.create_symbol(ident_ids, "uniqueness_join"));

            let jump = arena.alloc(Stmt::Jump(join_id, arena.alloc([])));

            let u = unique(layout_interner, ident_ids, jump);
            let n = not_unique(layout_interner, ident_ids, jump);

            let switch = |unique_symbol| {
                arena.alloc(Stmt::Switch {
                    cond_symbol: unique_symbol,
                    cond_layout: Layout::BOOL,
                    branches: &*arena.alloc([(
                        1,
                        BranchInfo::Unique {
                            scrutinee: symbol,
                            unique: true,
                        },
                        u.clone(),
                    )]),
                    default_branch: (
                        BranchInfo::Unique {
                            scrutinee: symbol,
                            unique: false,
                        },
                        n,
                    ),
                    ret_layout: environment.layout,
                })
            };

            let unique = unique_symbol(arena, ident_ids, environment, symbol, switch);

            arena.alloc(Stmt::Join {
                id: join_id,
                parameters: arena.alloc([]),
                body: continutation,
                remainder: unique,
            })
        }
    }
}

fn unique_symbol<'a, 'i>(
    arena: &'a Bump,
    ident_ids: &'i mut IdentIds,
    environment: &DropSpecializationEnvironment<'a>,
    symbol: Symbol,
    continuation: impl FnOnce(Symbol) -> &'a mut Stmt<'a>,
) -> &'a Stmt<'a> {
    let is_unique = environment.create_symbol(ident_ids, "is_unique");

    arena.alloc(Stmt::Let(
        is_unique,
        Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::RefCountIsUnique,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: arena.alloc([symbol]),
        }),
        Layout::BOOL,
        continuation(is_unique),
    ))
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

    symbol_layouts: MutMap<Symbol, InLayout<'a>>,

    // Keeps track of which parent symbol is indexed by which child symbol for structs
    struct_children: MutMap<Parent, Vec<'a, (Child, Index)>>,

    // Keeps track of which parent symbol is indexed by which child symbol for unions
    union_children: MutMap<(Parent, Tag), Vec<'a, (Child, Index)>>,

    // Keeps track of which parent symbol is indexed by which child symbol for boxes
    box_children: MutMap<Parent, Vec<'a, Child>>,

    // Keeps track of which parent symbol is indexed by which child symbol for lists
    list_children: MutMap<Parent, Vec<'a, (Child, Index)>>,

    // Keeps track of all incremented symbols.
    incremented_symbols: CountingMap<Symbol>,

    // Map containing the current known tag of a layout.
    symbol_tag: MutMap<Symbol, Tag>,

    // Map containing the current known index value of a symbol.
    symbol_index: MutMap<Symbol, Index>,

    // Map containing the current known length of a list.
    list_length: MutMap<Symbol, u64>,

    // A map containing the minimum number of symbol increments from jumps for a joinpoint.
    jump_incremented_symbols: MutMap<JoinPointId, CountingMap<Symbol>>,

    // A map containing the expected number of symbol increments from joinpoints for a jump.
    join_incremented_symbols: MutMap<JoinPointId, JoinUsage>,
}

#[derive(Clone)]
struct JoinUsage {
    join_consumes: CountingMap<Symbol>,
    join_returns: CountingMap<Symbol>,
}

impl<'a> DropSpecializationEnvironment<'a> {
    fn new(arena: &'a Bump, home: ModuleId, layout: InLayout<'a>) -> Self {
        Self {
            arena,
            home,
            layout,
            symbol_layouts: MutMap::default(),
            struct_children: MutMap::default(),
            union_children: MutMap::default(),
            box_children: MutMap::default(),
            list_children: MutMap::default(),
            incremented_symbols: CountingMap::new(),
            symbol_tag: MutMap::default(),
            symbol_index: MutMap::default(),
            list_length: MutMap::default(),
            jump_incremented_symbols: MutMap::default(),
            join_incremented_symbols: MutMap::default(),
        }
    }

    fn create_symbol(&self, ident_ids: &mut IdentIds, debug_name: &str) -> Symbol {
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
            .or_insert_with(|| Vec::new_in(self.arena))
            .push((child, index));
    }

    fn add_union_child(&mut self, parent: Parent, child: Child, tag: u16, index: Index) {
        self.union_children
            .entry((parent, tag))
            .or_insert_with(|| Vec::new_in(self.arena))
            .push((child, index));
    }

    fn add_list_child(&mut self, parent: Parent, child: Child, index: u64) {
        self.list_children
            .entry(parent)
            .or_insert_with(|| Vec::new_in(self.arena))
            .push((child, index));
    }

    fn add_list_child_symbol(&mut self, parent: Parent, child: Child, index: &Symbol) {
        if let Some(index) = self.symbol_index.get(index) {
            self.add_list_child(parent, child, *index)
        }
    }

    fn get_children(&self, parent: &Parent) -> Vec<'a, Symbol> {
        let mut res = Vec::new_in(self.arena);

        if let Some(children) = self.struct_children.get(parent) {
            res.extend(children.iter().rev().map(|(child, _)| child));
        }

        let children = self
            .union_children
            .iter()
            .filter(|(k, _v)| k.0 == *parent)
            .flat_map(|(_k, v)| v.iter().rev());
        res.extend(children.map(|(child, _)| child));

        if let Some(children) = self.box_children.get(parent) {
            res.extend(children.iter().rev());
        }

        if let Some(children) = self.list_children.get(parent) {
            res.extend(children.iter().rev().map(|(child, _)| child));
        }

        res
    }
}

/**
Reference count information
*/
enum RC {
    // Rc is important, moving an increment to after this function might break the program.
    // E.g. if the function checks for uniqueness and behaves differently based on that.
    Rc,
    // Rc is not important, moving an increment to after this function should have no effect.
    NoRc,
    // Rc effect is unknown.
    Uknown,
}

/*
Returns whether the reference count of arguments to this function is relevant to the program.
 */
fn low_level_no_rc(lowlevel: &LowLevel) -> RC {
    use LowLevel::*;

    match lowlevel {
        Unreachable => RC::Uknown,
        ListLenU64 | ListLenUsize | StrIsEmpty | StrCountUtf8Bytes | ListGetCapacity
        | ListWithCapacity | StrWithCapacity => RC::NoRc,
        ListReplaceUnsafe => RC::Rc,
        StrGetUnsafe | ListGetUnsafe => RC::NoRc,
        ListConcat => RC::Rc,
        StrConcat => RC::Rc,
        ListConcatUtf8 => RC::Rc,
        StrSubstringUnsafe => RC::Rc,
        StrReserve => RC::Rc,
        StrTrim => RC::Rc,
        StrTrimStart => RC::Rc,
        StrTrimEnd => RC::Rc,
        StrSplitOn => RC::NoRc,
        StrToNum => RC::NoRc,
        ListPrepend => RC::Rc,
        StrJoinWith => RC::NoRc,
        ListSortWith => RC::Rc,

        ListAppendUnsafe
        | ListReserve
        | ListSublist
        | ListDropAt
        | ListSwap
        | ListReleaseExcessCapacity
        | StrReleaseExcessCapacity
        | ListIncref
        | ListDecref => RC::Rc,

        Eq | NotEq => RC::NoRc,

        And | Or | NumAdd | NumAddWrap | NumAddChecked | NumAddSaturated | NumSub | NumSubWrap
        | NumSubChecked | NumSubSaturated | NumMul | NumMulWrap | NumMulSaturated
        | NumMulChecked | NumGt | NumGte | NumLt | NumLte | NumCompare | NumDivFrac
        | NumDivTruncUnchecked | NumDivCeilUnchecked | NumRemUnchecked | NumIsMultipleOf
        | NumPow | NumPowInt | NumBitwiseAnd | NumBitwiseXor | NumBitwiseOr | NumShiftLeftBy
        | NumShiftRightBy | NumShiftRightZfBy => RC::NoRc,

        NumToStr
        | NumAbs
        | NumNeg
        | NumSin
        | NumCos
        | NumTan
        | NumSqrtUnchecked
        | NumLogUnchecked
        | NumRound
        | NumCeiling
        | NumFloor
        | NumToFrac
        | Not
        | NumIsNan
        | NumIsInfinite
        | NumIsFinite
        | NumAtan
        | NumAcos
        | NumAsin
        | NumIntCast
        | NumToIntChecked
        | NumToFloatCast
        | NumToFloatChecked
        | NumCountLeadingZeroBits
        | NumCountTrailingZeroBits
        | NumCountOneBits
        | NumF32ToParts
        | NumF64ToParts
        | NumF32FromParts
        | NumF64FromParts => RC::NoRc,
        NumWithoutDecimalPoint | NumWithDecimalPoint => RC::NoRc,
        DictPseudoSeed => RC::NoRc,
        StrStartsWith | StrEndsWith => RC::NoRc,
        StrFromUtf8 => RC::Rc,
        StrToUtf8 => RC::Rc,
        StrRepeat => RC::NoRc,
        StrFromInt | StrFromFloat => RC::NoRc,
        Hash => RC::NoRc,

        ListIsUnique => RC::Rc,
        ListClone => RC::Rc,

        BoxExpr | UnboxExpr => {
            unreachable!("These lowlevel operations are turned into mono Expr's")
        }

        // only inserted for internal purposes. RC should not touch it
        PtrStore => RC::NoRc,
        PtrLoad => RC::NoRc,
        PtrCast => RC::NoRc,

        PtrClearTagId | RefCountIncRcPtr | RefCountDecRcPtr | RefCountIncDataPtr
        | RefCountDecDataPtr | RefCountIsUnique => {
            unreachable!("Only inserted *after* borrow checking: {:?}", lowlevel);
        }

        SetJmp | LongJmp | SetLongJmpBuffer => unreachable!("only inserted in dev backend codegen"),
    }
}

/// Map that contains a count for each key.
/// Keys with a count of 0 are kept around, so that it can be seen that they were once present.
#[derive(Clone, PartialEq, Eq)]
struct CountingMap<K: std::cmp::Eq + std::hash::Hash> {
    map: MutMap<K, u64>,
}

impl<K> CountingMap<K>
where
    K: Eq + std::hash::Hash + Clone,
{
    fn new() -> Self {
        Self {
            map: MutMap::default(),
        }
    }

    fn insert(&mut self, key: K) {
        self.insert_count(key, 1);
    }

    fn insert_count(&mut self, key: K, count: u64) {
        self.map
            .entry(key)
            .and_modify(|c| *c += count)
            .or_insert(count);
    }

    fn pop(&mut self, key: &K) -> bool {
        match self.map.get_mut(key) {
            Some(0) => false,
            Some(c) => {
                *c -= 1;
                true
            }
            None => false,
        }
    }

    fn contains(&self, symbol: &K) -> bool {
        self.map.contains_key(symbol)
    }

    fn drain(&mut self) -> Self {
        let res = self.clone();
        for (_, v) in self.map.iter_mut() {
            *v = 0;
        }
        res
    }

    fn clear(&mut self) {
        self.map.clear()
    }

    fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}
