// This program was written by Jelle Teeuwissen within a final
// thesis project of the Computing Science master program at Utrecht
// University under supervision of Wouter Swierstra (w.s.swierstra@uu.nl).

// Implementation based of Drop Specialization from Perceus: Garbage Free Reference Counting with Reuse
// https://www.microsoft.com/en-us/research/uploads/prod/2021/06/perceus-pldi21.pdf

#![allow(clippy::too_many_arguments)]

use std::iter::{self, Iterator};

use bumpalo::collections::vec::Vec;
use bumpalo::collections::CollectIn;

use roc_module::symbol::{IdentIds, ModuleId, Symbol};

use crate::ir::{Expr, ModifyRc, Proc, ProcLayout, Stmt, UpdateModeIds};
use crate::layout::{InLayout, Layout, LayoutInterner, STLayoutInterner};

use bumpalo::Bump;

use roc_collections::{MutMap, MutSet};

/**
Try to find increments of symbols followed by decrements of the symbol they were indexed out of (their parent).
Then inline the decrement operation of the parent and removing matching pairs of increments and decrements.
*/
pub fn specialize_drops<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i STLayoutInterner<'a>,
    home: ModuleId,
    ident_ids: &'i mut IdentIds,
    update_mode_ids: &'i mut UpdateModeIds,
    procs: &mut MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) {
    let environment = DropSpecializationEnvironment::new(arena, home);

    for ((_symbol, _layout), proc) in procs.iter_mut() {
        // Clone the symbol_rc_types_env and insert the symbol
        specialize_drops_proc(
            arena,
            layout_interner,
            ident_ids,
            &mut environment.clone(),
            proc,
        );
    }
}

fn specialize_drops_proc<'a, 'i>(
    arena: &'a Bump,
    layout_interner: &'i STLayoutInterner<'a>,
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
    layout_interner: &'i STLayoutInterner<'a>,
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
                Expr::Call(_) | Expr::Tag { .. } | Expr::Struct(_) => {
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
                Expr::StructAtIndex {
                    index,
                    field_layouts,
                    structure,
                } => {
                    environment.add_parent_child(*structure, *binding, *index);
                    // alloc_let_with_continuation!(environment)

                    // TODO do we need to remove the indexed value to prevent it from being dropped sooner?
                    // It will only be dropped sooner if the reference count is 1. Which can only happen if there is no increment before.
                    // So we should be fine.
                    alloc_let_with_continuation!(environment)
                }
                Expr::UnionAtIndex {
                    structure,
                    tag_id,
                    union_layout,
                    index,
                } => {
                    // TODO perhaps we need the union_layout later as well? if so, create a new function/map to store it.
                    environment.add_parent_child(*structure, *binding, *index);
                    alloc_let_with_continuation!(environment)
                }
                Expr::Reuse { .. } => {
                    alloc_let_with_continuation!(environment)
                }
                Expr::Reset {
                    symbol,
                    update_mode,
                } => {
                    // TODO allow to inline this to replace it with resetref
                    alloc_let_with_continuation!(environment)
                }
                Expr::ResetRef {
                    symbol,
                    update_mode,
                } => {
                    alloc_let_with_continuation!(environment)
                }
                Expr::RuntimeErrorFunction(_)
                | Expr::ExprBox { .. }
                | Expr::ExprUnbox { .. }
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
                let any = environment.any_incremented(*symbol);

                // Add a symbol for every increment performed.
                environment.add_incremented(*symbol, *count);

                let new_continuation = specialize_drops_stmt(
                    arena,
                    layout_interner,
                    ident_ids,
                    environment,
                    continuation,
                );

                let new_count = environment.get_incremented(*symbol);

                if any || new_count == 0 {
                    // There were increments before this one, best to let the first one do the increments.
                    // Or there are no increments left, so we can just continue.
                    new_continuation
                } else {
                    // This was the first increment, so we will do all increments here.
                    arena.alloc(Stmt::Refcounting(
                        ModifyRc::Inc(*symbol, new_count),
                        new_continuation,
                    ))
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

                if environment.pop_incremented(*symbol) {
                    // This decremented symbol was incremented before, so we can remove it.

                    specialize_drops_stmt(
                        arena,
                        layout_interner,
                        ident_ids,
                        environment,
                        continuation,
                    )
                } else {
                    // This decremented symbol was not incremented before, perhaps the children were.

                    let new_continuation = specialize_drops_stmt(
                        arena,
                        layout_interner,
                        ident_ids,
                        environment,
                        continuation,
                    );

                    let in_layout = environment.get_symbol_layout(symbol);
                    let layout = layout_interner.get(*in_layout);

                    match layout {
                        // Layout has children, try to inline them.
                        Layout::Struct { field_layouts, .. } => {
                            match environment.get_children(*symbol) {
                                Some(children) => {
                                    // For every struct index a symbol.
                                    let mut index_symbols = MutMap::default();
                                    // For every struct index a symbol.
                                    let mut popped_symbols = MutSet::default();

                                    for (index, layout) in field_layouts.iter().enumerate() {
                                        for (child, i) in
                                            children.iter().filter(|(_, i)| *i == index as u64)
                                        {
                                            let symbol_popped = environment.pop_incremented(*child);
                                            if symbol_popped {
                                                // Incremented before, we can remove the decrement.
                                                index_symbols.insert(index, *child);
                                                popped_symbols.insert(*child);
                                                break;
                                            } else {
                                                // Not incremented before, but we might use it to prevent indexing for drop.
                                                index_symbols.insert(index, *child);
                                            }
                                        }
                                    }

                                    let mut stmt = new_continuation;

                                    // Make sure every field is decremented.
                                    for (i, field_layout) in field_layouts.iter().enumerate() {
                                        // Only insert decrements for fields that are/contain refcounted values.
                                        if layout_interner.contains_refcounted(*field_layout) {
                                            stmt = match index_symbols.get(&i) {
                                                // This value has been indexed before, use that symbol.
                                                Some(s) => {
                                                    if popped_symbols.contains(s) {
                                                        // This symbol was popped, so we can skip the decrement.
                                                        stmt
                                                    } else {
                                                        // This symbol was indexed but not decremented, so we will decrement it.
                                                        arena.alloc(Stmt::Refcounting(
                                                            ModifyRc::Dec(*s),
                                                            stmt,
                                                        ))
                                                    }
                                                }

                                                // This value has not been index before, create a new symbol.
                                                None => {
                                                    let field_symbol = environment.create_symbol(
                                                        ident_ids,
                                                        &format!("field_val_{}", i),
                                                    );

                                                    let field_val_expr = Expr::StructAtIndex {
                                                        index: i as u64,
                                                        field_layouts: field_layouts,
                                                        structure: *symbol,
                                                    };
                                                    arena.alloc(Stmt::Let(
                                                        field_symbol,
                                                        field_val_expr,
                                                        *field_layout,
                                                        stmt,
                                                    ))
                                                }
                                            };
                                        }
                                    }

                                    stmt
                                }
                                None => {
                                    // No known children, keep decrementing the symbol.
                                    arena.alloc(Stmt::Refcounting(
                                        ModifyRc::Dec(*symbol),
                                        new_continuation,
                                    ))
                                }
                            }
                        }
                        // TODO: Implement this with uniqueness checks.
                        // Layout::Union(_) => {
                        //     todo!()
                        // }
                        _ => {
                            // No children, keep decrementing the symbol.
                            arena.alloc(Stmt::Refcounting(ModifyRc::Dec(*symbol), new_continuation))
                        }
                    }
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

type Index = u64;

type Parent = Symbol;

type Child = Symbol;

#[derive(Clone)]
struct DropSpecializationEnvironment<'a> {
    arena: &'a Bump,
    home: ModuleId,

    symbol_layouts: MutMap<Symbol, InLayout<'a>>,

    // Keeps track of which parent symbol is indexed by which child symbol.
    parent_children: MutMap<Parent, Vec<'a, (Child, Index)>>,

    // Keeps track of all incremented symbols.
    incremented_symbols: Vec<'a, Symbol>,
}

impl<'a> DropSpecializationEnvironment<'a> {
    fn new(arena: &'a Bump, home: ModuleId) -> Self {
        Self {
            arena,
            home,
            symbol_layouts: MutMap::default(),
            parent_children: MutMap::default(),
            incremented_symbols: Vec::new_in(arena),
        }
    }

    fn clone_without_incremented(&self) -> Self {
        Self {
            arena: self.arena,
            home: self.home,
            symbol_layouts: self.symbol_layouts.clone(),
            parent_children: self.parent_children.clone(),
            incremented_symbols: Vec::new_in(self.arena),
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

    fn add_parent_child(&mut self, parent: Symbol, child: Symbol, index: Index) {
        self.parent_children
            .entry(parent)
            .or_insert(Vec::new_in(self.arena))
            .push((child, index));
    }

    fn get_children(&self, parent: Symbol) -> Option<Vec<'a, (Child, Index)>> {
        self.parent_children.get(&parent).cloned()
    }

    /**
    Add a symbol for every increment performed.
     */
    fn add_incremented(&mut self, symbol: Symbol, count: u64) {
        self.incremented_symbols
            .extend(iter::repeat(symbol).take(count.try_into().unwrap()));
    }

    fn any_incremented(&self, symbol: Symbol) -> bool {
        self.incremented_symbols.contains(&symbol)
    }

    /**
    Return the amount of times a symbol still has to be incremented.
    Accounting for later consumtion and removal of the increment.
    */
    fn get_incremented(&mut self, symbol: Symbol) -> u64 {
        self.incremented_symbols
            .drain_filter(|s| *s == symbol)
            .count()
            .try_into()
            .unwrap()
    }

    fn pop_incremented(&mut self, symbol: Symbol) -> bool {
        match self
            .incremented_symbols
            .iter()
            .copied()
            .enumerate()
            .find_map(|(index, s)| (s == symbol).then_some(index))
        {
            Some(index) => {
                self.incremented_symbols.remove(index);
                true
            }
            None => false,
        }
    }

    // TODO assert that a parent is only inlined once / assert max single dec per parent.
}

/**
Free the memory of a symbol
*/
fn free<'a>(arena: &'a Bump, symbol: Symbol, continuation: &'a Stmt<'a>) -> &'a Stmt<'a> {
    // Currently using decref, but this checks the uniqueness of the symbol.
    // This function should only be called if it is known to be unique.
    // So instead this can be replaced with a free instruction that does not check the uniqueness.
    arena.alloc(Stmt::Refcounting(ModifyRc::DecRef(symbol), continuation))
}

// fn branch_drop_unique<'a>(
//     arena: &'a Bump,
//     dropped: Symbol,
//     continuation: &'a Stmt<'a>,
// ) -> &'a Stmt<'a> {
//     /**
//      * if unique xs
//      *    then drop x; drop xs; free xs
//      *    else decref xs
//      */
//     let unique_symbol = todo!("unique_symbol");

//     let joinpoint_id = todo!("joinpoint_id");
//     let jump = arena.alloc(Stmt::Jump(
//         joinpoint_id,
//         Vec::with_capacity_in(0, arena).into_bump_slice(),
//     ));

//     let unique_branch = arena.alloc(free(arena, dropped, jump));
//     let non_unique_branch = arena.alloc(Stmt::Refcounting(ModifyRc::DecRef(dropped), jump));

//     let branching = arena.alloc(Stmt::Switch {
//         cond_symbol: unique_symbol,
//         cond_layout: Layout::BOOL,
//         branches: todo!(),
//         default_branch: todo!(),
//         ret_layout: todo!(),
//     });

//     let condition = arena.alloc(Stmt::Let(
//         unique_symbol,
//         Expr::Call(Call {
//             call_type: CallType::LowLevel {
//                 op: Eq,
//                 update_mode: UpdateModeId::BACKEND_DUMMY,
//             },
//             arguments: arena.alloc_slice_copy(arguments),
//         }),
//         Layout::BOOL,
//         branching,
//     ));

//     arena.alloc(Stmt::Join {
//         id: joinpoint_id,
//         parameters: Vec::with_capacity_in(0, arena).into_bump_slice(),
//         body: continuation,
//         remainder: condition,
//     })
// }
// fn branch_drop_reuse_unique<'a>(arena: &'a Bump) {}

// TODO Figure out when unionindexes are inserted (just after a pattern match?)
// TODO Always index out all children (perhaps move all dup to after all indexing)
// TODO Remove duplicate indexes
// TODO Lowlevel is unqiue check
// TODO joinpoint split on isunique
// TODO insert decref, reuse token reference (raw pointer), free (decref until then.).
