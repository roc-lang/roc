use bumpalo::collections::vec::Vec;
use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, Symbol};

use crate::ir::{
    BranchInfo, Call, CallType, Expr, JoinPointId, Literal, Param, Stmt, UpdateModeId,
};
use crate::layout::{
    InLayout, Layout, LayoutInterner, LayoutRepr, STLayoutInterner, TagIdIntType, UnionLayout,
};

use super::{let_lowlevel, CodeGenHelp, Context, LAYOUT_BOOL};

const ARG_1: Symbol = Symbol::ARG_1;
const ARG_2: Symbol = Symbol::ARG_2;

pub fn eq_generic<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    layout: InLayout<'a>,
) -> Stmt<'a> {
    use crate::layout::Builtin::*;
    use LayoutRepr::*;
    let main_body = match layout_interner.get_repr(layout) {
        Builtin(Int(_) | Float(_) | Bool | Decimal) | FunctionPointer(_) => {
            unreachable!(
                "No generated proc for `==`. Use direct code gen for {:?}",
                layout
            )
        }
        Builtin(Str) => {
            unreachable!("No generated helper proc for `==` on Str. Use Zig function.")
        }
        Builtin(List(elem_layout)) => eq_list(root, ident_ids, ctx, layout_interner, elem_layout),
        Struct(field_layouts) => eq_struct(root, ident_ids, ctx, layout_interner, field_layouts),
        Union(union_layout) => eq_tag_union(root, ident_ids, ctx, layout_interner, union_layout),
        Ptr(inner_layout) => eq_boxed(root, ident_ids, ctx, layout_interner, inner_layout),
        LambdaSet(_) => unreachable!("`==` is not defined on functions"),
        Erased(_) => unreachable!("`==` is not defined on erased types"),
        RecursivePointer(_) => {
            unreachable!(
                "Can't perform `==` on RecursivePointer. Should have been replaced by a tag union."
            )
        }
    };

    Stmt::Let(
        Symbol::BOOL_TRUE,
        Expr::Literal(Literal::Bool(true)),
        LAYOUT_BOOL,
        root.arena.alloc(Stmt::Let(
            Symbol::BOOL_FALSE,
            Expr::Literal(Literal::Bool(false)),
            LAYOUT_BOOL,
            root.arena.alloc(main_body),
        )),
    )
}

fn if_pointers_equal_return_true<'a>(
    root: &CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    operands: [Symbol; 2],
    following: &'a Stmt<'a>,
) -> Stmt<'a> {
    let ptr1_addr = root.create_symbol(ident_ids, "addr1");
    let ptr2_addr = root.create_symbol(ident_ids, "addr2");
    let ptr_eq = root.create_symbol(ident_ids, "eq_addr");

    Stmt::Let(
        ptr1_addr,
        Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::PtrCast,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: root.arena.alloc([operands[0]]),
        }),
        root.layout_isize,
        root.arena.alloc(Stmt::Let(
            ptr2_addr,
            Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op: LowLevel::PtrCast,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: root.arena.alloc([operands[1]]),
            }),
            root.layout_isize,
            root.arena.alloc(Stmt::Let(
                ptr_eq,
                Expr::Call(Call {
                    call_type: CallType::LowLevel {
                        op: LowLevel::Eq,
                        update_mode: UpdateModeId::BACKEND_DUMMY,
                    },
                    arguments: root.arena.alloc([ptr1_addr, ptr2_addr]),
                }),
                LAYOUT_BOOL,
                root.arena.alloc(Stmt::Switch {
                    cond_symbol: ptr_eq,
                    cond_layout: LAYOUT_BOOL,
                    branches: root.arena.alloc([(
                        1,
                        BranchInfo::None,
                        Stmt::Ret(Symbol::BOOL_TRUE),
                    )]),
                    default_branch: (BranchInfo::None, following),
                    ret_layout: LAYOUT_BOOL,
                }),
            )),
        )),
    )
}

fn if_false_return_false<'a>(
    root: &CodeGenHelp<'a>,
    symbol: Symbol,
    following: Stmt<'a>,
) -> Stmt<'a> {
    Stmt::if_then_else(
        root.arena,
        symbol,
        Layout::BOOL,
        following,
        root.arena.alloc(Stmt::Ret(Symbol::BOOL_FALSE)),
    )
}

fn eq_struct<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    field_layouts: &'a [InLayout<'a>],
) -> Stmt<'a> {
    let mut else_stmt = Stmt::Ret(Symbol::BOOL_TRUE);
    for (i, layout) in field_layouts.iter().enumerate().rev() {
        let field1_sym = root.create_symbol(ident_ids, &format!("field_1_{i}"));
        let field1_expr = Expr::StructAtIndex {
            index: i as u64,
            field_layouts,
            structure: ARG_1,
        };
        let field1_stmt = |next| Stmt::Let(field1_sym, field1_expr, *layout, next);

        let field2_sym = root.create_symbol(ident_ids, &format!("field_2_{i}"));
        let field2_expr = Expr::StructAtIndex {
            index: i as u64,
            field_layouts,
            structure: ARG_2,
        };
        let field2_stmt = |next| Stmt::Let(field2_sym, field2_expr, *layout, next);

        let eq_call_expr = root
            .call_specialized_op(
                ident_ids,
                ctx,
                layout_interner,
                *layout,
                root.arena.alloc([field1_sym, field2_sym]),
            )
            .unwrap();

        let eq_call_name = format!("eq_call_{i}");
        let eq_call_sym = root.create_symbol(ident_ids, &eq_call_name);
        let eq_call_stmt = |next| Stmt::Let(eq_call_sym, eq_call_expr, LAYOUT_BOOL, next);

        else_stmt = field1_stmt(root.arena.alloc(
            //
            field2_stmt(root.arena.alloc(
                //
                eq_call_stmt(root.arena.alloc(
                    //
                    if_false_return_false(root, eq_call_sym, else_stmt),
                )),
            )),
        ))
    }

    else_stmt
}

fn eq_tag_union<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    union_layout: UnionLayout<'a>,
) -> Stmt<'a> {
    use UnionLayout::*;

    let parent_rec_ptr_layout = ctx.recursive_union;
    if !matches!(union_layout, NonRecursive(_)) {
        ctx.recursive_union = Some(union_layout);
    }

    let body = match union_layout {
        NonRecursive(&[]) => {
            // cannot be reached at runtime, but we need to generate valid code
            Stmt::Ret(Symbol::BOOL_TRUE)
        }
        NonRecursive(tags) => eq_tag_union_help(
            root,
            ident_ids,
            ctx,
            layout_interner,
            union_layout,
            tags,
            NullableId::None,
        ),

        Recursive(tags) => eq_tag_union_help(
            root,
            ident_ids,
            ctx,
            layout_interner,
            union_layout,
            tags,
            NullableId::None,
        ),

        NonNullableUnwrapped(field_layouts) => {
            let tags = root.arena.alloc([field_layouts]);
            eq_tag_union_help(
                root,
                ident_ids,
                ctx,
                layout_interner,
                union_layout,
                tags,
                NullableId::None,
            )
        }

        NullableWrapped {
            other_tags,
            nullable_id,
        } => eq_tag_union_help(
            root,
            ident_ids,
            ctx,
            layout_interner,
            union_layout,
            other_tags,
            NullableId::Wrapped(nullable_id),
        ),

        NullableUnwrapped {
            other_fields,
            nullable_id,
        } => eq_tag_union_help(
            root,
            ident_ids,
            ctx,
            layout_interner,
            union_layout,
            root.arena.alloc([other_fields]),
            NullableId::Unwrapped(nullable_id),
        ),
    };

    ctx.recursive_union = parent_rec_ptr_layout;

    body
}

enum NullableId {
    None,
    Wrapped(TagIdIntType),
    Unwrapped(bool),
}

fn eq_tag_union_help<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    union_layout: UnionLayout<'a>,
    tag_layouts: &'a [&'a [InLayout<'a>]],
    nullable_id: NullableId,
) -> Stmt<'a> {
    let tailrec_loop = JoinPointId(root.create_symbol(ident_ids, "tailrec_loop"));
    let is_non_recursive = matches!(union_layout, UnionLayout::NonRecursive(_));
    let operands = if is_non_recursive {
        [ARG_1, ARG_2]
    } else {
        [
            root.create_symbol(ident_ids, "a"),
            root.create_symbol(ident_ids, "b"),
        ]
    };

    let tag_id_layout = union_layout.tag_id_layout();

    let tag_id_a = root.create_symbol(ident_ids, "tag_id_a");
    let tag_id_a_stmt = |next| {
        Stmt::Let(
            tag_id_a,
            Expr::GetTagId {
                structure: operands[0],
                union_layout,
            },
            tag_id_layout,
            next,
        )
    };

    let tag_id_b = root.create_symbol(ident_ids, "tag_id_b");
    let tag_id_b_stmt = |next| {
        Stmt::Let(
            tag_id_b,
            Expr::GetTagId {
                structure: operands[1],
                union_layout,
            },
            tag_id_layout,
            next,
        )
    };

    let tag_ids_eq = root.create_symbol(ident_ids, "tag_ids_eq");
    let tag_ids_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::Eq,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: root.arena.alloc([tag_id_a, tag_id_b]),
    });
    let tag_ids_eq_stmt = |next| Stmt::Let(tag_ids_eq, tag_ids_expr, LAYOUT_BOOL, next);

    let if_equal_ids_branches =
        root.arena
            .alloc([(0, BranchInfo::None, Stmt::Ret(Symbol::BOOL_FALSE))]);

    //
    // Switch statement by tag ID
    //

    let mut tag_branches = Vec::with_capacity_in(tag_layouts.len(), root.arena);

    // If there's a null tag, check it first. We might not need to load any data from memory.
    match nullable_id {
        NullableId::Wrapped(id) => {
            tag_branches.push((id as u64, BranchInfo::None, Stmt::Ret(Symbol::BOOL_TRUE)))
        }
        NullableId::Unwrapped(id) => tag_branches.push((
            id as TagIdIntType as u64,
            BranchInfo::None,
            Stmt::Ret(Symbol::BOOL_TRUE),
        )),
        _ => (),
    }

    let default_tag = if let NullableId::Unwrapped(tag_id) = nullable_id {
        (!tag_id) as TagIdIntType
    } else {
        let mut tag_id: TagIdIntType = 0;

        for field_layouts in tag_layouts.iter().take(tag_layouts.len() - 1) {
            if let NullableId::Wrapped(null_id) = nullable_id {
                if tag_id == null_id as TagIdIntType {
                    tag_id += 1;
                }
            }

            let tag_stmt = eq_tag_fields(
                root,
                ident_ids,
                ctx,
                layout_interner,
                tailrec_loop,
                union_layout,
                field_layouts,
                operands,
                tag_id,
            );
            tag_branches.push((tag_id as u64, BranchInfo::None, tag_stmt));

            tag_id += 1;
        }

        tag_id
    };

    let tag_switch_stmt = Stmt::Switch {
        cond_symbol: tag_id_a,
        cond_layout: tag_id_layout,
        branches: tag_branches.into_bump_slice(),
        default_branch: (
            BranchInfo::None,
            root.arena.alloc(eq_tag_fields(
                root,
                ident_ids,
                ctx,
                layout_interner,
                tailrec_loop,
                union_layout,
                tag_layouts.last().unwrap(),
                operands,
                default_tag,
            )),
        ),
        ret_layout: LAYOUT_BOOL,
    };

    let if_equal_ids_stmt = Stmt::Switch {
        cond_symbol: tag_ids_eq,
        cond_layout: LAYOUT_BOOL,
        branches: if_equal_ids_branches,
        default_branch: (BranchInfo::None, root.arena.alloc(tag_switch_stmt)),
        ret_layout: LAYOUT_BOOL,
    };

    //
    // combine all the statements
    //
    let compare_values = tag_id_a_stmt(root.arena.alloc(
        //
        tag_id_b_stmt(root.arena.alloc(
            //
            tag_ids_eq_stmt(root.arena.alloc(
                //
                if_equal_ids_stmt,
            )),
        )),
    ));

    if is_non_recursive {
        compare_values
    } else {
        let compare_ptr_or_value = if_pointers_equal_return_true(
            root,
            ident_ids,
            operands,
            root.arena.alloc(compare_values),
        );

        let union_layout =
            layout_interner.insert_direct_no_semantic(LayoutRepr::Union(union_layout));

        let loop_params_iter = operands.iter().map(|arg| Param {
            symbol: *arg,
            layout: union_layout,
        });

        let loop_start = Stmt::Jump(tailrec_loop, root.arena.alloc([ARG_1, ARG_2]));

        Stmt::Join {
            id: tailrec_loop,
            parameters: root.arena.alloc_slice_fill_iter(loop_params_iter),
            body: root.arena.alloc(compare_ptr_or_value),
            remainder: root.arena.alloc(loop_start),
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn eq_tag_fields<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    tailrec_loop: JoinPointId,
    union_layout: UnionLayout<'a>,
    field_layouts: &'a [InLayout<'a>],
    operands: [Symbol; 2],
    tag_id: TagIdIntType,
) -> Stmt<'a> {
    // Find a RecursivePointer to use in the tail recursion loop
    // (If there are more than one, the others will use non-tail recursion)
    let rec_ptr_index = field_layouts.iter().position(|field| {
        matches!(
            layout_interner.get_repr(*field),
            LayoutRepr::RecursivePointer(_)
        )
    });

    let (tailrec_index, innermost_stmt) = match rec_ptr_index {
        None => {
            // This tag has no RecursivePointers. Set tailrec_index out of range.
            (field_layouts.len(), Stmt::Ret(Symbol::BOOL_TRUE))
        }

        Some(i) => {
            // Implement tail recursion on this RecursivePointer,
            // in the innermost `else` clause after all other fields have been checked
            let field1_sym = root.create_symbol(ident_ids, &format!("field_1_{tag_id}_{i}"));
            let field2_sym = root.create_symbol(ident_ids, &format!("field_2_{tag_id}_{i}"));

            let field1_expr = Expr::UnionAtIndex {
                union_layout,
                tag_id,
                index: i as u64,
                structure: operands[0],
            };

            let field2_expr = Expr::UnionAtIndex {
                union_layout,
                tag_id,
                index: i as u64,
                structure: operands[1],
            };

            let inner = Stmt::Let(
                field1_sym,
                field1_expr,
                field_layouts[i],
                root.arena.alloc(
                    //
                    Stmt::Let(
                        field2_sym,
                        field2_expr,
                        field_layouts[i],
                        root.arena.alloc(
                            //
                            Stmt::Jump(tailrec_loop, root.arena.alloc([field1_sym, field2_sym])),
                        ),
                    ),
                ),
            );

            (i, inner)
        }
    };

    let mut stmt = innermost_stmt;
    for (i, layout) in field_layouts.iter().enumerate().rev() {
        if i == tailrec_index {
            continue; // the tail-recursive field is handled elsewhere
        }

        let field1_sym = root.create_symbol(ident_ids, &format!("field_1_{tag_id}_{i}"));
        let field2_sym = root.create_symbol(ident_ids, &format!("field_2_{tag_id}_{i}"));

        let field1_expr = Expr::UnionAtIndex {
            union_layout,
            tag_id,
            index: i as u64,
            structure: operands[0],
        };

        let field2_expr = Expr::UnionAtIndex {
            union_layout,
            tag_id,
            index: i as u64,
            structure: operands[1],
        };

        let eq_call_expr = root
            .call_specialized_op(
                ident_ids,
                ctx,
                layout_interner,
                *layout,
                root.arena.alloc([field1_sym, field2_sym]),
            )
            .unwrap();

        let eq_call_name = format!("eq_call_{i}");
        let eq_call_sym = root.create_symbol(ident_ids, &eq_call_name);

        stmt = Stmt::Let(
            field1_sym,
            field1_expr,
            field_layouts[i],
            root.arena.alloc(
                //
                Stmt::Let(
                    field2_sym,
                    field2_expr,
                    field_layouts[i],
                    root.arena.alloc(
                        //
                        Stmt::Let(
                            eq_call_sym,
                            eq_call_expr,
                            LAYOUT_BOOL,
                            root.arena.alloc(
                                //
                                if_false_return_false(
                                    root,
                                    eq_call_sym,
                                    // else
                                    stmt,
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        )
    }
    stmt
}

fn eq_boxed<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    inner_layout: InLayout<'a>,
) -> Stmt<'a> {
    let a = root.create_symbol(ident_ids, "a");
    let b = root.create_symbol(ident_ids, "b");
    let result = root.create_symbol(ident_ids, "result");

    let a_expr = Expr::ptr_load(&ARG_1);
    let b_expr = Expr::ptr_load(&ARG_2);
    let eq_call_expr = root
        .call_specialized_op(
            ident_ids,
            ctx,
            layout_interner,
            inner_layout,
            root.arena.alloc([a, b]),
        )
        .unwrap();

    Stmt::Let(
        a,
        a_expr,
        inner_layout,
        root.arena.alloc(
            //
            Stmt::Let(
                b,
                b_expr,
                inner_layout,
                root.arena.alloc(
                    //
                    Stmt::Let(
                        result,
                        eq_call_expr,
                        LAYOUT_BOOL,
                        root.arena.alloc(Stmt::Ret(result)),
                    ),
                ),
            ),
        ),
    )
}

/// List equality
/// TODO, ListGetUnsafe no longer increments the refcount, so we can use it here.
/// We can't use `ListGetUnsafe` because it increments the refcount, and we don't want that.
/// Another way to dereference a heap pointer is to use `Expr::UnionAtIndex`.
/// To achieve this we use `PtrCast` to cast the element pointer to a "Ptr" layout.
/// Then we can increment the pointer in a loop, dereferencing it each time.
/// (An alternative approach would be to create a new lowlevel like ListPeekUnsafe.)
fn eq_list<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    elem_layout: InLayout<'a>,
) -> Stmt<'a> {
    use LowLevel::*;
    let layout_isize = root.layout_isize;
    let arena = root.arena;

    // A pointer layout (heap pointer to a single list element)
    let ptr_layout = layout_interner.insert_direct_no_semantic(LayoutRepr::Ptr(elem_layout));

    // Compare lengths

    let len_1 = root.create_symbol(ident_ids, "len_1");
    let len_2 = root.create_symbol(ident_ids, "len_2");
    let len_1_stmt = |next| let_lowlevel(arena, layout_isize, len_1, ListLenUsize, &[ARG_1], next);
    let len_2_stmt = |next| let_lowlevel(arena, layout_isize, len_2, ListLenUsize, &[ARG_2], next);

    let eq_len = root.create_symbol(ident_ids, "eq_len");
    let eq_len_stmt = |next| let_lowlevel(arena, LAYOUT_BOOL, eq_len, Eq, &[len_1, len_2], next);

    // if lengths are equal...

    // get element pointers
    let elements_1 = root.create_symbol(ident_ids, "elements_1");
    let elements_2 = root.create_symbol(ident_ids, "elements_2");
    let elements_1_expr = Expr::StructAtIndex {
        index: 0,
        field_layouts: root.arena.alloc([ptr_layout, layout_isize]),
        structure: ARG_1,
    };
    let elements_2_expr = Expr::StructAtIndex {
        index: 0,
        field_layouts: root.arena.alloc([ptr_layout, layout_isize]),
        structure: ARG_2,
    };
    let elements_1_stmt = |next| Stmt::Let(elements_1, elements_1_expr, ptr_layout, next);
    let elements_2_stmt = |next| Stmt::Let(elements_2, elements_2_expr, ptr_layout, next);

    // Cast to integers
    let start_1 = root.create_symbol(ident_ids, "start_1");
    let start_2 = root.create_symbol(ident_ids, "start_2");
    let start_1_stmt =
        |next| let_lowlevel(arena, layout_isize, start_1, PtrCast, &[elements_1], next);
    let start_2_stmt =
        |next| let_lowlevel(arena, layout_isize, start_2, PtrCast, &[elements_2], next);

    //
    // Loop initialisation
    //

    // let size = literal int
    let size = root.create_symbol(ident_ids, "size");
    let size_expr = Expr::Literal(Literal::Int(
        (layout_interner
            .get_repr(elem_layout)
            .stack_size(layout_interner) as i128)
            .to_ne_bytes(),
    ));
    let size_stmt = |next| Stmt::Let(size, size_expr, layout_isize, next);

    // let list_size = len_1 * size
    let list_size = root.create_symbol(ident_ids, "list_size");
    let list_size_stmt =
        |next| let_lowlevel(arena, layout_isize, list_size, NumMul, &[len_1, size], next);

    // let end_1 = start_1 + list_size
    let end_1 = root.create_symbol(ident_ids, "end_1");
    let end_1_stmt = |next| {
        let_lowlevel(
            arena,
            layout_isize,
            end_1,
            NumAdd,
            &[start_1, list_size],
            next,
        )
    };

    //
    // Loop name & parameters
    //

    let elems_loop = JoinPointId(root.create_symbol(ident_ids, "elems_loop"));
    let addr1 = root.create_symbol(ident_ids, "addr1");
    let addr2 = root.create_symbol(ident_ids, "addr2");

    let param_addr1 = Param {
        symbol: addr1,
        layout: layout_isize,
    };

    let param_addr2 = Param {
        symbol: addr2,
        layout: layout_isize,
    };

    //
    // if we haven't reached the end yet...
    //

    // Cast integers to pointers
    let ptr1 = root.create_symbol(ident_ids, "ptr1");
    let ptr2 = root.create_symbol(ident_ids, "ptr2");
    let ptr1_stmt = |next| let_lowlevel(arena, ptr_layout, ptr1, PtrCast, &[addr1], next);
    let ptr2_stmt = |next| let_lowlevel(arena, ptr_layout, ptr2, PtrCast, &[addr2], next);

    // Dereference the pointers to get the current elements
    let elem1 = root.create_symbol(ident_ids, "elem1");
    let elem2 = root.create_symbol(ident_ids, "elem2");
    let elem1_expr = Expr::ptr_load(arena.alloc(ptr1));
    let elem2_expr = Expr::ptr_load(arena.alloc(ptr2));
    let elem1_stmt = |next| Stmt::Let(elem1, elem1_expr, elem_layout, next);
    let elem2_stmt = |next| Stmt::Let(elem2, elem2_expr, elem_layout, next);

    // Compare the two current elements
    let eq_elems = root.create_symbol(ident_ids, "eq_elems");
    let eq_elems_args = root.arena.alloc([elem1, elem2]);
    let eq_elems_expr = root
        .call_specialized_op(ident_ids, ctx, layout_interner, elem_layout, eq_elems_args)
        .unwrap();

    let eq_elems_stmt = |next| Stmt::Let(eq_elems, eq_elems_expr, LAYOUT_BOOL, next);

    // If current elements are equal, loop back again
    let next_1 = root.create_symbol(ident_ids, "next_1");
    let next_2 = root.create_symbol(ident_ids, "next_2");
    let next_1_stmt =
        |next| let_lowlevel(arena, layout_isize, next_1, NumAdd, &[addr1, size], next);
    let next_2_stmt =
        |next| let_lowlevel(arena, layout_isize, next_2, NumAdd, &[addr2, size], next);

    let jump_back = Stmt::Jump(elems_loop, root.arena.alloc([next_1, next_2]));

    //
    // Control flow
    //

    let is_end = root.create_symbol(ident_ids, "is_end");
    let is_end_stmt =
        |next| let_lowlevel(arena, LAYOUT_BOOL, is_end, NumGte, &[addr1, end_1], next);

    let if_elems_not_equal = if_false_return_false(
        root,
        eq_elems,
        // else
        next_1_stmt(root.arena.alloc(
            //
            next_2_stmt(root.arena.alloc(
                //
                jump_back,
            )),
        )),
    );

    let if_end_of_list = Stmt::if_then_else(
        arena,
        is_end,
        Layout::BOOL,
        Stmt::Ret(Symbol::BOOL_TRUE),
        root.arena.alloc(
            //
            ptr1_stmt(root.arena.alloc(
                //
                ptr2_stmt(root.arena.alloc(
                    //
                    elem1_stmt(root.arena.alloc(
                        //
                        elem2_stmt(root.arena.alloc(
                            //
                            eq_elems_stmt(root.arena.alloc(
                                //
                                if_elems_not_equal,
                            )),
                        )),
                    )),
                )),
            )),
        ),
    );

    let joinpoint_loop = Stmt::Join {
        id: elems_loop,
        parameters: root.arena.alloc([param_addr1, param_addr2]),
        body: root.arena.alloc(
            //
            is_end_stmt(
                //
                root.arena.alloc(if_end_of_list),
            ),
        ),
        remainder: root
            .arena
            .alloc(Stmt::Jump(elems_loop, root.arena.alloc([start_1, start_2]))),
    };

    let if_different_lengths = if_false_return_false(
        root,
        eq_len,
        // else
        elements_1_stmt(root.arena.alloc(
            //
            elements_2_stmt(root.arena.alloc(
                //
                if_pointers_equal_return_true(
                    root,
                    ident_ids,
                    [elements_1, elements_2],
                    root.arena.alloc(
                        //
                        start_1_stmt(root.arena.alloc(
                            //
                            start_2_stmt(root.arena.alloc(
                                //
                                size_stmt(root.arena.alloc(
                                    //
                                    list_size_stmt(root.arena.alloc(
                                        //
                                        end_1_stmt(root.arena.alloc(
                                            //
                                            joinpoint_loop,
                                        )),
                                    )),
                                )),
                            )),
                        )),
                    ),
                ),
            )),
        )),
    );

    len_1_stmt(root.arena.alloc(
        //
        len_2_stmt(root.arena.alloc(
            //
            eq_len_stmt(root.arena.alloc(
                //
                if_different_lengths,
            )),
        )),
    ))
}
