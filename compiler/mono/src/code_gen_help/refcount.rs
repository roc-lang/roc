use bumpalo::collections::vec::Vec;
use roc_builtins::bitcode::IntWidth;
use roc_module::low_level::{LowLevel, LowLevel::*};
use roc_module::symbol::{IdentIds, Symbol};

use crate::code_gen_help::let_lowlevel;
use crate::ir::{
    BranchInfo, Call, CallType, Expr, JoinPointId, Literal, ModifyRc, Param, Stmt, UpdateModeId,
};
use crate::layout::{Builtin, Layout, TagIdIntType, UnionLayout};

use super::{CodeGenHelp, Context, HelperOp};

const LAYOUT_BOOL: Layout = Layout::Builtin(Builtin::Bool);
const LAYOUT_UNIT: Layout = Layout::UNIT;
const LAYOUT_PTR: Layout = Layout::RecursivePointer;
const LAYOUT_U32: Layout = Layout::Builtin(Builtin::Int(IntWidth::U32));

pub fn refcount_stmt<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout: Layout<'a>,
    modify: &ModifyRc,
    following: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    let arena = root.arena;

    match modify {
        ModifyRc::Inc(structure, amount) => {
            let layout_isize = root.layout_isize;

            // Define a constant for the amount to increment
            let amount_sym = root.create_symbol(ident_ids, "amount");
            let amount_expr = Expr::Literal(Literal::Int(*amount as i128));
            let amount_stmt = |next| Stmt::Let(amount_sym, amount_expr, layout_isize, next);

            // Call helper proc, passing the Roc structure and constant amount
            let call_result_empty = root.create_symbol(ident_ids, "call_result_empty");
            let call_expr = root
                .call_specialized_op(
                    ident_ids,
                    ctx,
                    layout,
                    arena.alloc([*structure, amount_sym]),
                )
                .unwrap();

            let call_stmt = Stmt::Let(call_result_empty, call_expr, LAYOUT_UNIT, following);
            arena.alloc(amount_stmt(arena.alloc(call_stmt)))
        }

        ModifyRc::Dec(structure) => {
            // Call helper proc, passing the Roc structure
            let call_result_empty = root.create_symbol(ident_ids, "call_result_empty");
            let call_expr = root
                .call_specialized_op(ident_ids, ctx, layout, arena.alloc([*structure]))
                .unwrap();
            let call_stmt = Stmt::Let(call_result_empty, call_expr, LAYOUT_UNIT, following);
            arena.alloc(call_stmt)
        }

        ModifyRc::DecRef(structure) => {
            match layout {
                // Str has no children, so we might as well do what we normally do and call the helper.
                Layout::Builtin(Builtin::Str) => {
                    ctx.op = HelperOp::Dec;
                    refcount_stmt(root, ident_ids, ctx, layout, modify, following)
                }

                // Struct and non-recursive Unions are stack-only, so DecRef is a no-op
                Layout::Struct { .. } => following,
                Layout::Union(UnionLayout::NonRecursive(_)) => following,

                // Inline the refcounting code instead of making a function. Don't iterate fields,
                // and replace any return statements with jumps to the `following` statement.
                _ => match ctx.op {
                    HelperOp::DecRef(jp_decref) => {
                        let rc_stmt = refcount_generic(root, ident_ids, ctx, layout, *structure);
                        let join = Stmt::Join {
                            id: jp_decref,
                            parameters: &[],
                            body: following,
                            remainder: arena.alloc(rc_stmt),
                        };
                        arena.alloc(join)
                    }
                    _ => unreachable!(),
                },
            }
        }
    }
}

pub fn refcount_generic<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout: Layout<'a>,
    structure: Symbol,
) -> Stmt<'a> {
    debug_assert!(is_rc_implemented_yet(&layout));
    let rc_todo = || todo!("Please update is_rc_implemented_yet for `{:?}`", layout);

    match layout {
        Layout::Builtin(Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal) => {
            unreachable!("Not refcounted: {:?}", layout)
        }
        Layout::Builtin(Builtin::Str) => refcount_str(root, ident_ids, ctx),
        Layout::Builtin(Builtin::List(elem_layout)) => {
            refcount_list(root, ident_ids, ctx, &layout, elem_layout, structure)
        }
        Layout::Builtin(Builtin::Dict(_, _) | Builtin::Set(_)) => rc_todo(),
        Layout::Struct { field_layouts, .. } => {
            refcount_struct(root, ident_ids, ctx, field_layouts, structure)
        }
        Layout::Union(union_layout) => {
            refcount_union(root, ident_ids, ctx, union_layout, structure)
        }
        Layout::LambdaSet(lambda_set) => {
            let runtime_layout = lambda_set.runtime_representation();
            refcount_generic(root, ident_ids, ctx, runtime_layout, structure)
        }
        Layout::RecursivePointer => rc_todo(),
    }
}

// Check if refcounting is implemented yet. In the long term, this will be deleted.
// In the short term, it helps us to skip refcounting and let it leak, so we can make
// progress incrementally. Kept in sync with generate_procs using assertions.
pub fn is_rc_implemented_yet(layout: &Layout) -> bool {
    use UnionLayout::*;

    match layout {
        Layout::Builtin(Builtin::Dict(..) | Builtin::Set(_)) => false,
        Layout::Builtin(Builtin::List(elem_layout)) => is_rc_implemented_yet(elem_layout),
        Layout::Builtin(_) => true,
        Layout::Struct { field_layouts, .. } => field_layouts.iter().all(is_rc_implemented_yet),
        Layout::Union(union_layout) => match union_layout {
            NonRecursive(tags) => tags
                .iter()
                .all(|fields| fields.iter().all(is_rc_implemented_yet)),
            Recursive(tags) => tags
                .iter()
                .all(|fields| fields.iter().all(is_rc_implemented_yet)),
            NonNullableUnwrapped(fields) => fields.iter().all(is_rc_implemented_yet),
            NullableWrapped { other_tags, .. } => other_tags
                .iter()
                .all(|fields| fields.iter().all(is_rc_implemented_yet)),
            NullableUnwrapped { other_fields, .. } => {
                other_fields.iter().all(is_rc_implemented_yet)
            }
        },
        Layout::LambdaSet(lambda_set) => {
            is_rc_implemented_yet(&lambda_set.runtime_representation())
        }
        Layout::RecursivePointer => true,
    }
}

fn rc_return_stmt<'a>(
    root: &CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
) -> Stmt<'a> {
    if let HelperOp::DecRef(jp_decref) = ctx.op {
        Stmt::Jump(jp_decref, &[])
    } else {
        let unit = root.create_symbol(ident_ids, "unit");
        let ret_stmt = root.arena.alloc(Stmt::Ret(unit));
        Stmt::Let(unit, Expr::Struct(&[]), LAYOUT_UNIT, ret_stmt)
    }
}

fn refcount_args<'a>(root: &CodeGenHelp<'a>, ctx: &Context<'a>, structure: Symbol) -> &'a [Symbol] {
    if ctx.op == HelperOp::Inc {
        // second argument is always `amount`, passed down through the call stack
        root.arena.alloc([structure, Symbol::ARG_2])
    } else {
        root.arena.alloc([structure])
    }
}

// Subtract a constant from a pointer to find the refcount
// Also does some type casting, so that we have different Symbols and Layouts
// for the 'pointer' and 'integer' versions of the address.
// This helps to avoid issues with the backends Symbol->Layout mapping.
pub fn rc_ptr_from_data_ptr<'a>(
    root: &CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    structure: Symbol,
    rc_ptr_sym: Symbol,
    mask_lower_bits: bool,
    following: &'a Stmt<'a>,
) -> Stmt<'a> {
    // Typecast the structure pointer to an integer
    // Backends expect a number Layout to choose the right "subtract" instruction
    let addr_sym = root.create_symbol(ident_ids, "addr");
    let addr_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::PtrCast,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: root.arena.alloc([structure]),
    });
    let addr_stmt = |next| Stmt::Let(addr_sym, addr_expr, root.layout_isize, next);

    // Mask for lower bits (for tag union id)
    let mask_sym = root.create_symbol(ident_ids, "mask");
    let mask_expr = Expr::Literal(Literal::Int(-(root.target_info.ptr_width() as i128)));
    let mask_stmt = |next| Stmt::Let(mask_sym, mask_expr, root.layout_isize, next);

    let masked_sym = root.create_symbol(ident_ids, "masked");
    let and_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::And,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: root.arena.alloc([addr_sym, mask_sym]),
    });
    let and_stmt = |next| Stmt::Let(masked_sym, and_expr, root.layout_isize, next);

    // Pointer size constant
    let ptr_size_sym = root.create_symbol(ident_ids, "ptr_size");
    let ptr_size_expr = Expr::Literal(Literal::Int(root.target_info.ptr_width() as i128));
    let ptr_size_stmt = |next| Stmt::Let(ptr_size_sym, ptr_size_expr, root.layout_isize, next);

    // Refcount address
    let rc_addr_sym = root.create_symbol(ident_ids, "rc_addr");
    let sub_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::NumSub,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: root.arena.alloc([
            if mask_lower_bits {
                masked_sym
            } else {
                addr_sym
            },
            ptr_size_sym,
        ]),
    });
    let sub_stmt = |next| Stmt::Let(rc_addr_sym, sub_expr, root.layout_isize, next);

    // Typecast the refcount address from integer to pointer
    let cast_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::PtrCast,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: root.arena.alloc([rc_addr_sym]),
    });
    let cast_stmt = |next| Stmt::Let(rc_ptr_sym, cast_expr, LAYOUT_PTR, next);

    if mask_lower_bits {
        addr_stmt(root.arena.alloc(
            //
            mask_stmt(root.arena.alloc(
                //
                and_stmt(root.arena.alloc(
                    //
                    ptr_size_stmt(root.arena.alloc(
                        //
                        sub_stmt(root.arena.alloc(
                            //
                            cast_stmt(root.arena.alloc(
                                //
                                following,
                            )),
                        )),
                    )),
                )),
            )),
        ))
    } else {
        addr_stmt(root.arena.alloc(
            //
            ptr_size_stmt(root.arena.alloc(
                //
                sub_stmt(root.arena.alloc(
                    //
                    cast_stmt(root.arena.alloc(
                        //
                        following,
                    )),
                )),
            )),
        ))
    }
}

fn modify_refcount<'a>(
    root: &CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    rc_ptr: Symbol,
    alignment: u32,
    following: &'a Stmt<'a>,
) -> Stmt<'a> {
    // Call the relevant Zig lowlevel to actually modify the refcount
    let zig_call_result = root.create_symbol(ident_ids, "zig_call_result");
    match ctx.op {
        HelperOp::Inc => {
            let zig_call_expr = Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op: LowLevel::RefCountInc,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: root.arena.alloc([rc_ptr, Symbol::ARG_2]),
            });
            Stmt::Let(zig_call_result, zig_call_expr, LAYOUT_UNIT, following)
        }

        HelperOp::Dec | HelperOp::DecRef(_) => {
            let alignment_sym = root.create_symbol(ident_ids, "alignment");
            let alignment_expr = Expr::Literal(Literal::Int(alignment as i128));
            let alignment_stmt = |next| Stmt::Let(alignment_sym, alignment_expr, LAYOUT_U32, next);

            let zig_call_expr = Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op: LowLevel::RefCountDec,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: root.arena.alloc([rc_ptr, alignment_sym]),
            });
            let zig_call_stmt = Stmt::Let(zig_call_result, zig_call_expr, LAYOUT_UNIT, following);

            alignment_stmt(root.arena.alloc(
                //
                zig_call_stmt,
            ))
        }

        _ => unreachable!(),
    }
}

/// Generate a procedure to modify the reference count of a Str
fn refcount_str<'a>(
    root: &CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
) -> Stmt<'a> {
    let string = Symbol::ARG_1;
    let layout_isize = root.layout_isize;

    // Get the string length as a signed int
    let len = root.create_symbol(ident_ids, "len");
    let len_expr = Expr::StructAtIndex {
        index: 1,
        field_layouts: root.arena.alloc([LAYOUT_PTR, layout_isize]),
        structure: string,
    };
    let len_stmt = |next| Stmt::Let(len, len_expr, layout_isize, next);

    // Zero
    let zero = root.create_symbol(ident_ids, "zero");
    let zero_expr = Expr::Literal(Literal::Int(0));
    let zero_stmt = |next| Stmt::Let(zero, zero_expr, layout_isize, next);

    // is_big_str = (len >= 0);
    // Treat len as isize so that the small string flag is the same as the sign bit
    let is_big_str = root.create_symbol(ident_ids, "is_big_str");
    let is_big_str_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::NumGte,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: root.arena.alloc([len, zero]),
    });
    let is_big_str_stmt = |next| Stmt::Let(is_big_str, is_big_str_expr, LAYOUT_BOOL, next);

    // Get the pointer to the string elements
    let elements = root.create_symbol(ident_ids, "elements");
    let elements_expr = Expr::StructAtIndex {
        index: 0,
        field_layouts: root.arena.alloc([LAYOUT_PTR, layout_isize]),
        structure: string,
    };
    let elements_stmt = |next| Stmt::Let(elements, elements_expr, layout_isize, next);

    // A pointer to the refcount value itself
    let rc_ptr = root.create_symbol(ident_ids, "rc_ptr");
    let alignment = root.target_info.ptr_width() as u32;

    let ret_unit_stmt = rc_return_stmt(root, ident_ids, ctx);
    let mod_rc_stmt = modify_refcount(
        root,
        ident_ids,
        ctx,
        rc_ptr,
        alignment,
        root.arena.alloc(ret_unit_stmt),
    );

    // Generate an `if` to skip small strings but modify big strings
    let then_branch = elements_stmt(root.arena.alloc(
        //
        rc_ptr_from_data_ptr(
            root,
            ident_ids,
            elements,
            rc_ptr,
            false,
            root.arena.alloc(
                //
                mod_rc_stmt,
            ),
        ),
    ));

    let if_stmt = Stmt::Switch {
        cond_symbol: is_big_str,
        cond_layout: LAYOUT_BOOL,
        branches: root.arena.alloc([(1, BranchInfo::None, then_branch)]),
        default_branch: (
            BranchInfo::None,
            root.arena.alloc(rc_return_stmt(root, ident_ids, ctx)),
        ),
        ret_layout: LAYOUT_UNIT,
    };

    // Combine the statements in sequence
    len_stmt(root.arena.alloc(
        //
        zero_stmt(root.arena.alloc(
            //
            is_big_str_stmt(root.arena.alloc(
                //
                if_stmt,
            )),
        )),
    ))
}

fn refcount_list<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout: &Layout,
    elem_layout: &'a Layout,
    structure: Symbol,
) -> Stmt<'a> {
    let layout_isize = root.layout_isize;
    let arena = root.arena;

    // A "Box" layout (heap pointer to a single list element)
    let box_union_layout = UnionLayout::NonNullableUnwrapped(arena.alloc([*elem_layout]));
    let box_layout = Layout::Union(box_union_layout);

    //
    // Check if the list is empty
    //

    let len = root.create_symbol(ident_ids, "len");
    let len_stmt = |next| let_lowlevel(arena, layout_isize, len, ListLen, &[structure], next);

    // Zero
    let zero = root.create_symbol(ident_ids, "zero");
    let zero_expr = Expr::Literal(Literal::Int(0));
    let zero_stmt = |next| Stmt::Let(zero, zero_expr, layout_isize, next);

    // let is_empty = lowlevel Eq len zero
    let is_empty = root.create_symbol(ident_ids, "is_empty");
    let is_empty_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::Eq,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: root.arena.alloc([len, zero]),
    });
    let is_empty_stmt = |next| Stmt::Let(is_empty, is_empty_expr, LAYOUT_BOOL, next);

    // get elements pointer
    let elements = root.create_symbol(ident_ids, "elements");
    let elements_expr = Expr::StructAtIndex {
        index: 0,
        field_layouts: arena.alloc([box_layout, layout_isize]),
        structure,
    };
    let elements_stmt = |next| Stmt::Let(elements, elements_expr, box_layout, next);

    //
    // modify refcount of the list and its elements
    // (elements first, to avoid use-after-free for Dec)
    //

    let rc_ptr = root.create_symbol(ident_ids, "rc_ptr");
    let alignment = layout.alignment_bytes(root.target_info);

    let ret_stmt = rc_return_stmt(root, ident_ids, ctx);
    let modify_list = modify_refcount(
        root,
        ident_ids,
        ctx,
        rc_ptr,
        alignment,
        arena.alloc(ret_stmt),
    );

    let get_rc_and_modify_list = rc_ptr_from_data_ptr(
        root,
        ident_ids,
        elements,
        rc_ptr,
        false,
        arena.alloc(modify_list),
    );

    let modify_elems_and_list = if elem_layout.is_refcounted() && !ctx.op.is_decref() {
        refcount_list_elems(
            root,
            ident_ids,
            ctx,
            elem_layout,
            LAYOUT_UNIT,
            box_union_layout,
            len,
            elements,
            get_rc_and_modify_list,
        )
    } else {
        get_rc_and_modify_list
    };

    //
    // Do nothing if the list is empty
    //

    let non_empty_branch = root.arena.alloc(
        //
        elements_stmt(root.arena.alloc(
            //
            modify_elems_and_list,
        )),
    );

    let if_stmt = Stmt::Switch {
        cond_symbol: is_empty,
        cond_layout: LAYOUT_BOOL,
        branches: root
            .arena
            .alloc([(1, BranchInfo::None, rc_return_stmt(root, ident_ids, ctx))]),
        default_branch: (BranchInfo::None, non_empty_branch),
        ret_layout: LAYOUT_UNIT,
    };

    len_stmt(arena.alloc(
        //
        zero_stmt(arena.alloc(
            //
            is_empty_stmt(arena.alloc(
                //
                if_stmt,
            )),
        )),
    ))
}

#[allow(clippy::too_many_arguments)]
fn refcount_list_elems<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    elem_layout: &Layout<'a>,
    ret_layout: Layout<'a>,
    box_union_layout: UnionLayout<'a>,
    length: Symbol,
    elements: Symbol,
    following: Stmt<'a>,
) -> Stmt<'a> {
    use LowLevel::*;
    let layout_isize = root.layout_isize;
    let arena = root.arena;

    // Cast to integer
    let start = root.create_symbol(ident_ids, "start");
    let start_stmt = |next| let_lowlevel(arena, layout_isize, start, PtrCast, &[elements], next);

    //
    // Loop initialisation
    //

    // let size = literal int
    let elem_size = root.create_symbol(ident_ids, "elem_size");
    let elem_size_expr = Expr::Literal(Literal::Int(
        elem_layout.stack_size(root.target_info) as i128
    ));
    let elem_size_stmt = |next| Stmt::Let(elem_size, elem_size_expr, layout_isize, next);

    // let list_size = len * size
    let list_size = root.create_symbol(ident_ids, "list_size");
    let list_size_stmt = |next| {
        let_lowlevel(
            arena,
            layout_isize,
            list_size,
            NumMul,
            &[length, elem_size],
            next,
        )
    };

    // let end = start + list_size
    let end = root.create_symbol(ident_ids, "end");
    let end_stmt = |next| let_lowlevel(arena, layout_isize, end, NumAdd, &[start, list_size], next);

    //
    // Loop name & parameter
    //

    let elems_loop = JoinPointId(root.create_symbol(ident_ids, "elems_loop"));
    let addr = root.create_symbol(ident_ids, "addr");

    let param_addr = Param {
        symbol: addr,
        borrow: false,
        layout: layout_isize,
    };

    //
    // if we haven't reached the end yet...
    //

    // Cast integer to box pointer
    let box_ptr = root.create_symbol(ident_ids, "box");
    let box_layout = Layout::Union(box_union_layout);
    let box_stmt = |next| let_lowlevel(arena, box_layout, box_ptr, PtrCast, &[addr], next);

    // Dereference the box pointer to get the current element
    let elem = root.create_symbol(ident_ids, "elem");
    let elem_expr = Expr::UnionAtIndex {
        structure: box_ptr,
        union_layout: box_union_layout,
        tag_id: 0,
        index: 0,
    };
    let elem_stmt = |next| Stmt::Let(elem, elem_expr, *elem_layout, next);

    //
    // Modify element refcount
    //

    let mod_elem_unit = root.create_symbol(ident_ids, "mod_elem_unit");
    let mod_elem_args = refcount_args(root, ctx, elem);
    let mod_elem_expr = root
        .call_specialized_op(ident_ids, ctx, *elem_layout, mod_elem_args)
        .unwrap();
    let mod_elem_stmt = |next| Stmt::Let(mod_elem_unit, mod_elem_expr, LAYOUT_UNIT, next);

    //
    // Next loop iteration
    //
    let next_addr = root.create_symbol(ident_ids, "next_addr");
    let next_addr_stmt = |next| {
        let_lowlevel(
            arena,
            layout_isize,
            next_addr,
            NumAdd,
            &[addr, elem_size],
            next,
        )
    };

    //
    // Control flow
    //

    let is_end = root.create_symbol(ident_ids, "is_end");
    let is_end_stmt = |next| let_lowlevel(arena, LAYOUT_BOOL, is_end, NumGte, &[addr, end], next);

    let if_end_of_list = Stmt::Switch {
        cond_symbol: is_end,
        cond_layout: LAYOUT_BOOL,
        ret_layout,
        branches: root.arena.alloc([(1, BranchInfo::None, following)]),
        default_branch: (
            BranchInfo::None,
            arena.alloc(box_stmt(arena.alloc(
                //
                elem_stmt(arena.alloc(
                    //
                    mod_elem_stmt(arena.alloc(
                        //
                        next_addr_stmt(arena.alloc(
                            //
                            Stmt::Jump(elems_loop, arena.alloc([next_addr])),
                        )),
                    )),
                )),
            ))),
        ),
    };

    let joinpoint_loop = Stmt::Join {
        id: elems_loop,
        parameters: arena.alloc([param_addr]),
        body: arena.alloc(
            //
            is_end_stmt(
                //
                arena.alloc(if_end_of_list),
            ),
        ),
        remainder: root
            .arena
            .alloc(Stmt::Jump(elems_loop, arena.alloc([start]))),
    };

    start_stmt(arena.alloc(
        //
        elem_size_stmt(arena.alloc(
            //
            list_size_stmt(arena.alloc(
                //
                end_stmt(arena.alloc(
                    //
                    joinpoint_loop,
                )),
            )),
        )),
    ))
}

fn refcount_struct<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    field_layouts: &'a [Layout<'a>],
    structure: Symbol,
) -> Stmt<'a> {
    let mut stmt = rc_return_stmt(root, ident_ids, ctx);

    for (i, field_layout) in field_layouts.iter().enumerate().rev() {
        if field_layout.contains_refcounted() {
            let field_val = root.create_symbol(ident_ids, &format!("field_val_{}", i));
            let field_val_expr = Expr::StructAtIndex {
                index: i as u64,
                field_layouts,
                structure,
            };
            let field_val_stmt = |next| Stmt::Let(field_val, field_val_expr, *field_layout, next);

            let mod_unit = root.create_symbol(ident_ids, &format!("mod_field_{}", i));
            let mod_args = refcount_args(root, ctx, field_val);
            let mod_expr = root
                .call_specialized_op(ident_ids, ctx, *field_layout, mod_args)
                .unwrap();
            let mod_stmt = |next| Stmt::Let(mod_unit, mod_expr, LAYOUT_UNIT, next);

            stmt = field_val_stmt(root.arena.alloc(
                //
                mod_stmt(root.arena.alloc(
                    //
                    stmt,
                )),
            ))
        }
    }

    stmt
}

fn refcount_union<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    union: UnionLayout<'a>,
    structure: Symbol,
) -> Stmt<'a> {
    use UnionLayout::*;

    let parent_rec_ptr_layout = ctx.recursive_union;
    if !matches!(union, NonRecursive(_)) {
        ctx.recursive_union = Some(union);
    }

    let body = match union {
        NonRecursive(tags) => refcount_union_nonrec(root, ident_ids, ctx, union, tags, structure),

        Recursive(tags) => {
            let (is_tailrec, tail_idx) = root.union_tail_recursion_fields(union);
            if is_tailrec && !ctx.op.is_decref() {
                refcount_union_tailrec(root, ident_ids, ctx, union, tags, None, tail_idx, structure)
            } else {
                refcount_union_rec(root, ident_ids, ctx, union, tags, None, structure)
            }
        }

        NonNullableUnwrapped(field_layouts) => {
            // We don't do tail recursion on NonNullableUnwrapped.
            // Its RecursionPointer is always nested inside a List, Option, or other sub-layout, since
            // a direct RecursionPointer is only possible if there's at least one non-recursive variant.
            // This nesting makes it harder to do tail recursion, so we just don't.
            let tags = root.arena.alloc([field_layouts]);
            refcount_union_rec(root, ident_ids, ctx, union, tags, None, structure)
        }

        NullableWrapped {
            other_tags: tags,
            nullable_id,
        } => {
            let null_id = Some(nullable_id);
            let (is_tailrec, tail_idx) = root.union_tail_recursion_fields(union);
            if is_tailrec && !ctx.op.is_decref() {
                refcount_union_tailrec(
                    root, ident_ids, ctx, union, tags, null_id, tail_idx, structure,
                )
            } else {
                refcount_union_rec(root, ident_ids, ctx, union, tags, null_id, structure)
            }
        }

        NullableUnwrapped {
            other_fields,
            nullable_id,
        } => {
            let null_id = Some(nullable_id as TagIdIntType);
            let tags = root.arena.alloc([other_fields]);
            let (is_tailrec, tail_idx) = root.union_tail_recursion_fields(union);
            if is_tailrec && !ctx.op.is_decref() {
                refcount_union_tailrec(
                    root, ident_ids, ctx, union, tags, null_id, tail_idx, structure,
                )
            } else {
                refcount_union_rec(root, ident_ids, ctx, union, tags, null_id, structure)
            }
        }
    };

    ctx.recursive_union = parent_rec_ptr_layout;

    body
}

fn refcount_union_nonrec<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    union_layout: UnionLayout<'a>,
    tag_layouts: &'a [&'a [Layout<'a>]],
    structure: Symbol,
) -> Stmt<'a> {
    let tag_id_layout = union_layout.tag_id_layout();

    let tag_id_sym = root.create_symbol(ident_ids, "tag_id");
    let tag_id_stmt = |next| {
        Stmt::Let(
            tag_id_sym,
            Expr::GetTagId {
                structure,
                union_layout,
            },
            tag_id_layout,
            next,
        )
    };

    let continuation = rc_return_stmt(root, ident_ids, ctx);

    let switch_stmt = refcount_union_contents(
        root,
        ident_ids,
        ctx,
        union_layout,
        tag_layouts,
        None,
        structure,
        tag_id_sym,
        tag_id_layout,
        continuation,
    );

    tag_id_stmt(root.arena.alloc(
        //
        switch_stmt,
    ))
}

#[allow(clippy::too_many_arguments)]
fn refcount_union_contents<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    union_layout: UnionLayout<'a>,
    tag_layouts: &'a [&'a [Layout<'a>]],
    null_id: Option<TagIdIntType>,
    structure: Symbol,
    tag_id_sym: Symbol,
    tag_id_layout: Layout<'a>,
    modify_union_stmt: Stmt<'a>,
) -> Stmt<'a> {
    let jp_modify_union = JoinPointId(root.create_symbol(ident_ids, "jp_modify_union"));
    let mut tag_branches = Vec::with_capacity_in(tag_layouts.len() + 1, root.arena);

    if let Some(id) = null_id {
        let ret = rc_return_stmt(root, ident_ids, ctx);
        tag_branches.push((id as u64, BranchInfo::None, ret));
    }

    let mut tag_id: TagIdIntType = 0;
    for field_layouts in tag_layouts.iter() {
        match null_id {
            Some(id) if id == tag_id => {
                tag_id += 1;
            }
            _ => {}
        }

        // After refcounting the fields, jump to modify the union itself
        // (Order is important, to avoid use-after-free for Dec)
        let following = Stmt::Jump(jp_modify_union, &[]);

        let fields_stmt = refcount_tag_fields(
            root,
            ident_ids,
            ctx,
            union_layout,
            field_layouts,
            structure,
            tag_id,
            following,
        );

        tag_branches.push((tag_id as u64, BranchInfo::None, fields_stmt));

        tag_id += 1;
    }

    let default_stmt: Stmt<'a> = tag_branches.pop().unwrap().2;

    let tag_id_switch = Stmt::Switch {
        cond_symbol: tag_id_sym,
        cond_layout: tag_id_layout,
        branches: tag_branches.into_bump_slice(),
        default_branch: (BranchInfo::None, root.arena.alloc(default_stmt)),
        ret_layout: LAYOUT_UNIT,
    };

    Stmt::Join {
        id: jp_modify_union,
        parameters: &[],
        body: root.arena.alloc(modify_union_stmt),
        remainder: root.arena.alloc(tag_id_switch),
    }
}

fn refcount_union_rec<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    union_layout: UnionLayout<'a>,
    tag_layouts: &'a [&'a [Layout<'a>]],
    null_id: Option<TagIdIntType>,
    structure: Symbol,
) -> Stmt<'a> {
    let tag_id_layout = union_layout.tag_id_layout();

    let tag_id_sym = root.create_symbol(ident_ids, "tag_id");
    let tag_id_stmt = |next| {
        Stmt::Let(
            tag_id_sym,
            Expr::GetTagId {
                structure,
                union_layout,
            },
            tag_id_layout,
            next,
        )
    };

    let rc_structure_stmt = {
        let rc_ptr = root.create_symbol(ident_ids, "rc_ptr");

        let alignment = Layout::Union(union_layout).alignment_bytes(root.target_info);
        let ret_stmt = rc_return_stmt(root, ident_ids, ctx);
        let modify_structure_stmt = modify_refcount(
            root,
            ident_ids,
            ctx,
            rc_ptr,
            alignment,
            root.arena.alloc(ret_stmt),
        );

        rc_ptr_from_data_ptr(
            root,
            ident_ids,
            structure,
            rc_ptr,
            union_layout.stores_tag_id_in_pointer(root.target_info),
            root.arena.alloc(modify_structure_stmt),
        )
    };

    let rc_contents_then_structure = if ctx.op.is_decref() {
        rc_structure_stmt
    } else {
        refcount_union_contents(
            root,
            ident_ids,
            ctx,
            union_layout,
            tag_layouts,
            null_id,
            structure,
            tag_id_sym,
            tag_id_layout,
            rc_structure_stmt,
        )
    };

    if ctx.op.is_decref() && null_id.is_none() {
        rc_contents_then_structure
    } else {
        tag_id_stmt(root.arena.alloc(
            //
            rc_contents_then_structure,
        ))
    }
}

// Refcount a recursive union using tail-call elimination to limit stack growth
#[allow(clippy::too_many_arguments)]
fn refcount_union_tailrec<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    union_layout: UnionLayout<'a>,
    tag_layouts: &'a [&'a [Layout<'a>]],
    null_id: Option<TagIdIntType>,
    tailrec_indices: Vec<'a, Option<usize>>,
    initial_structure: Symbol,
) -> Stmt<'a> {
    let tailrec_loop = JoinPointId(root.create_symbol(ident_ids, "tailrec_loop"));
    let current = root.create_symbol(ident_ids, "current");
    let next_ptr = root.create_symbol(ident_ids, "next_ptr");
    let layout = Layout::Union(union_layout);

    let tag_id_layout = union_layout.tag_id_layout();

    let tag_id_sym = root.create_symbol(ident_ids, "tag_id");
    let tag_id_stmt = |next| {
        Stmt::Let(
            tag_id_sym,
            Expr::GetTagId {
                structure: current,
                union_layout,
            },
            tag_id_layout,
            next,
        )
    };

    // Do refcounting on the structure itself
    // In the control flow, this comes *after* refcounting the fields
    // It receives a `next` parameter to pass through to the outer joinpoint
    let rc_structure_stmt = {
        let rc_ptr = root.create_symbol(ident_ids, "rc_ptr");
        let next_addr = root.create_symbol(ident_ids, "next_addr");

        let exit_stmt = rc_return_stmt(root, ident_ids, ctx);
        let jump_to_loop = Stmt::Jump(tailrec_loop, root.arena.alloc([next_ptr]));

        let loop_or_exit = Stmt::Switch {
            cond_symbol: next_addr,
            cond_layout: root.layout_isize,
            branches: root.arena.alloc([(0, BranchInfo::None, exit_stmt)]),
            default_branch: (BranchInfo::None, root.arena.alloc(jump_to_loop)),
            ret_layout: LAYOUT_UNIT,
        };
        let loop_or_exit_based_on_next_addr = {
            let_lowlevel(
                root.arena,
                root.layout_isize,
                next_addr,
                PtrCast,
                &[next_ptr],
                root.arena.alloc(loop_or_exit),
            )
        };

        let alignment = layout.alignment_bytes(root.target_info);
        let modify_structure_stmt = modify_refcount(
            root,
            ident_ids,
            ctx,
            rc_ptr,
            alignment,
            root.arena.alloc(loop_or_exit_based_on_next_addr),
        );

        rc_ptr_from_data_ptr(
            root,
            ident_ids,
            current,
            rc_ptr,
            union_layout.stores_tag_id_in_pointer(root.target_info),
            root.arena.alloc(modify_structure_stmt),
        )
    };

    let rc_contents_then_structure = {
        let jp_modify_union = JoinPointId(root.create_symbol(ident_ids, "jp_modify_union"));
        let mut tag_branches = Vec::with_capacity_in(tag_layouts.len() + 1, root.arena);

        // If this is null, there is no refcount, no `next`, no fields. Just return.
        if let Some(id) = null_id {
            let ret = rc_return_stmt(root, ident_ids, ctx);
            tag_branches.push((id as u64, BranchInfo::None, ret));
        }

        let mut tag_id: TagIdIntType = 0;
        for (field_layouts, opt_tailrec_index) in tag_layouts.iter().zip(tailrec_indices) {
            match null_id {
                Some(id) if id == tag_id => {
                    tag_id += 1;
                }
                _ => {}
            }

            // After refcounting the fields, jump to modify the union itself.
            // The loop param is a pointer to the next union. It gets passed through two jumps.
            let (non_tailrec_fields, jump_to_modify_union) =
                if let Some(tailrec_index) = opt_tailrec_index {
                    let mut filtered = Vec::with_capacity_in(field_layouts.len() - 1, root.arena);
                    let mut tail_stmt = None;
                    for (i, field) in field_layouts.iter().enumerate() {
                        if i != tailrec_index {
                            filtered.push(*field);
                        } else {
                            let field_val =
                                root.create_symbol(ident_ids, &format!("field_{}_{}", tag_id, i));
                            let field_val_expr = Expr::UnionAtIndex {
                                union_layout,
                                tag_id,
                                index: i as u64,
                                structure: current,
                            };
                            let jump_params = root.arena.alloc([field_val]);
                            let jump = root.arena.alloc(Stmt::Jump(jp_modify_union, jump_params));
                            tail_stmt = Some(Stmt::Let(field_val, field_val_expr, *field, jump));
                        }
                    }

                    (filtered.into_bump_slice(), tail_stmt.unwrap())
                } else {
                    let zero = root.create_symbol(ident_ids, "zero");
                    let zero_expr = Expr::Literal(Literal::Int(0));
                    let zero_stmt = |next| Stmt::Let(zero, zero_expr, root.layout_isize, next);

                    let null = root.create_symbol(ident_ids, "null");
                    let null_stmt =
                        |next| let_lowlevel(root.arena, layout, null, PtrCast, &[zero], next);

                    let tail_stmt = zero_stmt(root.arena.alloc(
                        //
                        null_stmt(root.arena.alloc(
                            //
                            Stmt::Jump(jp_modify_union, root.arena.alloc([null])),
                        )),
                    ));

                    (*field_layouts, tail_stmt)
                };

            let fields_stmt = refcount_tag_fields(
                root,
                ident_ids,
                ctx,
                union_layout,
                non_tailrec_fields,
                current,
                tag_id,
                jump_to_modify_union,
            );

            tag_branches.push((tag_id as u64, BranchInfo::None, fields_stmt));

            tag_id += 1;
        }

        let default_stmt: Stmt<'a> = tag_branches.pop().unwrap().2;

        let tag_id_switch = Stmt::Switch {
            cond_symbol: tag_id_sym,
            cond_layout: tag_id_layout,
            branches: tag_branches.into_bump_slice(),
            default_branch: (BranchInfo::None, root.arena.alloc(default_stmt)),
            ret_layout: LAYOUT_UNIT,
        };

        let jp_param = Param {
            symbol: next_ptr,
            borrow: true,
            layout,
        };

        Stmt::Join {
            id: jp_modify_union,
            parameters: root.arena.alloc([jp_param]),
            body: root.arena.alloc(rc_structure_stmt),
            remainder: root.arena.alloc(tag_id_switch),
        }
    };

    let loop_body = tag_id_stmt(root.arena.alloc(
        //
        rc_contents_then_structure,
    ));

    let loop_init = Stmt::Jump(tailrec_loop, root.arena.alloc([initial_structure]));
    let loop_param = Param {
        symbol: current,
        borrow: true,
        layout: Layout::Union(union_layout),
    };

    Stmt::Join {
        id: tailrec_loop,
        parameters: root.arena.alloc([loop_param]),
        body: root.arena.alloc(loop_body),
        remainder: root.arena.alloc(loop_init),
    }
}

#[allow(clippy::too_many_arguments)]
fn refcount_tag_fields<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    union_layout: UnionLayout<'a>,
    field_layouts: &'a [Layout<'a>],
    structure: Symbol,
    tag_id: TagIdIntType,
    following: Stmt<'a>,
) -> Stmt<'a> {
    let mut stmt = following;

    for (i, field_layout) in field_layouts.iter().enumerate().rev() {
        if field_layout.contains_refcounted() {
            let field_val = root.create_symbol(ident_ids, &format!("field_{}_{}", tag_id, i));
            let field_val_expr = Expr::UnionAtIndex {
                union_layout,
                tag_id,
                index: i as u64,
                structure,
            };
            let field_val_stmt = |next| Stmt::Let(field_val, field_val_expr, *field_layout, next);

            let mod_unit = root.create_symbol(ident_ids, &format!("mod_field_{}_{}", tag_id, i));
            let mod_args = refcount_args(root, ctx, field_val);
            let mod_expr = root
                .call_specialized_op(ident_ids, ctx, *field_layout, mod_args)
                .unwrap();
            let mod_stmt = |next| Stmt::Let(mod_unit, mod_expr, LAYOUT_UNIT, next);

            stmt = field_val_stmt(root.arena.alloc(
                //
                mod_stmt(root.arena.alloc(
                    //
                    stmt,
                )),
            ))
        }
    }

    stmt
}
