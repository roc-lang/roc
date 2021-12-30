use roc_builtins::bitcode::IntWidth;
use roc_module::low_level::{LowLevel, LowLevel::*};
use roc_module::symbol::{IdentIds, Symbol};

use crate::code_gen_help::let_lowlevel;
use crate::ir::{
    BranchInfo, Call, CallType, Expr, JoinPointId, Literal, Param, Stmt, UpdateModeId,
};
use crate::layout::{Builtin, Layout, UnionLayout};

use super::{CodeGenHelp, Context, HelperOp};

const LAYOUT_BOOL: Layout = Layout::Builtin(Builtin::Bool);
const LAYOUT_UNIT: Layout = Layout::Struct(&[]);
const LAYOUT_PTR: Layout = Layout::RecursivePointer;
const LAYOUT_U32: Layout = Layout::Builtin(Builtin::Int(IntWidth::U32));

const ARG_1: Symbol = Symbol::ARG_1;
const ARG_2: Symbol = Symbol::ARG_2;

pub fn refcount_generic<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout: Layout<'a>,
) -> Stmt<'a> {
    debug_assert!(is_rc_implemented_yet(&layout));
    let rc_todo = || todo!("Please update is_rc_implemented_yet for `{:?}`", layout);

    match layout {
        Layout::Builtin(Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal) => {
            unreachable!("Not refcounted: {:?}", layout)
        }
        Layout::Builtin(Builtin::Str) => refcount_str(root, ident_ids, ctx),
        Layout::Builtin(Builtin::List(elem_layout)) => {
            refcount_list(root, ident_ids, ctx, &layout, elem_layout)
        }
        Layout::Builtin(Builtin::Dict(_, _) | Builtin::Set(_)) => rc_todo(),
        Layout::Struct(_) => rc_todo(),
        Layout::Union(_) => rc_todo(),
        Layout::LambdaSet(_) => {
            unreachable!("Refcounting on LambdaSet is invalid. Should be a Union at runtime.")
        }
        Layout::RecursivePointer => rc_todo(),
    }
}

// Check if refcounting is implemented yet. In the long term, this will be deleted.
// In the short term, it helps us to skip refcounting and let it leak, so we can make
// progress incrementally. Kept in sync with generate_procs using assertions.
pub fn is_rc_implemented_yet(layout: &Layout) -> bool {
    matches!(layout, Layout::Builtin(Builtin::Str | Builtin::List(_)))
}

fn return_unit<'a>(root: &CodeGenHelp<'a>, ident_ids: &mut IdentIds) -> Stmt<'a> {
    let unit = root.create_symbol(ident_ids, "unit");
    let ret_stmt = root.arena.alloc(Stmt::Ret(unit));
    Stmt::Let(unit, Expr::Struct(&[]), LAYOUT_UNIT, ret_stmt)
}

fn refcount_args<'a>(root: &CodeGenHelp<'a>, ctx: &Context<'a>, structure: Symbol) -> &'a [Symbol] {
    if ctx.op == HelperOp::Inc {
        // second argument is always `amount`, passed down through the call stack
        root.arena.alloc([structure, ARG_2])
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

    // Pointer size constant
    let ptr_size_sym = root.create_symbol(ident_ids, "ptr_size");
    let ptr_size_expr = Expr::Literal(Literal::Int(root.ptr_size as i128));
    let ptr_size_stmt = |next| Stmt::Let(ptr_size_sym, ptr_size_expr, root.layout_isize, next);

    // Refcount address
    let rc_addr_sym = root.create_symbol(ident_ids, "rc_addr");
    let rc_addr_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::NumSub,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: root.arena.alloc([addr_sym, ptr_size_sym]),
    });
    let rc_addr_stmt = |next| Stmt::Let(rc_addr_sym, rc_addr_expr, root.layout_isize, next);

    // Typecast the refcount address from integer to pointer
    let rc_ptr_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::PtrCast,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: root.arena.alloc([rc_addr_sym]),
    });
    let rc_ptr_stmt = |next| Stmt::Let(rc_ptr_sym, rc_ptr_expr, LAYOUT_PTR, next);

    addr_stmt(root.arena.alloc(
        //
        ptr_size_stmt(root.arena.alloc(
            //
            rc_addr_stmt(root.arena.alloc(
                //
                rc_ptr_stmt(root.arena.alloc(
                    //
                    following,
                )),
            )),
        )),
    ))
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
                arguments: root.arena.alloc([rc_ptr, ARG_2]),
            });
            Stmt::Let(zig_call_result, zig_call_expr, LAYOUT_UNIT, following)
        }

        HelperOp::Dec | HelperOp::DecRef => {
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
    let string = ARG_1;
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
    let alignment = root.ptr_size;

    let ret_unit_stmt = return_unit(root, ident_ids);
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
            root.arena.alloc(return_unit(root, ident_ids)),
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
    let len_stmt = |next| let_lowlevel(arena, layout_isize, len, ListLen, &[ARG_1], next);

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
        structure: ARG_1,
    };
    let elements_stmt = |next| Stmt::Let(elements, elements_expr, box_layout, next);

    //
    // modify refcount of the list and its elements
    //

    let rc_ptr = root.create_symbol(ident_ids, "rc_ptr");
    let alignment = layout.alignment_bytes(root.ptr_size);

    let modify_elems = if elem_layout.is_refcounted() {
        refcount_list_elems(
            root,
            ident_ids,
            ctx,
            elem_layout,
            LAYOUT_UNIT,
            box_union_layout,
            len,
            elements,
        )
    } else {
        return_unit(root, ident_ids)
    };
    let modify_list = modify_refcount(
        root,
        ident_ids,
        ctx,
        rc_ptr,
        alignment,
        arena.alloc(modify_elems),
    );
    let modify_list_and_elems = elements_stmt(arena.alloc(
        //
        rc_ptr_from_data_ptr(root, ident_ids, elements, rc_ptr, arena.alloc(modify_list)),
    ));

    //
    // Do nothing if the list is empty
    //

    let if_stmt = Stmt::Switch {
        cond_symbol: is_empty,
        cond_layout: LAYOUT_BOOL,
        branches: root
            .arena
            .alloc([(1, BranchInfo::None, return_unit(root, ident_ids))]),
        default_branch: (BranchInfo::None, root.arena.alloc(modify_list_and_elems)),
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
    let size = root.create_symbol(ident_ids, "size");
    let size_expr = Expr::Literal(Literal::Int(elem_layout.stack_size(root.ptr_size) as i128));
    let size_stmt = |next| Stmt::Let(size, size_expr, layout_isize, next);

    // let list_size = len * size
    let list_size = root.create_symbol(ident_ids, "list_size");
    let list_size_stmt = |next| {
        let_lowlevel(
            arena,
            layout_isize,
            list_size,
            NumMul,
            &[length, size],
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
    let next_addr_stmt =
        |next| let_lowlevel(arena, layout_isize, next_addr, NumAdd, &[addr, size], next);

    //
    // Control flow
    //

    let is_end = root.create_symbol(ident_ids, "is_end");
    let is_end_stmt = |next| let_lowlevel(arena, LAYOUT_BOOL, is_end, NumGte, &[addr, end], next);

    let if_end_of_list = Stmt::Switch {
        cond_symbol: is_end,
        cond_layout: LAYOUT_BOOL,
        ret_layout,
        branches: root
            .arena
            .alloc([(1, BranchInfo::None, return_unit(root, ident_ids))]),
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
        size_stmt(arena.alloc(
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
