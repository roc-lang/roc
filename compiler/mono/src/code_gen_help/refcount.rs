use roc_builtins::bitcode::IntWidth;
use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, Symbol};

use crate::ir::{BranchInfo, Call, CallType, Expr, Literal, Stmt, UpdateModeId};
use crate::layout::{Builtin, Layout};

use super::{CodeGenHelp, Context, HelperOp};

const LAYOUT_BOOL: Layout = Layout::Builtin(Builtin::Bool);
const LAYOUT_UNIT: Layout = Layout::Struct(&[]);
const LAYOUT_PTR: Layout = Layout::RecursivePointer;
const LAYOUT_U32: Layout = Layout::Builtin(Builtin::Int(IntWidth::U32));

const ARG_1: Symbol = Symbol::ARG_1;
const ARG_2: Symbol = Symbol::ARG_2;

pub fn refcount_generic<'a>(
    root: &CodeGenHelp<'a>,
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
        Layout::Builtin(Builtin::Dict(_, _) | Builtin::Set(_) | Builtin::List(_)) => rc_todo(),
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
    matches!(layout, Layout::Builtin(Builtin::Str))
}

fn return_unit<'a>(root: &CodeGenHelp<'a>, ident_ids: &mut IdentIds) -> Stmt<'a> {
    let unit = root.create_symbol(ident_ids, "unit");
    let ret_stmt = root.arena.alloc(Stmt::Ret(unit));
    Stmt::Let(unit, Expr::Struct(&[]), LAYOUT_UNIT, ret_stmt)
}

// Subtract a constant from a pointer to find the refcount
// Also does some type casting, so that we have different Symbols and Layouts
// for the 'pointer' and 'integer' versions of the address.
// This helps to avoid issues with the backends Symbol->Layout mapping.
pub fn rc_ptr_from_struct<'a>(
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
        arguments: root.arena.alloc([structure, ptr_size_sym]),
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

/// Generate a procedure to modify the reference count of a Str
fn refcount_str<'a>(
    root: &CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
) -> Stmt<'a> {
    let op = ctx.op;

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

    // Alignment constant (same value as ptr_size but different layout)
    let alignment = root.create_symbol(ident_ids, "alignment");
    let alignment_expr = Expr::Literal(Literal::Int(root.ptr_size as i128));
    let alignment_stmt = |next| Stmt::Let(alignment, alignment_expr, LAYOUT_U32, next);

    // Call the relevant Zig lowlevel to actually modify the refcount
    let zig_call_result = root.create_symbol(ident_ids, "zig_call_result");
    let zig_call_expr = match op {
        HelperOp::Inc => Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::RefCountInc,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: root.arena.alloc([rc_ptr, ARG_2]),
        }),
        HelperOp::Dec | HelperOp::DecRef => Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::RefCountDec,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: root.arena.alloc([rc_ptr, alignment]),
        }),
        _ => unreachable!(),
    };
    let zig_call_stmt = |next| Stmt::Let(zig_call_result, zig_call_expr, LAYOUT_UNIT, next);

    // Generate an `if` to skip small strings but modify big strings
    let then_branch = elements_stmt(root.arena.alloc(
        //
        rc_ptr_from_struct(
            root,
            ident_ids,
            elements,
            rc_ptr,
            root.arena.alloc(
                //
                alignment_stmt(root.arena.alloc(
                    //
                    zig_call_stmt(root.arena.alloc(
                        //
                        Stmt::Ret(zig_call_result),
                    )),
                )),
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
