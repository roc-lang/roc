#![allow(clippy::too_many_arguments)]

use bumpalo::collections::vec::Vec;
use bumpalo::collections::CollectIn;
use roc_error_macros::todo_lambda_erasure;
use roc_module::low_level::{LowLevel, LowLevel::*};
use roc_module::symbol::{IdentIds, Symbol};

use crate::code_gen_help::let_lowlevel;
use crate::ir::{
    BranchInfo, Call, CallType, Expr, JoinPointId, Literal, ModifyRc, Param, Stmt, UpdateModeId,
};
use crate::layout::{
    Builtin, InLayout, Layout, LayoutInterner, LayoutRepr, STLayoutInterner, TagIdIntType,
    UnionLayout,
};

use super::{CodeGenHelp, Context, HelperOp};

const LAYOUT_BOOL: InLayout = Layout::BOOL;
const LAYOUT_UNIT: InLayout = Layout::UNIT;
const LAYOUT_U32: InLayout = Layout::U32;

pub fn refcount_stmt<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    layout: InLayout<'a>,
    modify: &ModifyRc,
    following: &'a Stmt<'a>,
) -> &'a Stmt<'a> {
    let arena = root.arena;

    match modify {
        ModifyRc::Inc(structure, amount) => {
            let layout_isize = root.layout_isize;

            // Define a constant for the amount to increment
            let amount_sym = root.create_symbol(ident_ids, "amount");
            let amount_expr = Expr::Literal(Literal::Int((*amount as i128).to_ne_bytes()));
            let amount_stmt = |next| Stmt::Let(amount_sym, amount_expr, layout_isize, next);

            // Call helper proc, passing the Roc structure and constant amount
            let call_result_empty = root.create_symbol(ident_ids, "call_result_empty");
            let call_expr = root
                .call_specialized_op(
                    ident_ids,
                    ctx,
                    layout_interner,
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
                .call_specialized_op(
                    ident_ids,
                    ctx,
                    layout_interner,
                    layout,
                    arena.alloc([*structure]),
                )
                .unwrap();
            let call_stmt = Stmt::Let(call_result_empty, call_expr, LAYOUT_UNIT, following);
            arena.alloc(call_stmt)
        }

        ModifyRc::DecRef(structure) => {
            match layout_interner.get_repr(layout) {
                // Str has no children, so Dec is the same as DecRef.
                // List has children, but it is dealt with by LowLevel::ListDecRef.
                // It correctly handles both Dec and DecRef
                LayoutRepr::Builtin(Builtin::Str | Builtin::List(_)) => {
                    ctx.op = HelperOp::Dec;
                    refcount_stmt(
                        root,
                        ident_ids,
                        ctx,
                        layout_interner,
                        layout,
                        &ModifyRc::Dec(*structure),
                        following,
                    )
                }

                // Struct and non-recursive Unions are stack-only, so DecRef is a no-op
                LayoutRepr::Struct { .. } => following,
                LayoutRepr::Union(UnionLayout::NonRecursive(_)) => following,

                // Inline the refcounting code instead of making a function. Don't iterate fields,
                // and replace any return statements with jumps to the `following` statement.
                _ => match ctx.op {
                    HelperOp::DecRef(jp_decref) => {
                        let rc_stmt = refcount_generic(
                            root,
                            ident_ids,
                            ctx,
                            layout_interner,
                            layout,
                            *structure,
                        );
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
        ModifyRc::Free(_) => {
            unreachable!("free should be handled by the backend directly")
        }
    }
}

pub fn refcount_indirect<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    element_layout: InLayout<'a>,
    structure: Symbol,
) -> Stmt<'a> {
    let arena = root.arena;

    let unit = root.create_symbol(ident_ids, "unit");
    let loaded = root.create_symbol(ident_ids, "loaded");

    let indirect_op = ctx.op;
    let direct_op = match ctx.op {
        HelperOp::IndirectIncN => HelperOp::IncN,
        HelperOp::IndirectInc => HelperOp::Inc,
        HelperOp::IndirectDec => HelperOp::Dec,
        _ => unreachable!(),
    };

    // we've done the indirection, the inner value shoud be inc- or decremented directly
    ctx.op = direct_op;

    let mod_args = refcount_args(root, ctx, loaded);
    let opt_mod_expr =
        root.call_specialized_op(ident_ids, ctx, layout_interner, element_layout, mod_args);

    // set the op back to indirect ; this is important for correct layout generation
    ctx.op = indirect_op;

    if let Some(mod_expr) = opt_mod_expr {
        Stmt::Let(
            loaded,
            Expr::ptr_load(arena.alloc(structure)),
            element_layout,
            arena.alloc(
                //
                Stmt::Let(
                    unit,
                    mod_expr,
                    Layout::UNIT,
                    arena.alloc(
                        //
                        Stmt::Ret(unit),
                    ),
                ),
            ),
        )
    } else {
        rc_return_stmt(root, ident_ids, ctx)
    }
}

pub fn refcount_generic<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    layout: InLayout<'a>,
    structure: Symbol,
) -> Stmt<'a> {
    match layout_interner.get_repr(layout) {
        LayoutRepr::Builtin(
            Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal,
        )
        | LayoutRepr::FunctionPointer(_) => {
            // Generate a dummy function that immediately returns Unit
            // Some higher-order Zig builtins *always* call an RC function on List elements.
            rc_return_stmt(root, ident_ids, ctx)
        }
        LayoutRepr::Builtin(Builtin::Str) => refcount_str(root, ident_ids, ctx, structure),
        LayoutRepr::Builtin(Builtin::List(element_layout)) => refcount_list(
            root,
            ident_ids,
            ctx,
            layout_interner,
            element_layout,
            structure,
        ),
        LayoutRepr::Struct(field_layouts) => refcount_struct(
            root,
            ident_ids,
            ctx,
            layout_interner,
            field_layouts,
            structure,
        ),
        LayoutRepr::Union(union_layout) => refcount_union(
            root,
            ident_ids,
            ctx,
            layout_interner,
            layout,
            union_layout,
            structure,
        ),
        LayoutRepr::LambdaSet(lambda_set) => {
            let runtime_layout = lambda_set.representation;
            refcount_generic(
                root,
                ident_ids,
                ctx,
                layout_interner,
                runtime_layout,
                structure,
            )
        }
        LayoutRepr::Erased(_) => {
            todo_lambda_erasure!()
        }
        LayoutRepr::RecursivePointer(_) => unreachable!(
            "We should never call a refcounting helper on a RecursivePointer layout directly"
        ),
        LayoutRepr::Ptr(_) => {
            unreachable!("We should never call a refcounting helper on a Ptr layout directly")
        }
    }
}

pub fn refcount_reset_proc_body<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    layout: InLayout<'a>,
    structure: Symbol,
) -> Stmt<'a> {
    let rc_ptr = root.create_symbol(ident_ids, "rc_ptr");
    let rc = root.create_symbol(ident_ids, "rc");
    let refcount_1 = root.create_symbol(ident_ids, "refcount_1");
    let is_unique = root.create_symbol(ident_ids, "is_unique");
    let addr = root.create_symbol(ident_ids, "addr");

    let union_layout = match layout_interner.get_repr(layout) {
        LayoutRepr::Union(u) => u,
        _ => unimplemented!("Reset is only implemented for UnionLayout"),
    };

    // Whenever we recurse into a child layout we will want to Decrement
    ctx.op = HelperOp::Dec;
    ctx.recursive_union = Some(union_layout);
    let recursion_ptr =
        layout_interner.insert_direct_no_semantic(LayoutRepr::RecursivePointer(layout));

    // Reset structure is unique. Decrement its children and return a pointer to the allocation.
    let then_stmt = {
        use UnionLayout::*;

        let tag_layouts;
        let mut null_id = None;
        match union_layout {
            NonRecursive(tags) => {
                tag_layouts = tags;
            }
            Recursive(tags) => {
                tag_layouts = tags;
            }
            NonNullableUnwrapped(field_layouts) => {
                tag_layouts = root.arena.alloc([field_layouts]);
            }
            NullableWrapped {
                other_tags: tags,
                nullable_id,
            } => {
                null_id = Some(nullable_id);
                tag_layouts = tags;
            }
            NullableUnwrapped {
                other_fields,
                nullable_id,
            } => {
                null_id = Some(nullable_id as TagIdIntType);
                tag_layouts = root.arena.alloc([other_fields]);
            }
        };

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

        let rc_contents_stmt = refcount_union_contents(
            root,
            ident_ids,
            ctx,
            layout_interner,
            union_layout,
            tag_layouts,
            null_id,
            structure,
            tag_id_sym,
            tag_id_layout,
            Stmt::Ret(addr),
        );

        tag_id_stmt(root.arena.alloc(
            //
            rc_contents_stmt,
        ))
    };

    // Reset structure is not unique. Decrement it and return a NULL pointer.
    let else_stmt = {
        let decrement_unit = root.create_symbol(ident_ids, "decrement_unit");
        let decrement_expr = root
            .call_specialized_op(
                ident_ids,
                ctx,
                layout_interner,
                layout,
                root.arena.alloc([structure]),
            )
            .unwrap();
        let decrement_stmt = |next| Stmt::Let(decrement_unit, decrement_expr, LAYOUT_UNIT, next);

        // Null pointer with union layout
        let null = root.create_symbol(ident_ids, "null");
        let null_stmt = |next| Stmt::Let(null, Expr::NullPointer, layout, next);

        decrement_stmt(root.arena.alloc(
            //
            null_stmt(root.arena.alloc(
                //
                Stmt::Ret(null),
            )),
        ))
    };

    let if_stmt = Stmt::Switch {
        cond_symbol: is_unique,
        cond_layout: LAYOUT_BOOL,
        branches: root.arena.alloc([(1, BranchInfo::None, then_stmt)]),
        default_branch: (BranchInfo::None, root.arena.alloc(else_stmt)),
        ret_layout: layout,
    };

    // Uniqueness test
    let is_unique_stmt = {
        let_lowlevel(
            root.arena,
            LAYOUT_BOOL,
            is_unique,
            Eq,
            &[rc, refcount_1],
            root.arena.alloc(if_stmt),
        )
    };

    // Constant for unique refcount
    let refcount_1_encoded = (1i128).to_ne_bytes();
    let refcount_1_expr = Expr::Literal(Literal::Int(refcount_1_encoded));
    let refcount_1_stmt = Stmt::Let(
        refcount_1,
        refcount_1_expr,
        root.layout_isize,
        root.arena.alloc(is_unique_stmt),
    );

    // Refcount value
    let rc_expr = Expr::ptr_load(root.arena.alloc(rc_ptr));

    let rc_stmt = Stmt::Let(
        rc,
        rc_expr,
        root.layout_isize,
        root.arena.alloc(refcount_1_stmt),
    );

    let mask_lower_bits = match layout_interner.get_repr(layout) {
        LayoutRepr::Union(ul) => ul.stores_tag_id_in_pointer(root.target),
        _ => false,
    };

    // Refcount pointer
    let rc_ptr_stmt = {
        rc_ptr_from_data_ptr_help(
            root,
            ident_ids,
            structure,
            rc_ptr,
            mask_lower_bits,
            root.arena.alloc(rc_stmt),
            addr,
            recursion_ptr,
        )
    };

    rc_ptr_stmt
}

pub fn refcount_resetref_proc_body<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    layout: InLayout<'a>,
    structure: Symbol,
) -> Stmt<'a> {
    let rc_ptr = root.create_symbol(ident_ids, "rc_ptr");
    let rc = root.create_symbol(ident_ids, "rc");
    let refcount_1 = root.create_symbol(ident_ids, "refcount_1");
    let is_unique = root.create_symbol(ident_ids, "is_unique");
    let addr = root.create_symbol(ident_ids, "addr");

    let union_layout = match layout_interner.get_repr(layout) {
        LayoutRepr::Union(u) => u,
        _ => unimplemented!("Resetref is only implemented for UnionLayout"),
    };

    // Whenever we recurse into a child layout we will want to Decrement
    ctx.op = HelperOp::Dec;
    ctx.recursive_union = Some(union_layout);
    let recursion_ptr =
        layout_interner.insert_direct_no_semantic(LayoutRepr::RecursivePointer(layout));

    // Reset structure is unique. Return a pointer to the allocation.
    let then_stmt = Stmt::Ret(addr);

    // Reset structure is not unique. Decrement it and return a NULL pointer.
    let else_stmt = {
        // Set up the context for a decref.
        let jp_decref = JoinPointId(root.create_symbol(ident_ids, "jp_decref"));
        ctx.op = HelperOp::DecRef(jp_decref);

        // Generate the decref code.
        let rc_stmt = refcount_generic(root, ident_ids, ctx, layout_interner, layout, structure);

        // Null pointer with union layout
        let null = root.create_symbol(ident_ids, "null");
        let null_stmt = |next| Stmt::Let(null, Expr::NullPointer, layout, next);

        // Inline the refcounting code instead of making a function. Don't iterate fields,
        // and replace any return statements with jumps to the `following` statement.
        let join = Stmt::Join {
            id: jp_decref,
            parameters: &[],
            body: root.arena.alloc(root.arena.alloc(
                //
                null_stmt(root.arena.alloc(
                    //
                    Stmt::Ret(null),
                )),
            )),
            remainder: root.arena.alloc(rc_stmt),
        };

        root.arena.alloc(join)
    };

    let if_stmt = Stmt::if_then_else(
        root.arena,
        is_unique,
        layout,
        then_stmt,
        root.arena.alloc(else_stmt),
    );

    // Uniqueness test
    let is_unique_stmt = {
        let_lowlevel(
            root.arena,
            LAYOUT_BOOL,
            is_unique,
            Eq,
            &[rc, refcount_1],
            root.arena.alloc(if_stmt),
        )
    };

    // Constant for unique refcount
    let refcount_1_encoded = (1i128).to_ne_bytes();
    let refcount_1_expr = Expr::Literal(Literal::Int(refcount_1_encoded));
    let refcount_1_stmt = Stmt::Let(
        refcount_1,
        refcount_1_expr,
        root.layout_isize,
        root.arena.alloc(is_unique_stmt),
    );

    // Refcount value
    let rc_expr = Expr::ptr_load(root.arena.alloc(rc_ptr));

    let rc_stmt = Stmt::Let(
        rc,
        rc_expr,
        root.layout_isize,
        root.arena.alloc(refcount_1_stmt),
    );

    let mask_lower_bits = match layout_interner.get_repr(layout) {
        LayoutRepr::Union(ul) => ul.stores_tag_id_in_pointer(root.target),
        _ => false,
    };

    // Refcount pointer
    let rc_ptr_stmt = {
        rc_ptr_from_data_ptr_help(
            root,
            ident_ids,
            structure,
            rc_ptr,
            mask_lower_bits,
            root.arena.alloc(rc_stmt),
            addr,
            recursion_ptr,
        )
    };

    rc_ptr_stmt
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
    if matches!(ctx.op, HelperOp::IncN | HelperOp::IndirectIncN) {
        // second argument is always `amount`, passed down through the call stack
        root.arena.alloc([structure, Symbol::ARG_2])
    } else {
        root.arena.alloc([structure])
    }
}

fn rc_ptr_from_data_ptr_help<'a>(
    root: &CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    structure: Symbol,
    rc_ptr_sym: Symbol,
    mask_lower_bits: bool,
    following: &'a Stmt<'a>,
    addr_sym: Symbol,
    recursion_ptr: InLayout<'a>,
) -> Stmt<'a> {
    // symbol of a pointer with any tag id bits cleared
    let cleared_sym = if mask_lower_bits {
        root.create_symbol(ident_ids, "cleared")
    } else {
        structure
    };

    let clear_tag_id_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::PtrClearTagId,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: root.arena.alloc([structure]),
    });
    let clear_tag_id_stmt =
        |next| Stmt::Let(cleared_sym, clear_tag_id_expr, root.layout_isize, next);

    // Typecast the structure pointer to an integer
    // Backends expect a number Layout to choose the right "subtract" instruction
    let as_int_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::PtrCast,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: root.arena.alloc([cleared_sym]),
    });
    let as_int_stmt = |next| Stmt::Let(addr_sym, as_int_expr, root.layout_isize, next);

    // Pointer size constant
    let ptr_size_sym = root.create_symbol(ident_ids, "ptr_size");
    let ptr_size_expr = Expr::Literal(Literal::Int(
        (root.target.ptr_width() as i128).to_ne_bytes(),
    ));
    let ptr_size_stmt = |next| Stmt::Let(ptr_size_sym, ptr_size_expr, root.layout_isize, next);

    // Refcount address
    let rc_addr_sym = root.create_symbol(ident_ids, "rc_addr");
    let sub_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::NumSubSaturated,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: root.arena.alloc([addr_sym, ptr_size_sym]),
    });
    let sub_stmt = |next| Stmt::Let(rc_addr_sym, sub_expr, Layout::usize(root.target), next);

    // Typecast the refcount address from integer to pointer
    let cast_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::PtrCast,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: root.arena.alloc([rc_addr_sym]),
    });
    let cast_stmt = |next| Stmt::Let(rc_ptr_sym, cast_expr, recursion_ptr, next);

    let body = as_int_stmt(root.arena.alloc(
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
    ));

    if mask_lower_bits {
        clear_tag_id_stmt(root.arena.alloc(body))
    } else {
        body
    }
}

enum Pointer {
    ToData(Symbol),
    #[allow(unused)]
    ToRefcount(Symbol),
}

fn modify_refcount<'a>(
    root: &CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    ptr: Pointer,
    alignment: u32,
    following: &'a Stmt<'a>,
) -> Stmt<'a> {
    // Call the relevant Zig lowlevel to actually modify the refcount
    let zig_call_result = root.create_symbol(ident_ids, "zig_call_result");
    match ctx.op {
        HelperOp::Inc => {
            let (op, ptr) = match ptr {
                Pointer::ToData(s) => (LowLevel::RefCountIncDataPtr, s),
                Pointer::ToRefcount(s) => (LowLevel::RefCountIncRcPtr, s),
            };

            let amount_sym = root.create_symbol(ident_ids, "amount");
            let amount_expr = Expr::Literal(Literal::Int(1_i128.to_ne_bytes()));
            let amount_stmt = |next| Stmt::Let(amount_sym, amount_expr, root.layout_isize, next);

            let zig_call_expr = Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: root.arena.alloc([ptr, amount_sym]),
            });
            let zig_call_stmt = Stmt::Let(zig_call_result, zig_call_expr, LAYOUT_UNIT, following);

            amount_stmt(root.arena.alloc(
                //
                zig_call_stmt,
            ))
        }

        HelperOp::IncN => {
            let (op, ptr) = match ptr {
                Pointer::ToData(s) => (LowLevel::RefCountIncDataPtr, s),
                Pointer::ToRefcount(s) => (LowLevel::RefCountIncRcPtr, s),
            };

            let zig_call_expr = Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: root.arena.alloc([ptr, Symbol::ARG_2]),
            });
            Stmt::Let(zig_call_result, zig_call_expr, LAYOUT_UNIT, following)
        }

        HelperOp::Dec | HelperOp::DecRef(_) => {
            debug_assert!(alignment >= root.target.ptr_width() as u32);

            let (op, ptr) = match ptr {
                Pointer::ToData(s) => (LowLevel::RefCountDecDataPtr, s),
                Pointer::ToRefcount(s) => (LowLevel::RefCountDecRcPtr, s),
            };

            let alignment_sym = root.create_symbol(ident_ids, "alignment");
            let alignment_expr = Expr::Literal(Literal::Int((alignment as i128).to_ne_bytes()));
            let alignment_stmt = |next| Stmt::Let(alignment_sym, alignment_expr, LAYOUT_U32, next);

            // This function is not used for lists, so this is always false.
            let elements_refcounted_sym = root.create_symbol(ident_ids, "elements_refcounted");
            let elements_refcounted_expr = Expr::Literal(Literal::Bool(false));
            let elements_refcounted_stmt = |next| {
                Stmt::Let(
                    elements_refcounted_sym,
                    elements_refcounted_expr,
                    LAYOUT_BOOL,
                    next,
                )
            };

            let zig_call_expr = Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: root
                    .arena
                    .alloc([ptr, alignment_sym, elements_refcounted_sym]),
            });
            let zig_call_stmt = Stmt::Let(zig_call_result, zig_call_expr, LAYOUT_UNIT, following);

            alignment_stmt(root.arena.alloc(
                //
                elements_refcounted_stmt(root.arena.alloc(
                    //
                    zig_call_stmt,
                )),
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
    string: Symbol,
) -> Stmt<'a> {
    let arena = root.arena;
    let layout_isize = root.layout_isize;
    let field_layouts = arena.alloc([Layout::OPAQUE_PTR, layout_isize, layout_isize]);

    // Get the last word as a signed int
    let last_word = root.create_symbol(ident_ids, "last_word");
    let last_word_expr = Expr::StructAtIndex {
        index: 2,
        field_layouts,
        structure: string,
    };
    let last_word_stmt = |next| Stmt::Let(last_word, last_word_expr, layout_isize, next);

    // Zero
    let zero = root.create_symbol(ident_ids, "zero");
    let zero_expr = Expr::Literal(Literal::Int(0i128.to_ne_bytes()));
    let zero_stmt = |next| Stmt::Let(zero, zero_expr, layout_isize, next);

    // is_big_str = (last_word >= 0);
    // Treat last word as isize so that the small string flag is the same as the sign bit
    // (assuming a little-endian target, where the sign bit is in the last byte of the word)
    let is_big_str = root.create_symbol(ident_ids, "is_big_str");
    let is_big_str_stmt = |next| {
        let_lowlevel(
            arena,
            LAYOUT_BOOL,
            is_big_str,
            NumGte,
            &[last_word, zero],
            next,
        )
    };

    //
    // Check for seamless slice
    //

    // Get the length field as a signed int
    let length = root.create_symbol(ident_ids, "length");
    let length_expr = Expr::StructAtIndex {
        index: 1,
        field_layouts,
        structure: string,
    };
    let length_stmt = |next| Stmt::Let(length, length_expr, layout_isize, next);

    let alignment = root.target.ptr_width() as u32;

    // let is_slice = lowlevel NumLt length zero
    let is_slice = root.create_symbol(ident_ids, "is_slice");
    let is_slice_stmt =
        |next| let_lowlevel(arena, LAYOUT_BOOL, is_slice, NumLt, &[length, zero], next);

    //
    // Branch on seamless slice vs "real" string
    //

    let return_unit = arena.alloc(rc_return_stmt(root, ident_ids, ctx));

    let one = root.create_symbol(ident_ids, "one");
    let one_expr = Expr::Literal(Literal::Int(1i128.to_ne_bytes()));
    let one_stmt = |next| Stmt::Let(one, one_expr, layout_isize, next);

    let data_ptr_int = root.create_symbol(ident_ids, "data_ptr_int");
    let data_ptr_int_stmt = |next| {
        let_lowlevel(
            arena,
            layout_isize,
            data_ptr_int,
            PtrCast,
            &[last_word],
            next,
        )
    };

    let data_ptr = root.create_symbol(ident_ids, "data_ptr");
    let data_ptr_stmt = |next| {
        let_lowlevel(
            arena,
            layout_isize,
            data_ptr,
            NumShiftLeftBy,
            &[data_ptr_int, one],
            next,
        )
    };

    // when the string is a slice, the capacity field is a pointer to the refcount
    let slice_branch = one_stmt(arena.alloc(
        //
        data_ptr_int_stmt(arena.alloc(
            //
            data_ptr_stmt(arena.alloc(
                //
                modify_refcount(
                    root,
                    ident_ids,
                    ctx,
                    Pointer::ToData(data_ptr),
                    alignment,
                    return_unit,
                ),
            )),
        )),
    ));

    // Characters pointer for a real string
    let string_chars = root.create_symbol(ident_ids, "string_chars");
    let string_chars_expr = Expr::StructAtIndex {
        index: 0,
        field_layouts,
        structure: string,
    };
    let string_chars_stmt = |next| Stmt::Let(string_chars, string_chars_expr, layout_isize, next);

    let modify_refcount_stmt = modify_refcount(
        root,
        ident_ids,
        ctx,
        Pointer::ToData(string_chars),
        alignment,
        return_unit,
    );

    let string_branch = arena.alloc(
        //
        string_chars_stmt(arena.alloc(
            //
            modify_refcount_stmt,
        )),
    );

    let if_slice = Stmt::if_then_else(
        root.arena,
        is_slice,
        Layout::UNIT,
        slice_branch,
        string_branch,
    );

    //
    // JoinPoint for slice vs list
    //

    let modify_stmt = length_stmt(arena.alloc(
        //
        is_slice_stmt(arena.alloc(
            //
            if_slice,
        )),
    ));

    let if_big_stmt = Stmt::if_then_else(
        root.arena,
        is_big_str,
        Layout::UNIT,
        modify_stmt,
        root.arena.alloc(rc_return_stmt(root, ident_ids, ctx)),
    );

    // Combine the statements in sequence
    last_word_stmt(arena.alloc(
        //
        zero_stmt(arena.alloc(
            //
            is_big_str_stmt(arena.alloc(
                //
                if_big_stmt,
            )),
        )),
    ))
}

fn refcount_list<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    element_layout: InLayout<'a>,
    list: Symbol,
) -> Stmt<'a> {
    let arena = root.arena;

    let ret_stmt = arena.alloc(rc_return_stmt(root, ident_ids, ctx));

    let rc_list_expr = match ctx.op {
        HelperOp::IncN | HelperOp::Inc => {
            let rc_list_args = refcount_args(root, ctx, list);
            Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op: LowLevel::ListIncref,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: rc_list_args,
            })
        }
        HelperOp::DecRef(_) | HelperOp::Dec => {
            let (rc_sym, linker_data) = root.gen_refcount_proc(
                ident_ids,
                layout_interner,
                element_layout,
                HelperOp::IndirectDec,
            );
            ctx.new_linker_data.extend_from_slice(&linker_data);
            Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op: LowLevel::ListDecref,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: root.arena.alloc([list, rc_sym]),
            })
        }
        _ => unreachable!(),
    };
    let rc_list_unit = root.create_symbol(ident_ids, "rc_list");
    Stmt::Let(rc_list_unit, rc_list_expr, LAYOUT_UNIT, ret_stmt)
}

fn refcount_struct<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    field_layouts: &'a [InLayout<'a>],
    structure: Symbol,
) -> Stmt<'a> {
    let mut stmt = rc_return_stmt(root, ident_ids, ctx);

    for (i, field_layout) in field_layouts.iter().enumerate().rev() {
        if layout_interner.contains_refcounted(*field_layout) {
            let field_val = root.create_symbol(ident_ids, &format!("field_val_{i}"));
            let field_val_expr = Expr::StructAtIndex {
                index: i as u64,
                field_layouts,
                structure,
            };
            let field_val_stmt = |next| Stmt::Let(field_val, field_val_expr, *field_layout, next);

            let mod_unit = root.create_symbol(ident_ids, &format!("mod_field_{i}"));
            let mod_args = refcount_args(root, ctx, field_val);
            let mod_expr = root
                .call_specialized_op(ident_ids, ctx, layout_interner, *field_layout, mod_args)
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
    layout_interner: &mut STLayoutInterner<'a>,
    union_in_layout: InLayout<'a>,
    union: UnionLayout<'a>,
    structure: Symbol,
) -> Stmt<'a> {
    use UnionLayout::*;

    let parent_rec_ptr_layout = ctx.recursive_union;
    if !matches!(union, NonRecursive(_)) {
        ctx.recursive_union = Some(union);
    }

    let body = match union {
        NonRecursive(tags) => refcount_union_nonrec(
            root,
            ident_ids,
            ctx,
            layout_interner,
            union,
            tags,
            structure,
        ),

        Recursive(tags) => {
            let tailrec_idx = root.union_tail_recursion_fields(union_in_layout, union);
            if let (Some(tail_idx), true) = (tailrec_idx, ctx.op.is_dec()) {
                refcount_union_tailrec(
                    root,
                    ident_ids,
                    ctx,
                    layout_interner,
                    union,
                    tags,
                    None,
                    tail_idx,
                    structure,
                )
            } else {
                refcount_union_rec(
                    root,
                    ident_ids,
                    ctx,
                    layout_interner,
                    union,
                    tags,
                    None,
                    structure,
                )
            }
        }

        NonNullableUnwrapped(field_layouts) => {
            // We don't do tail recursion on NonNullableUnwrapped.
            // Its RecursionPointer is always nested inside a List, Option, or other sub-layout, since
            // a direct RecursionPointer is only possible if there's at least one non-recursive variant.
            // This nesting makes it harder to do tail recursion, so we just don't.
            let tags = root.arena.alloc([field_layouts]);
            refcount_union_rec(
                root,
                ident_ids,
                ctx,
                layout_interner,
                union,
                tags,
                None,
                structure,
            )
        }

        NullableWrapped {
            other_tags: tags,
            nullable_id,
        } => {
            let null_id = Some(nullable_id);
            let tailrec_idx = root.union_tail_recursion_fields(union_in_layout, union);
            if let (Some(tail_idx), true) = (tailrec_idx, ctx.op.is_dec()) {
                refcount_union_tailrec(
                    root,
                    ident_ids,
                    ctx,
                    layout_interner,
                    union,
                    tags,
                    null_id,
                    tail_idx,
                    structure,
                )
            } else {
                refcount_union_rec(
                    root,
                    ident_ids,
                    ctx,
                    layout_interner,
                    union,
                    tags,
                    null_id,
                    structure,
                )
            }
        }

        NullableUnwrapped {
            other_fields,
            nullable_id,
        } => {
            let null_id = Some(nullable_id as TagIdIntType);
            let tags = root.arena.alloc([other_fields]);
            let tailrec_idx = root.union_tail_recursion_fields(union_in_layout, union);
            if let (Some(tail_idx), true) = (tailrec_idx, ctx.op.is_dec()) {
                refcount_union_tailrec(
                    root,
                    ident_ids,
                    ctx,
                    layout_interner,
                    union,
                    tags,
                    null_id,
                    tail_idx,
                    structure,
                )
            } else {
                refcount_union_rec(
                    root,
                    ident_ids,
                    ctx,
                    layout_interner,
                    union,
                    tags,
                    null_id,
                    structure,
                )
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
    layout_interner: &mut STLayoutInterner<'a>,
    union_layout: UnionLayout<'a>,
    tag_layouts: &'a [&'a [InLayout<'a>]],
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

    if tag_layouts.is_empty() {
        continuation
    } else {
        let switch_stmt = refcount_union_contents(
            root,
            ident_ids,
            ctx,
            layout_interner,
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
}

fn refcount_union_contents<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    union_layout: UnionLayout<'a>,
    tag_layouts: &'a [&'a [InLayout<'a>]],
    null_id: Option<TagIdIntType>,
    structure: Symbol,
    tag_id_sym: Symbol,
    tag_id_layout: InLayout<'a>,
    next_stmt: Stmt<'a>,
) -> Stmt<'a> {
    let jp_contents_modified = JoinPointId(root.create_symbol(ident_ids, "jp_contents_modified"));
    let mut tag_branches = Vec::with_capacity_in(tag_layouts.len() + 1, root.arena);

    if let Some(id) = null_id {
        let ret = rc_return_stmt(root, ident_ids, ctx);
        tag_branches.push((id as u64, BranchInfo::None, ret));
    };

    for (field_layouts, tag_id) in tag_layouts
        .iter()
        .zip((0..).filter(|tag_id| !matches!(null_id, Some(id) if tag_id == &id)))
    {
        // After refcounting the fields, jump to modify the union itself
        // (Order is important, to avoid use-after-free for Dec)
        let following = Stmt::Jump(jp_contents_modified, &[]);

        let field_layouts = field_layouts
            .iter()
            .copied()
            .enumerate()
            .collect_in::<Vec<_>>(root.arena)
            .into_bump_slice();

        let fields_stmt = refcount_tag_fields(
            root,
            ident_ids,
            ctx,
            layout_interner,
            union_layout,
            field_layouts,
            structure,
            tag_id,
            following,
        );

        tag_branches.push((tag_id as u64, BranchInfo::None, fields_stmt));
    }

    let default_stmt: Stmt<'a> = tag_branches.pop().unwrap().2;

    let tag_id_switch = Stmt::Switch {
        cond_symbol: tag_id_sym,
        cond_layout: tag_id_layout,
        branches: tag_branches.into_bump_slice(),
        default_branch: (BranchInfo::None, root.arena.alloc(default_stmt)),
        ret_layout: LAYOUT_UNIT,
    };

    if let UnionLayout::NonRecursive(_) = union_layout {
        Stmt::Join {
            id: jp_contents_modified,
            parameters: &[],
            body: root.arena.alloc(next_stmt),
            remainder: root.arena.alloc(tag_id_switch),
        }
    } else {
        let is_unique = root.create_symbol(ident_ids, "is_unique");

        let switch_with_unique_check = Stmt::if_then_else(
            root.arena,
            is_unique,
            Layout::UNIT,
            tag_id_switch,
            root.arena.alloc(Stmt::Jump(jp_contents_modified, &[])),
        );

        let switch_with_unique_check_and_let = let_lowlevel(
            root.arena,
            Layout::BOOL,
            is_unique,
            LowLevel::RefCountIsUnique,
            &[structure],
            root.arena.alloc(switch_with_unique_check),
        );

        Stmt::Join {
            id: jp_contents_modified,
            parameters: &[],
            body: root.arena.alloc(next_stmt),
            remainder: root.arena.alloc(switch_with_unique_check_and_let),
        }
    }
}

fn refcount_union_rec<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    union_layout: UnionLayout<'a>,
    tag_layouts: &'a [&'a [InLayout<'a>]],
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
        let alignment = LayoutRepr::Union(union_layout).allocation_alignment_bytes(layout_interner);
        let ret_stmt = rc_return_stmt(root, ident_ids, ctx);

        modify_refcount(
            root,
            ident_ids,
            ctx,
            Pointer::ToData(structure),
            alignment,
            root.arena.alloc(ret_stmt),
        )
    };

    let rc_contents_then_structure = if ctx.op.is_dec() {
        refcount_union_contents(
            root,
            ident_ids,
            ctx,
            layout_interner,
            union_layout,
            tag_layouts,
            null_id,
            structure,
            tag_id_sym,
            tag_id_layout,
            rc_structure_stmt,
        )
    } else {
        rc_structure_stmt
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
fn refcount_union_tailrec<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    union_layout: UnionLayout<'a>,
    tag_layouts: &'a [&'a [InLayout<'a>]],
    null_id: Option<TagIdIntType>,
    tailrec_indices: Vec<'a, Option<usize>>,
    initial_structure: Symbol,
) -> Stmt<'a> {
    let tailrec_loop = JoinPointId(root.create_symbol(ident_ids, "tailrec_loop"));
    let current = root.create_symbol(ident_ids, "current");
    let next_ptr = root.create_symbol(ident_ids, "next_ptr");
    let layout = layout_interner.insert_direct_no_semantic(LayoutRepr::Union(union_layout));

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

        let alignment = layout_interner.allocation_alignment_bytes(layout);
        modify_refcount(
            root,
            ident_ids,
            ctx,
            Pointer::ToData(current),
            alignment,
            root.arena.alloc(loop_or_exit_based_on_next_addr),
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

        for ((field_layouts, opt_tailrec_index), tag_id) in tag_layouts
            .iter()
            .zip(tailrec_indices)
            .zip((0..).filter(|tag_id| !matches!(null_id, Some(id) if tag_id == &id)))
        {
            // After refcounting the fields, jump to modify the union itself.
            // The loop param is a pointer to the next union. It gets passed through two jumps.
            let (non_tailrec_fields, jump_to_modify_union) =
                if let Some(tailrec_index) = opt_tailrec_index {
                    let mut filtered = Vec::with_capacity_in(field_layouts.len() - 1, root.arena);
                    let mut tail_stmt = None;
                    for (i, field) in field_layouts.iter().enumerate() {
                        if i != tailrec_index {
                            filtered.push((i, *field));
                        } else {
                            let field_val =
                                root.create_symbol(ident_ids, &format!("field_{tag_id}_{i}"));
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
                    let null = root.create_symbol(ident_ids, "null");
                    let null_stmt = |next| Stmt::Let(null, Expr::NullPointer, layout, next);

                    let tail_stmt = null_stmt(root.arena.alloc(
                        //
                        Stmt::Jump(jp_modify_union, root.arena.alloc([null])),
                    ));

                    let field_layouts = field_layouts
                        .iter()
                        .copied()
                        .enumerate()
                        .collect_in::<Vec<_>>(root.arena)
                        .into_bump_slice();

                    (field_layouts, tail_stmt)
                };

            let fields_stmt = refcount_tag_fields(
                root,
                ident_ids,
                ctx,
                layout_interner,
                union_layout,
                non_tailrec_fields,
                current,
                tag_id,
                jump_to_modify_union,
            );

            tag_branches.push((tag_id as u64, BranchInfo::None, fields_stmt));
        }

        let default_stmt: Stmt<'a> = tag_branches.pop().unwrap().2;

        let tag_id_switch = Stmt::Switch {
            cond_symbol: tag_id_sym,
            cond_layout: tag_id_layout,
            branches: tag_branches.into_bump_slice(),
            default_branch: (BranchInfo::None, root.arena.alloc(default_stmt)),
            ret_layout: LAYOUT_UNIT,
        };

        let is_unique = root.create_symbol(ident_ids, "is_unique");
        let null_pointer = root.create_symbol(ident_ids, "null_pointer");

        let jump_with_null_ptr = Stmt::Let(
            null_pointer,
            Expr::NullPointer,
            layout_interner.insert_direct_no_semantic(LayoutRepr::Union(union_layout)),
            root.arena.alloc(Stmt::Jump(
                jp_modify_union,
                root.arena.alloc([null_pointer]),
            )),
        );

        let switch_with_unique_check = Stmt::if_then_else(
            root.arena,
            is_unique,
            Layout::UNIT,
            tag_id_switch,
            root.arena.alloc(jump_with_null_ptr),
        );

        let switch_with_unique_check_and_let = let_lowlevel(
            root.arena,
            Layout::BOOL,
            is_unique,
            LowLevel::RefCountIsUnique,
            &[current],
            root.arena.alloc(switch_with_unique_check),
        );

        let jp_param = Param {
            symbol: next_ptr,
            layout,
        };

        Stmt::Join {
            id: jp_modify_union,
            parameters: root.arena.alloc([jp_param]),
            body: root.arena.alloc(rc_structure_stmt),
            remainder: root.arena.alloc(switch_with_unique_check_and_let),
        }
    };

    let loop_body = tag_id_stmt(root.arena.alloc(
        //
        rc_contents_then_structure,
    ));

    let loop_init = Stmt::Jump(tailrec_loop, root.arena.alloc([initial_structure]));
    let union_layout = layout_interner.insert_direct_no_semantic(LayoutRepr::Union(union_layout));
    let loop_param = Param {
        symbol: current,
        layout: union_layout,
    };

    Stmt::Join {
        id: tailrec_loop,
        parameters: root.arena.alloc([loop_param]),
        body: root.arena.alloc(loop_body),
        remainder: root.arena.alloc(loop_init),
    }
}

fn refcount_tag_fields<'a>(
    root: &mut CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    ctx: &mut Context<'a>,
    layout_interner: &mut STLayoutInterner<'a>,
    union_layout: UnionLayout<'a>,
    field_layouts: &'a [(usize, InLayout<'a>)],
    structure: Symbol,
    tag_id: TagIdIntType,
    following: Stmt<'a>,
) -> Stmt<'a> {
    let mut stmt = following;

    for (i, field_layout) in field_layouts.iter().rev() {
        if layout_interner.contains_refcounted(*field_layout) {
            let field_val = root.create_symbol(ident_ids, &format!("field_{tag_id}_{i}"));
            let field_val_expr = Expr::UnionAtIndex {
                union_layout,
                tag_id,
                index: *i as u64,
                structure,
            };
            let field_val_stmt = |next| Stmt::Let(field_val, field_val_expr, *field_layout, next);

            let mod_unit = root.create_symbol(ident_ids, &format!("mod_field_{tag_id}_{i}"));
            let mod_args = refcount_args(root, ctx, field_val);
            let mod_expr = root
                .call_specialized_op(ident_ids, ctx, layout_interner, *field_layout, mod_args)
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
