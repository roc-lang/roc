use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_builtins::bitcode::IntWidth;
use roc_module::ident::Ident;
use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};

use crate::ir::{
    BranchInfo, Call, CallSpecId, CallType, Expr, HostExposedLayouts, JoinPointId, Literal,
    ModifyRc, Param, Proc, ProcLayout, SelfRecursive, Stmt, UpdateModeId,
};
use crate::layout::{Builtin, Layout, UnionLayout};

const LAYOUT_BOOL: Layout = Layout::Builtin(Builtin::Bool);
const LAYOUT_UNIT: Layout = Layout::Struct(&[]);
const LAYOUT_PTR: Layout = Layout::RecursivePointer;
const LAYOUT_U32: Layout = Layout::Builtin(Builtin::Int(IntWidth::U32));

const ARG_1: Symbol = Symbol::ARG_1;
const ARG_2: Symbol = Symbol::ARG_2;

/// "Infinite" reference count, for static values
/// Ref counts are encoded as negative numbers where isize::MIN represents 1
pub const REFCOUNT_MAX: usize = 0;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum HelperOp {
    Inc,
    Dec,
    DecRef,
    Eq,
}

impl From<&ModifyRc> for HelperOp {
    fn from(modify: &ModifyRc) -> Self {
        match modify {
            ModifyRc::Inc(..) => Self::Inc,
            ModifyRc::Dec(_) => Self::Dec,
            ModifyRc::DecRef(_) => Self::DecRef,
        }
    }
}

#[derive(Debug)]
struct SpecializedProc<'a> {
    op: HelperOp,
    layout: Layout<'a>,
    proc: Proc<'a>,
}

#[derive(Debug)]
struct Context<'a> {
    new_linker_data: Vec<'a, (Symbol, ProcLayout<'a>)>,
    rec_ptr_layout: Option<UnionLayout<'a>>,
    op: HelperOp,
}

/// Generate specialized helper procs for code gen
/// ----------------------------------------------
///
/// Some low level operations need specialized helper procs to traverse data structures at runtime.
/// This includes refcounting, hashing, and equality checks.
///
/// For example, when checking List equality, we need to visit each element and compare them.
/// Depending on the type of the list elements, we may need to recurse deeper into each element.
/// For tag unions, we may need branches for different tag IDs, etc.
///
/// This module creates specialized helper procs for all such operations and types used in the program.
///
/// The backend drives the process, in two steps:
/// 1) When it sees the relevant node, it calls CodeGenHelp to get the replacement IR.
///    CodeGenHelp returns IR for a call to the helper proc, and remembers the specialization.
/// 2) After the backend has generated code for all user procs, it takes the IR for all of the
///    specialized helpers procs, and generates target code for them too.
///
pub struct CodeGenHelp<'a> {
    arena: &'a Bump,
    home: ModuleId,
    ptr_size: u32,
    layout_isize: Layout<'a>,
    specialized_procs: Vec<'a, SpecializedProc<'a>>,
}

impl<'a> CodeGenHelp<'a> {
    pub fn new(arena: &'a Bump, intwidth_isize: IntWidth, home: ModuleId) -> Self {
        CodeGenHelp {
            arena,
            home,
            ptr_size: intwidth_isize.stack_size(),
            layout_isize: Layout::Builtin(Builtin::Int(intwidth_isize)),
            specialized_procs: Vec::with_capacity_in(16, arena),
        }
    }

    pub fn take_procs(&mut self) -> Vec<'a, Proc<'a>> {
        let procs_iter = self
            .specialized_procs
            .drain(0..)
            .map(|SpecializedProc { proc, .. }| proc);

        Vec::from_iter_in(procs_iter, self.arena)
    }

    // ============================================================================
    //
    //              CALL GENERATED PROCS
    //
    // ============================================================================

    /// Expand a `Refcounting` node to a `Let` node that calls a specialized helper proc.
    /// The helper procs themselves are to be generated later with `generate_procs`
    pub fn expand_refcount_stmt(
        &mut self,
        ident_ids: &mut IdentIds,
        layout: Layout<'a>,
        modify: &ModifyRc,
        following: &'a Stmt<'a>,
    ) -> (&'a Stmt<'a>, Vec<'a, (Symbol, ProcLayout<'a>)>) {
        if !Self::is_rc_implemented_yet(&layout) {
            // Just a warning, so we can decouple backend development from refcounting development.
            // When we are closer to completion, we can change it to a panic.
            println!(
                "WARNING! MEMORY LEAK! Refcounting not yet implemented for Layout {:?}",
                layout
            );
            return (following, Vec::new_in(self.arena));
        }

        let arena = self.arena;

        let mut ctx = Context {
            new_linker_data: Vec::new_in(self.arena),
            rec_ptr_layout: None,
            op: HelperOp::from(modify),
        };

        match modify {
            ModifyRc::Inc(structure, amount) => {
                let layout_isize = self.layout_isize;

                // Define a constant for the amount to increment
                let amount_sym = self.create_symbol(ident_ids, "amount");
                let amount_expr = Expr::Literal(Literal::Int(*amount as i128));
                let amount_stmt = |next| Stmt::Let(amount_sym, amount_expr, layout_isize, next);

                // Call helper proc, passing the Roc structure and constant amount
                let call_result_empty = self.create_symbol(ident_ids, "call_result_empty");
                let call_expr = self.call_specialized_op(
                    ident_ids,
                    &mut ctx,
                    layout,
                    arena.alloc([*structure, amount_sym]),
                );
                let call_stmt = Stmt::Let(call_result_empty, call_expr, LAYOUT_UNIT, following);
                let rc_stmt = arena.alloc(amount_stmt(arena.alloc(call_stmt)));

                (rc_stmt, ctx.new_linker_data)
            }

            ModifyRc::Dec(structure) => {
                // Call helper proc, passing the Roc structure
                let call_result_empty = self.create_symbol(ident_ids, "call_result_empty");
                let call_expr = self.call_specialized_op(
                    ident_ids,
                    &mut ctx,
                    layout,
                    arena.alloc([*structure]),
                );

                let rc_stmt = arena.alloc(Stmt::Let(
                    call_result_empty,
                    call_expr,
                    LAYOUT_UNIT,
                    following,
                ));

                (rc_stmt, ctx.new_linker_data)
            }

            ModifyRc::DecRef(structure) => {
                // No generated procs for DecRef, just lowlevel ops
                let rc_ptr_sym = self.create_symbol(ident_ids, "rc_ptr");

                // Pass the refcount pointer to the lowlevel call (see utils.zig)
                let call_result_empty = self.create_symbol(ident_ids, "call_result_empty");
                let call_expr = Expr::Call(Call {
                    call_type: CallType::LowLevel {
                        op: LowLevel::RefCountDec,
                        update_mode: UpdateModeId::BACKEND_DUMMY,
                    },
                    arguments: arena.alloc([rc_ptr_sym]),
                });
                let call_stmt = Stmt::Let(call_result_empty, call_expr, LAYOUT_UNIT, following);

                let rc_stmt = arena.alloc(self.rc_ptr_from_struct(
                    ident_ids,
                    *structure,
                    rc_ptr_sym,
                    arena.alloc(call_stmt),
                ));

                (rc_stmt, ctx.new_linker_data)
            }
        }
    }

    // Check if refcounting is implemented yet. In the long term, this will be deleted.
    // In the short term, it helps us to skip refcounting and let it leak, so we can make
    // progress incrementally. Kept in sync with generate_procs using assertions.
    fn is_rc_implemented_yet(layout: &Layout) -> bool {
        matches!(layout, Layout::Builtin(Builtin::Str))
    }

    /// Replace a generic `Lowlevel::Eq` call with a specialized helper proc.
    /// The helper procs themselves are to be generated later with `generate_procs`
    pub fn call_specialized_equals(
        &mut self,
        ident_ids: &mut IdentIds,
        layout: &Layout<'a>,
        arguments: &'a [Symbol],
    ) -> (Expr<'a>, Vec<'a, (Symbol, ProcLayout<'a>)>) {
        let mut ctx = Context {
            new_linker_data: Vec::new_in(self.arena),
            rec_ptr_layout: None,
            op: HelperOp::Eq,
        };

        let expr = self.call_specialized_op(ident_ids, &mut ctx, *layout, arguments);

        dbg!(&ctx);

        (expr, ctx.new_linker_data)
    }

    // ============================================================================
    //
    //              CALL SPECIALIZED OP
    //
    // ============================================================================

    fn call_specialized_op(
        &mut self,
        ident_ids: &mut IdentIds,
        ctx: &mut Context<'a>,
        layout: Layout<'a>,
        arguments: &[Symbol],
    ) -> Expr<'a> {
        use HelperOp::*;

        if layout_needs_helper_proc(&layout, ctx.op) {
            let proc_name = self.find_or_create_proc(ident_ids, ctx, layout);

            let (ret_layout, arg_layouts): (&'a Layout<'a>, &'a [Layout<'a>]) = {
                match ctx.op {
                    Dec | DecRef => (&LAYOUT_UNIT, self.arena.alloc([layout])),
                    Inc => (&LAYOUT_UNIT, self.arena.alloc([layout, self.layout_isize])),
                    Eq => (&LAYOUT_BOOL, self.arena.alloc([layout, layout])),
                }
            };

            Expr::Call(Call {
                call_type: CallType::ByName {
                    name: proc_name,
                    ret_layout,
                    arg_layouts,
                    specialization_id: CallSpecId::BACKEND_DUMMY,
                },
                arguments: self.arena.alloc_slice_copy(arguments),
            })
        } else {
            Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op: LowLevel::Eq,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: self.arena.alloc_slice_copy(arguments),
            })
        }
    }

    fn find_or_create_proc(
        &mut self,
        ident_ids: &mut IdentIds,
        ctx: &mut Context<'a>,
        layout: Layout<'a>,
    ) -> Symbol {
        use HelperOp::*;

        let found = self
            .specialized_procs
            .iter()
            .find(|spec| spec.op == ctx.op && spec.layout == layout);

        if let Some(spec) = found {
            return spec.proc.name;
        }

        let (proc_symbol, proc_layout) = self.create_proc_symbol(ident_ids, ctx, &layout);
        ctx.new_linker_data.push((proc_symbol, proc_layout));

        // Generate the body of the Proc
        let (ret_layout, body) = match ctx.op {
            Inc | Dec | DecRef => (LAYOUT_UNIT, self.refcount_generic(ident_ids, ctx, layout)),
            Eq => (LAYOUT_BOOL, self.eq_generic(ident_ids, ctx, layout)),
        };

        let args: &'a [(Layout<'a>, Symbol)] = {
            let roc_value = (layout, ARG_1);
            match ctx.op {
                Inc => {
                    let inc_amount = (self.layout_isize, ARG_2);
                    self.arena.alloc([roc_value, inc_amount])
                }
                Dec | DecRef => self.arena.alloc([roc_value]),
                Eq => self.arena.alloc([roc_value, (layout, ARG_2)]),
            }
        };

        let proc = Proc {
            name: proc_symbol,
            args,
            body,
            closure_data_layout: None,
            ret_layout,
            is_self_recursive: SelfRecursive::NotSelfRecursive,
            must_own_arguments: false,
            host_exposed_layouts: HostExposedLayouts::NotHostExposed,
        };

        self.specialized_procs.push(SpecializedProc {
            op: ctx.op,
            layout,
            proc,
        });

        proc_symbol
    }

    fn create_proc_symbol(
        &self,
        ident_ids: &mut IdentIds,
        ctx: &mut Context<'a>,
        layout: &Layout<'a>,
    ) -> (Symbol, ProcLayout<'a>) {
        let layout_name = layout_debug_name(layout);
        let debug_name = format!(
            "#help{:?}_{}_{}",
            ctx.op,
            layout_name,
            self.specialized_procs.len()
        );
        let proc_symbol: Symbol = self.create_symbol(ident_ids, &debug_name);

        let proc_layout = match ctx.op {
            HelperOp::Inc => ProcLayout {
                arguments: self.arena.alloc([*layout, self.layout_isize]),
                result: LAYOUT_UNIT,
            },
            HelperOp::Dec => ProcLayout {
                arguments: self.arena.alloc([*layout]),
                result: LAYOUT_UNIT,
            },
            HelperOp::DecRef => unreachable!("No generated Proc for DecRef"),
            HelperOp::Eq => ProcLayout {
                arguments: self.arena.alloc([*layout, *layout]),
                result: LAYOUT_BOOL,
            },
        };

        (proc_symbol, proc_layout)
    }

    fn create_symbol(&self, ident_ids: &mut IdentIds, debug_name: &str) -> Symbol {
        let ident_id = ident_ids.add(Ident::from(debug_name));
        Symbol::new(self.home, ident_id)
    }

    // ============================================================================
    //
    //              GENERATE REFCOUNTING
    //
    // ============================================================================

    fn refcount_generic(
        &self,
        ident_ids: &mut IdentIds,
        ctx: &mut Context<'a>,
        layout: Layout<'a>,
    ) -> Stmt<'a> {
        debug_assert!(Self::is_rc_implemented_yet(&layout));
        let rc_todo = || todo!("Please update is_rc_implemented_yet for `{:?}`", layout);

        match layout {
            Layout::Builtin(
                Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal,
            ) => unreachable!("Not refcounted: {:?}", layout),
            Layout::Builtin(Builtin::Str) => self.refcount_str(ident_ids, ctx),
            Layout::Builtin(Builtin::Dict(_, _) | Builtin::Set(_) | Builtin::List(_)) => rc_todo(),
            Layout::Struct(_) => rc_todo(),
            Layout::Union(_) => rc_todo(),
            Layout::LambdaSet(_) => {
                unreachable!("Refcounting on LambdaSet is invalid. Should be a Union at runtime.")
            }
            Layout::RecursivePointer => rc_todo(),
        }
    }

    fn return_unit(&self, ident_ids: &mut IdentIds) -> Stmt<'a> {
        let unit = self.create_symbol(ident_ids, "unit");
        let ret_stmt = self.arena.alloc(Stmt::Ret(unit));
        Stmt::Let(unit, Expr::Struct(&[]), LAYOUT_UNIT, ret_stmt)
    }

    // Subtract a constant from a pointer to find the refcount
    // Also does some type casting, so that we have different Symbols and Layouts
    // for the 'pointer' and 'integer' versions of the address.
    // This helps to avoid issues with the backends Symbol->Layout mapping.
    fn rc_ptr_from_struct(
        &self,
        ident_ids: &mut IdentIds,
        structure: Symbol,
        rc_ptr_sym: Symbol,
        following: &'a Stmt<'a>,
    ) -> Stmt<'a> {
        // Typecast the structure pointer to an integer
        // Backends expect a number Layout to choose the right "subtract" instruction
        let addr_sym = self.create_symbol(ident_ids, "addr");
        let addr_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::PtrCast,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: self.arena.alloc([structure]),
        });
        let addr_stmt = |next| Stmt::Let(addr_sym, addr_expr, self.layout_isize, next);

        // Pointer size constant
        let ptr_size_sym = self.create_symbol(ident_ids, "ptr_size");
        let ptr_size_expr = Expr::Literal(Literal::Int(self.ptr_size as i128));
        let ptr_size_stmt = |next| Stmt::Let(ptr_size_sym, ptr_size_expr, self.layout_isize, next);

        // Refcount address
        let rc_addr_sym = self.create_symbol(ident_ids, "rc_addr");
        let rc_addr_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::NumSub,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: self.arena.alloc([structure, ptr_size_sym]),
        });
        let rc_addr_stmt = |next| Stmt::Let(rc_addr_sym, rc_addr_expr, self.layout_isize, next);

        // Typecast the refcount address from integer to pointer
        let rc_ptr_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::PtrCast,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: self.arena.alloc([rc_addr_sym]),
        });
        let rc_ptr_stmt = |next| Stmt::Let(rc_ptr_sym, rc_ptr_expr, LAYOUT_PTR, next);

        addr_stmt(self.arena.alloc(
            //
            ptr_size_stmt(self.arena.alloc(
                //
                rc_addr_stmt(self.arena.alloc(
                    //
                    rc_ptr_stmt(self.arena.alloc(
                        //
                        following,
                    )),
                )),
            )),
        ))
    }

    /// Generate a procedure to modify the reference count of a Str
    fn refcount_str(&self, ident_ids: &mut IdentIds, ctx: &mut Context<'a>) -> Stmt<'a> {
        let op = ctx.op;

        let string = ARG_1;
        let layout_isize = self.layout_isize;

        // Get the string length as a signed int
        let len = self.create_symbol(ident_ids, "len");
        let len_expr = Expr::StructAtIndex {
            index: 1,
            field_layouts: self.arena.alloc([LAYOUT_PTR, layout_isize]),
            structure: string,
        };
        let len_stmt = |next| Stmt::Let(len, len_expr, layout_isize, next);

        // Zero
        let zero = self.create_symbol(ident_ids, "zero");
        let zero_expr = Expr::Literal(Literal::Int(0));
        let zero_stmt = |next| Stmt::Let(zero, zero_expr, layout_isize, next);

        // is_big_str = (len >= 0);
        // Treat len as isize so that the small string flag is the same as the sign bit
        let is_big_str = self.create_symbol(ident_ids, "is_big_str");
        let is_big_str_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::NumGte,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: self.arena.alloc([len, zero]),
        });
        let is_big_str_stmt = |next| Stmt::Let(is_big_str, is_big_str_expr, LAYOUT_BOOL, next);

        // Get the pointer to the string elements
        let elements = self.create_symbol(ident_ids, "elements");
        let elements_expr = Expr::StructAtIndex {
            index: 0,
            field_layouts: self.arena.alloc([LAYOUT_PTR, layout_isize]),
            structure: string,
        };
        let elements_stmt = |next| Stmt::Let(elements, elements_expr, layout_isize, next);

        // A pointer to the refcount value itself
        let rc_ptr = self.create_symbol(ident_ids, "rc_ptr");

        // Alignment constant (same value as ptr_size but different layout)
        let alignment = self.create_symbol(ident_ids, "alignment");
        let alignment_expr = Expr::Literal(Literal::Int(self.ptr_size as i128));
        let alignment_stmt = |next| Stmt::Let(alignment, alignment_expr, LAYOUT_U32, next);

        // Call the relevant Zig lowlevel to actually modify the refcount
        let zig_call_result = self.create_symbol(ident_ids, "zig_call_result");
        let zig_call_expr = match op {
            HelperOp::Inc => Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op: LowLevel::RefCountInc,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: self.arena.alloc([rc_ptr, ARG_2]),
            }),
            HelperOp::Dec | HelperOp::DecRef => Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op: LowLevel::RefCountDec,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: self.arena.alloc([rc_ptr, alignment]),
            }),
            _ => unreachable!(),
        };
        let zig_call_stmt = |next| Stmt::Let(zig_call_result, zig_call_expr, LAYOUT_UNIT, next);

        // Generate an `if` to skip small strings but modify big strings
        let then_branch = elements_stmt(self.arena.alloc(
            //
            self.rc_ptr_from_struct(
                ident_ids,
                elements,
                rc_ptr,
                self.arena.alloc(
                    //
                    alignment_stmt(self.arena.alloc(
                        //
                        zig_call_stmt(self.arena.alloc(
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
            branches: self.arena.alloc([(1, BranchInfo::None, then_branch)]),
            default_branch: (
                BranchInfo::None,
                self.arena.alloc(self.return_unit(ident_ids)),
            ),
            ret_layout: LAYOUT_UNIT,
        };

        // Combine the statements in sequence
        len_stmt(self.arena.alloc(
            //
            zero_stmt(self.arena.alloc(
                //
                is_big_str_stmt(self.arena.alloc(
                    //
                    if_stmt,
                )),
            )),
        ))
    }

    // ============================================================================
    //
    //              GENERATE EQUALS
    //
    // ============================================================================

    fn eq_generic(
        &mut self,
        ident_ids: &mut IdentIds,
        ctx: &mut Context<'a>,
        layout: Layout<'a>,
    ) -> Stmt<'a> {
        let eq_todo = || todo!("Specialized `==` operator for `{:?}`", layout);

        let main_body = match layout {
            Layout::Builtin(
                Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal,
            ) => unreachable!(
                "No generated proc for `==`. Use direct code gen for {:?}",
                layout
            ),
            Layout::Builtin(Builtin::Str) => {
                unreachable!("No generated helper proc for `==` on Str. Use Zig function.")
            }
            Layout::Builtin(Builtin::Dict(_, _) | Builtin::Set(_)) => eq_todo(),
            Layout::Builtin(Builtin::List(elem_layout)) => {
                self.eq_list(ident_ids, ctx, elem_layout)
            }
            Layout::Struct(field_layouts) => self.eq_struct(ident_ids, ctx, field_layouts),
            Layout::Union(union_layout) => self.eq_tag_union(ident_ids, ctx, union_layout),
            Layout::LambdaSet(_) => unreachable!("`==` is not defined on functions"),
            Layout::RecursivePointer => eq_todo(),
        };

        Stmt::Let(
            Symbol::BOOL_TRUE,
            Expr::Literal(Literal::Int(1)),
            LAYOUT_BOOL,
            self.arena.alloc(Stmt::Let(
                Symbol::BOOL_FALSE,
                Expr::Literal(Literal::Int(0)),
                LAYOUT_BOOL,
                self.arena.alloc(main_body),
            )),
        )
    }

    fn if_pointers_equal_return_true(
        &self,
        ident_ids: &mut IdentIds,
        following: &'a Stmt<'a>,
    ) -> Stmt<'a> {
        let ptr1_addr = self.create_symbol(ident_ids, "addr1");
        let ptr2_addr = self.create_symbol(ident_ids, "addr2");
        let ptr_eq = self.create_symbol(ident_ids, "eq_addr");

        Stmt::Let(
            ptr1_addr,
            Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op: LowLevel::PtrCast,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: self.arena.alloc([ARG_1]),
            }),
            self.layout_isize,
            self.arena.alloc(Stmt::Let(
                ptr2_addr,
                Expr::Call(Call {
                    call_type: CallType::LowLevel {
                        op: LowLevel::PtrCast,
                        update_mode: UpdateModeId::BACKEND_DUMMY,
                    },
                    arguments: self.arena.alloc([ARG_2]),
                }),
                self.layout_isize,
                self.arena.alloc(Stmt::Let(
                    ptr_eq,
                    Expr::Call(Call {
                        call_type: CallType::LowLevel {
                            op: LowLevel::Eq,
                            update_mode: UpdateModeId::BACKEND_DUMMY,
                        },
                        arguments: self.arena.alloc([ptr1_addr, ptr2_addr]),
                    }),
                    LAYOUT_BOOL,
                    self.arena.alloc(Stmt::Switch {
                        cond_symbol: ptr_eq,
                        cond_layout: LAYOUT_BOOL,
                        branches: self.arena.alloc([(
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

    fn if_false_return_false(&self, symbol: Symbol, following: &'a Stmt<'a>) -> Stmt<'a> {
        Stmt::Switch {
            cond_symbol: symbol,
            cond_layout: LAYOUT_BOOL,
            branches: self
                .arena
                .alloc([(0, BranchInfo::None, Stmt::Ret(Symbol::BOOL_FALSE))]),
            default_branch: (BranchInfo::None, following),
            ret_layout: LAYOUT_BOOL,
        }
    }

    fn eq_struct(
        &mut self,
        ident_ids: &mut IdentIds,
        ctx: &mut Context<'a>,
        field_layouts: &'a [Layout<'a>],
    ) -> Stmt<'a> {
        let else_clause = self.eq_fields(ident_ids, ctx, 0, field_layouts);
        self.if_pointers_equal_return_true(ident_ids, self.arena.alloc(else_clause))
    }

    fn eq_fields(
        &mut self,
        ident_ids: &mut IdentIds,
        ctx: &mut Context<'a>,
        tag_id: u64,
        field_layouts: &'a [Layout<'a>],
    ) -> Stmt<'a> {
        let mut stmt = Stmt::Ret(Symbol::BOOL_TRUE);
        for (i, layout) in field_layouts.iter().enumerate().rev() {
            let field1_sym = self.create_symbol(ident_ids, &format!("field_1_{}_{}", tag_id, i));
            let field1_expr = Expr::StructAtIndex {
                index: i as u64,
                field_layouts,
                structure: ARG_1,
            };
            let field1_stmt = |next| Stmt::Let(field1_sym, field1_expr, *layout, next);

            let field2_sym = self.create_symbol(ident_ids, &format!("field_2_{}_{}", tag_id, i));
            let field2_expr = Expr::StructAtIndex {
                index: i as u64,
                field_layouts,
                structure: ARG_2,
            };
            let field2_stmt = |next| Stmt::Let(field2_sym, field2_expr, *layout, next);

            let eq_call_expr = self.call_specialized_op(
                ident_ids,
                ctx,
                *layout,
                self.arena.alloc([field1_sym, field2_sym]),
            );

            let eq_call_name = format!("eq_call_{}", i);
            let eq_call_sym = self.create_symbol(ident_ids, &eq_call_name);
            let eq_call_stmt = |next| Stmt::Let(eq_call_sym, eq_call_expr, LAYOUT_BOOL, next);

            stmt = field1_stmt(self.arena.alloc(
                //
                field2_stmt(self.arena.alloc(
                    //
                    eq_call_stmt(self.arena.alloc(
                        //
                        self.if_false_return_false(eq_call_sym, self.arena.alloc(stmt)),
                    )),
                )),
            ))
        }
        stmt
    }

    fn eq_tag_union(
        &mut self,
        ident_ids: &mut IdentIds,
        ctx: &mut Context<'a>,
        union_layout: UnionLayout<'a>,
    ) -> Stmt<'a> {
        use UnionLayout::*;

        let parent_rec_ptr_layout = ctx.rec_ptr_layout;
        if !matches!(union_layout, NonRecursive(_)) {
            ctx.rec_ptr_layout = Some(union_layout);
        }

        let main_stmt = match union_layout {
            NonRecursive(tags) => self.eq_tag_union_help(ident_ids, ctx, union_layout, tags, None),

            Recursive(tags) => self.eq_tag_union_help(ident_ids, ctx, union_layout, tags, None),

            NonNullableUnwrapped(field_layouts) => self.eq_fields(ident_ids, ctx, 0, field_layouts),

            NullableWrapped {
                other_tags,
                nullable_id,
            } => {
                self.eq_tag_union_help(ident_ids, ctx, union_layout, other_tags, Some(nullable_id))
            }

            NullableUnwrapped {
                other_fields,
                nullable_id: n,
            } => self.eq_tag_union_help(
                ident_ids,
                ctx,
                union_layout,
                self.arena.alloc([other_fields]),
                Some(n as u16),
            ),
        };

        ctx.rec_ptr_layout = parent_rec_ptr_layout;

        self.if_pointers_equal_return_true(ident_ids, self.arena.alloc(main_stmt))
    }

    fn eq_tag_union_help(
        &mut self,
        ident_ids: &mut IdentIds,
        ctx: &mut Context<'a>,
        union_layout: UnionLayout<'a>,
        tag_layouts: &'a [&'a [Layout<'a>]],
        nullable_id: Option<u16>,
    ) -> Stmt<'a> {
        let tag_id_layout = union_layout.tag_id_layout();

        let tag_id_a = self.create_symbol(ident_ids, "tag_id_a");
        let tag_id_a_stmt = |next| {
            Stmt::Let(
                tag_id_a,
                Expr::GetTagId {
                    structure: ARG_1,
                    union_layout,
                },
                tag_id_layout,
                next,
            )
        };

        let tag_id_b = self.create_symbol(ident_ids, "tag_id_b");
        let tag_id_b_stmt = |next| {
            Stmt::Let(
                tag_id_b,
                Expr::GetTagId {
                    structure: ARG_2,
                    union_layout,
                },
                tag_id_layout,
                next,
            )
        };

        let tag_ids_eq = self.create_symbol(ident_ids, "tag_ids_eq");
        let tag_ids_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::Eq,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: self.arena.alloc([tag_id_a, tag_id_b]),
        });
        let tag_ids_eq_stmt = |next| Stmt::Let(tag_ids_eq, tag_ids_expr, LAYOUT_BOOL, next);

        let if_equal_ids_branches =
            self.arena
                .alloc([(0, BranchInfo::None, Stmt::Ret(Symbol::BOOL_FALSE))]);

        //
        // Switch statement by tag ID
        //

        let mut tag_branches = Vec::with_capacity_in(tag_layouts.len(), self.arena);

        // If there's a null tag, check it first. We might not need to load any data from memory.
        if let Some(id) = nullable_id {
            tag_branches.push((id as u64, BranchInfo::None, Stmt::Ret(Symbol::BOOL_TRUE)))
        }

        let mut tag_id: u64 = 0;
        for field_layouts in tag_layouts.iter().take(tag_layouts.len() - 1) {
            if let Some(null_id) = nullable_id {
                if tag_id == null_id as u64 {
                    tag_id += 1;
                }
            }

            let tag_stmt = self.eq_fields(ident_ids, ctx, tag_id, field_layouts);
            tag_branches.push((tag_id, BranchInfo::None, tag_stmt));

            tag_id += 1;
        }

        let tag_switch_stmt = Stmt::Switch {
            cond_symbol: tag_id_a,
            cond_layout: tag_id_layout,
            branches: tag_branches.into_bump_slice(),
            default_branch: (
                BranchInfo::None,
                self.arena.alloc(self.eq_fields(
                    ident_ids,
                    ctx,
                    tag_id,
                    tag_layouts.last().unwrap(),
                )),
            ),
            ret_layout: LAYOUT_BOOL,
        };

        let if_equal_ids_stmt = Stmt::Switch {
            cond_symbol: tag_ids_eq,
            cond_layout: LAYOUT_BOOL,
            branches: if_equal_ids_branches,
            default_branch: (BranchInfo::None, self.arena.alloc(tag_switch_stmt)),
            ret_layout: LAYOUT_BOOL,
        };

        //
        // combine all the statments
        //
        tag_id_a_stmt(self.arena.alloc(
            //
            tag_id_b_stmt(self.arena.alloc(
                //
                tag_ids_eq_stmt(self.arena.alloc(
                    //
                    if_equal_ids_stmt,
                )),
            )),
        ))
    }

    /// List equality
    /// We can't use `ListGetUnsafe` because it increments the refcount, and we don't want that.
    /// Another way to dereference a heap pointer is to use `Expr::UnionAtIndex`.
    /// To achieve this we use `PtrCast` to cast the element pointer to a "Box" layout.
    /// Then we can increment the Box pointer in a loop, dereferencing it each time.
    /// (An alternative approach would be to create a new lowlevel like ListPeekUnsafe.)
    fn eq_list(
        &mut self,
        ident_ids: &mut IdentIds,
        ctx: &mut Context<'a>,
        elem_layout: &Layout<'a>,
    ) -> Stmt<'a> {
        use LowLevel::*;
        let layout_isize = self.layout_isize;
        let arena = self.arena;

        // A "Box" layout (heap pointer to a single list element)
        let box_union_layout = UnionLayout::NonNullableUnwrapped(self.arena.alloc([*elem_layout]));
        let box_layout = Layout::Union(box_union_layout);

        // Compare lengths

        let len_1 = self.create_symbol(ident_ids, "len_1");
        let len_2 = self.create_symbol(ident_ids, "len_2");
        let len_1_stmt = |next| let_lowlevel(arena, layout_isize, len_1, ListLen, &[ARG_1], next);
        let len_2_stmt = |next| let_lowlevel(arena, layout_isize, len_2, ListLen, &[ARG_2], next);

        let eq_len = self.create_symbol(ident_ids, "eq_len");
        let eq_len_stmt =
            |next| let_lowlevel(arena, LAYOUT_BOOL, eq_len, Eq, &[len_1, len_2], next);

        // if lengths are equal...

        // get element pointers
        let elements_1 = self.create_symbol(ident_ids, "elements_1");
        let elements_2 = self.create_symbol(ident_ids, "elements_2");
        let elements_1_expr = Expr::StructAtIndex {
            index: 0,
            field_layouts: self.arena.alloc([box_layout, layout_isize]),
            structure: ARG_1,
        };
        let elements_2_expr = Expr::StructAtIndex {
            index: 0,
            field_layouts: self.arena.alloc([box_layout, layout_isize]),
            structure: ARG_2,
        };
        let elements_1_stmt = |next| Stmt::Let(elements_1, elements_1_expr, box_layout, next);
        let elements_2_stmt = |next| Stmt::Let(elements_2, elements_2_expr, box_layout, next);

        // Cast to integers
        let start_1 = self.create_symbol(ident_ids, "start_1");
        let start_2 = self.create_symbol(ident_ids, "start_2");
        let start_1_stmt =
            |next| let_lowlevel(arena, layout_isize, start_1, PtrCast, &[elements_1], next);
        let start_2_stmt =
            |next| let_lowlevel(arena, layout_isize, start_2, PtrCast, &[elements_2], next);

        //
        // Loop initialisation
        //

        // let size = literal int
        let size = self.create_symbol(ident_ids, "size");
        let size_expr = Expr::Literal(Literal::Int(elem_layout.stack_size(self.ptr_size) as i128));
        let size_stmt = |next| Stmt::Let(size, size_expr, layout_isize, next);

        // let list_size = len_1 * size
        let list_size = self.create_symbol(ident_ids, "list_size");
        let list_size_stmt =
            |next| let_lowlevel(arena, layout_isize, list_size, NumMul, &[len_1, size], next);

        // let end_1 = start_1 + list_size
        let end_1 = self.create_symbol(ident_ids, "end_1");
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

        let elems_loop = JoinPointId(self.create_symbol(ident_ids, "elems_loop"));
        let addr1 = self.create_symbol(ident_ids, "addr1");
        let addr2 = self.create_symbol(ident_ids, "addr2");

        let param_addr1 = Param {
            symbol: addr1,
            borrow: false,
            layout: layout_isize,
        };

        let param_addr2 = Param {
            symbol: addr2,
            borrow: false,
            layout: layout_isize,
        };

        //
        // if we haven't reached the end yet...
        //

        // Cast integers to box pointers
        let box1 = self.create_symbol(ident_ids, "box1");
        let box2 = self.create_symbol(ident_ids, "box2");
        let box1_stmt = |next| let_lowlevel(arena, box_layout, box1, PtrCast, &[addr1], next);
        let box2_stmt = |next| let_lowlevel(arena, box_layout, box2, PtrCast, &[addr2], next);

        // Dereference the box pointers to get the current elements
        let elem1 = self.create_symbol(ident_ids, "elem1");
        let elem2 = self.create_symbol(ident_ids, "elem2");
        let elem1_expr = Expr::UnionAtIndex {
            structure: box1,
            union_layout: box_union_layout,
            tag_id: 0,
            index: 0,
        };
        let elem2_expr = Expr::UnionAtIndex {
            structure: box2,
            union_layout: box_union_layout,
            tag_id: 0,
            index: 0,
        };
        let elem1_stmt = |next| Stmt::Let(elem1, elem1_expr, *elem_layout, next);
        let elem2_stmt = |next| Stmt::Let(elem2, elem2_expr, *elem_layout, next);

        // Compare the two current elements
        let eq_elems = self.create_symbol(ident_ids, "eq_elems");
        let eq_elems_expr = self.call_specialized_op(ident_ids, ctx, *elem_layout, &[elem1, elem2]);

        let eq_elems_stmt = |next| Stmt::Let(eq_elems, eq_elems_expr, LAYOUT_BOOL, next);

        // If current elements are equal, loop back again
        let next_1 = self.create_symbol(ident_ids, "next_1");
        let next_2 = self.create_symbol(ident_ids, "next_2");
        let next_1_stmt =
            |next| let_lowlevel(arena, layout_isize, next_1, NumAdd, &[addr1, size], next);
        let next_2_stmt =
            |next| let_lowlevel(arena, layout_isize, next_2, NumAdd, &[addr2, size], next);

        let jump_back = Stmt::Jump(elems_loop, self.arena.alloc([next_1, next_2]));

        //
        // Control flow
        //

        let is_end = self.create_symbol(ident_ids, "is_end");
        let is_end_stmt =
            |next| let_lowlevel(arena, LAYOUT_BOOL, is_end, NumGte, &[addr1, end_1], next);

        let if_elems_not_equal = self.if_false_return_false(
            eq_elems,
            // else
            self.arena.alloc(
                //
                next_1_stmt(self.arena.alloc(
                    //
                    next_2_stmt(self.arena.alloc(
                        //
                        jump_back,
                    )),
                )),
            ),
        );

        let if_end_of_list = Stmt::Switch {
            cond_symbol: is_end,
            cond_layout: LAYOUT_BOOL,
            ret_layout: LAYOUT_BOOL,
            branches: self
                .arena
                .alloc([(1, BranchInfo::None, Stmt::Ret(Symbol::BOOL_TRUE))]),
            default_branch: (
                BranchInfo::None,
                self.arena.alloc(
                    //
                    box1_stmt(self.arena.alloc(
                        //
                        box2_stmt(self.arena.alloc(
                            //
                            elem1_stmt(self.arena.alloc(
                                //
                                elem2_stmt(self.arena.alloc(
                                    //
                                    eq_elems_stmt(self.arena.alloc(
                                        //
                                        if_elems_not_equal,
                                    )),
                                )),
                            )),
                        )),
                    )),
                ),
            ),
        };

        let joinpoint_loop = Stmt::Join {
            id: elems_loop,
            parameters: self.arena.alloc([param_addr1, param_addr2]),
            body: self.arena.alloc(
                //
                is_end_stmt(
                    //
                    self.arena.alloc(if_end_of_list),
                ),
            ),
            remainder: self
                .arena
                .alloc(Stmt::Jump(elems_loop, self.arena.alloc([start_1, start_2]))),
        };

        let if_different_lengths = self.if_false_return_false(
            eq_len,
            // else
            self.arena.alloc(
                //
                elements_1_stmt(self.arena.alloc(
                    //
                    elements_2_stmt(self.arena.alloc(
                        //
                        start_1_stmt(self.arena.alloc(
                            //
                            start_2_stmt(self.arena.alloc(
                                //
                                size_stmt(self.arena.alloc(
                                    //
                                    list_size_stmt(self.arena.alloc(
                                        //
                                        end_1_stmt(self.arena.alloc(
                                            //
                                            joinpoint_loop,
                                        )),
                                    )),
                                )),
                            )),
                        )),
                    )),
                )),
            ),
        );

        let pointers_else = len_1_stmt(self.arena.alloc(
            //
            len_2_stmt(self.arena.alloc(
                //
                eq_len_stmt(self.arena.alloc(
                    //
                    if_different_lengths,
                )),
            )),
        ));

        self.if_pointers_equal_return_true(ident_ids, self.arena.alloc(pointers_else))
    }
}

fn let_lowlevel<'a>(
    arena: &'a Bump,
    result_layout: Layout<'a>,
    result: Symbol,
    op: LowLevel,
    arguments: &[Symbol],
    next: &'a Stmt<'a>,
) -> Stmt<'a> {
    Stmt::Let(
        result,
        Expr::Call(Call {
            call_type: CallType::LowLevel {
                op,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: arena.alloc_slice_copy(arguments),
        }),
        result_layout,
        next,
    )
}

/// Helper to derive a debug function name from a layout
fn layout_debug_name<'a>(layout: &Layout<'a>) -> &'static str {
    match layout {
        Layout::Builtin(Builtin::List(_)) => "list",
        Layout::Builtin(Builtin::Set(_)) => "set",
        Layout::Builtin(Builtin::Dict(_, _)) => "dict",
        Layout::Builtin(Builtin::Str) => "str",
        Layout::Struct(_) => "struct",
        Layout::Union(_) => "union",
        Layout::LambdaSet(_) => "lambdaset",
        _ => unreachable!("Can't create helper proc name for {:?}", layout),
    }
}

fn layout_needs_helper_proc(layout: &Layout, op: HelperOp) -> bool {
    match layout {
        Layout::Builtin(Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal) => {
            false
        }

        Layout::Builtin(Builtin::Str) => {
            // Str type can use either Zig functions or generated IR, since it's not generic.
            // Eq uses a Zig function, refcount uses generated IR.
            // Both are fine, they were just developed at different times.
            matches!(op, HelperOp::Inc | HelperOp::Dec | HelperOp::DecRef)
        }

        Layout::Builtin(Builtin::Dict(_, _) | Builtin::Set(_) | Builtin::List(_)) => true,

        Layout::Struct(fields) => !fields.is_empty(),

        Layout::Union(UnionLayout::NonRecursive(tags)) => !tags.is_empty(),

        Layout::Union(_) => true,

        Layout::LambdaSet(_) | Layout::RecursivePointer => false,
    }
}
