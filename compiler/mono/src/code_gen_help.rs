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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum HelperOp {
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
    /// Specializations to generate
    /// Order of insertion is preserved, since it is important for Wasm backend
    specs: Vec<'a, (Layout<'a>, HelperOp, Symbol)>,
}

impl<'a> CodeGenHelp<'a> {
    pub fn new(arena: &'a Bump, intwidth_isize: IntWidth, home: ModuleId) -> Self {
        CodeGenHelp {
            arena,
            home,
            ptr_size: intwidth_isize.stack_size(),
            layout_isize: Layout::Builtin(Builtin::Int(intwidth_isize)),
            specs: Vec::with_capacity_in(16, arena),
        }
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

        match modify {
            ModifyRc::Inc(structure, amount) => {
                let layout_isize = self.layout_isize;

                let (proc_name, new_procs_info) =
                    self.get_or_create_proc_symbols_recursive(ident_ids, &layout, HelperOp::Inc);

                // Define a constant for the amount to increment
                let amount_sym = self.create_symbol(ident_ids, "amount");
                let amount_expr = Expr::Literal(Literal::Int(*amount as i128));
                let amount_stmt = |next| Stmt::Let(amount_sym, amount_expr, layout_isize, next);

                // Call helper proc, passing the Roc structure and constant amount
                let arg_layouts = arena.alloc([layout, layout_isize]);
                let call_result_empty = self.create_symbol(ident_ids, "call_result_empty");
                let call_expr = Expr::Call(Call {
                    call_type: CallType::ByName {
                        name: proc_name,
                        ret_layout: &LAYOUT_UNIT,
                        arg_layouts,
                        specialization_id: CallSpecId::BACKEND_DUMMY,
                    },
                    arguments: arena.alloc([*structure, amount_sym]),
                });
                let call_stmt = Stmt::Let(call_result_empty, call_expr, LAYOUT_UNIT, following);
                let rc_stmt = arena.alloc(amount_stmt(arena.alloc(call_stmt)));

                (rc_stmt, new_procs_info)
            }

            ModifyRc::Dec(structure) => {
                let (proc_name, new_procs_info) =
                    self.get_or_create_proc_symbols_recursive(ident_ids, &layout, HelperOp::Dec);

                // Call helper proc, passing the Roc structure
                let call_result_empty = self.create_symbol(ident_ids, "call_result_empty");
                let call_expr = Expr::Call(Call {
                    call_type: CallType::ByName {
                        name: proc_name,
                        ret_layout: &LAYOUT_UNIT,
                        arg_layouts: arena.alloc([layout]),
                        specialization_id: CallSpecId::BACKEND_DUMMY,
                    },
                    arguments: arena.alloc([*structure]),
                });

                let rc_stmt = arena.alloc(Stmt::Let(
                    call_result_empty,
                    call_expr,
                    LAYOUT_UNIT,
                    following,
                ));

                (rc_stmt, new_procs_info)
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

                (rc_stmt, Vec::new_in(self.arena))
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
    ) -> (&'a Expr<'a>, Vec<'a, (Symbol, ProcLayout<'a>)>) {
        // Record a specialization and get its name
        let (proc_name, new_procs_info) =
            self.get_or_create_proc_symbols_recursive(ident_ids, layout, HelperOp::Eq);

        // Call the specialized helper
        let arg_layouts = self.arena.alloc([*layout, *layout]);
        let expr = self.arena.alloc(Expr::Call(Call {
            call_type: CallType::ByName {
                name: proc_name,
                ret_layout: &LAYOUT_BOOL,
                arg_layouts,
                specialization_id: CallSpecId::BACKEND_DUMMY,
            },
            arguments,
        }));

        (expr, new_procs_info)
    }

    // ============================================================================
    //
    //              CREATE SPECIALIZATIONS
    //
    // ============================================================================

    /// Find the Symbol of the procedure for this layout and operation
    /// If any new helper procs are needed for this layout or its children,
    /// return their details in a vector.
    fn get_or_create_proc_symbols_recursive(
        &mut self,
        ident_ids: &mut IdentIds,
        layout: &Layout<'a>,
        op: HelperOp,
    ) -> (Symbol, Vec<'a, (Symbol, ProcLayout<'a>)>) {
        let mut new_procs_info = Vec::new_in(self.arena);

        let proc_symbol =
            self.get_or_create_proc_symbols_visit(ident_ids, &mut new_procs_info, op, layout);

        (proc_symbol, new_procs_info)
    }

    fn get_or_create_proc_symbols_visit(
        &mut self,
        ident_ids: &mut IdentIds,
        new_procs_info: &mut Vec<'a, (Symbol, ProcLayout<'a>)>,
        op: HelperOp,
        layout: &Layout<'a>,
    ) -> Symbol {
        if let Layout::LambdaSet(lambda_set) = layout {
            return self.get_or_create_proc_symbols_visit(
                ident_ids,
                new_procs_info,
                op,
                &lambda_set.runtime_representation(),
            );
        }

        let (symbol, new_proc_layout) = self.get_or_create_proc_symbol(ident_ids, layout, op);

        if let Some(proc_layout) = new_proc_layout {
            new_procs_info.push((symbol, proc_layout));

            let mut visit_child = |child| {
                if layout_needs_helper_proc(child, op) {
                    self.get_or_create_proc_symbols_visit(ident_ids, new_procs_info, op, child);
                }
            };

            let mut visit_children = |children: &'a [Layout]| {
                for child in children {
                    visit_child(child);
                }
            };

            let mut visit_tags = |tags: &'a [&'a [Layout]]| {
                for tag in tags {
                    visit_children(tag);
                }
            };

            match layout {
                Layout::Builtin(builtin) => match builtin {
                    Builtin::Dict(key, value) => {
                        visit_child(key);
                        visit_child(value);
                    }
                    Builtin::Set(element) | Builtin::List(element) => visit_child(element),
                    _ => {}
                },
                Layout::Struct(fields) => visit_children(fields),
                Layout::Union(union_layout) => match union_layout {
                    UnionLayout::NonRecursive(tags) => visit_tags(tags),
                    UnionLayout::Recursive(tags) => visit_tags(tags),
                    UnionLayout::NonNullableUnwrapped(fields) => visit_children(fields),
                    UnionLayout::NullableWrapped { other_tags, .. } => visit_tags(other_tags),
                    UnionLayout::NullableUnwrapped { other_fields, .. } => {
                        visit_children(other_fields)
                    }
                },
                Layout::LambdaSet(_) => unreachable!(),
                Layout::RecursivePointer => {}
            }
        }

        symbol
    }

    fn get_or_create_proc_symbol(
        &mut self,
        ident_ids: &mut IdentIds,
        layout: &Layout<'a>,
        op: HelperOp,
    ) -> (Symbol, Option<ProcLayout<'a>>) {
        let found = self.specs.iter().find(|(l, o, _)| l == layout && *o == op);

        if let Some((_, _, existing_symbol)) = found {
            (*existing_symbol, None)
        } else {
            let layout_name = layout_debug_name(layout);
            let debug_name = format!("#help{:?}_{}", op, layout_name);
            let new_symbol: Symbol = self.create_symbol(ident_ids, &debug_name);
            self.specs.push((*layout, op, new_symbol));

            let new_proc_layout = match op {
                HelperOp::Inc => Some(ProcLayout {
                    arguments: self.arena.alloc([*layout, self.layout_isize]),
                    result: LAYOUT_UNIT,
                }),
                HelperOp::Dec => Some(ProcLayout {
                    arguments: self.arena.alloc([*layout]),
                    result: LAYOUT_UNIT,
                }),
                HelperOp::DecRef => None,
                HelperOp::Eq => Some(ProcLayout {
                    arguments: self.arena.alloc([*layout, *layout]),
                    result: LAYOUT_BOOL,
                }),
            };

            (new_symbol, new_proc_layout)
        }
    }

    fn create_symbol(&self, ident_ids: &mut IdentIds, debug_name: &str) -> Symbol {
        let ident_id = ident_ids.add(Ident::from(debug_name));
        Symbol::new(self.home, ident_id)
    }

    // ============================================================================
    //
    //              GENERATE PROCS
    //
    // ============================================================================

    /// Generate refcounting helper procs, each specialized to a particular Layout.
    /// For example `List (Result { a: Str, b: Int } Str)` would get its own helper
    /// to update the refcounts on the List, the Result and the strings.
    pub fn generate_procs(&self, arena: &'a Bump, ident_ids: &mut IdentIds) -> Vec<'a, Proc<'a>> {
        use HelperOp::*;

        // Clone the specializations so we can loop over them safely
        // We need to keep self.specs for lookups of sub-procedures during generation
        // Maybe could avoid this by separating specs vector from CodeGenHelp, letting backend own both.
        let mut specs = self.specs.clone();

        let procs_iter = specs.drain(0..).map(|(layout, op, proc_symbol)| {
            let (ret_layout, body) = match op {
                Inc | Dec | DecRef => (LAYOUT_UNIT, self.refcount_generic(ident_ids, layout, op)),
                Eq => (LAYOUT_BOOL, self.eq_generic(ident_ids, layout)),
            };

            let roc_value = (layout, ARG_1);
            let args: &'a [(Layout<'a>, Symbol)] = match op {
                HelperOp::Inc => {
                    let inc_amount = (self.layout_isize, ARG_2);
                    self.arena.alloc([roc_value, inc_amount])
                }
                HelperOp::Dec | HelperOp::DecRef => self.arena.alloc([roc_value]),
                HelperOp::Eq => self.arena.alloc([roc_value, (layout, ARG_2)]),
            };

            Proc {
                name: proc_symbol,
                args,
                body,
                closure_data_layout: None,
                ret_layout,
                is_self_recursive: SelfRecursive::NotSelfRecursive,
                must_own_arguments: false,
                host_exposed_layouts: HostExposedLayouts::NotHostExposed,
            }
        });

        Vec::from_iter_in(procs_iter, arena)
    }

    /// Apply the HelperOp to a field of a data structure
    /// Only called while generating bodies of helper procs
    /// The list of specializations should be complete by this time
    fn apply_op_to_sub_layout(
        &self,
        op: HelperOp,
        sub_layout: &Layout<'a>,
        arguments: &[Symbol],
    ) -> Expr<'a> {
        let found = self
            .specs
            .iter()
            .find(|(l, o, _)| l == sub_layout && *o == op);

        if let Some((_, _, proc_name)) = found {
            let arg_layouts: &[Layout<'a>] = match op {
                HelperOp::Eq => self.arena.alloc([*sub_layout, *sub_layout]),
                HelperOp::Inc => self.arena.alloc([*sub_layout, self.layout_isize]),
                HelperOp::Dec => self.arena.alloc([*sub_layout]),
                HelperOp::DecRef => unreachable!("DecRef is not recursive"),
            };
            let ret_layout = if matches!(op, HelperOp::Eq) {
                &LAYOUT_BOOL
            } else {
                &LAYOUT_UNIT
            };

            Expr::Call(Call {
                call_type: CallType::ByName {
                    name: *proc_name,
                    ret_layout,
                    arg_layouts,
                    specialization_id: CallSpecId::BACKEND_DUMMY,
                },
                arguments: self.arena.alloc_slice_copy(arguments),
            })
        } else {
            // By the time we get here (generating helper procs), the list of specializations is complete.
            // So if we didn't find one, we must be at a leaf of the layout tree.
            debug_assert!(!layout_needs_helper_proc(sub_layout, op));

            let lowlevel = match op {
                HelperOp::Eq => LowLevel::Eq,
                HelperOp::Inc => LowLevel::RefCountInc,
                HelperOp::Dec => LowLevel::RefCountDec,
                HelperOp::DecRef => unreachable!("DecRef is not recursive"),
            };

            Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op: lowlevel,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: self.arena.alloc_slice_copy(arguments),
            })
        }
    }

    // ============================================================================
    //
    //              GENERATE REFCOUNTING
    //
    // ============================================================================

    fn refcount_generic(
        &self,
        ident_ids: &mut IdentIds,
        layout: Layout<'a>,
        op: HelperOp,
    ) -> Stmt<'a> {
        debug_assert!(Self::is_rc_implemented_yet(&layout));
        let rc_todo = || todo!("Please update is_rc_implemented_yet for `{:?}`", layout);

        match layout {
            Layout::Builtin(
                Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal,
            ) => unreachable!("Not refcounted: {:?}", layout),
            Layout::Builtin(Builtin::Str) => self.refcount_str(ident_ids, op),
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
    fn refcount_str(&self, ident_ids: &mut IdentIds, op: HelperOp) -> Stmt<'a> {
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

    fn eq_generic(&self, ident_ids: &mut IdentIds, layout: Layout<'a>) -> Stmt<'a> {
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
            Layout::Builtin(Builtin::List(elem_layout)) => self.eq_list(ident_ids, elem_layout),
            Layout::Struct(field_layouts) => self.eq_struct(ident_ids, field_layouts),
            Layout::Union(union_layout) => self.eq_tag_union(ident_ids, union_layout),
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

    fn eq_struct(&self, ident_ids: &mut IdentIds, field_layouts: &'a [Layout<'a>]) -> Stmt<'a> {
        let else_clause = self.eq_fields(ident_ids, 0, field_layouts, None);
        self.if_pointers_equal_return_true(ident_ids, self.arena.alloc(else_clause))
    }

    fn eq_fields(
        &self,
        ident_ids: &mut IdentIds,
        tag_id: u64,
        field_layouts: &'a [Layout<'a>],
        rec_ptr_layout: Option<Layout<'a>>,
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

            let sub_layout = match (layout, rec_ptr_layout) {
                (Layout::RecursivePointer, Some(rec_layout)) => self.arena.alloc(rec_layout),
                _ => layout,
            };

            let eq_call_expr =
                self.apply_op_to_sub_layout(HelperOp::Eq, sub_layout, &[field1_sym, field2_sym]);
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

    fn eq_tag_union(&self, ident_ids: &mut IdentIds, union_layout: UnionLayout<'a>) -> Stmt<'a> {
        use UnionLayout::*;

        let main_stmt = match union_layout {
            NonRecursive(tags) => self.eq_tag_union_help(ident_ids, union_layout, tags, None),

            Recursive(tags) => self.eq_tag_union_help(ident_ids, union_layout, tags, None),

            NonNullableUnwrapped(field_layouts) => self.eq_fields(
                ident_ids,
                0,
                field_layouts,
                Some(Layout::Union(union_layout)),
            ),

            NullableWrapped {
                other_tags,
                nullable_id,
            } => self.eq_tag_union_help(ident_ids, union_layout, other_tags, Some(nullable_id)),

            NullableUnwrapped {
                other_fields,
                nullable_id: n,
            } => self.eq_tag_union_help(
                ident_ids,
                union_layout,
                self.arena.alloc([other_fields]),
                Some(n as u16),
            ),
        };

        self.if_pointers_equal_return_true(ident_ids, self.arena.alloc(main_stmt))
    }

    fn eq_tag_union_help(
        &self,
        ident_ids: &mut IdentIds,
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
        let tag_ids_eq_stmt = |next| {
            Stmt::Let(
                tag_ids_eq,
                Expr::Call(Call {
                    call_type: CallType::LowLevel {
                        op: LowLevel::Eq,
                        update_mode: UpdateModeId::BACKEND_DUMMY,
                    },
                    arguments: self.arena.alloc([tag_id_a, tag_id_b]),
                }),
                LAYOUT_BOOL,
                next,
            )
        };

        let if_equal_ids_stmt = |next| Stmt::Switch {
            cond_symbol: tag_ids_eq,
            cond_layout: LAYOUT_BOOL,
            branches: self
                .arena
                .alloc([(0, BranchInfo::None, Stmt::Ret(Symbol::BOOL_FALSE))]),
            default_branch: (BranchInfo::None, next),
            ret_layout: LAYOUT_BOOL,
        };

        //
        // Switch statement by tag ID
        //

        let mut tag_branches = Vec::with_capacity_in(tag_layouts.len(), self.arena);

        // If there's a null tag, check it first. We might not need to load any data from memory.
        if let Some(id) = nullable_id {
            tag_branches.push((id as u64, BranchInfo::None, Stmt::Ret(Symbol::BOOL_TRUE)))
        }

        let recursive_ptr_layout = Some(Layout::Union(union_layout));

        let mut tag_id: u64 = 0;
        for field_layouts in tag_layouts.iter().take(tag_layouts.len() - 1) {
            if let Some(null_id) = nullable_id {
                if tag_id == null_id as u64 {
                    tag_id += 1;
                }
            }

            tag_branches.push((
                tag_id,
                BranchInfo::None,
                self.eq_fields(ident_ids, tag_id, field_layouts, recursive_ptr_layout),
            ));

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
                    tag_id,
                    tag_layouts.last().unwrap(),
                    recursive_ptr_layout,
                )),
            ),
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
                    if_equal_ids_stmt(self.arena.alloc(
                        //
                        tag_switch_stmt,
                    )),
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
    fn eq_list(&self, ident_ids: &mut IdentIds, elem_layout: &Layout<'a>) -> Stmt<'a> {
        use LowLevel::*;
        let layout_isize = self.layout_isize;

        // A "Box" layout (heap pointer to a single list element)
        let box_union_layout = UnionLayout::NonNullableUnwrapped(self.arena.alloc([*elem_layout]));
        let box_layout = Layout::Union(box_union_layout);

        // Compare lengths

        let len_1 = self.create_symbol(ident_ids, "len_1");
        let len_2 = self.create_symbol(ident_ids, "len_2");
        let len_1_stmt = |next| self.let_lowlevel(layout_isize, len_1, ListLen, &[ARG_1], next);
        let len_2_stmt = |next| self.let_lowlevel(layout_isize, len_2, ListLen, &[ARG_2], next);

        let eq_len = self.create_symbol(ident_ids, "eq_len");
        let eq_len_stmt = |next| self.let_lowlevel(LAYOUT_BOOL, eq_len, Eq, &[len_1, len_2], next);

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
        let start_addr_1 = self.create_symbol(ident_ids, "start_addr_1");
        let start_addr_2 = self.create_symbol(ident_ids, "start_addr_2");
        let start_addr_1_stmt =
            |next| self.let_lowlevel(layout_isize, start_addr_1, PtrCast, &[elements_1], next);
        let start_addr_2_stmt =
            |next| self.let_lowlevel(layout_isize, start_addr_2, PtrCast, &[elements_2], next);

        //
        // Loop initialisation
        //

        // let elem_size = literal int
        let elem_size = self.create_symbol(ident_ids, "elem_size");
        let elem_size_expr =
            Expr::Literal(Literal::Int(elem_layout.stack_size(self.ptr_size) as i128));
        let elem_size_stmt = |next| Stmt::Let(elem_size, elem_size_expr, layout_isize, next);

        // let list_size = len_1 * elem_size
        let list_size = self.create_symbol(ident_ids, "list_size");
        let list_size_stmt =
            |next| self.let_lowlevel(layout_isize, list_size, NumMul, &[len_1, elem_size], next);

        // let end_addr_1 = start_addr_1 + list_size
        let end_addr_1 = self.create_symbol(ident_ids, "end_addr_1");
        let end_addr_1_stmt = |next| {
            self.let_lowlevel(
                layout_isize,
                end_addr_1,
                NumAdd,
                &[start_addr_1, list_size],
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
        let box1_stmt = |next| self.let_lowlevel(box_layout, box1, PtrCast, &[addr1], next);
        let box2_stmt = |next| self.let_lowlevel(box_layout, box2, PtrCast, &[addr2], next);

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
        let eq_elems_expr = self.apply_op_to_sub_layout(HelperOp::Eq, elem_layout, &[elem1, elem2]);
        let eq_elems_stmt = |next| Stmt::Let(eq_elems, eq_elems_expr, LAYOUT_BOOL, next);

        // If current elements are equal, loop back again
        let next_addr_1 = self.create_symbol(ident_ids, "next_addr_1");
        let next_addr_2 = self.create_symbol(ident_ids, "next_addr_2");
        let next_addr_1_stmt =
            |next| self.let_lowlevel(layout_isize, next_addr_1, NumAdd, &[addr1, elem_size], next);
        let next_addr_2_stmt =
            |next| self.let_lowlevel(layout_isize, next_addr_2, NumAdd, &[addr2, elem_size], next);

        let jump_back = Stmt::Jump(elems_loop, self.arena.alloc([next_addr_1, next_addr_2]));

        //
        // Control flow
        //

        let is_end = self.create_symbol(ident_ids, "is_end");
        let is_end_stmt =
            |next| self.let_lowlevel(LAYOUT_BOOL, is_end, NumGte, &[addr1, end_addr_1], next);

        let if_elems_not_equal = self.if_false_return_false(
            eq_elems,
            // else
            self.arena.alloc(
                //
                next_addr_1_stmt(self.arena.alloc(
                    //
                    next_addr_2_stmt(self.arena.alloc(
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
            remainder: self.arena.alloc(Stmt::Jump(
                elems_loop,
                self.arena.alloc([start_addr_1, start_addr_2]),
            )),
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
                        start_addr_1_stmt(self.arena.alloc(
                            //
                            start_addr_2_stmt(self.arena.alloc(
                                //
                                elem_size_stmt(self.arena.alloc(
                                    //
                                    list_size_stmt(self.arena.alloc(
                                        //
                                        end_addr_1_stmt(self.arena.alloc(
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

    fn let_lowlevel(
        &self,
        result_layout: Layout<'a>,
        result: Symbol,
        op: LowLevel,
        args: &[Symbol],
        next: &'a Stmt<'a>,
    ) -> Stmt<'a> {
        Stmt::Let(
            result,
            Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: self.arena.alloc_slice_copy(args),
            }),
            result_layout,
            next,
        )
    }
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
