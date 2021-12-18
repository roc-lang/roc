use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_builtins::bitcode::IntWidth;
use roc_module::ident::Ident;
use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};

use crate::ir::{
    BranchInfo, Call, CallSpecId, CallType, Expr, HostExposedLayouts, Literal, ModifyRc, Proc,
    ProcLayout, SelfRecursive, Stmt, UpdateModeId,
};
use crate::layout::{Builtin, Layout, UnionLayout};

const LAYOUT_BOOL: Layout = Layout::Builtin(Builtin::Bool);
const LAYOUT_UNIT: Layout = Layout::Struct(&[]);
const LAYOUT_PTR: Layout = Layout::RecursivePointer;
const LAYOUT_U32: Layout = Layout::Builtin(Builtin::Int(IntWidth::U32));

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
                // No generated procs for DecRef, just lowlevel calls

                // Get a pointer to the refcount itself
                let rc_ptr_sym = self.create_symbol(ident_ids, "rc_ptr");
                let rc_ptr_expr = Expr::Call(Call {
                    call_type: CallType::LowLevel {
                        op: LowLevel::RefCountGetPtr,
                        update_mode: UpdateModeId::BACKEND_DUMMY,
                    },
                    arguments: arena.alloc([*structure]),
                });
                let rc_ptr_stmt = |next| Stmt::Let(rc_ptr_sym, rc_ptr_expr, LAYOUT_PTR, next);

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
                let rc_stmt = arena.alloc(rc_ptr_stmt(arena.alloc(call_stmt)));

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
    pub fn specialize_equals(
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
    //              TRAVERSE LAYOUT & CREATE PROC NAMES
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
                self.get_or_create_proc_symbols_visit(ident_ids, new_procs_info, op, child);
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
    pub fn generate_procs(
        &mut self,
        arena: &'a Bump,
        ident_ids: &mut IdentIds,
    ) -> Vec<'a, Proc<'a>> {
        use HelperOp::*;

        // Move the vector out of self, so we can loop over it safely
        let mut specs = std::mem::replace(&mut self.specs, Vec::with_capacity_in(0, arena));

        let procs_iter = specs.drain(0..).map(|(layout, op, proc_symbol)| match op {
            Inc | Dec | DecRef => {
                debug_assert!(Self::is_rc_implemented_yet(&layout));
                let rc_todo = || todo!("Please update is_rc_implemented_yet for `{:?}`", layout);

                match layout {
                    Layout::Builtin(
                        Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal,
                    ) => unreachable!("Not refcounted: {:?}", layout),
                    Layout::Builtin(Builtin::Str) => {
                        self.gen_modify_str(ident_ids, op, proc_symbol)
                    }
                    Layout::Builtin(Builtin::Dict(_, _) | Builtin::Set(_) | Builtin::List(_)) => {
                        rc_todo()
                    }
                    Layout::Struct(_) => rc_todo(),
                    Layout::Union(union_layout) => match union_layout {
                        UnionLayout::NonRecursive(_) => rc_todo(),
                        UnionLayout::Recursive(_) => rc_todo(),
                        UnionLayout::NonNullableUnwrapped(_) => rc_todo(),
                        UnionLayout::NullableWrapped { .. } => rc_todo(),
                        UnionLayout::NullableUnwrapped { .. } => rc_todo(),
                    },
                    Layout::LambdaSet(_) => unreachable!(
                        "Refcounting on LambdaSet is invalid. Should be a Union at runtime."
                    ),
                    Layout::RecursivePointer => rc_todo(),
                }
            }
            Eq => {
                let eq_todo = || todo!("Specialized `==` operator for `{:?}`", layout);

                match layout {
                    Layout::Builtin(
                        Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal,
                    ) => unreachable!(
                        "No generated proc for `==`. Use direct code gen for {:?}",
                        layout
                    ),
                    Layout::Builtin(Builtin::Str) => {
                        unreachable!("No generated helper proc for `==` on Str. Use Zig function.")
                    }
                    Layout::Builtin(Builtin::Dict(_, _) | Builtin::Set(_) | Builtin::List(_)) => {
                        eq_todo()
                    }
                    Layout::Struct(_) => eq_todo(),
                    Layout::Union(union_layout) => {
                        self.eq_tag_union(ident_ids, proc_symbol, union_layout)
                    }
                    Layout::LambdaSet(_) => unreachable!("`==` is not defined on functions"),
                    Layout::RecursivePointer => eq_todo(),
                }
            }
        });

        Vec::from_iter_in(procs_iter, arena)
    }

    fn return_unit(&self, ident_ids: &mut IdentIds) -> Stmt<'a> {
        let unit = self.create_symbol(ident_ids, "unit");
        let ret_stmt = self.arena.alloc(Stmt::Ret(unit));
        Stmt::Let(unit, Expr::Struct(&[]), LAYOUT_UNIT, ret_stmt)
    }

    fn gen_args(&self, op: HelperOp, layout: Layout<'a>) -> &'a [(Layout<'a>, Symbol)] {
        let roc_value = (layout, Symbol::ARG_1);
        match op {
            HelperOp::Inc => {
                let inc_amount = (self.layout_isize, Symbol::ARG_2);
                self.arena.alloc([roc_value, inc_amount])
            }
            HelperOp::Dec | HelperOp::DecRef => self.arena.alloc([roc_value]),
            HelperOp::Eq => self.arena.alloc([roc_value, (layout, Symbol::ARG_2)]),
        }
    }

    // ============================================================================
    //
    //              GENERATE REFCOUNTING
    //
    // ============================================================================

    /// Generate a procedure to modify the reference count of a Str
    fn gen_modify_str(
        &mut self,
        ident_ids: &mut IdentIds,
        op: HelperOp,
        proc_name: Symbol,
    ) -> Proc<'a> {
        let string = Symbol::ARG_1;
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
        let elements_stmt = |next| Stmt::Let(elements, elements_expr, LAYOUT_PTR, next);

        // Get a pointer to the refcount value, just below the elements pointer
        let rc_ptr = self.create_symbol(ident_ids, "rc_ptr");
        let rc_ptr_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::RefCountGetPtr,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: self.arena.alloc([elements]),
        });
        let rc_ptr_stmt = |next| Stmt::Let(rc_ptr, rc_ptr_expr, LAYOUT_PTR, next);

        // Alignment constant
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
                arguments: self.arena.alloc([rc_ptr, Symbol::ARG_2]),
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
            rc_ptr_stmt(self.arena.alloc(
                //
                alignment_stmt(self.arena.alloc(
                    //
                    zig_call_stmt(self.arena.alloc(
                        //
                        Stmt::Ret(zig_call_result),
                    )),
                )),
            )),
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
        let body = len_stmt(self.arena.alloc(
            //
            zero_stmt(self.arena.alloc(
                //
                is_big_str_stmt(self.arena.alloc(
                    //
                    if_stmt,
                )),
            )),
        ));

        let args = self.gen_args(op, Layout::Builtin(Builtin::Str));

        Proc {
            name: proc_name,
            args,
            body,
            closure_data_layout: None,
            ret_layout: LAYOUT_UNIT,
            is_self_recursive: SelfRecursive::NotSelfRecursive,
            must_own_arguments: false,
            host_exposed_layouts: HostExposedLayouts::NotHostExposed,
        }
    }

    // ============================================================================
    //
    //              GENERATE EQUALS
    //
    // ============================================================================

    fn eq_tag_union(
        &mut self,
        ident_ids: &mut IdentIds,
        proc_name: Symbol,
        union_layout: UnionLayout<'a>,
    ) -> Proc<'a> {
        //
        todo!("return something")
    }
}

/// Helper to derive a debug function name from a layout
fn layout_debug_name<'a>(layout: &Layout<'a>) -> &'static str {
    match layout {
        Layout::Builtin(Builtin::List(_)) => "list",
        Layout::Builtin(Builtin::Set(_)) => "set",
        Layout::Builtin(Builtin::Dict(_, _)) => "dict",
        Layout::Builtin(Builtin::Str) => "str",
        Layout::Builtin(builtin) => {
            debug_assert!(!builtin.is_refcounted());
            unreachable!("Builtin {:?} is not refcounted", builtin);
        }
        Layout::Struct(_) => "struct",
        Layout::Union(_) => "union",
        Layout::LambdaSet(_) => "lambdaset",
        Layout::RecursivePointer => "recursive_pointer",
    }
}
