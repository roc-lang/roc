use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_builtins::bitcode::IntWidth;
use roc_module::low_level::LowLevel;
use roc_module::symbol::{Interns, ModuleId, Symbol};

use crate::ir::{
    BranchInfo, Call, CallSpecId, CallType, Expr, HostExposedLayouts, Literal, ModifyRc, Proc,
    ProcLayout, SelfRecursive, Stmt, UpdateModeId,
};
use crate::layout::{Builtin, Layout};

/*
    Generate specialized refcounting procedures in IR format,
    which can then be lowered by any of the backends
*/

const LAYOUT_BOOL: Layout = Layout::Builtin(Builtin::Bool);
const LAYOUT_UNIT: Layout = Layout::Struct(&[]);
const LAYOUT_PTR: Layout = Layout::RecursivePointer;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RefcountOp {
    Inc,
    Dec,
    DecRef,
}

pub struct RefcountProcGenerator<'a> {
    arena: &'a Bump,
    home: ModuleId,
    next_symbol_id: u32,
    layout_isize: Layout<'a>,
    /// List of refcounting procs to generate, specialised by Layout and RefCountOp
    /// Order of insertion is preserved, since it is important for Wasm backend
    pub procs_to_generate: Vec<'a, (Layout<'a>, RefcountOp, Symbol)>,
}

impl<'a> RefcountProcGenerator<'a> {
    pub fn new(arena: &'a Bump, intwidth_isize: IntWidth, home: ModuleId) -> Self {
        RefcountProcGenerator {
            arena,
            home,
            next_symbol_id: 0,
            layout_isize: Layout::Builtin(Builtin::Int(intwidth_isize)),
            procs_to_generate: Vec::with_capacity_in(16, arena),
        }
    }

    /// Expands the IR node Stmt::Refcounting to a more detailed IR Stmt that calls a helper proc.
    /// The helper procs themselves can be generated later by calling `generate_refcount_proc`
    /// in a loop over `procs_to_generate`. Helpers are specialized to a particular Layout.
    pub fn expand_refcount_stmt_to_proc_call<'b>(
        &mut self,
        layout: Layout<'a>,
        modify: &ModifyRc,
        following: &'a Stmt<'a>,
    ) -> (Stmt<'a>, Option<(Symbol, ProcLayout<'a>)>) {
        match modify {
            ModifyRc::Inc(structure, amount) => {
                let (is_existing, proc_name) = self.get_proc_name(layout, RefcountOp::Inc);

                // Define a constant for the amount to increment
                let amount_sym = self.unique_symbol();
                let amount_expr = Expr::Literal(Literal::Int(*amount as i128));
                let amount_stmt = |next| Stmt::Let(amount_sym, amount_expr, LAYOUT_UNIT, next);

                // Call helper proc, passing the Roc structure and constant amount
                let arg_layouts = self.arena.alloc([layout, self.layout_isize]);
                let call_result_dummy = self.unique_symbol();
                let call_expr = Expr::Call(Call {
                    call_type: CallType::ByName {
                        name: proc_name,
                        ret_layout: &LAYOUT_UNIT,
                        arg_layouts,
                        specialization_id: CallSpecId::BACKEND_DUMMY,
                    },
                    arguments: self.arena.alloc([*structure, amount_sym]),
                });
                let call_stmt = Stmt::Let(call_result_dummy, call_expr, LAYOUT_UNIT, following);
                let rc_stmt = amount_stmt(self.arena.alloc(call_stmt));

                // Create a linker symbol for the helper proc if this is the first usage
                let new_proc_info = if is_existing {
                    None
                } else {
                    Some((
                        proc_name,
                        ProcLayout {
                            arguments: arg_layouts,
                            result: LAYOUT_UNIT,
                        },
                    ))
                };

                (rc_stmt, new_proc_info)
            }

            ModifyRc::Dec(structure) => {
                let (is_existing, proc_name) = self.get_proc_name(layout, RefcountOp::Dec);

                // Call helper proc, passing the Roc structure
                let arg_layouts = self.arena.alloc([layout, self.layout_isize]);
                let call_result_dummy = self.unique_symbol();
                let call_expr = Expr::Call(Call {
                    call_type: CallType::ByName {
                        name: proc_name,
                        ret_layout: &LAYOUT_UNIT,
                        arg_layouts: self.arena.alloc([layout]),
                        specialization_id: CallSpecId::BACKEND_DUMMY,
                    },
                    arguments: self.arena.alloc([*structure]),
                });

                let rc_stmt = Stmt::Let(call_result_dummy, call_expr, LAYOUT_UNIT, following);

                // Create a linker symbol for the helper proc if this is the first usage
                let new_proc_info = if is_existing {
                    None
                } else {
                    Some((
                        proc_name,
                        ProcLayout {
                            arguments: arg_layouts,
                            result: LAYOUT_UNIT,
                        },
                    ))
                };

                (rc_stmt, new_proc_info)
            }

            ModifyRc::DecRef(structure) => {
                // No generated procs for DecRef, just lowlevel calls

                // Get a pointer to the refcount itself
                let rc_ptr_sym = self.unique_symbol();
                let rc_ptr_expr = Expr::Call(Call {
                    call_type: CallType::LowLevel {
                        op: LowLevel::RefCountGetPtr,
                        update_mode: UpdateModeId::BACKEND_DUMMY,
                    },
                    arguments: self.arena.alloc([*structure]),
                });
                let rc_ptr_stmt = |next| Stmt::Let(rc_ptr_sym, rc_ptr_expr, LAYOUT_PTR, next);

                // Pass the refcount pointer to the lowlevel call (see utils.zig)
                let call_result_dummy = self.unique_symbol();
                let call_expr = Expr::Call(Call {
                    call_type: CallType::LowLevel {
                        op: LowLevel::RefCountDec,
                        update_mode: UpdateModeId::BACKEND_DUMMY,
                    },
                    arguments: self.arena.alloc([rc_ptr_sym]),
                });
                let call_stmt = Stmt::Let(call_result_dummy, call_expr, LAYOUT_UNIT, following);
                let rc_stmt = rc_ptr_stmt(self.arena.alloc(call_stmt));

                (rc_stmt, None)
            }
        }
    }

    /// Generate a refcounting helper proc, specialized to a particular Layout.
    /// For example `List (Result { a: Str, b: Int } Str)` would get its own helper
    /// to update the refcounts on the List, the Result and the strings.
    /// This method should be called once for every item in procs_to_generate
    pub fn generate_refcount_proc(
        &mut self,
        layout: Layout<'a>,
        op: RefcountOp,
        symbol: Symbol,
    ) -> Proc<'a> {
        match layout {
            Layout::Builtin(Builtin::Str) => self.gen_modify_str(op, symbol),
            _ => todo!("Refcounting is not yet implemented for Layout {:?}", layout),
        }
    }

    /// Find the name of the procedure for this layout and refcount operation,
    /// or create one if needed. "Names" are really just auto-generated Symbols.
    fn get_proc_name(&mut self, layout: Layout<'a>, op: RefcountOp) -> (bool, Symbol) {
        let found = self
            .procs_to_generate
            .iter()
            .find(|(l, o, _)| *l == layout && *o == op);

        if let Some((_, _, existing_name)) = found {
            (true, *existing_name)
        } else {
            let new_name: Symbol = self.unique_symbol();
            self.procs_to_generate.push((layout, op, new_name));
            (false, new_name)
        }
    }

    fn unique_symbol(&mut self) -> Symbol {
        let id = self.next_symbol_id;
        self.next_symbol_id += 1;
        Interns::from_index(self.home, id)
    }

    fn return_unit(&mut self) -> Stmt<'a> {
        let unit = self.unique_symbol();
        let ret_stmt = self.arena.alloc(Stmt::Ret(unit));
        Stmt::Let(unit, Expr::Struct(&[]), LAYOUT_UNIT, ret_stmt)
    }

    fn gen_args(&mut self, op: RefcountOp, layout: Layout<'a>) -> &'a [(Layout<'a>, Symbol)] {
        let roc_value = (layout, Symbol::ARG_1);
        match op {
            RefcountOp::Inc => {
                let inc_amount = (self.layout_isize, Symbol::ARG_2);
                self.arena.alloc([roc_value, inc_amount])
            }
            RefcountOp::Dec | RefcountOp::DecRef => self.arena.alloc([roc_value]),
        }
    }

    /// Generate a procedure to modify the reference count of a Str
    fn gen_modify_str(&mut self, op: RefcountOp, proc_name: Symbol) -> Proc<'a> {
        let string = Symbol::ARG_1;
        let layout_isize = self.layout_isize;

        // Get the string length as a signed int
        let len = self.unique_symbol();
        let len_expr = Expr::StructAtIndex {
            index: 1,
            field_layouts: self.arena.alloc([LAYOUT_PTR, layout_isize]),
            structure: string,
        };
        let len_stmt = |next| Stmt::Let(len, len_expr, layout_isize, next);

        // Zero
        let zero = self.unique_symbol();
        let zero_expr = Expr::Literal(Literal::Int(0));
        let zero_stmt = |next| Stmt::Let(zero, zero_expr, layout_isize, next);

        // is_big_str = (len >= 0);
        // Check the "sign bit" (small string flag) is zero
        let is_big_str = self.unique_symbol();
        let is_big_str_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::NumGte,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: self.arena.alloc([len, zero]),
        });
        let is_big_str_stmt = |next| Stmt::Let(is_big_str, is_big_str_expr, LAYOUT_BOOL, next);

        // Get the pointer to the string elements
        let elements = self.unique_symbol();
        let elements_expr = Expr::StructAtIndex {
            index: 0,
            field_layouts: self.arena.alloc([LAYOUT_PTR, layout_isize]),
            structure: string,
        };
        let elements_stmt = |next| Stmt::Let(elements, elements_expr, LAYOUT_PTR, next);

        // Get a pointer to the refcount value, just below the elements pointer
        let rc_ptr = self.unique_symbol();
        let rc_ptr_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::RefCountGetPtr,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: self.arena.alloc([string]),
        });
        let rc_ptr_stmt = |next| Stmt::Let(rc_ptr, rc_ptr_expr, LAYOUT_PTR, next);

        // Call the relevant Zig lowlevel to actually modify the refcount
        let zig_call_result = self.unique_symbol();
        let zig_call_expr = match op {
            RefcountOp::Inc => Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op: LowLevel::RefCountInc,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: self.arena.alloc([rc_ptr, Symbol::ARG_2]),
            }),
            RefcountOp::Dec | RefcountOp::DecRef => Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op: LowLevel::RefCountDec,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments: self.arena.alloc([rc_ptr]),
            }),
        };
        let zig_call_stmt = |next| Stmt::Let(zig_call_result, zig_call_expr, LAYOUT_BOOL, next);

        // Generate an `if` to skip small strings but modify big strings
        let then_branch = elements_stmt(self.arena.alloc(
            //
            rc_ptr_stmt(self.arena.alloc(
                //
                zig_call_stmt(self.arena.alloc(
                    //
                    Stmt::Ret(zig_call_result),
                )),
            )),
        ));
        let if_stmt = Stmt::Switch {
            cond_symbol: is_big_str,
            cond_layout: LAYOUT_BOOL,
            branches: self.arena.alloc([(1, BranchInfo::None, then_branch)]),
            default_branch: (BranchInfo::None, self.arena.alloc(self.return_unit())),
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use roc_collections::all::MutMap;
    use roc_module::ident::{IdentStr, ModuleName};
    use roc_module::symbol::{PackageModuleIds, PackageQualified};

    // TODO: Find a cleaner way to do the setup without nasty hacks!
    // I made a load of private fields public, and Symbol constants are not set up, etc.
    fn setup() -> (ModuleId, IdentIds) {
        let home = ModuleId(0);
        let home_ident = IdentStr::from("test");
        let debug_name = PackageQualified::Unqualified(ModuleName(home_ident));
        PackageModuleIds::insert_debug_name(home, &debug_name);
        let mut ident_ids = IdentIds {
            by_ident: MutMap::default(),
            by_id: std::vec::Vec::default(),
            next_generated_name: 0,
        };

        // Prevent clashing with Symbol constants that we use
        let skip = Symbol::ARG_8.as_u64() >> 32;
        for _ in 0..skip {
            ident_ids.gen_unique();
        }

        (home, ident_ids)
    }

    #[test]
    fn test_str() {
        let (home, mut ident_ids) = setup();
        let arena = &Bump::new();

        let testcases = [
            (IntWidth::I32, RefcountOp::Inc),
            (IntWidth::I32, RefcountOp::Dec),
            (IntWidth::I64, RefcountOp::Inc),
            (IntWidth::I64, RefcountOp::Dec),
        ];

        for (intwidth_isize, mode) in testcases {
            let mut env = Env {
                arena,
                home,
                ident_ids: &mut ident_ids,
                intwidth_isize,
            };
            let (name, proc_layout, proc) = gen_modify_str(&mut env, mode);
            println!(
                "\n({:?}, {:?}):\n{}",
                name,
                proc_layout,
                proc.to_pretty(200)
            );
        }
    }
}
