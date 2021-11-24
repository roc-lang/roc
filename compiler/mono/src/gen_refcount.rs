use bumpalo::Bump;
use roc_builtins::bitcode::IntWidth;
use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};

use crate::ir::{
    BranchInfo, Call, CallType, Expr, HostExposedLayouts, Literal, Proc, ProcLayout, SelfRecursive,
    Stmt, UpdateModeId,
};
use crate::layout::{Builtin, Layout};

/*
    Generate specialized refcounting procedures in IR format,
    which can then be lowered by any of the backends
*/

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RefcountOp {
    Inc,
    Dec,
    DecRef,
}

const LAYOUT_BOOL: Layout = Layout::Builtin(Builtin::Bool);
const LAYOUT_UNIT: Layout = Layout::Struct(&[]);

struct Env<'a, 'i> {
    pub arena: &'a Bump,
    pub home: ModuleId,
    pub ident_ids: &'i mut IdentIds,
    pub intwidth_isize: IntWidth, // this is the only thing not on the ir.rs Env
    pub update_mode_counter: u64,
}

impl<'a, 'i> Env<'a, 'i> {
    fn layout_isize(&self) -> Layout<'a> {
        Layout::Builtin(Builtin::Int(self.intwidth_isize))
    }

    fn layout_ptr(&self) -> Layout<'a> {
        Layout::RecursivePointer
    }

    fn unique_symbol(&mut self) -> Symbol {
        let ident_id = self.ident_ids.gen_unique();

        Symbol::new(self.home, ident_id)
    }

    fn next_update_mode_id(&mut self) -> UpdateModeId {
        let id = UpdateModeId {
            id: self.update_mode_counter,
        };

        self.update_mode_counter += 1;

        id
    }
}

/// Helper to return Unit from a procedure
fn return_unit<'a>(env: &mut Env<'a, '_>) -> Stmt<'a> {
    let unit = env.unique_symbol();
    let ret_stmt = env.arena.alloc(Stmt::Ret(unit));
    Stmt::Let(unit, Expr::Struct(&[]), LAYOUT_UNIT, ret_stmt)
}

/// Helper to generate procedure arguments
fn gen_args<'a>(
    env: &mut Env<'a, '_>,
    op: RefcountOp,
    layout: Layout<'a>,
) -> (&'a [Layout<'a>], &'a [(Layout<'a>, Symbol)]) {
    let roc_value = (layout, Symbol::ARG_1);
    match op {
        RefcountOp::Inc => {
            let layout_isize = env.layout_isize();
            let inc_amount = (layout_isize, Symbol::ARG_2);
            (
                env.arena.alloc([roc_value.0, inc_amount.0]),
                env.arena.alloc([roc_value, inc_amount]),
            )
        }
        RefcountOp::Dec | RefcountOp::DecRef => {
            (env.arena.alloc([roc_value.0]), env.arena.alloc([roc_value]))
        }
    }
}

/// Generate a procedure to modify the reference count of a Str
#[allow(dead_code)]
fn gen_modify_str<'a>(env: &mut Env<'a, '_>, op: RefcountOp) -> (Symbol, ProcLayout<'a>, Proc<'a>) {
    let string = Symbol::ARG_1;
    let layout_isize = env.layout_isize();
    let layout_ptr = env.layout_ptr();

    // Get the string length as a signed int
    let len = env.unique_symbol();
    let len_expr = Expr::StructAtIndex {
        index: 1,
        field_layouts: env.arena.alloc([layout_ptr, layout_isize]),
        structure: string,
    };
    let len_stmt = |next| Stmt::Let(len, len_expr, layout_isize, next);

    // Zero
    let zero = env.unique_symbol();
    let zero_expr = Expr::Literal(Literal::Int(0));
    let zero_stmt = |next| Stmt::Let(zero, zero_expr, layout_isize, next);

    // is_big_str = (len >= 0);
    // Check the "sign bit" (small string flag) is zero
    let is_big_str = env.unique_symbol();
    let is_big_str_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::NumGte,
            update_mode: env.next_update_mode_id(),
        },
        arguments: env.arena.alloc([len, zero]),
    });
    let is_big_str_stmt = |next| Stmt::Let(is_big_str, is_big_str_expr, LAYOUT_BOOL, next);

    // Get the pointer to the string elements
    let elements = env.unique_symbol();
    let elements_expr = Expr::StructAtIndex {
        index: 0,
        field_layouts: env.arena.alloc([layout_ptr, layout_isize]),
        structure: string,
    };
    let elements_stmt = |next| Stmt::Let(elements, elements_expr, layout_ptr, next);

    // Get a pointer to the refcount value, just below the elements pointer
    let rc_ptr = env.unique_symbol();
    let rc_ptr_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::RefCountGetPtr,
            update_mode: env.next_update_mode_id(),
        },
        arguments: env.arena.alloc([string]),
    });
    let rc_ptr_stmt = |next| Stmt::Let(rc_ptr, rc_ptr_expr, layout_ptr, next);

    // Call the relevant Zig lowlevel to actually modify the refcount
    let zig_call_result = env.unique_symbol();
    let zig_call_expr = match op {
        RefcountOp::Inc => Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::RefCountInc,
                update_mode: env.next_update_mode_id(),
            },
            arguments: env.arena.alloc([rc_ptr, Symbol::ARG_2]),
        }),
        RefcountOp::Dec | RefcountOp::DecRef => Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::RefCountDec,
                update_mode: env.next_update_mode_id(),
            },
            arguments: env.arena.alloc([rc_ptr]),
        }),
    };
    let zig_call_stmt = |next| Stmt::Let(zig_call_result, zig_call_expr, LAYOUT_BOOL, next);

    // Generate an `if` to skip small strings but modify big strings
    let then_branch = elements_stmt(env.arena.alloc(
        //
        rc_ptr_stmt(env.arena.alloc(
            //
            zig_call_stmt(env.arena.alloc(
                //
                Stmt::Ret(zig_call_result),
            )),
        )),
    ));
    let if_stmt = Stmt::Switch {
        cond_symbol: is_big_str,
        cond_layout: LAYOUT_BOOL,
        branches: env.arena.alloc([(1, BranchInfo::None, then_branch)]),
        default_branch: (BranchInfo::None, env.arena.alloc(return_unit(env))),
        ret_layout: LAYOUT_UNIT,
    };

    // Combine the statements in sequence
    let body = len_stmt(env.arena.alloc(
        //
        zero_stmt(env.arena.alloc(
            //
            is_big_str_stmt(env.arena.alloc(
                //
                if_stmt,
            )),
        )),
    ));

    let name = env.unique_symbol();
    let (arg_layouts, args) = gen_args(env, op, Layout::Builtin(Builtin::Str));
    let proc_layout = ProcLayout {
        arguments: arg_layouts,
        result: LAYOUT_UNIT,
    };

    let proc = Proc {
        name,
        args,
        body,
        closure_data_layout: None,
        ret_layout: LAYOUT_UNIT,
        is_self_recursive: SelfRecursive::NotSelfRecursive,
        must_own_arguments: false,
        host_exposed_layouts: HostExposedLayouts::NotHostExposed,
    };

    (name, proc_layout, proc)
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
                update_mode_counter: 0,
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
