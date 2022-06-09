use bumpalo::Bump;
use roc_builtins::bitcode::IntWidth;
use roc_collections::MutMap;
use roc_module::ident::{ForeignSymbol, ModuleName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, ModuleId, Symbol, Interns};
use roc_mono::ir::{
    Call, CallType, Expr, HostExposedLayouts, Proc, ProcLayout, SelfRecursive, Stmt, UpdateModeId,
};
use roc_mono::layout::{Builtin, Layout};
use std::fs;
use std::process::Command;

const TEST_HOST_SOURCE: &str = "tests/linking_tests_host.zig";
const TEST_HOST_TARGET: &str = "tests/linking_tests_host.wasm";

fn build_host() -> Vec<u8> {
    let args = [
        "build-obj",
        "-target",
        "wasm32-freestanding-musl",
        TEST_HOST_SOURCE,
        &format!("-femit-bin={}", TEST_HOST_TARGET),
    ];

    // println!("zig {}", args.join(" "));

    Command::new("zig")
        .args(args)
        .output()
        .expect("failed to compile host");

    fs::read(TEST_HOST_TARGET).unwrap()
}

fn build_app<'a>(arena: &'a Bump) -> MutMap<(Symbol, ProcLayout<'a>), Proc<'a>> {
    let int_layout = Layout::Builtin(Builtin::Int(IntWidth::I32));
    let int_layout_ref = arena.alloc(int_layout);

    // Abusing some constants, because all the stuff to make Symbols is private!
    let proc_name = Symbol::USER_FUNCTION;
    let js_call_result = Symbol::DEV_TMP;
    let host_call_result = Symbol::DEV_TMP2;
    let return_value = Symbol::DEV_TMP3;

    let js_call = Expr::Call(Call {
        call_type: CallType::Foreign {
            foreign_symbol: ForeignSymbol::from("js_called_directly_from_roc"),
            ret_layout: int_layout_ref,
        },
        arguments: &[Symbol::ARG_1],
    });

    let host_call = Expr::Call(Call {
        call_type: CallType::Foreign {
            foreign_symbol: ForeignSymbol::from("host_called_directly_from_roc"),
            ret_layout: int_layout_ref,
        },
        arguments: &[Symbol::ARG_1],
    });

    let add = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::NumAdd,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: arena.alloc([js_call_result, host_call_result]),
    });

    let body = Stmt::Let(
        js_call_result,
        js_call,
        int_layout,
        arena.alloc(Stmt::Let(
            host_call_result,
            host_call,
            int_layout,
            arena.alloc(Stmt::Let(
                return_value,
                add,
                int_layout,
                arena.alloc(Stmt::Ret(return_value)),
            )),
        )),
    );

    let proc = Proc {
        name: proc_name,
        args: arena.alloc([(int_layout, Symbol::ARG_1)]),
        body,
        closure_data_layout: None,
        ret_layout: int_layout,
        is_self_recursive: SelfRecursive::NotSelfRecursive,
        must_own_arguments: false,
        host_exposed_layouts: HostExposedLayouts::NotHostExposed,
    };

    let proc_layout = ProcLayout {
        arguments: arena.alloc([int_layout]),
        result: int_layout,
    };

    let mut app = MutMap::default();
    app.insert((proc_name, proc_layout), proc);
    app
}

fn build_interns() -> Interns {
    todo!()
}

#[test]
fn test_linking() {
    let arena = Bump::new();
    let host_bytes = build_host();
    let app_mono = build_app(&arena);
    let interns = build_interns();
    todo!()
}
