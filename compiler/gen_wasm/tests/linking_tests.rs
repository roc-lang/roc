use bumpalo::Bump;
use roc_gen_wasm::Env;
use std::fs;
use std::process::Command;

use roc_builtins::bitcode::IntWidth;
use roc_collections::{MutMap, MutSet};
use roc_gen_wasm::wasm_module::WasmModule;
use roc_module::ident::{ForeignSymbol, ModuleName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{
    IdentIds, IdentIdsByModule, Interns, ModuleId, ModuleIds, PackageModuleIds, PackageQualified,
    Symbol,
};
use roc_mono::ir::{
    Call, CallType, Expr, HostExposedLayouts, Proc, ProcLayout, SelfRecursive, Stmt, UpdateModeId,
};
use roc_mono::layout::{Builtin, Layout};

const TEST_HOST_SOURCE: &str = "tests/linking_tests_host.zig";
const TEST_HOST_TARGET: &str = "tests/linking_tests_host.wasm";

fn build_host() -> std::vec::Vec<u8> {
    let args = [
        "build-obj",
        "-target",
        "wasm32-freestanding-musl",
        TEST_HOST_SOURCE,
        &format!("-femit-bin={}", TEST_HOST_TARGET),
    ];

    Command::new("zig")
        .args(args)
        .output()
        .expect("failed to compile host");

    println!("Built linking test host at {}", TEST_HOST_TARGET);
    fs::read(TEST_HOST_TARGET).unwrap()
}

fn create_symbol(home: ModuleId, ident_ids: &mut IdentIds, debug_name: &str) -> Symbol {
    let ident_id = ident_ids.add_str(debug_name);
    Symbol::new(home, ident_id)
}

// Build a fake Roc app in mono IR
// Calls two host functions, one Wasm and one JS
fn build_app_mono<'a>(
    arena: &'a Bump,
    home: ModuleId,
    ident_ids: &mut IdentIds,
) -> (Symbol, MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>) {
    let int_layout = Layout::Builtin(Builtin::Int(IntWidth::I32));
    let int_layout_ref = arena.alloc(int_layout);

    let proc_name = create_symbol(home, ident_ids, "proc_name");
    let js_call_result = create_symbol(home, ident_ids, "js_call_result");
    let host_call_result = create_symbol(home, ident_ids, "host_call_result");
    let return_value = create_symbol(home, ident_ids, "return_value");

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

    (proc_name, app)
}

struct BackendInputs<'a> {
    env: Env<'a>,
    interns: Interns,
    host_module: WasmModule<'a>,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
}

impl<'a> BackendInputs<'a> {
    fn new(arena: &'a Bump) -> Self {
        // Compile the host from an external source file
        let host_bytes = build_host();
        let host_module: WasmModule = roc_gen_wasm::parse_host(arena, &host_bytes).unwrap();

        // Identifier stuff to build the mono IR
        let module_name = ModuleName::from("UserApp");
        let pkg_qualified_module_name = PackageQualified::Unqualified(module_name);
        let mut package_module_ids = PackageModuleIds::default();
        let module_id: ModuleId = package_module_ids.get_or_insert(&pkg_qualified_module_name);
        let mut ident_ids = IdentIds::default();

        // IR for the app
        let (roc_main_sym, procedures) = build_app_mono(arena, module_id, &mut ident_ids);
        let mut exposed_to_host = MutSet::default();
        exposed_to_host.insert(roc_main_sym);
        let env = Env {
            arena,
            module_id,
            exposed_to_host,
        };

        // Identifier stuff for the backend
        let module_ids = ModuleIds::default();
        let mut all_ident_ids: IdentIdsByModule = IdentIds::exposed_builtins(1);
        all_ident_ids.insert(module_id, ident_ids);
        let interns = Interns {
            module_ids,
            all_ident_ids,
        };

        BackendInputs {
            env,
            interns,
            host_module,
            procedures,
        }
    }
}

#[test]
fn test_linking_without_dce() {
    let arena = Bump::new();

    let BackendInputs {
        env,
        mut interns,
        host_module,
        procedures,
    } = BackendInputs::new(&arena);

    let host_import_names = Vec::from_iter(host_module.import.imports.iter().map(|i| i.name));
    assert_eq!(
        &host_import_names,
        &[
            "__linear_memory",
            "__stack_pointer",
            "js_called_indirectly_from_roc",
            "js_called_indirectly_from_main",
            "js_unused",
            "js_called_directly_from_roc",
            "js_called_directly_from_main",
            "__indirect_function_table",
        ]
    );

    // If linking between app and host fails, backend will panic
    let (final_module, _called_preload_fns, _roc_main_index) =
        roc_gen_wasm::build_app_module(&env, &mut interns, host_module, procedures);

    {
        let mut buffer = Vec::with_capacity(final_module.size());
        final_module.serialize(&mut buffer);
        fs::write("tests/without_dce.wasm", buffer).unwrap();
    }

    let final_import_names = Vec::from_iter(final_module.import.imports.iter().map(|i| i.name));

    assert_eq!(
        &final_import_names,
        &[
            "js_called_indirectly_from_roc",
            "js_called_indirectly_from_main",
            "js_unused",
            "js_called_directly_from_roc",
            "js_called_directly_from_main",
        ]
    );
}

#[test]
fn test_linking_with_dce() {
    let arena = Bump::new();

    let BackendInputs {
        env,
        mut interns,
        host_module,
        procedures,
    } = BackendInputs::new(&arena);

    let host_import_names = Vec::from_iter(host_module.import.imports.iter().map(|i| i.name));
    assert_eq!(
        &host_import_names,
        &[
            "__linear_memory",
            "__stack_pointer",
            "js_called_indirectly_from_roc",
            "js_called_indirectly_from_main",
            "js_unused",
            "js_called_directly_from_roc",
            "js_called_directly_from_main",
            "__indirect_function_table",
        ]
    );

    // If linking between app and host fails, backend will panic
    let (mut final_module, called_preload_fns, _roc_main_index) =
        roc_gen_wasm::build_app_module(&env, &mut interns, host_module, procedures);

    final_module.eliminate_dead_code(env.arena, &called_preload_fns);

    let final_import_names = Vec::from_iter(final_module.import.imports.iter().map(|i| i.name));

    assert_eq!(
        &final_import_names,
        &[
            "js_called_indirectly_from_roc",
            "js_called_indirectly_from_main",
            "js_called_directly_from_roc",
            "js_called_directly_from_main",
        ]
    );
}
