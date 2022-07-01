#![cfg(feature = "gen-wasm")]

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
    Call, CallType, Expr, HostExposedLayouts, Literal, Proc, ProcLayout, SelfRecursive, Stmt,
    UpdateModeId,
};
use roc_mono::layout::{Builtin, LambdaName, Layout};

const LINKING_TEST_HOST_WASM: &str = "build/wasm_linking_test_host.wasm";
const LINKING_TEST_HOST_NATIVE: &str = "build/wasm_linking_test_host";

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

    let app_proc = create_symbol(home, ident_ids, "app_proc");
    let js_call_result = create_symbol(home, ident_ids, "js_call_result");
    let host_call_result = create_symbol(home, ident_ids, "host_call_result");
    let bitflag = create_symbol(home, ident_ids, "bitflag");
    let or1 = create_symbol(home, ident_ids, "or1");
    let or2 = create_symbol(home, ident_ids, "or2");

    let js_call = Expr::Call(Call {
        call_type: CallType::Foreign {
            foreign_symbol: ForeignSymbol::from("js_called_directly_from_roc"),
            ret_layout: int_layout_ref,
        },
        arguments: &[],
    });

    let host_call = Expr::Call(Call {
        call_type: CallType::Foreign {
            foreign_symbol: ForeignSymbol::from("host_called_directly_from_roc"),
            ret_layout: int_layout_ref,
        },
        arguments: &[],
    });

    let mut bitflag_bytes = [0; 16];
    bitflag_bytes[0] = 0x20;
    let bitflag_literal = Expr::Literal(Literal::Int(bitflag_bytes));

    let or1_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::Or,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: arena.alloc([js_call_result, host_call_result]),
    });

    let or2_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::Or,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: arena.alloc([or1, bitflag]),
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
                or1,
                or1_expr,
                int_layout,
                arena.alloc(Stmt::Let(
                    bitflag,
                    bitflag_literal,
                    int_layout,
                    arena.alloc(Stmt::Let(
                        or2,
                        or2_expr,
                        int_layout,
                        //
                        arena.alloc(Stmt::Ret(or2)),
                    )),
                )),
            )),
        )),
    );

    let proc = Proc {
        name: LambdaName::no_niche(app_proc),
        args: &[],
        body,
        closure_data_layout: None,
        ret_layout: int_layout,
        is_self_recursive: SelfRecursive::NotSelfRecursive,
        must_own_arguments: false,
        host_exposed_layouts: HostExposedLayouts::NotHostExposed,
    };

    let proc_layout = ProcLayout {
        arguments: &[],
        result: int_layout,
        captures_niche: &[],
    };

    let mut app = MutMap::default();
    app.insert((app_proc, proc_layout), proc);

    (app_proc, app)
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
        let host_bytes = fs::read(LINKING_TEST_HOST_WASM).unwrap();
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

fn execute_wasm_bytes(bytes: &[u8]) -> Result<wasmer::Instance, String> {
    use wasmer::{Function, Module, Store};

    let store = Store::default();
    let wasmer_module = Module::new(&store, bytes).map_err(|e| e.to_string())?;

    let import_object = wasmer::imports!(
        "env" => {
            "js_called_indirectly_from_roc" => Function::new_native(&store, js_called_indirectly_from_roc),
            "js_called_indirectly_from_main" => Function::new_native(&store, js_called_indirectly_from_main),
            "js_unused" => Function::new_native(&store, js_unused),
            "js_called_directly_from_roc" => Function::new_native(&store, js_called_directly_from_roc),
            "js_called_directly_from_main" => Function::new_native(&store, js_called_directly_from_main),
        }
    );

    let instance =
        wasmer::Instance::new(&wasmer_module, &import_object).map_err(|e| e.to_string())?;

    let start = instance
        .exports
        .get_function("_start")
        .map_err(|e| e.to_string())?;
    start.call(&[]).map_err(|e| e.to_string())?;

    Ok(instance)
}

fn js_called_directly_from_roc() -> i32 {
    return 0x01;
}
fn js_called_indirectly_from_roc() -> i32 {
    return 0x02;
}
fn js_called_directly_from_main() -> i32 {
    return 0x04;
}
fn js_called_indirectly_from_main() -> i32 {
    return 0x08;
}
fn js_unused() -> i32 {
    return 0x10;
}

fn get_wasm_result(instance: &wasmer::Instance) -> i32 {
    let memory = instance
        .exports
        .get_memory(roc_gen_wasm::MEMORY_NAME)
        .unwrap();
    let memory_bytes = unsafe { memory.data_unchecked() };

    // The Wasm Global tells us the memory location of the global value
    let global = instance.exports.get_global("host_result").unwrap();
    let global_addr = global.get().unwrap_i32() as usize;
    let mut global_bytes = [0; 4];
    global_bytes.copy_from_slice(&memory_bytes[global_addr..][..4]);
    i32::from_le_bytes(global_bytes)
}

fn get_native_result() -> i32 {
    let output = Command::new(LINKING_TEST_HOST_NATIVE)
        .output()
        .expect(&format!("failed to run {}", LINKING_TEST_HOST_NATIVE));

    let result_str = std::str::from_utf8(&output.stdout).unwrap().trim();
    result_str.parse().unwrap()
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
            "roc__app_proc_1_exposed",
            "__indirect_function_table",
        ]
    );

    let (final_module, _called_preload_fns, _roc_main_index) =
        roc_gen_wasm::build_app_module(&env, &mut interns, host_module, procedures);

    let mut buffer = Vec::with_capacity(final_module.size());
    final_module.serialize(&mut buffer);

    if std::env::var("DEBUG_WASM").is_ok() {
        fs::write("build/without_dce.wasm", &buffer).unwrap();
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

    let instance = execute_wasm_bytes(&buffer).unwrap();

    let wasm_result = get_wasm_result(&instance);
    assert_eq!(wasm_result, get_native_result());
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

    let host_import_names = Vec::from_iter(host_module.import.imports.iter().map(|imp| imp.name));
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
            "roc__app_proc_1_exposed",
            "__indirect_function_table",
        ]
    );

    assert!(&host_module.names.function_names.is_empty());

    let (mut final_module, called_preload_fns, _roc_main_index) =
        roc_gen_wasm::build_app_module(&env, &mut interns, host_module, procedures);

    final_module.eliminate_dead_code(env.arena, called_preload_fns);

    let mut buffer = Vec::with_capacity(final_module.size());
    final_module.serialize(&mut buffer);
    if std::env::var("DEBUG_WASM").is_ok() {
        fs::write("build/with_dce.wasm", &buffer).unwrap();
    }

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

    assert_eq!(
        &final_module.names.function_names[0..5],
        &[
            (0, "js_called_indirectly_from_roc"),
            (1, "js_called_indirectly_from_main"),
            (2, "js_called_directly_from_roc"),
            (3, "js_called_directly_from_main"),
            (4, "js_unused"),
        ]
    );

    let instance = execute_wasm_bytes(&buffer).unwrap();

    let wasm_result = get_wasm_result(&instance);
    assert_eq!(wasm_result, get_native_result());
}
