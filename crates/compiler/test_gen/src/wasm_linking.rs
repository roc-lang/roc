#![cfg(feature = "gen-wasm")]

use bumpalo::Bump;
use roc_gen_wasm::Env;
use roc_target::Target;
use std::fs;
use std::process::Command;

use roc_collections::{MutMap, MutSet};
use roc_module::ident::{ForeignSymbol, ModuleName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{
    IdentIds, IdentIdsByModule, Interns, ModuleId, ModuleIds, PackageModuleIds, PackageQualified,
    Symbol,
};
use roc_mono::ir::{
    Call, CallType, Expr, Literal, Proc, ProcLayout, SelfRecursive, Stmt, UpdateModeId,
};
use roc_mono::layout::{LambdaName, Layout, Niche, STLayoutInterner};
use roc_wasm_interp::{wasi, ImportDispatcher, Instance, WasiDispatcher};
use roc_wasm_module::{Value, WasmModule};

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
    let int_layout = Layout::I32;

    let app_proc = create_symbol(home, ident_ids, "app_proc");
    let js_call_result = create_symbol(home, ident_ids, "js_call_result");
    let host_call_result = create_symbol(home, ident_ids, "host_call_result");
    let bitflag = create_symbol(home, ident_ids, "bitflag");
    let or1 = create_symbol(home, ident_ids, "or1");
    let or2 = create_symbol(home, ident_ids, "or2");

    let js_call = Expr::Call(Call {
        call_type: CallType::Foreign {
            foreign_symbol: ForeignSymbol::from("js_called_directly_from_roc"),
            ret_layout: int_layout,
        },
        arguments: &[],
    });

    let host_call = Expr::Call(Call {
        call_type: CallType::Foreign {
            foreign_symbol: ForeignSymbol::from("host_called_directly_from_roc"),
            ret_layout: int_layout,
        },
        arguments: &[],
    });

    let mut bitflag_bytes = [0; 16];
    bitflag_bytes[0] = 0x20;
    let bitflag_literal = Expr::Literal(Literal::Int(bitflag_bytes));

    let or1_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::NumBitwiseOr,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: arena.alloc([js_call_result, host_call_result]),
    });

    let or2_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::NumBitwiseOr,
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
        is_erased: false,
    };

    let proc_layout = ProcLayout {
        arguments: &[],
        result: int_layout,
        niche: Niche::NONE,
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
            stack_bytes: Env::DEFAULT_STACK_BYTES,
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

struct TestDispatcher<'a> {
    wasi: WasiDispatcher<'a>,
}

impl ImportDispatcher for TestDispatcher<'_> {
    fn dispatch(
        &mut self,
        module_name: &str,
        function_name: &str,
        arguments: &[Value],
        memory: &mut [u8],
    ) -> Option<Value> {
        if module_name == wasi::MODULE_NAME {
            self.wasi.dispatch(function_name, arguments, memory)
        } else if module_name == "env" {
            match function_name {
                "js_called_directly_from_roc" => Some(Value::I32(0x01)),
                "js_called_indirectly_from_roc" => Some(Value::I32(0x02)),
                "js_called_directly_from_main" => Some(Value::I32(0x04)),
                "js_called_indirectly_from_main" => Some(Value::I32(0x08)),
                "js_unused" => Some(Value::I32(0x10)),
                _ => panic!("Unknown import env.{}", function_name),
            }
        } else {
            panic!(
                "TestDispatcher does not implement {}.{}",
                module_name, function_name
            );
        }
    }
}

fn execute_wasm_module<'a>(arena: &'a Bump, orig_module: WasmModule<'a>) -> Result<i32, String> {
    // FIXME: see if we can skip serializing and re-parsing!
    // Some metadata seems to be left over from the host file. e.g. CodeSection::section_start
    let module = {
        let mut buffer = Vec::with_capacity(orig_module.size());
        orig_module.serialize(&mut buffer);
        WasmModule::preload(arena, &buffer, false).map_err(|e| format!("{:?}", e))?
    };

    let dispatcher = TestDispatcher {
        wasi: wasi::WasiDispatcher::default(),
    };
    let is_debug_mode = false;
    let mut inst = Instance::for_module(&arena, &module, dispatcher, is_debug_mode)?;

    // In Zig, main can only return u8 or void, but our result is too wide for that.
    // But I want to use main so that I can test that _start is created for it!
    // So return void from main, and call another function to get the result.
    inst.call_export("_start", [])?;

    // FIXME: read_host_result does not actually appear as an export!
    // The interpreter has to look it up in debug info! (Apparently Wasm3 did this!)
    // If we change gen_wasm to export it, then it does the same for js_unused,
    // so we can't test import elimination and function reordering.
    // We should to come back to this and fix it.
    inst.call_export("read_host_result", [])?
        .ok_or(String::from("expected a return value"))?
        .expect_i32()
        .map_err(|type_err| format!("{:?}", type_err))
}

fn get_native_result() -> i32 {
    let output = Command::new(LINKING_TEST_HOST_NATIVE)
        .output()
        .expect(&format!("failed to run {}", LINKING_TEST_HOST_NATIVE));

    let result_str = std::str::from_utf8(&output.stdout).unwrap().trim();
    result_str.parse().unwrap()
}

fn test_help(
    eliminate_dead_code: bool,
    expected_host_import_names: &[&str],
    expected_linked_import_names: &[&str],
    expected_eliminated_names: &[&str],
    dump_filename: &str,
) {
    let arena = Bump::new();
    let mut layout_interner = STLayoutInterner::with_capacity(4, Target::Wasm32);

    let BackendInputs {
        env,
        mut interns,
        host_module,
        procedures,
    } = BackendInputs::new(&arena);

    let mut host_import_names =
        Vec::from_iter(host_module.import.imports.iter().map(|imp| imp.name));
    host_import_names.sort();
    assert_eq!(&host_import_names, expected_host_import_names);

    assert!(&host_module.names.function_names.is_empty());

    let (mut linked_module, called_fns, _roc_main_index) = roc_gen_wasm::build_app_module(
        &env,
        &mut layout_interner,
        &mut interns,
        host_module,
        procedures,
    );

    if eliminate_dead_code {
        linked_module.eliminate_dead_code(env.arena, called_fns);
    }

    if std::env::var("DEBUG_WASM").is_ok() {
        let mut buffer = Vec::with_capacity(linked_module.size());
        linked_module.serialize(&mut buffer);
        fs::write(dump_filename, &buffer).unwrap();
    }

    let mut linked_import_names =
        Vec::from_iter(linked_module.import.imports.iter().map(|i| i.name));
    linked_import_names.sort();
    assert_eq!(&linked_import_names, expected_linked_import_names);

    // eliminated imports appear after the non-eliminated ones in the name section
    let import_count = linked_import_names.len();
    let eliminated_count = expected_eliminated_names.len();
    let mut eliminated_names = Vec::from_iter(
        linked_module.names.function_names[import_count..][..eliminated_count]
            .iter()
            .map(|(_, name)| *name),
    );
    eliminated_names.sort();
    assert_eq!(&eliminated_names, expected_eliminated_names);

    let wasm_result = execute_wasm_module(&arena, linked_module).unwrap();
    assert_eq!(wasm_result, get_native_result());
}

const EXPECTED_HOST_IMPORT_NAMES: [&'static str; 8] = [
    "__linear_memory",                // linked with the Roc app, not imported from JS
    "__stack_pointer",                // linked with the Roc app, not imported from JS
    "js_called_directly_from_main",   // imported from JS
    "js_called_directly_from_roc",    // imported from JS
    "js_called_indirectly_from_main", // imported from JS
    "js_called_indirectly_from_roc",  // imported from JS
    "js_unused",                      // Dead code. If DCE enabled, not imported from JS.
    "roc__app_proc_1_exposed",        // linked with the Roc app, not imported from JS
];

#[test]
fn test_linking_without_dce() {
    let eliminate_dead_code = false;
    let dump_filename = "build/without_dce.wasm";

    let expected_linked_import_names = &[
        "js_called_directly_from_main",
        "js_called_directly_from_roc",
        "js_called_indirectly_from_main",
        "js_called_indirectly_from_roc",
        "js_unused",
    ];

    let expected_eliminated_names = &[];

    test_help(
        eliminate_dead_code,
        &EXPECTED_HOST_IMPORT_NAMES,
        expected_linked_import_names,
        expected_eliminated_names,
        dump_filename,
    );
}

#[test]
fn test_linking_with_dce() {
    let eliminate_dead_code = true;
    let dump_filename = "build/with_dce.wasm";

    let expected_final_import_names = &[
        "js_called_directly_from_main",
        "js_called_directly_from_roc",
        "js_called_indirectly_from_main",
        "js_called_indirectly_from_roc",
    ];

    let expected_eliminated_names = &["js_unused"];

    test_help(
        eliminate_dead_code,
        &EXPECTED_HOST_IMPORT_NAMES,
        expected_final_import_names,
        expected_eliminated_names,
        dump_filename,
    );
}
