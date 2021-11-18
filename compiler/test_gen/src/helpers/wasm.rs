use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use std::cell::Cell;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use tempfile::{tempdir, TempDir};

use crate::helpers::from_wasm32_memory::FromWasm32Memory;
use crate::helpers::wasm32_test_result::Wasm32TestResult;
use roc_builtins::bitcode;
use roc_can::builtins::builtin_defs_map;
use roc_collections::all::{MutMap, MutSet};
use roc_gen_wasm::wasm_module::linking::{WasmObjectSymbol, WASM_SYM_UNDEFINED};
use roc_gen_wasm::wasm_module::sections::{Import, ImportDesc};
use roc_gen_wasm::wasm_module::{
    CodeBuilder, Export, ExportType, LocalId, Signature, SymInfo, ValueType, WasmModule,
};
use roc_gen_wasm::MEMORY_NAME;

const TEST_WRAPPER_NAME: &str = "test_wrapper";

std::thread_local! {
    static TEST_COUNTER: Cell<u32> = Cell::new(0);
}

fn promote_expr_to_module(src: &str) -> String {
    let mut buffer = String::from("app \"test\" provides [ main ] to \"./platform\"\n\nmain =\n");

    for line in src.lines() {
        // indent the body!
        buffer.push_str("    ");
        buffer.push_str(line);
        buffer.push('\n');
    }

    buffer
}

#[allow(dead_code)]
pub fn helper_wasm<'a, T: Wasm32TestResult>(
    arena: &'a bumpalo::Bump,
    src: &str,
    stdlib: &'a roc_builtins::std::StdLib,
    _result_type_dummy: &T,
) -> wasmer::Instance {
    use std::path::{Path, PathBuf};

    let filename = PathBuf::from("Test.roc");
    let src_dir = Path::new("fake/test/path");

    let module_src;
    let temp;
    if src.starts_with("app") {
        // this is already a module
        module_src = src;
    } else {
        // this is an expression, promote it to a module
        temp = promote_expr_to_module(src);
        module_src = &temp;
    }

    let exposed_types = MutMap::default();
    let loaded = roc_load::file::load_and_monomorphize_from_str(
        arena,
        filename,
        module_src,
        stdlib,
        src_dir,
        exposed_types,
        8,
        builtin_defs_map,
    );

    let loaded = loaded.expect("failed to load module");

    use roc_load::file::MonomorphizedModule;
    let MonomorphizedModule {
        procedures,
        interns,
        exposed_to_host,
        ..
    } = loaded;

    // You can comment and uncomment this block out to get more useful information
    // while you're working on the wasm backend!
    // {
    //     println!("=========== Procedures ==========");
    //     println!("{:?}", procedures);
    //     println!("=================================\n");

    //     println!("=========== Interns    ==========");
    //     println!("{:?}", interns);
    //     println!("=================================\n");

    //     println!("=========== Exposed    ==========");
    //     println!("{:?}", exposed_to_host);
    //     println!("=================================\n");
    // }

    debug_assert_eq!(exposed_to_host.len(), 1);

    let exposed_to_host = exposed_to_host.keys().copied().collect::<MutSet<_>>();

    let env = roc_gen_wasm::Env {
        arena,
        interns,
        exposed_to_host,
    };

    let (mut wasm_module, main_fn_index) =
        roc_gen_wasm::build_module_help(&env, procedures).unwrap();

    T::insert_test_wrapper(arena, &mut wasm_module, TEST_WRAPPER_NAME, main_fn_index);

    // We can either generate the test platform or write an external source file, whatever works
    generate_test_platform(&mut wasm_module, arena);

    let mut module_bytes = std::vec::Vec::with_capacity(4096);
    wasm_module.serialize_mut(&mut module_bytes);

    // now, do wasmer stuff

    use wasmer::{Instance, Module, Store};

    let store = Store::default();

    // Keep the final .wasm file for debugging with wasm-objdump or wasm2wat
    const DEBUG_WASM_FILE: bool = true;

    let wasmer_module = {
        let tmp_dir: TempDir; // directory for normal test runs, deleted when dropped
        let debug_dir: String; // persistent directory for debugging

        let wasm_build_dir: &Path = if DEBUG_WASM_FILE {
            // Directory name based on a hash of the Roc source
            let mut hash_state = DefaultHasher::new();
            src.hash(&mut hash_state);
            let src_hash = hash_state.finish();
            debug_dir = format!("/tmp/roc/gen_wasm/{:016x}", src_hash);
            std::fs::create_dir_all(&debug_dir).unwrap();
            println!(
                "Debug command:\n\twasm-objdump -sdx {}/final.wasm",
                &debug_dir
            );
            Path::new(&debug_dir)
        } else {
            tmp_dir = tempdir().unwrap();
            tmp_dir.path()
        };

        let final_wasm_file = wasm_build_dir.join("final.wasm");
        let app_o_file = wasm_build_dir.join("app.o");
        let libc_a_file = "../gen_wasm/lib/libc.a";

        // write the module to a file so the linker can access it
        std::fs::write(&app_o_file, &module_bytes).unwrap();

        let _linker_output = std::process::Command::new("zig")
            .args(&[
                "wasm-ld",
                // input files
                app_o_file.to_str().unwrap(),
                bitcode::BUILTINS_WASM32_OBJ_PATH,
                libc_a_file,
                // output
                "-o",
                final_wasm_file.to_str().unwrap(),
                // we don't define `_start`
                "--no-entry",
                // If you only specify test_wrapper, it will stop at the call to UserApp_main_1
                // But if you specify both exports, you get all the dependencies.
                //
                // It seems that it will not write out an export you didn't explicitly specify,
                // even if it's a dependency of another export!
                // In our case we always export main and test_wrapper so that's OK.
                "--export",
                "test_wrapper",
                "--export",
                "#UserApp_main_1",
            ])
            .output()
            .unwrap();

        // dbg!(_linker_output);

        Module::from_file(&store, &final_wasm_file).unwrap()
    };

    // First, we create the `WasiEnv`
    use wasmer_wasi::WasiState;
    let mut wasi_env = WasiState::new("hello").finalize().unwrap();

    // Then, we get the import object related to our WASI
    // and attach it to the Wasm instance.
    let import_object = wasi_env
        .import_object(&wasmer_module)
        .unwrap_or_else(|_| wasmer::imports!());

    Instance::new(&wasmer_module, &import_object).unwrap()
}

#[allow(dead_code)]
pub fn assert_wasm_evals_to_help<T>(src: &str, expected: T) -> Result<T, String>
where
    T: FromWasm32Memory + Wasm32TestResult,
{
    let arena = bumpalo::Bump::new();

    // NOTE the stdlib must be in the arena; just taking a reference will segfault
    let stdlib = arena.alloc(roc_builtins::std::standard_stdlib());

    let instance = crate::helpers::wasm::helper_wasm(&arena, src, stdlib, &expected);

    let memory = instance.exports.get_memory(MEMORY_NAME).unwrap();

    let test_wrapper = instance.exports.get_function(TEST_WRAPPER_NAME).unwrap();

    match test_wrapper.call(&[]) {
        Err(e) => Err(format!("{:?}", e)),
        Ok(result) => {
            let address = match result[0] {
                wasmer::Value::I32(a) => a,
                _ => panic!(),
            };

            let output = <T as FromWasm32Memory>::decode(memory, address as u32);

            Ok(output)
        }
    }
}

#[allow(unused_macros)]
macro_rules! assert_wasm_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        match $crate::helpers::wasm::assert_wasm_evals_to_help::<$ty>($src, $expected) {
            Err(msg) => panic!("{:?}", msg),
            Ok(actual) => {
                assert_eq!($transform(actual), $expected)
            }
        }
    };

    ($src:expr, $expected:expr, $ty:ty) => {
        $crate::helpers::wasm::assert_wasm_evals_to!(
            $src,
            $expected,
            $ty,
            $crate::helpers::wasm::identity
        );
    };

    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        $crate::helpers::wasm::assert_wasm_evals_to!($src, $expected, $ty, $transform);
    };
}

#[allow(unused_macros)]
macro_rules! assert_evals_to {
    ($src:expr, $expected:expr, $ty:ty) => {{
        assert_evals_to!($src, $expected, $ty, $crate::helpers::wasm::identity);
    }};
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        // Same as above, except with an additional transformation argument.
        {
            $crate::helpers::wasm::assert_wasm_evals_to!($src, $expected, $ty, $transform);
        }
    };
}

#[allow(dead_code)]
pub fn identity<T>(value: T) -> T {
    value
}

#[allow(unused_imports)]
pub(crate) use assert_evals_to;
#[allow(unused_imports)]
pub(crate) use assert_wasm_evals_to;

fn wrap_libc_fn<'a>(
    module: &mut WasmModule<'a>,
    arena: &'a Bump,
    roc_name: &'a str,
    libc_name: &'a str,
    params: &'a [(ValueType, bool)],
    ret_type: Option<ValueType>,
) {
    let symbol_table = module.linking.symbol_table_mut();

    // Type signatures
    let mut wrapper_signature = Signature {
        param_types: Vec::with_capacity_in(params.len(), arena),
        ret_type,
    };
    let mut libc_signature = Signature {
        param_types: Vec::with_capacity_in(params.len(), arena),
        ret_type,
    };
    for (ty, used) in params.iter() {
        wrapper_signature.param_types.push(*ty);
        if *used {
            libc_signature.param_types.push(*ty);
        }
    }

    /*
     * Import a function from libc
     */
    let libc_signature_index = module.types.insert(libc_signature);

    // Import
    let import_index = module.import.entries.len() as u32;
    module.import.entries.push(Import {
        module: "env",
        name: libc_name.to_string(),
        description: ImportDesc::Func {
            signature_index: libc_signature_index,
        },
    });

    // Linker info
    let libc_sym_idx = symbol_table.len() as u32;
    symbol_table.push(SymInfo::Function(WasmObjectSymbol::Imported {
        flags: WASM_SYM_UNDEFINED,
        index: import_index,
    }));

    /*
     * Export a wrapper function
     */

    // Declaration
    let wrapper_sig_index = module.types.insert(wrapper_signature);
    module.function.signature_indices.push(wrapper_sig_index);

    // Body
    let mut code_builder = CodeBuilder::new(arena);
    let mut num_libc_args = 0;
    for (i, (_, used)) in params.iter().enumerate() {
        if *used {
            code_builder.get_local(LocalId(i as u32));
            num_libc_args += 1;
        }
    }
    code_builder.call(
        import_index,
        libc_sym_idx,
        num_libc_args,
        ret_type.is_some(),
    );
    code_builder.build_fn_header(&[], 0, None);
    let wrapper_index = module.code.code_builders.len() as u32;
    module.code.code_builders.push(code_builder);

    // Export
    module.export.entries.push(Export {
        name: roc_name.to_string(),
        ty: ExportType::Func,
        index: wrapper_index,
    });

    // Linker symbol
    symbol_table.push(SymInfo::Function(WasmObjectSymbol::Defined {
        flags: 0,
        index: wrapper_index,
        name: roc_name.to_string(),
    }));
}

fn generate_test_platform<'a>(module: &mut WasmModule<'a>, arena: &'a Bump) {
    use ValueType::I32;

    wrap_libc_fn(
        module,
        arena,
        "roc_alloc",
        "malloc",
        // only the first argument of roc_alloc is passed to malloc
        &[(I32, true), (I32, false)],
        Some(I32),
    );
    wrap_libc_fn(
        module,
        arena,
        "roc_dealloc",
        "free",
        &[(I32, true), (I32, false)],
        None,
    );
    wrap_libc_fn(
        module,
        arena,
        "roc_realloc",
        "realloc",
        &[(I32, true), (I32, false), (I32, true), (I32, false)],
        Some(I32),
    );
}
