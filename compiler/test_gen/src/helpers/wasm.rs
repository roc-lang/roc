use std::cell::Cell;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use crate::helpers::from_wasm32_memory::FromWasm32Memory;
use crate::helpers::wasm32_test_result::Wasm32TestResult;
use roc_can::builtins::builtin_defs_map;
use roc_collections::all::{MutMap, MutSet};

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
    let main_fn_symbol = loaded.entry_point.symbol;
    let main_fn_index = procedures
        .keys()
        .position(|(s, _)| *s == main_fn_symbol)
        .unwrap();

    let exposed_to_host = exposed_to_host.keys().copied().collect::<MutSet<_>>();

    let env = roc_gen_wasm::Env {
        arena,
        interns,
        exposed_to_host,
    };

    let mut wasm_module = roc_gen_wasm::build_module_help(&env, procedures).unwrap();

    T::insert_test_wrapper(
        arena,
        &mut wasm_module,
        TEST_WRAPPER_NAME,
        main_fn_index as u32,
    );

    let mut module_bytes = std::vec::Vec::with_capacity(4096);
    wasm_module.serialize(&mut module_bytes);

    // for debugging (e.g. with wasm2wat or wasm-objdump)
    if false {
        use std::io::Write;

        let mut hash_state = DefaultHasher::new();
        src.hash(&mut hash_state);
        let src_hash = hash_state.finish();

        // Filename contains a hash of the Roc test source code. Helpful when comparing across commits.
        let dir = "/tmp/roc/compiler/gen_wasm/output";
        std::fs::create_dir_all(dir).unwrap();
        let path = format!("{}/test-{:016x}.wasm", dir, src_hash);

        // Print out filename (appears just after test name)
        println!("dumping file {:?}", path);

        match std::fs::File::create(path) {
            Err(e) => eprintln!("Problem creating wasm debug file: {:?}", e),
            Ok(mut file) => {
                file.write_all(&module_bytes).unwrap();
            }
        }
    }

    // now, do wasmer stuff

    use wasmer::{Instance, Module, Store};

    let store = Store::default();
    // let module = Module::from_file(&store, &test_wasm_path).unwrap();
    let wasmer_module = Module::from_binary(&store, &module_bytes).unwrap();

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

    let memory = instance.exports.get_memory("memory").unwrap();

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
