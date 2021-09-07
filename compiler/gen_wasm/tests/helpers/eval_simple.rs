use roc_can::builtins::builtin_defs_map;
use roc_collections::all::{MutMap, MutSet};
// use roc_std::{RocDec, RocList, RocOrder, RocStr};

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
pub fn helper_wasm<'a>(
    arena: &'a bumpalo::Bump,
    src: &str,
    stdlib: &'a roc_builtins::std::StdLib,
    _is_gen_test: bool,
    _ignore_problems: bool,
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
        procedures: top_procedures,
        interns,
        exposed_to_host,
        ..
    } = loaded;

    let mut procedures = MutMap::default();

    for (key, proc) in top_procedures {
        procedures.insert(key, proc);
    }

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

    let exposed_to_host = exposed_to_host.keys().copied().collect::<MutSet<_>>();

    let env = gen_wasm::Env {
        arena,
        interns,
        exposed_to_host,
    };

    let module_bytes = gen_wasm::build_module(&env, procedures).unwrap();

    // for debugging (e.g. with wasm2wat)
    if true {
        use std::io::Write;
        let mut file =
            std::fs::File::create("/home/brian/Documents/roc/compiler/gen_wasm/debug.wasm").unwrap();
        file.write_all(&module_bytes).unwrap();
    }

    // now, do wasmer stuff

    use wasmer::{Instance, Module, Store};
    
    let store = Store::default();
    // let module = Module::from_file(&store, &test_wasm_path).unwrap();
    let module = Module::from_binary(&store, &module_bytes).unwrap();

    // First, we create the `WasiEnv`
    use wasmer_wasi::WasiState;
    let mut wasi_env = WasiState::new("hello")
        .finalize()
        .unwrap();

    // Then, we get the import object related to our WASI
    // and attach it to the Wasm instance.
    let import_object = wasi_env
        .import_object(&module)
        .unwrap_or_else(|_| wasmer::imports!());

    Instance::new(&module, &import_object).unwrap()
}

#[allow(dead_code)]
pub fn assert_wasm_evals_to_help<T>(src: &str, ignore_problems: bool) -> Result<T, String>
where T: Copy
{
    let arena = bumpalo::Bump::new();

    // NOTE the stdlib must be in the arena; just taking a reference will segfault
    let stdlib = arena.alloc(roc_builtins::std::standard_stdlib());

    let is_gen_test = true;
    let instance =
        crate::helpers::eval_simple::helper_wasm(&arena, src, stdlib, is_gen_test, ignore_problems);

    let main_function = instance.exports.get_function("#UserApp_main_1").unwrap();

    match main_function.call(&[]) {
        Err(e) => Err(format!("{:?}", e)),
        Ok(result) => {
            let integer = match result[0] {
                wasmer::Value::I64(a) => a,
                _ => panic!(),
            };

            let output_ptr: &T;
            unsafe {
                output_ptr = std::mem::transmute::<&i64, &T>(&integer);
            }

            Ok(*output_ptr)
        }
    }
}

#[macro_export]
macro_rules! assert_wasm_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $ignore_problems:expr) => {
        match $crate::helpers::eval_simple::assert_wasm_evals_to_help::<$ty>($src, $ignore_problems) {
            Err(msg) => println!("{:?}", msg),
            Ok(actual) => {
                #[allow(clippy::bool_assert_comparison)]
                assert_eq!($transform(actual), $expected)
            }
        }
    };

    ($src:expr, $expected:expr, $ty:ty) => {
        $crate::assert_wasm_evals_to!($src, $expected, $ty, $crate::helpers::eval_simple::identity, false);
    };

    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        $crate::assert_wasm_evals_to!($src, $expected, $ty, $transform, false);
    };
}

#[macro_export]
macro_rules! assert_evals_to {
    ($src:expr, $expected:expr, $ty:ty) => {{
        assert_evals_to!($src, $expected, $ty, $crate::helpers::eval_simple::identity);
    }};
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        // Same as above, except with an additional transformation argument.
        {
            $crate::assert_wasm_evals_to!($src, $expected, $ty, $transform, false);
        }
    };
}

#[allow(dead_code)]
pub fn identity<T>(value: T) -> T {
    value
}
