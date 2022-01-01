use core::cell::Cell;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use tempfile::{tempdir, TempDir};
use wasmer::{Memory, WasmPtr};

use crate::helpers::from_wasm32_memory::FromWasm32Memory;
use crate::helpers::wasm32_test_result::Wasm32TestResult;
use roc_builtins::bitcode;
use roc_can::builtins::builtin_defs_map;
use roc_collections::all::{MutMap, MutSet};
use roc_gen_wasm::{DEBUG_LOG_SETTINGS, MEMORY_NAME};

// Should manually match build.rs
const PLATFORM_FILENAME: &str = "wasm_test_platform";
const OUT_DIR_VAR: &str = "TEST_GEN_OUT";
const LIBC_PATH_VAR: &str = "TEST_GEN_WASM_LIBC_PATH";
const COMPILER_RT_PATH_VAR: &str = "TEST_GEN_WASM_COMPILER_RT_PATH";

#[allow(unused_imports)]
use roc_mono::ir::PRETTY_PRINT_IR_SYMBOLS;

const TEST_WRAPPER_NAME: &str = "test_wrapper";

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

pub enum TestType {
    /// Test that some Roc code evaluates to the right result
    Evaluate,
    /// Test that some Roc values have the right refcount
    Refcount,
}

#[allow(dead_code)]
pub fn compile_and_load<'a, T: Wasm32TestResult>(
    arena: &'a bumpalo::Bump,
    src: &str,
    stdlib: &'a roc_builtins::std::StdLib,
    _test_wrapper_type_info: PhantomData<T>,
    test_type: TestType,
) -> wasmer::Instance {
    let (app_module_bytes, needs_linking) =
        compile_roc_to_wasm_bytes(arena, src, stdlib, _test_wrapper_type_info);

    let keep_test_binary = DEBUG_LOG_SETTINGS.keep_test_binary;
    let build_dir_hash = if keep_test_binary {
        // Keep the output files for debugging, in a directory with a hash in the name
        Some(src_hash(src))
    } else {
        // Use a temporary build directory for linking, then delete it
        None
    };

    let final_bytes = if needs_linking || keep_test_binary {
        run_linker(app_module_bytes, build_dir_hash, test_type)
    } else {
        app_module_bytes
    };

    load_bytes_into_runtime(final_bytes)
}

fn src_hash(src: &str) -> u64 {
    let mut hash_state = DefaultHasher::new();
    src.hash(&mut hash_state);
    hash_state.finish()
}

fn compile_roc_to_wasm_bytes<'a, T: Wasm32TestResult>(
    arena: &'a bumpalo::Bump,
    src: &str,
    stdlib: &'a roc_builtins::std::StdLib,
    _test_wrapper_type_info: PhantomData<T>,
) -> (Vec<u8>, bool) {
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
    let ptr_bytes = 4;
    let loaded = roc_load::file::load_and_monomorphize_from_str(
        arena,
        filename,
        module_src,
        stdlib,
        src_dir,
        exposed_types,
        ptr_bytes,
        builtin_defs_map,
    );

    let loaded = loaded.expect("failed to load module");

    use roc_load::file::MonomorphizedModule;
    let MonomorphizedModule {
        module_id,
        procedures,
        mut interns,
        exposed_to_host,
        ..
    } = loaded;

    debug_assert_eq!(exposed_to_host.len(), 1);

    let exposed_to_host = exposed_to_host.keys().copied().collect::<MutSet<_>>();

    let env = roc_gen_wasm::Env {
        arena,
        module_id,
        exposed_to_host,
    };

    let (mut wasm_module, main_fn_index) =
        roc_gen_wasm::build_module_help(&env, &mut interns, procedures).unwrap();

    T::insert_test_wrapper(arena, &mut wasm_module, TEST_WRAPPER_NAME, main_fn_index);

    let needs_linking = !wasm_module.import.entries.is_empty();

    let mut app_module_bytes = std::vec::Vec::with_capacity(4096);
    wasm_module.serialize_mut(&mut app_module_bytes);

    (app_module_bytes, needs_linking)
}

fn run_linker(
    app_module_bytes: Vec<u8>,
    build_dir_hash: Option<u64>,
    test_type: TestType,
) -> Vec<u8> {
    let tmp_dir: TempDir; // directory for normal test runs, deleted when dropped
    let debug_dir: String; // persistent directory for debugging

    let wasm_build_dir: &Path = if let Some(src_hash) = build_dir_hash {
        debug_dir = format!("/tmp/roc/gen_wasm/{:016x}", src_hash);
        std::fs::create_dir_all(&debug_dir).unwrap();
        println!(
            "Debug commands:\n\twasm-objdump -dx {}/app.o\n\twasm-objdump -dx {}/final.wasm",
            &debug_dir, &debug_dir,
        );
        Path::new(&debug_dir)
    } else {
        tmp_dir = tempdir().unwrap();
        tmp_dir.path()
    };

    let final_wasm_file = wasm_build_dir.join("final.wasm");
    let app_o_file = wasm_build_dir.join("app.o");
    let test_out_dir = std::env::var(OUT_DIR_VAR).unwrap();
    let test_platform_o = format!("{}/{}.o", test_out_dir, PLATFORM_FILENAME);
    let libc_a_file = std::env::var(LIBC_PATH_VAR).unwrap();
    let compiler_rt_o_file = std::env::var(COMPILER_RT_PATH_VAR).unwrap();

    // write the module to a file so the linker can access it
    std::fs::write(&app_o_file, &app_module_bytes).unwrap();

    let mut args = vec![
        "wasm-ld",
        // input files
        app_o_file.to_str().unwrap(),
        bitcode::BUILTINS_WASM32_OBJ_PATH,
        &test_platform_o,
        &libc_a_file,
        &compiler_rt_o_file,
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
        "--export",
        "test_wrapper",
        "--export",
        "#UserApp_main_1",
    ];

    if matches!(test_type, TestType::Refcount) {
        // If we always export this, tests run ~2.5x slower! Not sure why.
        args.extend_from_slice(&["--export", "init_refcount_test"]);
    }

    let linker_output = std::process::Command::new(&crate::helpers::zig_executable())
        .args(&args)
        .output()
        .unwrap();

    if !linker_output.status.success() {
        print!("\nLINKER FAILED\n");
        for arg in args {
            print!("{} ", arg);
        }
        println!("\n{}", std::str::from_utf8(&linker_output.stdout).unwrap());
        println!("{}", std::str::from_utf8(&linker_output.stderr).unwrap());
    }

    std::fs::read(final_wasm_file).unwrap()
}

fn load_bytes_into_runtime(bytes: Vec<u8>) -> wasmer::Instance {
    use wasmer::{Module, Store};
    use wasmer_wasi::WasiState;

    let store = Store::default();
    let wasmer_module = Module::new(&store, &bytes).unwrap();

    // First, we create the `WasiEnv`
    let mut wasi_env = WasiState::new("hello").finalize().unwrap();

    // Then, we get the import object related to our WASI
    // and attach it to the Wasm instance.
    let import_object = wasi_env
        .import_object(&wasmer_module)
        .unwrap_or_else(|_| wasmer::imports!());

    wasmer::Instance::new(&wasmer_module, &import_object).unwrap()
}

#[allow(dead_code)]
pub fn assert_wasm_evals_to_help<T>(src: &str, phantom: PhantomData<T>) -> Result<T, String>
where
    T: FromWasm32Memory + Wasm32TestResult,
{
    let arena = bumpalo::Bump::new();

    // NOTE the stdlib must be in the arena; just taking a reference will segfault
    let stdlib = arena.alloc(roc_builtins::std::standard_stdlib());

    let instance =
        crate::helpers::wasm::compile_and_load(&arena, src, stdlib, phantom, TestType::Evaluate);

    let memory = instance.exports.get_memory(MEMORY_NAME).unwrap();

    let test_wrapper = instance.exports.get_function(TEST_WRAPPER_NAME).unwrap();

    match test_wrapper.call(&[]) {
        Err(e) => Err(format!("{:?}", e)),
        Ok(result) => {
            let address = match result[0] {
                wasmer::Value::I32(a) => a,
                _ => panic!(),
            };

            if false {
                println!("test_wrapper returned 0x{:x}", address);
                println!("Stack:");
                crate::helpers::wasm::debug_memory_hex(memory, address, std::mem::size_of::<T>());
            }
            if false {
                println!("Heap:");
                // Manually provide address and size based on printf in wasm_test_platform.c
                crate::helpers::wasm::debug_memory_hex(memory, 0x11440, 24);
            }
            let output = <T as FromWasm32Memory>::decode(memory, address as u32);

            Ok(output)
        }
    }
}

#[allow(dead_code)]
pub fn assert_wasm_refcounts_help<T>(
    src: &str,
    phantom: PhantomData<T>,
    num_refcounts: usize,
) -> Result<Vec<u32>, String>
where
    T: FromWasm32Memory + Wasm32TestResult,
{
    let arena = bumpalo::Bump::new();

    // NOTE the stdlib must be in the arena; just taking a reference will segfault
    let stdlib = arena.alloc(roc_builtins::std::standard_stdlib());

    let instance =
        crate::helpers::wasm::compile_and_load(&arena, src, stdlib, phantom, TestType::Refcount);

    let memory = instance.exports.get_memory(MEMORY_NAME).unwrap();

    let init_refcount_test = instance.exports.get_function("init_refcount_test").unwrap();
    let init_result = init_refcount_test.call(&[wasmer::Value::I32(num_refcounts as i32)]);
    let refcount_array_addr = match init_result {
        Err(e) => return Err(format!("{:?}", e)),
        Ok(result) => match result[0] {
            wasmer::Value::I32(a) => a,
            _ => panic!(),
        },
    };
    // An array of refcount pointers
    let refcount_ptr_array: WasmPtr<WasmPtr<i32>, wasmer::Array> =
        WasmPtr::new(refcount_array_addr as u32);
    let refcount_ptrs: &[Cell<WasmPtr<i32>>] = refcount_ptr_array
        .deref(memory, 0, num_refcounts as u32)
        .unwrap();

    let test_wrapper = instance.exports.get_function(TEST_WRAPPER_NAME).unwrap();
    match test_wrapper.call(&[]) {
        Err(e) => return Err(format!("{:?}", e)),
        Ok(_) => {}
    }

    let mut refcounts = Vec::with_capacity(num_refcounts);
    for i in 0..num_refcounts {
        let rc_ptr = refcount_ptrs[i].get();
        let rc = if rc_ptr.offset() == 0 {
            // RC pointer has been set to null, which means the value has been freed.
            // In tests, we simply represent this as zero refcount.
            0
        } else {
            let rc_encoded = rc_ptr.deref(memory).unwrap().get();
            (rc_encoded - i32::MIN + 1) as u32
        };
        refcounts.push(rc);
    }
    Ok(refcounts)
}

/// Print out hex bytes of the test result, and a few words on either side
/// Can be handy for debugging misalignment issues etc.
pub fn debug_memory_hex(memory: &Memory, address: i32, size: usize) {
    let memory_words: &[u32] = unsafe {
        let memory_bytes = memory.data_unchecked();
        std::mem::transmute(memory_bytes)
    };

    let extra_words = 2;
    let result_start = (address as usize) / 4;
    let result_end = result_start + ((size + 3) / 4);
    let start = result_start - extra_words;
    let end = result_end + extra_words;

    for index in start..end {
        let result_marker = if index >= result_start && index < result_end {
            "|"
        } else {
            " "
        };
        println!(
            "{:x} {} {:08x}",
            index * 4,
            result_marker,
            memory_words[index]
        );
    }
    println!();
}

#[allow(unused_macros)]
macro_rules! assert_wasm_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        let phantom = std::marker::PhantomData;
        match $crate::helpers::wasm::assert_wasm_evals_to_help::<$ty>($src, phantom) {
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

#[allow(unused_macros)]
macro_rules! assert_refcounts {
    // We need the result type to generate the test_wrapper, even though we ignore the value!
    // We can't just call `main` with no args, because some tests return structs, via pointer arg!
    // Also we need to know how much stack space to reserve for the struct.
    ($src: expr, $ty: ty, $expected_refcounts: expr) => {{
        let phantom = std::marker::PhantomData;
        let num_refcounts = $expected_refcounts.len();
        let result =
            $crate::helpers::wasm::assert_wasm_refcounts_help::<$ty>($src, phantom, num_refcounts);
        match result {
            Err(msg) => panic!("{:?}", msg),
            Ok(actual_refcounts) => {
                assert_eq!(&actual_refcounts, $expected_refcounts)
            }
        }
    }};
}

#[allow(unused_imports)]
pub(crate) use assert_evals_to;
#[allow(unused_imports)]
pub(crate) use assert_wasm_evals_to;

#[allow(unused_imports)]
pub(crate) use assert_refcounts;
