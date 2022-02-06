use core::cell::Cell;
use roc_gen_wasm::wasm_module::{Export, ExportType};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use wasmer::{Memory, WasmPtr};

use crate::helpers::from_wasm32_memory::FromWasm32Memory;
use crate::helpers::wasm32_test_result::Wasm32TestResult;
use roc_can::builtins::builtin_defs_map;
use roc_collections::all::{MutMap, MutSet};
use roc_gen_wasm::{DEBUG_LOG_SETTINGS, MEMORY_NAME};

// Should manually match build.rs
const PLATFORM_FILENAME: &str = "wasm_test_platform";
const OUT_DIR_VAR: &str = "TEST_GEN_OUT";

const TEST_WRAPPER_NAME: &str = "test_wrapper";
const INIT_REFCOUNT_NAME: &str = "init_refcount_test";

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
    _test_type: TestType,
) -> wasmer::Instance {
    let platform_bytes = load_platform_and_builtins();

    let compiled_bytes =
        compile_roc_to_wasm_bytes(arena, stdlib, &platform_bytes, src, _test_wrapper_type_info);

    if DEBUG_LOG_SETTINGS.keep_test_binary {
        let build_dir_hash = src_hash(src);
        save_wasm_file(&compiled_bytes, build_dir_hash)
    };

    load_bytes_into_runtime(compiled_bytes)
}

fn load_platform_and_builtins() -> std::vec::Vec<u8> {
    let out_dir = std::env::var(OUT_DIR_VAR).unwrap();
    let platform_path = Path::new(&out_dir).join([PLATFORM_FILENAME, "o"].join("."));
    std::fs::read(&platform_path).unwrap()
}

fn src_hash(src: &str) -> u64 {
    let mut hash_state = DefaultHasher::new();
    src.hash(&mut hash_state);
    hash_state.finish()
}

fn compile_roc_to_wasm_bytes<'a, T: Wasm32TestResult>(
    arena: &'a bumpalo::Bump,
    stdlib: &'a roc_builtins::std::StdLib,
    preload_bytes: &[u8],
    src: &str,
    _test_wrapper_type_info: PhantomData<T>,
) -> Vec<u8> {
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
        roc_target::TargetInfo::default_wasm32(),
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

    debug_assert_eq!(exposed_to_host.values.len(), 1);

    let exposed_to_host = exposed_to_host
        .values
        .keys()
        .copied()
        .collect::<MutSet<_>>();

    let env = roc_gen_wasm::Env {
        arena,
        module_id,
        exposed_to_host,
    };

    let (mut module, called_preload_fns, main_fn_index) =
        roc_gen_wasm::build_module_without_test_wrapper(
            &env,
            &mut interns,
            preload_bytes,
            procedures,
        );

    T::insert_test_wrapper(arena, &mut module, TEST_WRAPPER_NAME, main_fn_index);

    // Export the initialiser function for refcount tests
    let init_refcount_bytes = INIT_REFCOUNT_NAME.as_bytes();
    let init_refcount_idx = module.names.functions[init_refcount_bytes];
    module.export.append(Export {
        name: arena.alloc_slice_copy(init_refcount_bytes),
        ty: ExportType::Func,
        index: init_refcount_idx,
    });

    module.remove_dead_preloads(env.arena, called_preload_fns);

    let mut app_module_bytes = std::vec::Vec::with_capacity(module.size());
    module.serialize(&mut app_module_bytes);

    app_module_bytes
}

fn save_wasm_file(app_module_bytes: &[u8], build_dir_hash: u64) {
    let debug_dir_str = format!("/tmp/roc/gen_wasm/{:016x}", build_dir_hash);
    let debug_dir_path = Path::new(&debug_dir_str);
    let final_wasm_file = debug_dir_path.join("final.wasm");

    std::fs::create_dir_all(debug_dir_path).unwrap();
    std::fs::write(&final_wasm_file, app_module_bytes).unwrap();

    println!(
        "Debug command:\n\twasm-objdump -dx {}",
        final_wasm_file.to_str().unwrap()
    );
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

    let expected_len = num_refcounts as i32;
    let init_refcount_test = instance.exports.get_function(INIT_REFCOUNT_NAME).unwrap();
    let init_result = init_refcount_test.call(&[wasmer::Value::I32(expected_len)]);
    let refcount_vector_addr = match init_result {
        Err(e) => return Err(format!("{:?}", e)),
        Ok(result) => match result[0] {
            wasmer::Value::I32(a) => a,
            _ => panic!(),
        },
    };

    // Run the test
    let test_wrapper = instance.exports.get_function(TEST_WRAPPER_NAME).unwrap();
    match test_wrapper.call(&[]) {
        Err(e) => return Err(format!("{:?}", e)),
        Ok(_) => {}
    }

    // Check we got the right number of refcounts
    let refcount_vector_len: WasmPtr<i32> = WasmPtr::new(refcount_vector_addr as u32);
    let actual_len = refcount_vector_len.deref(memory).unwrap().get();
    if actual_len != expected_len {
        panic!("Expected {} refcounts but got {}", expected_len, actual_len);
    }

    // Read the actual refcount values
    let refcount_ptr_array: WasmPtr<WasmPtr<i32>, wasmer::Array> =
        WasmPtr::new(4 + refcount_vector_addr as u32);
    let refcount_ptrs: &[Cell<WasmPtr<i32>>] = refcount_ptr_array
        .deref(memory, 0, num_refcounts as u32)
        .unwrap();

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
