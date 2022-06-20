use super::RefCount;
use crate::helpers::from_wasmer_memory::FromWasmerMemory;
use roc_collections::all::MutSet;
use roc_gen_wasm::wasm32_result::Wasm32Result;
use roc_gen_wasm::wasm_module::{Export, ExportType};
use roc_gen_wasm::{DEBUG_SETTINGS, MEMORY_NAME};
use roc_load::Threading;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use wasmer::{Memory, WasmPtr};

// Should manually match build.rs
const PLATFORM_FILENAME: &str = "wasm_test_platform";
const OUT_DIR_VAR: &str = "TEST_GEN_OUT";

const TEST_WRAPPER_NAME: &str = "test_wrapper";
const INIT_REFCOUNT_NAME: &str = "init_refcount_test";
const PANIC_MSG_NAME: &str = "panic_msg";

fn promote_expr_to_module(src: &str) -> String {
    let mut buffer = String::from("app \"test\" provides [main] to \"./platform\"\n\nmain =\n");

    for line in src.lines() {
        // indent the body!
        buffer.push_str("    ");
        buffer.push_str(line);
        buffer.push('\n');
    }

    buffer
}

#[allow(dead_code)]
pub fn compile_and_load<'a, T: Wasm32Result>(
    arena: &'a bumpalo::Bump,
    src: &str,
    test_wrapper_type_info: PhantomData<T>,
) -> wasmer::Instance {
    let platform_path = get_preprocessed_host_path();
    let platform_bytes = std::fs::read(&platform_path).unwrap();
    println!("Loading test host {}", platform_path.display());

    let compiled_bytes =
        compile_roc_to_wasm_bytes(arena, &platform_bytes, src, test_wrapper_type_info);

    if DEBUG_SETTINGS.keep_test_binary {
        let build_dir_hash = src_hash(src);
        save_wasm_file(&compiled_bytes, build_dir_hash)
    };

    load_bytes_into_runtime(&compiled_bytes)
}

fn get_preprocessed_host_path() -> PathBuf {
    let out_dir = std::env::var(OUT_DIR_VAR).unwrap();
    Path::new(&out_dir)
        .join([PLATFORM_FILENAME, "o"].join("."))
        .to_path_buf()
}

fn src_hash(src: &str) -> u64 {
    let mut hash_state = DefaultHasher::new();
    src.hash(&mut hash_state);
    hash_state.finish()
}

fn compile_roc_to_wasm_bytes<'a, T: Wasm32Result>(
    arena: &'a bumpalo::Bump,
    host_bytes: &[u8],
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

    let loaded = roc_load::load_and_monomorphize_from_str(
        arena,
        filename,
        module_src,
        src_dir,
        Default::default(),
        roc_target::TargetInfo::default_wasm32(),
        roc_reporting::report::RenderTarget::ColorTerminal,
        Threading::Single,
    );

    let loaded = loaded.expect("failed to load module");

    use roc_load::MonomorphizedModule;
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

    let host_module = roc_gen_wasm::parse_host(env.arena, host_bytes).unwrap_or_else(|e| {
        panic!(
            "I ran into a problem with the host object file, {} at offset 0x{:x}:\n{}",
            get_preprocessed_host_path().display(),
            e.offset,
            e.message
        )
    });

    let (mut module, called_preload_fns, main_fn_index) =
        roc_gen_wasm::build_app_module(&env, &mut interns, host_module, procedures);

    T::insert_wrapper(arena, &mut module, TEST_WRAPPER_NAME, main_fn_index);

    // Export the initialiser function for refcount tests
    let init_refcount_idx = module
        .names
        .function_names
        .iter()
        .filter(|(_, name)| *name == INIT_REFCOUNT_NAME)
        .map(|(i, _)| *i)
        .next()
        .unwrap();
    module.export.append(Export {
        name: INIT_REFCOUNT_NAME,
        ty: ExportType::Func,
        index: init_refcount_idx,
    });

    module.eliminate_dead_code(env.arena, called_preload_fns);

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

fn load_bytes_into_runtime(bytes: &[u8]) -> wasmer::Instance {
    use wasmer::{Module, Store};
    use wasmer_wasi::WasiState;

    let store = Store::default();
    let wasmer_module = Module::new(&store, bytes).unwrap();

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
pub fn assert_evals_to_help<T>(src: &str, phantom: PhantomData<T>) -> Result<T, String>
where
    T: FromWasmerMemory + Wasm32Result,
{
    let arena = bumpalo::Bump::new();

    let instance = crate::helpers::wasm::compile_and_load(&arena, src, phantom);

    let memory = instance.exports.get_memory(MEMORY_NAME).unwrap();

    let test_wrapper = instance.exports.get_function(TEST_WRAPPER_NAME).unwrap();

    match test_wrapper.call(&[]) {
        Err(e) => {
            if let Some(msg) = get_roc_panic_msg(&instance, memory) {
                Err(msg)
            } else {
                Err(e.to_string())
            }
        }
        Ok(result) => {
            let address = result[0].unwrap_i32();

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
            let output = <T as FromWasmerMemory>::decode(memory, address as u32);

            Ok(output)
        }
    }
}

/// Our test roc_panic stores a pointer to its message in a global variable so we can find it.
fn get_roc_panic_msg(instance: &wasmer::Instance, memory: &Memory) -> Option<String> {
    let memory_bytes = unsafe { memory.data_unchecked() };

    // We need to dereference twice!
    // The Wasm Global only points at the memory location of the C global value
    let panic_msg_global = instance.exports.get_global(PANIC_MSG_NAME).unwrap();
    let global_addr = panic_msg_global.get().unwrap_i32() as usize;
    let global_ptr = memory_bytes[global_addr..].as_ptr() as *const u32;

    // Dereference again to find the bytes of the message string
    let msg_addr = unsafe { *global_ptr };
    if msg_addr == 0 {
        return None;
    }
    let msg_index = msg_addr as usize;
    let msg_len = memory_bytes[msg_index..]
        .iter()
        .position(|c| *c == 0)
        .unwrap();
    let msg_bytes = memory_bytes[msg_index..][..msg_len].to_vec();
    let msg = unsafe { String::from_utf8_unchecked(msg_bytes) };
    Some(msg)
}

#[allow(dead_code)]
pub fn assert_wasm_refcounts_help<T>(
    src: &str,
    phantom: PhantomData<T>,
    num_refcounts: usize,
) -> Result<Vec<RefCount>, String>
where
    T: FromWasmerMemory + Wasm32Result,
{
    let arena = bumpalo::Bump::new();

    let instance = crate::helpers::wasm::compile_and_load(&arena, src, phantom);

    let memory = instance.exports.get_memory(MEMORY_NAME).unwrap();

    let expected_len = num_refcounts as i32;
    let init_refcount_test = instance.exports.get_function(INIT_REFCOUNT_NAME).unwrap();
    let init_result = init_refcount_test.call(&[wasmer::Value::I32(expected_len)]);
    let refcount_vector_addr = match init_result {
        Err(e) => return Err(format!("{:?}", e)),
        Ok(result) => result[0].unwrap_i32(),
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
    let refcount_ptrs = refcount_ptr_array
        .deref(memory, 0, num_refcounts as u32)
        .unwrap();

    let mut refcounts = Vec::with_capacity(num_refcounts);
    for i in 0..num_refcounts {
        let rc_ptr = refcount_ptrs[i].get();
        let rc = if rc_ptr.offset() == 0 {
            RefCount::Deallocated
        } else {
            let rc_encoded: i32 = rc_ptr.deref(memory).unwrap().get();
            if rc_encoded == 0 {
                RefCount::Constant
            } else {
                let rc = rc_encoded - i32::MIN + 1;
                RefCount::Live(rc as u32)
            }
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
macro_rules! assert_evals_to {
    ($src:expr, $expected:expr, $ty:ty) => {
        $crate::helpers::wasm::assert_evals_to!(
            $src,
            $expected,
            $ty,
            $crate::helpers::wasm::identity,
            false
        )
    };

    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        $crate::helpers::wasm::assert_evals_to!($src, $expected, $ty, $transform, false);
    };

    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $ignore_problems: expr) => {{
        let phantom = std::marker::PhantomData;
        let _ = $ignore_problems; // Always ignore "problems"! One backend (LLVM) is enough to cover them.
        match $crate::helpers::wasm::assert_evals_to_help::<$ty>($src, phantom) {
            Err(msg) => panic!("{}", msg),
            Ok(actual) => {
                assert_eq!($transform(actual), $expected)
            }
        }
    }};
}

#[allow(unused_macros)]
macro_rules! expect_runtime_error_panic {
    ($src:expr) => {{
        $crate::helpers::wasm::assert_evals_to!(
            $src,
            false, // fake value/type for eval
            bool,
            $crate::helpers::wasm::identity,
            true // ignore problems
        );
    }};
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
pub(crate) use expect_runtime_error_panic;

#[allow(unused_imports)]
pub(crate) use assert_refcounts;
