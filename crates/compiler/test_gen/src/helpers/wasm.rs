use super::RefCount;
use crate::helpers::from_wasm32_memory::FromWasm32Memory;
use roc_collections::all::MutSet;
use roc_gen_wasm::wasm32_result::Wasm32Result;
use roc_gen_wasm::wasm_module::{Export, ExportType};
use roc_gen_wasm::DEBUG_SETTINGS;
use roc_load::Threading;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Mutex;

const TEST_WRAPPER_NAME: &str = "test_wrapper";
const INIT_REFCOUNT_NAME: &str = "init_refcount_test";

macro_rules! host_bytes_path {
    () => {
        // Should manually match build.rs. include_bytes! requires a string literal.
        "../../build/wasm_test_platform.wasm"
    };
}

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
pub fn compile_to_wasm_bytes<'a, T: Wasm32Result>(
    arena: &'a bumpalo::Bump,
    src: &str,
    test_wrapper_type_info: PhantomData<T>,
) -> Vec<u8> {
    let platform_bytes = include_bytes!(host_bytes_path!());
    println!("Loading test host {}", host_bytes_path!());

    let compiled_bytes =
        compile_roc_to_wasm_bytes(arena, platform_bytes, src, test_wrapper_type_info);

    if DEBUG_SETTINGS.keep_test_binary {
        let build_dir_hash = src_hash(src);
        save_wasm_file(&compiled_bytes, build_dir_hash)
    };

    compiled_bytes
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
            host_bytes_path!(),
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

#[allow(dead_code)]
pub fn assert_evals_to_help<T>(src: &str, phantom: PhantomData<T>) -> Result<T, String>
where
    T: FromWasm32Memory + Wasm32Result,
{
    let arena = bumpalo::Bump::new();

    let wasm_bytes = crate::helpers::wasm::compile_to_wasm_bytes(&arena, src, phantom);

    use wasm3::Environment;
    use wasm3::Module;

    let env = Environment::new().expect("Unable to create environment");
    let rt = env
        .create_runtime(1024 * 60)
        .expect("Unable to create runtime");

    let module = Module::parse(&env, &wasm_bytes[..]).expect("Unable to parse module");

    let mut module = rt.load_module(module).expect("Unable to load module");

    let panic_msg: Rc<Mutex<Option<(i32, i32)>>> = Default::default();
    let panic_msg_for_closure = panic_msg.clone();

    module.link_wasi().unwrap();
    let try_link_panic = module.link_closure(
        "env",
        "send_panic_msg_to_rust",
        move |_call_context, args: (i32, i32)| {
            let mut w = panic_msg_for_closure.lock().unwrap();
            *w = Some(args);
            Ok(())
        },
    );
    match try_link_panic {
        Ok(()) => {}
        Err(wasm3::error::Error::FunctionNotFound) => {}
        Err(e) => panic!("{:?}", e),
    }

    let test_wrapper = module
        .find_function::<(), i32>(TEST_WRAPPER_NAME)
        .expect("Unable to find test wrapper function");

    match test_wrapper.call() {
        Err(e) => {
            if let Some((msg_ptr, msg_len)) = *panic_msg.lock().unwrap() {
                let memory: &[u8] = unsafe {
                    let memory_ptr: *const [u8] = rt.memory();
                    let (_, memory_size) = std::mem::transmute::<_, (usize, usize)>(memory_ptr);
                    std::slice::from_raw_parts(memory_ptr as _, memory_size)
                };

                let msg_bytes = &memory[msg_ptr as usize..][..msg_len as usize];
                let msg = std::str::from_utf8(msg_bytes).unwrap();

                Err(format!("Roc failed with message: \"{}\"", msg))
            } else {
                Err(format!("{}", e))
            }
        }
        Ok(address) => {
            let (_, memory_size) = unsafe { std::mem::transmute::<_, (usize, usize)>(rt.memory()) };
            let memory: &[u8] =
                unsafe { std::slice::from_raw_parts(rt.memory() as _, memory_size) };

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
) -> Result<Vec<RefCount>, String>
where
    T: FromWasm32Memory + Wasm32Result,
{
    let arena = bumpalo::Bump::new();

    let wasm_bytes = crate::helpers::wasm::compile_to_wasm_bytes(&arena, src, phantom);

    use wasm3::Environment;
    use wasm3::Module;

    let env = Environment::new().expect("Unable to create environment");
    let rt = env
        .create_runtime(1024 * 60)
        .expect("Unable to create runtime");

    let module = Module::parse(&env, &wasm_bytes[..]).expect("Unable to parse module");

    let mut module = rt.load_module(module).expect("Unable to load module");

    let panic_msg: Rc<Mutex<Option<(i32, i32)>>> = Default::default();
    let panic_msg_for_closure = panic_msg.clone();

    module.link_wasi().unwrap();
    let try_link_panic = module.link_closure(
        "env",
        "send_panic_msg_to_rust",
        move |_call_context, args: (i32, i32)| {
            let mut w = panic_msg_for_closure.lock().unwrap();
            *w = Some(args);
            Ok(())
        },
    );
    match try_link_panic {
        Ok(()) => {}
        Err(wasm3::error::Error::FunctionNotFound) => {}
        Err(e) => panic!("{:?}", e),
    }

    let expected_len = num_refcounts as i32;
    let init_refcount_test = module
        .find_function::<i32, i32>(INIT_REFCOUNT_NAME)
        .expect("Unable to find refcount test init function");
    let init_result = init_refcount_test.call(expected_len);
    let mut refcount_vector_offset = match init_result {
        Err(e) => return Err(format!("{:?}", e)),
        Ok(addr) => addr as usize,
    };

    // Run the test
    let test_wrapper = module
        .find_function::<(), i32>(TEST_WRAPPER_NAME)
        .expect("Unable to find test wrapper function");
    test_wrapper.call().map_err(|e| format!("{:?}", e))?;

    let memory: &[u8] = unsafe {
        let memory_ptr: *const [u8] = rt.memory();
        let (_, memory_size) = std::mem::transmute::<_, (usize, usize)>(memory_ptr);
        std::slice::from_raw_parts(memory_ptr as _, memory_size)
    };

    let mut refcount_vector_len_bytes = [0u8; 4];
    refcount_vector_len_bytes.copy_from_slice(&memory[refcount_vector_offset..][..4]);
    let actual_len = i32::from_le_bytes(refcount_vector_len_bytes);

    if actual_len != expected_len {
        return Err(format!(
            "Expected {} refcounts but got {}",
            expected_len, actual_len
        ));
    }

    // Read the actual refcount values
    let mut refcounts = Vec::with_capacity(num_refcounts);
    for _ in 0..num_refcounts {
        refcount_vector_offset += 4;

        // The vector contains refcount pointers
        let mut rc_ptr_bytes = [0u8; 4];
        rc_ptr_bytes.copy_from_slice(&memory[refcount_vector_offset..][..4]);
        let rc_ptr = u32::from_le_bytes(rc_ptr_bytes) as usize;

        let rc = if rc_ptr == 0 {
            RefCount::Deallocated
        } else {
            let mut rc_bytes = [0u8; 4];
            rc_bytes.copy_from_slice(&memory[rc_ptr..][..4]);
            let rc_encoded = i32::from_le_bytes(rc_bytes);

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
pub fn debug_memory_hex(memory_bytes: &[u8], address: i32, size: usize) {
    let memory_words: &[u32] =
        unsafe { std::slice::from_raw_parts(memory_bytes.as_ptr().cast(), memory_bytes.len() / 4) };

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
        $crate::helpers::wasm::assert_evals_to!($src, $expected, $ty, $transform, false)
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
