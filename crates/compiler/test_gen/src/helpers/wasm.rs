use super::{RefCount, RefCountLoc};
use crate::helpers::from_wasm32_memory::FromWasm32Memory;
use bumpalo::Bump;
use roc_collections::all::MutSet;
use roc_gen_wasm::wasm32_result::Wasm32Result;
use roc_gen_wasm::DEBUG_SETTINGS;
use roc_load::{ExecutionMode, LoadConfig, Threading};
use roc_packaging::cache::RocCacheDir;
use roc_reporting::report::DEFAULT_PALETTE_HTML;
use roc_solve::FunctionKind;
use roc_std::RocStr;
use roc_wasm_interp::{wasi, ImportDispatcher, Instance, WasiDispatcher};
use roc_wasm_module::{Export, ExportType, Value, WasmModule};
use std::marker::PhantomData;
use std::path::PathBuf;

const TEST_WRAPPER_NAME: &str = "test_wrapper";
const INIT_REFCOUNT_NAME: &str = "init_refcount_test";

macro_rules! host_bytes_path {
    () => {
        // Should manually match build.rs. include_bytes! requires a string literal.
        concat!(env!("OUT_DIR"), "/wasm_test_platform.wasm")
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

fn write_final_wasm() -> bool {
    use roc_debug_flags::dbg_do;

    dbg_do!(roc_debug_flags::ROC_WRITE_FINAL_WASM, {
        return true;
    });

    DEBUG_SETTINGS.keep_test_binary
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

    if write_final_wasm() {
        let build_dir_hash = crate::helpers::src_hash(src);
        crate::helpers::save_wasm_file(&compiled_bytes, build_dir_hash)
    };

    compiled_bytes
}

fn compile_roc_to_wasm_bytes<'a, T: Wasm32Result>(
    arena: &'a bumpalo::Bump,
    host_bytes: &[u8],
    src: &str,
    _test_wrapper_type_info: PhantomData<T>,
) -> Vec<u8> {
    let filename = PathBuf::from("Test.roc");
    let src_dir = PathBuf::from("fake/test/path");

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

    let load_config = LoadConfig {
        target: roc_target::Target::Wasm32,
        render: roc_reporting::report::RenderTarget::ColorTerminal,
        palette: DEFAULT_PALETTE_HTML,
        threading: Threading::Single,
        exec_mode: ExecutionMode::Executable,
        function_kind: FunctionKind::LambdaSet,
    };
    let loaded = roc_load::load_and_monomorphize_from_str(
        arena,
        filename,
        module_src,
        src_dir,
        None,
        RocCacheDir::Disallowed,
        load_config,
    );

    let loaded = loaded.expect("failed to load module");

    use roc_load::MonomorphizedModule;
    let MonomorphizedModule {
        module_id,
        procedures,
        mut interns,
        exposed_to_host,
        mut layout_interner,
        ..
    } = loaded;

    debug_assert_eq!(exposed_to_host.top_level_values.len(), 1);

    let exposed_to_host = exposed_to_host
        .top_level_values
        .keys()
        .copied()
        .collect::<MutSet<_>>();

    let env = roc_gen_wasm::Env {
        arena,
        module_id,
        exposed_to_host,
        stack_bytes: roc_gen_wasm::Env::DEFAULT_STACK_BYTES,
    };

    let host_module = roc_gen_wasm::parse_host(env.arena, host_bytes).unwrap_or_else(|e| {
        panic!(
            "I ran into a problem with the host object file, {} at offset 0x{:x}:\n{}",
            host_bytes_path!(),
            e.offset,
            e.message
        )
    });

    let (mut module, mut called_fns, main_fn_index) = roc_gen_wasm::build_app_module(
        &env,
        &mut layout_interner,
        &mut interns,
        host_module,
        procedures,
    );

    T::insert_wrapper(arena, &mut module, TEST_WRAPPER_NAME, main_fn_index);
    called_fns.push(true);

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

    module.eliminate_dead_code(env.arena, called_fns);

    let mut app_module_bytes = std::vec::Vec::with_capacity(module.size());
    module.serialize(&mut app_module_bytes);

    app_module_bytes
}

#[allow(dead_code)]
pub fn assert_evals_to_help<T>(src: &str, phantom: PhantomData<T>) -> Result<T, String>
where
    T: FromWasm32Memory + Wasm32Result,
{
    let arena = bumpalo::Bump::new();

    let wasm_bytes = crate::helpers::wasm::compile_to_wasm_bytes(&arena, src, phantom);

    run_wasm_test_bytes::<T>(TEST_WRAPPER_NAME, wasm_bytes)
}

struct TestDispatcher<'a> {
    wasi: WasiDispatcher<'a>,
}

impl<'a> ImportDispatcher for TestDispatcher<'a> {
    fn dispatch(
        &mut self,
        module_name: &str,
        function_name: &str,
        arguments: &[Value],
        memory: &mut [u8],
    ) -> Option<Value> {
        if module_name == wasi::MODULE_NAME {
            self.wasi.dispatch(function_name, arguments, memory)
        } else if module_name == "env" && function_name == "send_panic_msg_to_rust" {
            let msg_ptr = arguments[0].expect_i32().unwrap();
            let panic_tag = arguments[1].expect_i32().unwrap();
            let roc_msg = RocStr::decode(memory, msg_ptr as _);
            let msg = match panic_tag {
                0 => format!(r#"Roc failed with message: "{roc_msg}""#),
                1 => format!(r#"User crash with message: "{roc_msg}""#),
                _ => format!(r#"Got an invald panic tag: "{panic_tag}""#),
            };
            panic!("{}", msg)
        } else {
            panic!(
                "TestDispatcher does not implement {}.{}",
                module_name, function_name
            );
        }
    }
}

pub(crate) fn run_wasm_test_bytes<T>(
    test_wrapper_name: &str,
    wasm_bytes: Vec<u8>,
) -> Result<T, String>
where
    T: FromWasm32Memory + Wasm32Result,
{
    let arena = Bump::new();
    let require_relocatable = false;
    let module = WasmModule::preload(&arena, &wasm_bytes, require_relocatable)
        .map_err(|e| format!("{:?}", e))?;
    run_wasm_test_module(&arena, test_wrapper_name, &module)
}

pub(crate) fn run_wasm_test_module<'a, T>(
    arena: &'a Bump,
    test_wrapper_name: &str,
    module: &WasmModule<'a>,
) -> Result<T, String>
where
    T: FromWasm32Memory + Wasm32Result,
{
    let dispatcher = TestDispatcher {
        wasi: wasi::WasiDispatcher::default(),
    };
    let is_debug_mode = roc_debug_flags::dbg_set!(roc_debug_flags::ROC_LOG_WASM_INTERP);
    let mut inst = Instance::for_module(&arena, &module, dispatcher, is_debug_mode)?;
    let opt_value = inst.call_export(test_wrapper_name, [])?;
    let addr_value = opt_value.ok_or("No return address from Wasm test")?;
    let addr = addr_value.expect_i32().map_err(|e| format!("{:?}", e))?;
    let output = <T as FromWasm32Memory>::decode(&inst.memory, addr as u32);
    Ok(output)
}

#[allow(dead_code)]
pub fn assert_wasm_refcounts_help<T>(
    src: &str,
    phantom: PhantomData<T>,
    refcount_locs: &[RefCountLoc],
) -> Result<Vec<RefCount>, String>
where
    T: FromWasm32Memory + Wasm32Result,
{
    let arena = bumpalo::Bump::new();

    let wasm_bytes = crate::helpers::wasm::compile_to_wasm_bytes(&arena, src, phantom);

    let require_relocatable = false;
    let module = WasmModule::preload(&arena, &wasm_bytes, require_relocatable)
        .map_err(|e| format!("{:?}", e))?;

    let dispatcher = TestDispatcher {
        wasi: wasi::WasiDispatcher::default(),
    };
    let is_debug_mode = roc_debug_flags::dbg_set!(roc_debug_flags::ROC_LOG_WASM_INTERP);
    let mut inst = Instance::for_module(&arena, &module, dispatcher, is_debug_mode)?;

    // Allocate a vector in the test host that refcounts will be copied into
    let num_refcounts = refcount_locs.len();
    let mut refcount_vector_addr: i32 = inst
        .call_export(INIT_REFCOUNT_NAME, [Value::I32(num_refcounts as i32)])?
        .ok_or_else(|| format!("No return address from {}", INIT_REFCOUNT_NAME))?
        .expect_i32()
        .map_err(|type_err| format!("{:?}", type_err))?;

    // Run the test, ignoring the result
    let _result_addr: i32 = inst
        .call_export(TEST_WRAPPER_NAME, [])?
        .ok_or_else(|| format!("No return address from {}", TEST_WRAPPER_NAME))?
        .expect_i32()
        .map_err(|type_err| format!("{:?}", type_err))?;

    // Read the length of the vector in the C host
    let actual_num_refcounts = read_i32(&inst.memory, refcount_vector_addr) as usize;
    if actual_num_refcounts != num_refcounts {
        return Err(format!(
            "Expected {} refcounts but got {}",
            num_refcounts, actual_num_refcounts
        ));
    }

    // Read the refcounts
    let mut refcounts = Vec::with_capacity(num_refcounts);
    for refcount_loc in refcount_locs {
        // Get the next RC pointer from the host's vector
        refcount_vector_addr += 4;
        let mut rc_ptr = read_i32(&inst.memory, refcount_vector_addr);
        let rc = if rc_ptr == 0 {
            RefCount::Deallocated
        } else {
            // If size is store on the heap for this type, the rc pointer is directly after.
            if matches!(refcount_loc, RefCountLoc::AfterSize) {
                rc_ptr += 4
            }

            let rc = read_i32(&inst.memory, rc_ptr);

            if rc == 0 {
                RefCount::Constant
            } else {
                RefCount::Live(rc as u32)
            }
        };
        refcounts.push(rc);
    }
    Ok(refcounts)
}

fn read_i32(memory: &[u8], addr: i32) -> i32 {
    let index = addr as usize;
    let mut bytes = [0; 4];
    bytes.copy_from_slice(&memory[index..][..4]);
    i32::from_le_bytes(bytes)
}

/// Print out hex bytes of the test result, and a few words on either side
/// Can be handy for debugging misalignment issues etc.
#[allow(dead_code)]
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

#[allow(dead_code)]
pub fn identity<T>(value: T) -> T {
    value
}

#[allow(unused_macros)]
macro_rules! assert_refcounts {
    // We need the result type to generate the test_wrapper, even though we ignore the value!
    // We can't just call `main` with no args, because some tests return structs, via pointer arg!
    // Also we need to know how much stack space to reserve for the struct.
    ($src: expr, $ty: ty, $expected: expr) => {{
        let phantom = std::marker::PhantomData;
        let (refcount_locs, expected_refcounts): (
            Vec<$crate::helpers::RefCountLoc>,
            Vec<$crate::helpers::RefCount>,
        ) = $expected.into_iter().map(|x| *x).unzip();
        let result =
            $crate::helpers::wasm::assert_wasm_refcounts_help::<$ty>($src, phantom, &refcount_locs);
        match result {
            Err(msg) => panic!("{:?}", msg),
            Ok(actual_refcounts) => {
                assert_eq!(actual_refcounts, expected_refcounts)
            }
        }
    }};
}

#[allow(unused_imports)]
pub(crate) use assert_evals_to;

#[allow(unused_imports)]
pub(crate) use assert_refcounts;
