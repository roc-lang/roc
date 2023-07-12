extern crate bumpalo;

pub mod platform_functions;

#[cfg(feature = "gen-dev")]
pub mod dev;
pub mod from_wasm32_memory;
#[cfg(feature = "gen-llvm")]
pub mod llvm;
#[cfg(any(feature = "gen-wasm", feature = "gen-llvm-wasm"))]
pub mod wasm;

#[allow(dead_code)]
pub(crate) fn src_hash(src: &str) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hash_state = DefaultHasher::new();
    src.hash(&mut hash_state);
    hash_state.finish()
}

#[allow(dead_code)]
pub(crate) fn save_wasm_file(app_module_bytes: &[u8], build_dir_hash: u64) {
    use std::path::Path;

    let debug_dir_str = format!("/tmp/roc/gen_wasm/{build_dir_hash:016x}");
    let debug_dir_path = Path::new(&debug_dir_str);
    let final_wasm_file = debug_dir_path.join("final.wasm");

    std::fs::create_dir_all(debug_dir_path).unwrap();
    std::fs::write(&final_wasm_file, app_module_bytes).unwrap();

    println!(
        "Debug command:\n\twasm-objdump -dx {}",
        final_wasm_file.to_str().unwrap()
    );
}

/// Used in the with_larger_debug_stack() function, for tests that otherwise
/// run out of stack space in debug builds (but don't in --release builds)
#[allow(dead_code)]
const EXPANDED_STACK_SIZE: usize = 8 * 1024 * 1024;

/// Without this, some tests pass in `cargo test --release` but fail without
/// the --release flag because they run out of stack space. This increases
/// stack size for debug builds only, while leaving the stack space at the default
/// amount for release builds.
#[allow(dead_code)]
#[cfg(debug_assertions)]
pub fn with_larger_debug_stack<F>(run_test: F)
where
    F: FnOnce(),
    F: Send,
    F: 'static,
{
    std::thread::Builder::new()
        .stack_size(EXPANDED_STACK_SIZE)
        .spawn(run_test)
        .expect("Error while spawning expanded dev stack size thread")
        .join()
        .expect("Error while joining expanded dev stack size thread")
}

/// In --release builds, don't increase the stack size. Run the test normally.
/// This way, we find out if any of our tests are blowing the stack even after
/// optimizations in release builds.
#[allow(dead_code)]
#[cfg(not(debug_assertions))]
#[inline(always)]
pub fn with_larger_debug_stack<F>(run_test: F)
where
    F: FnOnce(),
    F: Send,
    F: 'static,
{
    run_test()
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RefCount {
    Live(u32),
    Deallocated,
    Constant,
}
