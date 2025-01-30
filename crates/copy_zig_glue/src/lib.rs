use std::path::Path;
use std::path::PathBuf;
use std::sync::Once;
use std::{fs, io};

static ZIG_PLATFORM_COPY_GLUE_ONCE: Once = Once::new();

/// Copies the glue source files for zig platforms from the roc builtins
/// this is only temporary, we should create a zig package for the test platforms
pub fn initialize_zig_test_platforms() {
    ZIG_PLATFORM_COPY_GLUE_ONCE.call_once(|| {
        copy_zig_glue();
    });
}

/// Copy the zig builtins source files into each of the test platform folders.
///
/// This is a simple way to use these files without creating a zig package,
/// to support the upgrade from zig 0.11.0 to 0.13.0, and keep the scope of the change smaller.
///
/// Once we have zig 0.13.0, we should add a zig package for the test platforms.
pub fn copy_zig_glue() {
    let workspace_dir = roc_test_utils_dir::workspace_root();
    let zig_builtins_source_dir = workspace_dir.join("crates/compiler/builtins/bitcode/src");

    let zig_test_platforms_dirs: Vec<PathBuf> = vec![
        workspace_dir.join("crates/cli/tests/test-projects/platform_requires_pkg/platform/glue"),
        workspace_dir.join("crates/cli/tests/test-projects/algorithms/fibonacci-platform/glue"),
        workspace_dir.join("crates/cli/tests/test-projects/algorithms/quicksort-platform/glue"),
        workspace_dir.join("crates/cli/tests/benchmarks/platform/glue"),
        workspace_dir.join("crates/valgrind_tests/zig-platform/glue"),
        workspace_dir.join("crates/cli/tests/test-projects/test-platform-effects-zig/glue"),
        workspace_dir.join("crates/cli/tests/test-projects/test-platform-simple-zig/glue"),
        workspace_dir.join("crates/cli/tests/test-projects/multiple_exposed/platform/glue"),
        workspace_dir.join("crates/cli/tests/test-projects/tui/platform/glue"),
        workspace_dir.join("crates/cli/tests/platform-switching/zig-platform/glue"),
    ];

    for target_dir in zig_test_platforms_dirs {
        copy_dir_all(&zig_builtins_source_dir, &target_dir).unwrap_or_else(|e| {
            panic!(
                "unable to copy zig builtins source files to {}: {}",
                target_dir.display(),
                e
            )
        });
        println!("Copied zig glue source files into {}", target_dir.display());
    }
    println!("DONE");
}

fn copy_dir_all(src: impl AsRef<Path>, dst: impl AsRef<Path>) -> io::Result<()> {
    fs::create_dir_all(&dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let ty = entry.file_type()?;
        if ty.is_dir() {
            copy_dir_all(entry.path(), dst.as_ref().join(entry.file_name()))?;
        } else {
            fs::copy(entry.path(), dst.as_ref().join(entry.file_name()))?;
        }
    }
    Ok(())
}
