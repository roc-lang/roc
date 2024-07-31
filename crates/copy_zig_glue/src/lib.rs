use std::path::Path;
use std::{fs, io};

use std::path::PathBuf;

/// Copy the zig builtins source files into each of the test platform folders.
///
/// This is a simple way to use these files without creating a zig package,
/// to support the upgrade from zig 0.11.0 to 0.13.0, and keep the scope of the change smaller.
///
/// Once we have zig 0.13.0, and also removed platform/host rebuilding, we will need to upgrade the
/// test platforms so they produce thier own host binaries, and this will be the best time to
/// upgrade to a zig package and can also be done incrementally withouth changing all the platforms in one go.
pub fn copy_zig_glue() {
    let workspace_dir = roc_test_utils_dir::workspace_root();
    let zig_builtins_source_dir = workspace_dir.join("crates/compiler/builtins/bitcode/src");

    let zig_test_platforms_dirs: Vec<PathBuf> = vec![
        workspace_dir.join("crates/cli/tests/algorithms/fibonacci-platform/glue"),
        workspace_dir.join("crates/cli/tests/algorithms/quicksort-platform/glue"),
        workspace_dir.join("crates/cli/tests/benchmarks/platform/glue"),
        workspace_dir.join("crates/cli/tests/expects/zig-platform/glue"),
        workspace_dir.join("crates/cli/tests/fixtures/multi-dep-str/platform/glue"),
        workspace_dir.join("crates/cli/tests/fixtures/multi-dep-thunk/platform/glue"),
        workspace_dir.join("crates/cli/tests/fixtures/packages/platform/glue"),
        workspace_dir.join("crates/valgrind/zig-platform/glue"),
        workspace_dir.join("examples/cli/effects-platform/glue"),
        workspace_dir.join("examples/cli/tui-platform/glue"),
        workspace_dir.join("examples/platform-switching/zig-platform/glue"),
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
