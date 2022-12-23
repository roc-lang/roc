fn main() {
    // goal: build the platform, so tests can use `precompiled-platform=true`

    // first, make sure the roc binary is up to data
    // cli_utils::helpers::build_roc_bin_cached();

    // next, we just compile one of our examples
    let roc_path = std::path::Path::new("../../target/debug/roc");
    let out = cli_utils::helpers::run_roc_with_stdin_and_env(
        roc_path,
        &["run", "tests/str_concat_1.roc"],
        &[],
        &[],
    );

    if !out.status.success() {
        panic!(
            "Building platform with `{:?}` failed!\n\n{}\n\n{}",
            out.cmd_str, out.stdout, out.stderr
        );
    }
}
