#![cfg(test)]
#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
// we actually want to compare against the literal float bits
#![allow(clippy::clippy::float_cmp)]

#[macro_use]
extern crate pretty_assertions;

use test_mono_macros::*;

use roc_can::builtins::builtin_defs_map;
use roc_collections::all::MutMap;
use roc_module::symbol::Symbol;
use roc_mono::ir::Proc;

use roc_mono::ir::TopLevelFunctionLayout;

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

fn compiles_to_ir(test_name: &str, src: &str) {
    use bumpalo::Bump;
    use std::path::{Path, PathBuf};

    let arena = &Bump::new();

    // let stdlib = roc_builtins::unique::uniq_stdlib();
    let stdlib = roc_builtins::std::standard_stdlib();
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
        &module_src,
        &stdlib,
        src_dir,
        exposed_types,
        8,
        builtin_defs_map,
    );

    let mut loaded = match loaded {
        Ok(x) => x,
        Err(roc_load::file::LoadingProblem::FormattedReport(report)) => {
            println!("{}", report);
            panic!();
        }
        Err(e) => panic!("{:?}", e),
    };

    use roc_load::file::MonomorphizedModule;
    let MonomorphizedModule {
        module_id: home,
        procedures,
        exposed_to_host,
        ..
    } = loaded;

    let can_problems = loaded.can_problems.remove(&home).unwrap_or_default();
    let type_problems = loaded.type_problems.remove(&home).unwrap_or_default();
    let mono_problems = loaded.mono_problems.remove(&home).unwrap_or_default();

    if !can_problems.is_empty() {
        println!("Ignoring {} canonicalization problems", can_problems.len());
    }

    assert_eq!(type_problems, Vec::new());
    assert_eq!(mono_problems, Vec::new());

    debug_assert_eq!(exposed_to_host.len(), 1);

    let main_fn_symbol = exposed_to_host.keys().copied().next().unwrap();

    verify_procedures(test_name, procedures, main_fn_symbol);
}

#[cfg(debug_assertions)]
fn verify_procedures(
    test_name: &str,
    procedures: MutMap<(Symbol, TopLevelFunctionLayout<'_>), Proc<'_>>,
    main_fn_symbol: Symbol,
) {
    let index = procedures
        .keys()
        .position(|(s, _)| *s == main_fn_symbol)
        .unwrap();

    let mut procs_string = procedures
        .values()
        .map(|proc| proc.to_pretty(200))
        .collect::<Vec<_>>();

    let main_fn = procs_string.swap_remove(index);

    procs_string.sort();
    procs_string.push(main_fn);

    let result = procs_string.join("\n");

    let path = format!("generated/{}.txt", test_name);
    std::fs::create_dir_all("generated").unwrap();
    std::fs::write(&path, result).unwrap();

    use std::process::Command;
    let is_tracked = Command::new("git")
        .args(&["ls-files", "--error-unmatch", &path])
        .output()
        .unwrap();

    if !is_tracked.status.success() {
        panic!(
            "The file {:?} is not tracked by git. Try using `git add` on it",
            &path
        );
    }

    let has_changes = Command::new("git")
        .args(&["diff", "--color=always", &path])
        .output()
        .unwrap();

    if !has_changes.status.success() {
        eprintln!("`git diff {:?}` failed", &path);
        unreachable!();
    }

    if !has_changes.stdout.is_empty() {
        println!("{}", std::str::from_utf8(&has_changes.stdout).unwrap());
        panic!("Output changed: resolve conflicts and `git add` the file.");
    }
}

// NOTE because the Show instance of module names is different in --release mode,
// these tests would all fail. In the future, when we do interesting optimizations,
// we'll likely want some tests for --release too.
#[cfg(not(debug_assertions))]
fn verify_procedures(
    _expected: &str,
    _procedures: MutMap<(Symbol, TopLevelFunctionLayout<'_>), Proc<'_>>,
    _main_fn_symbol: Symbol,
) {
    // Do nothing
}

#[mono_test]
fn ir_int_literal() {
    r#"
    5
    "#
}

#[mono_test]
fn ir_int_add() {
    r#"
    x = [ 1,2 ]
    5 + 4 + 3 + List.len x
    "#
}
